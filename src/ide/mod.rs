#![cfg(feature = "lsp")]

use crate::frontend::ast::{AstNode, Regex, RuleDecl};
use crate::{tokenize, Cst, NodeRef, Parser, Rule, SemanticData, SemanticPass, Token};
use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::files::SimpleFile;
use logos::{Logos, Span};
use std::collections::HashMap;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use tower_lsp::lsp_types::*;

use self::lookup::*;

mod lookup;

struct Analyzer {
    handle: JoinHandle<()>,
    req_tx: mpsc::Sender<Request>,
    noti_rx: mpsc::Receiver<Notification>,
}

#[derive(Default)]
pub struct Cache {
    analyzers: HashMap<Url, Analyzer>,
}

impl Cache {
    pub fn analyze(&mut self, uri: &Url, text: String) {
        let (req_tx, req_rx) = mpsc::channel::<Request>(32);
        let (noti_tx, noti_rx) = mpsc::channel::<Notification>(32);
        let handle = tokio::spawn(analyze(uri.clone(), text, req_rx, noti_tx));

        self.analyzers.insert(
            uri.clone(),
            Analyzer {
                handle,
                req_tx,
                noti_rx,
            },
        );
    }
    pub fn invalidate(&self, uri: &Url) {
        if self.analyzers.contains_key(uri) {
            self.analyzers[uri].handle.abort();
        }
    }
    pub async fn get_diagnostics(&mut self, uri: &Url) -> Vec<Diagnostic> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Diagnostic).await.unwrap();
        if let Some(Notification::PublishDiagnostics(items)) = analyzer.noti_rx.recv().await {
            items
        } else {
            vec![]
        }
    }
    pub async fn hover(&mut self, uri: &Url, pos: Position) -> Option<(String, Range)> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Hover(pos)).await.unwrap();
        if let Some(Notification::Hover(hover)) = analyzer.noti_rx.recv().await {
            hover
        } else {
            None
        }
    }
    pub async fn goto_definition(&mut self, uri: &Url, pos: Position) -> Option<Range> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer
            .req_tx
            .send(Request::GotoDefinition(pos))
            .await
            .unwrap();
        if let Some(Notification::GotoDefinition(hover)) = analyzer.noti_rx.recv().await {
            hover
        } else {
            None
        }
    }
    pub async fn references(&mut self, uri: &Url, pos: Position, with_def: bool) -> Vec<Location> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer
            .req_tx
            .send(Request::References(pos, with_def))
            .await
            .unwrap();
        if let Some(Notification::References(ranges)) = analyzer.noti_rx.recv().await {
            ranges
        } else {
            vec![]
        }
    }
}

enum Request {
    Diagnostic,
    Hover(Position),
    GotoDefinition(Position),
    References(Position, bool),
}

enum Notification {
    PublishDiagnostics(Vec<Diagnostic>),
    Hover(Option<(String, Range)>),
    GotoDefinition(Option<Range>),
    References(Vec<Location>),
}

async fn analyze(
    uri: Url,
    source: String,
    mut req: mpsc::Receiver<Request>,
    noti: mpsc::Sender<Notification>,
) {
    let path = uri.as_str();
    let mut diags = vec![];

    let (tokens, ranges) = tokenize(Token::lexer(&source), &mut diags);
    let cst = Parser::parse(&source, tokens, ranges, &mut diags);
    let sema = SemanticPass::run(&cst, &mut diags);
    let file = SimpleFile::new(path, source.as_str());

    while let Some(req) = req.recv().await {
        match req {
            Request::Diagnostic => {
                let mut diags = diags
                    .iter()
                    .map(|diag| to_lsp_diag(&file, &uri, diag))
                    .collect::<Vec<_>>();
                let mut hints = related_as_hints(&diags);
                diags.append(&mut hints);
                noti.send(Notification::PublishDiagnostics(diags))
                    .await
                    .unwrap();
            }
            Request::Hover(pos) => {
                let pos = compat::position_to_offset(&file, &pos);
                let res = hover(&cst, &sema, pos)
                    .map(|(msg, span)| (msg, compat::span_to_range(&file, &span)));
                noti.send(Notification::Hover(res)).await.unwrap();
            }
            Request::GotoDefinition(pos) => {
                let pos = compat::position_to_offset(&file, &pos);
                let range = lookup_definition(&cst, &sema, pos)
                    .map(|node| compat::span_to_range(&file, &cst.get_span(node).unwrap()));
                noti.send(Notification::GotoDefinition(range))
                    .await
                    .unwrap();
            }
            Request::References(pos, with_def) => {
                let pos = compat::position_to_offset(&file, &pos);
                let ranges = lookup_references(&cst, &sema, pos, with_def)
                    .into_iter()
                    .map(|node| {
                        Location::new(
                            uri.clone(),
                            compat::span_to_range(&file, &cst.get_span(node).unwrap()),
                        )
                    })
                    .collect();
                noti.send(Notification::References(ranges)).await.unwrap();
            }
        }
    }
    while let Some(req) = req.recv().await {
        if let Request::Diagnostic = req {
            let mut diags = diags
                .iter()
                .map(|diag| to_lsp_diag(&file, &uri, diag))
                .collect::<Vec<_>>();
            let mut hints = related_as_hints(&diags);
            diags.append(&mut hints);
            noti.send(Notification::PublishDiagnostics(diags))
                .await
                .unwrap();
        }
    }
}

fn hover(cst: &Cst, sema: &SemanticData, pos: usize) -> Option<(String, Span)> {
    let node = lookup_node(cst, NodeRef::ROOT, pos)?;
    let span = cst.get_span(node)?;

    if let Some(regex) = Regex::cast(cst, node) {
        let first = &sema
            .first_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let follow = &sema
            .follow_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let predict = &sema
            .predict_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));

        match regex {
            Regex::Star(_) | Regex::Plus(_) => {
                let recovery = &sema
                    .recovery_sets
                    .get(&regex.syntax())
                    .map_or("{}".to_string(), |s| format!("{s:?}"));
                Some((format!(
                    "**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n**Recovery:** {recovery}\n"
                ), span))
            }
            Regex::Name(_) | Regex::Symbol(_) => {
                let comment_attached_node = sema
                    .decl_bindings
                    .get(&node)
                    .and_then(|decl| cst.get_span(*decl))
                    .and_then(|span| {
                        find_node(cst, NodeRef::ROOT, span.start, |r| {
                            r == Rule::TokenList || r == Rule::RuleDecl
                        })
                    });
                let mut comment_nodes = vec![];
                if let Some(comment_attached_node) = comment_attached_node {
                    for i in 1.. {
                        if let Some(node) = cst.get_token(
                            NodeRef(comment_attached_node.0.saturating_sub(i)),
                            Token::DocComment,
                        ) {
                            comment_nodes.push(node);
                        } else {
                            break;
                        }
                    }
                }
                let mut comment = String::new();
                for (val, _) in comment_nodes.iter().rev() {
                    comment.push_str(val.strip_prefix("///").unwrap().trim_start());
                }
                if !comment.is_empty() {
                    comment.push_str("---\n")
                }
                Some((
                    format!("{comment}**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n"),
                    span,
                ))
            }
            _ => Some((
                format!("**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n"),
                span,
            )),
        }
    } else if let Some(rule) = RuleDecl::cast(cst, node) {
        let regex = rule.regex(cst)?;
        let first = &sema
            .first_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let follow = &sema
            .follow_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let predict = &sema
            .predict_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let pattern = sema
            .patterns
            .get(&rule)
            .map_or("None".to_string(), |pattern| format!("{pattern:?}"));
        Some((format!("**Pattern:** {pattern}\n**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n"), span))
    } else {
        None
    }
}

fn to_lsp_related(
    file: &SimpleFile<&str, &str>,
    span: &Span,
    uri: &Url,
    msg: &str,
) -> DiagnosticRelatedInformation {
    DiagnosticRelatedInformation {
        location: Location::new(uri.clone(), compat::span_to_range(file, span)),
        message: msg.to_string(),
    }
}

fn to_lsp_diag(
    file: &SimpleFile<&str, &str>,
    uri: &Url,
    diag: &super::frontend::parser::Diagnostic,
) -> Diagnostic {
    let related = diag
        .labels
        .iter()
        .filter_map(|label| {
            if label.style == LabelStyle::Secondary {
                Some(to_lsp_related(file, &label.range, uri, &label.message))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    Diagnostic::new(
        diag.labels
            .first()
            .map_or(tower_lsp::lsp_types::Range::default(), |label| {
                compat::span_to_range(file, &label.range)
            }),
        Some(match diag.severity {
            Severity::Error | Severity::Bug => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            _ => DiagnosticSeverity::HINT,
        }),
        diag.code.clone().map(NumberOrString::String),
        None,
        diag.message.clone(),
        Some(related),
        None,
    )
}
fn related_as_hints(diags: &[Diagnostic]) -> Vec<Diagnostic> {
    let mut hints = vec![];
    for diag in diags.iter() {
        if let Some(related) = &diag.related_information {
            for r in related.iter() {
                hints.push(Diagnostic::new(
                    r.location.range,
                    Some(DiagnosticSeverity::HINT),
                    diag.code.clone(),
                    None,
                    r.message.clone(),
                    None,
                    None,
                ));
            }
        }
    }
    hints
}

/// required functions due to different versions of lsp-types in codespan and tower-lsp
mod compat {
    use crate::Span;
    use codespan_reporting::files::SimpleFile;

    pub fn position_to_offset(
        file: &SimpleFile<&str, &str>,
        pos: &tower_lsp::lsp_types::Position,
    ) -> usize {
        codespan_lsp::position_to_byte_index(
            file,
            (),
            &lsp_types::Position::new(pos.line, pos.character),
        )
        .unwrap()
    }

    pub fn span_to_range(
        file: &SimpleFile<&str, &str>,
        span: &Span,
    ) -> tower_lsp::lsp_types::Range {
        let range = codespan_lsp::byte_span_to_range(file, (), span.clone()).unwrap();
        tower_lsp::lsp_types::Range::new(
            tower_lsp::lsp_types::Position::new(range.start.line, range.start.character),
            tower_lsp::lsp_types::Position::new(range.end.line, range.end.character),
        )
    }
}
