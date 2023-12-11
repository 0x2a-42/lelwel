#![cfg(feature = "lsp")]

use crate::frontend::ast::{Element, ElementKind, Module, Node, Regex, RegexKind};
use crate::frontend::parser::{Parser, Token, TokenStream};
use crate::frontend::sema::SemanticPass;
use crate::frontend::symbols::StringInterner;
use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::files::SimpleFile;
use logos::{Logos, Span};
use std::collections::HashMap;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use tower_lsp::lsp_types::*;

use self::lookup::{LookupDefinition, LookupNode, LookupReferences};

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
            self.analyzers[&uri].handle.abort();
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
    content: String,
    mut req: mpsc::Receiver<Request>,
    noti: mpsc::Sender<Notification>,
) {
    let path = uri.as_str();
    let mut tokens = TokenStream::new(Token::lexer(&content), path);

    let file = SimpleFile::new(path, &content);

    let mut diags = vec![];
    let mut interner = StringInterner::new();
    if let Some(mut module) = Parser::parse(&mut tokens, &mut diags, &mut interner) {
        SemanticPass::run(&mut module, &mut diags);
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
                    let res = hover(&module, pos)
                        .map(|(msg, span)| (msg, compat::span_to_range(&file, &span)));
                    noti.send(Notification::Hover(res)).await.unwrap();
                }
                Request::GotoDefinition(pos) => {
                    let pos = compat::position_to_offset(&file, &pos);
                    let range = LookupDefinition::find(&module, pos)
                        .map(|elem| compat::span_to_range(&file, &elem.span));
                    noti.send(Notification::GotoDefinition(range))
                        .await
                        .unwrap();
                }
                Request::References(pos, with_def) => {
                    let pos = compat::position_to_offset(&file, &pos);
                    let ranges = LookupReferences::find(&module, pos, with_def)
                        .iter()
                        .map(|span| Location::new(uri.clone(), compat::span_to_range(&file, span)))
                        .collect();
                    noti.send(Notification::References(ranges)).await.unwrap();
                }
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

fn hover(module: &Module, pos: usize) -> Option<(String, Span)> {
    let hover_ret_pars = |ret: &str, pars: &str| {
        format!(
            "---\n**Return:** `{}`  \n**Parameters:** `{}`",
            if ret.is_empty() { " " } else { ret },
            if pars.is_empty() { " " } else { pars },
        )
    };
    let hover_element = |element: &Element| match element.kind {
        ElementKind::Rule {
            regex, ret, pars, ..
        }
        | ElementKind::Start {
            regex, ret, pars, ..
        } => {
            let doc = if !element.doc.is_empty() {
                format!("---\n{}", element.doc)
            } else {
                "".to_string()
            };
            let regex = module.get_regex(regex).unwrap();
            let doc = format!(
                "**First:** {:?}  \n**Follow:** {:?}\n{}\n{}",
                regex.first,
                regex.follow,
                hover_ret_pars(ret, pars),
                doc,
            );
            Some((doc, element.span.clone()))
        }
        _ => None,
    };
    match LookupNode::find(module, pos)? {
        Node::Element(element) => hover_element(element),
        Node::Regex(regex) => match regex {
            Regex {
                kind:
                    RegexKind::Id { elem, .. }
                    | RegexKind::Str { elem, .. }
                    | RegexKind::Predicate { elem, .. }
                    | RegexKind::Action { elem, .. },
                ..
            } => {
                let doc = if let Some(element) = module.get_element(*elem) {
                    if !element.doc.is_empty() {
                        format!("---\n{}", element.doc)
                    } else {
                        "".to_string()
                    }
                } else {
                    "".to_string()
                };
                Some((
                    format!(
                        "**First:** {:?}  \n**Follow:** {:?}\n{}",
                        regex.first, regex.follow, doc,
                    ),
                    regex.span.clone(),
                ))
            }
            Regex {
                kind: RegexKind::ErrorHandler { .. },
                ..
            } => Some((
                format!(
                    "**Follow:** {:?}  \n**Cancel:** {:?}\n",
                    regex.follow, regex.cancel
                ),
                regex.span.clone(),
            )),
            _ => Some((
                format!(
                    "**First:** {:?}  \n**Follow:** {:?}\n",
                    regex.first, regex.follow
                ),
                regex.span.clone(),
            )),
        },
    }
}

fn to_lsp_related(
    file: &SimpleFile<&str, &String>,
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
    file: &SimpleFile<&str, &String>,
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
        compat::span_to_range(file, &diag.labels[0].range),
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
    use crate::frontend::ast::Span;
    use codespan_reporting::files::SimpleFile;

    pub fn position_to_offset(
        file: &SimpleFile<&str, &String>,
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
        file: &SimpleFile<&str, &String>,
        span: &Span,
    ) -> tower_lsp::lsp_types::Range {
        let range = codespan_lsp::byte_span_to_range(file, (), span.clone()).unwrap();
        tower_lsp::lsp_types::Range::new(
            tower_lsp::lsp_types::Position::new(range.start.line, range.start.character),
            tower_lsp::lsp_types::Position::new(range.end.line, range.end.character),
        )
    }
}
