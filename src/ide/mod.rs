#![cfg(feature = "lsp")]

use crate::{Parser, SemanticPass, Span};
use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::files::SimpleFile;
use lsp_types::*;
use rustc_hash::FxHashMap;
use std::sync::mpsc;
use std::thread::JoinHandle;

use self::completion::*;
use self::hover::*;
use self::lookup::*;

mod completion;
mod hover;
mod lookup;

struct Analyzer {
    handle: JoinHandle<()>,
    req_tx: mpsc::Sender<Request>,
    noti_rx: mpsc::Receiver<Notification>,
}

#[derive(Default)]
pub struct Cache {
    analyzers: FxHashMap<Url, Analyzer>,
}

impl Cache {
    pub fn analyze(&mut self, uri: Url, text: String) {
        let (req_tx, req_rx) = mpsc::channel::<Request>();
        let (noti_tx, noti_rx) = mpsc::channel::<Notification>();
        let key = uri.clone();
        let handle = std::thread::spawn(move || analyze(uri, text, req_rx, noti_tx));

        self.analyzers.insert(
            key,
            Analyzer {
                handle,
                req_tx,
                noti_rx,
            },
        );
    }
    pub fn invalidate(&mut self, uri: &Url) {
        if let Some(analyzer) = self.analyzers.remove(uri) {
            analyzer.req_tx.send(Request::Cancel).unwrap();
            analyzer.handle.join().unwrap();
        }
    }
    pub fn get_diagnostics(&mut self, uri: &Url) -> Vec<Diagnostic> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Diagnostic).unwrap();
        if let Ok(Notification::PublishDiagnostics(items)) = analyzer.noti_rx.recv() {
            items
        } else {
            vec![]
        }
    }
    pub fn hover(&mut self, uri: &Url, pos: Position) -> Option<(String, Range)> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Hover(pos)).unwrap();
        if let Ok(Notification::Hover(hover)) = analyzer.noti_rx.recv() {
            hover
        } else {
            None
        }
    }
    pub fn goto_definition(&mut self, uri: &Url, pos: Position) -> Option<Location> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::GotoDefinition(pos)).unwrap();
        if let Ok(Notification::GotoDefinition(location)) = analyzer.noti_rx.recv() {
            location
        } else {
            None
        }
    }
    pub fn references(&mut self, uri: &Url, pos: Position, with_def: bool) -> Vec<Location> {
        let analyzer = self.analyzers.get_mut(uri).unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer
            .req_tx
            .send(Request::References(pos, with_def))
            .unwrap();
        if let Ok(Notification::References(ranges)) = analyzer.noti_rx.recv() {
            ranges
        } else {
            vec![]
        }
    }
    pub fn completion(&mut self, params: CompletionParams) -> Option<CompletionResponse> {
        let analyzer = self
            .analyzers
            .get_mut(&params.text_document_position.text_document.uri)
            .unwrap();
        assert!(!analyzer.handle.is_finished());
        analyzer.req_tx.send(Request::Completion(params)).unwrap();
        if let Ok(Notification::Completion(resp)) = analyzer.noti_rx.recv() {
            resp
        } else {
            None
        }
    }
}

enum Request {
    Diagnostic,
    Hover(Position),
    GotoDefinition(Position),
    References(Position, bool),
    Completion(CompletionParams),
    Cancel,
}

enum Notification {
    PublishDiagnostics(Vec<Diagnostic>),
    Hover(Option<(String, Range)>),
    GotoDefinition(Option<Location>),
    References(Vec<Location>),
    Completion(Option<CompletionResponse>),
}

fn analyze(
    uri: Url,
    source: String,
    req: mpsc::Receiver<Request>,
    noti: mpsc::Sender<Notification>,
) {
    let path = uri.to_file_path().unwrap();
    let parser_path = path.parent().unwrap().join("parser.rs");
    let mut diags = vec![];

    let cst = Parser::new(&source, &mut diags).parse(&mut diags);
    let sema = SemanticPass::run(&cst, &mut diags);
    let file = SimpleFile::new(path.to_str().unwrap(), source.as_str());

    while let Ok(req) = req.recv() {
        match req {
            Request::Diagnostic => {
                let mut diags = diags
                    .iter()
                    .map(|diag| to_lsp_diag(&file, &uri, diag))
                    .collect::<Vec<_>>();
                let mut hints = related_as_hints(&diags);
                diags.append(&mut hints);
                noti.send(Notification::PublishDiagnostics(diags)).unwrap();
            }
            Request::Hover(pos) => {
                let pos = compat::position_to_offset(&file, &pos);
                let res = hover(&cst, &sema, pos)
                    .map(|(msg, span)| (msg, compat::span_to_range(&file, &span)));
                noti.send(Notification::Hover(res)).unwrap();
            }
            Request::GotoDefinition(pos) => {
                let pos = compat::position_to_offset(&file, &pos);
                let location = lookup_definition(&cst, &sema, pos, &uri, &file, &parser_path);
                noti.send(Notification::GotoDefinition(location)).unwrap();
            }
            Request::References(pos, with_def) => {
                let pos = compat::position_to_offset(&file, &pos);
                let ranges = lookup_references(&cst, &sema, pos, with_def)
                    .into_iter()
                    .map(|node| {
                        Location::new(uri.clone(), compat::span_to_range(&file, &cst.span(node)))
                    })
                    .collect();
                noti.send(Notification::References(ranges)).unwrap();
            }
            Request::Completion(params) => {
                let pos =
                    compat::position_to_offset(&file, &params.text_document_position.position);
                noti.send(Notification::Completion(completion(&cst, pos, &sema)))
                    .unwrap();
            }
            Request::Cancel => {
                return;
            }
        }
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

    let mut message = diag.message.clone();
    if let Some(primary_label_message) = diag.labels.iter().find_map(|label| {
        (label.style == LabelStyle::Primary && !label.message.is_empty())
            .then_some(label.message.as_str())
    }) {
        // append message of primary label if it is not empty
        message.push(' ');
        message.push_str(primary_label_message);
    }

    Diagnostic::new(
        diag.labels
            .first()
            .map_or(lsp_types::Range::default(), |label| {
                compat::span_to_range(file, &label.range)
            }),
        Some(match diag.severity {
            Severity::Error | Severity::Bug => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            _ => DiagnosticSeverity::HINT,
        }),
        diag.code.clone().map(NumberOrString::String),
        None,
        message,
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

/// required functions due to different versions of lsp-types in codespan
mod compat {
    use crate::Span;
    use codespan_reporting::files::SimpleFile;

    pub fn position_to_offset(file: &SimpleFile<&str, &str>, pos: &lsp_types::Position) -> usize {
        codespan_lsp::position_to_byte_index(
            file,
            (),
            &lsp_types_old::Position::new(pos.line, pos.character),
        )
        .unwrap()
    }

    pub fn span_to_range(file: &SimpleFile<&str, &str>, span: &Span) -> lsp_types::Range {
        let range = codespan_lsp::byte_span_to_range(file, (), span.clone()).unwrap();
        lsp_types::Range::new(
            lsp_types::Position::new(range.start.line, range.start.character),
            lsp_types::Position::new(range.end.line, range.end.character),
        )
    }
}
