use std::error::Error;

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
    },
    request::{GotoDefinition, HoverRequest, References},
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, GotoDefinitionResponse, Hover,
    HoverContents, HoverProviderCapability, InitializeParams, Location, MarkupContent, MarkupKind,
    OneOf, PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};

use lelwel::{frontend::*, ide::*};

macro_rules! request_match {
    ( $req_ty:ty, $server:expr, $connection:expr, $req:expr ) => {
        match cast_request::<$req_ty>($req) {
            Ok((id, params)) => {
                let resp = <$req_ty>::handle(&$server, id, params);
                $connection.sender.send(Message::Response(resp))?;
                continue;
            }
            Err(req) => req,
        }
    };
}

macro_rules! notification_match {
    ( $noti_ty:ty, $server:expr, $connection:expr, $noti:expr ) => {
        match cast_notification::<$noti_ty>($noti) {
            Ok(params) => {
                if let Some(resp) = <$noti_ty>::handle(&mut $server, params) {
                    $connection.sender.send(Message::Notification(resp))?;
                }
                continue;
            }
            Err(noti) => noti,
        }
    };
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("starting lelwel language server");
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        ..Default::default()
    })
    .unwrap();

    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(&connection, initialization_params)?;
    io_threads.join()?;

    eprintln!("shutting down lelwel language server");
    Ok(())
}

fn main_loop(
    connection: &Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    let mut server = Server::new();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                let req = request_match!(HoverRequest, server, connection, req);
                let req = request_match!(GotoDefinition, server, connection, req);
                let _ = request_match!(References, server, connection, req);
            }
            Message::Notification(noti) => {
                let noti = notification_match!(DidChangeTextDocument, server, connection, noti);
                let noti = notification_match!(DidOpenTextDocument, server, connection, noti);
                let _ = notification_match!(DidCloseTextDocument, server, connection, noti);
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

trait RequestHandler: lsp_types::request::Request {
    fn handle(server: &Server, id: RequestId, params: Self::Params) -> Response;
}

impl RequestHandler for HoverRequest {
    fn handle(server: &Server, id: RequestId, params: Self::Params) -> Response {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = token::Position::from(params.text_document_position_params.position);
        if let Some((text, range)) = server.hover(uri.path(), pos) {
            let result = Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: text,
                }),
                range: Some(Range::from(range)),
            });
            let result = serde_json::to_value(&result).unwrap();
            Response::new_ok(id, result)
        } else {
            Response::new_ok(id, serde_json::Value::Null)
        }
    }
}

impl RequestHandler for GotoDefinition {
    fn handle(server: &Server, id: RequestId, params: Self::Params) -> Response {
        let uri = params.text_document_position_params.text_document.uri;
        let range = server.goto_def(
            uri.path(),
            token::Position::from(params.text_document_position_params.position),
        );
        if let Some(range) = range {
            let range = Range::from(range);
            let loc = Location::new(uri, range);
            let result = Some(GotoDefinitionResponse::Scalar(loc));
            let result = serde_json::to_value(&result).unwrap();
            Response::new_ok(id, result)
        } else {
            Response::new_ok(id, serde_json::Value::Null)
        }
    }
}

impl RequestHandler for References {
    fn handle(server: &Server, id: RequestId, params: Self::Params) -> Response {
        let uri = params.text_document_position.text_document.uri;
        let ranges = server.references(
            uri.path(),
            token::Position::from(params.text_document_position.position),
            params.context.include_declaration,
        );
        if let Some(ranges) = ranges {
            let mut locs = vec![];
            for range in ranges {
                locs.push(Location::new(uri.clone(), Range::from(range)));
            }
            let result = serde_json::to_value(&locs).unwrap();
            Response::new_ok(id, result)
        } else {
            Response::new_ok(id, serde_json::Value::Null)
        }
    }
}

trait NotificationHandler: lsp_types::notification::Notification {
    fn handle(server: &mut Server, params: Self::Params) -> Option<Notification>;
}

impl NotificationHandler for DidOpenTextDocument {
    fn handle(server: &mut Server, params: Self::Params) -> Option<Notification> {
        let url = params.text_document.uri;
        eprintln!("opened document {:?}", url.path());
        let diag = server.analyze(url.path(), params.text_document.text);
        Some(generate_diagnostics(url, diag))
    }
}

impl NotificationHandler for DidChangeTextDocument {
    fn handle(server: &mut Server, params: Self::Params) -> Option<Notification> {
        let url = params.text_document.uri;
        let diag = server.analyze(
            url.path(),
            params.content_changes.into_iter().next().unwrap().text,
        );
        Some(generate_diagnostics(url, diag))
    }
}

impl NotificationHandler for DidCloseTextDocument {
    fn handle(server: &mut Server, params: Self::Params) -> Option<Notification> {
        let url = params.text_document.uri;
        eprintln!("closed document {:?}", url.path());
        server.close(url.path());
        None
    }
}

fn to_lsp_related(range: &token::Range, url: &Url, msg: &str) -> DiagnosticRelatedInformation {
    DiagnosticRelatedInformation {
        location: Location::new(url.clone(), Range::from(*range)),
        message: msg.to_string(),
    }
}

fn generate_diagnostics(url: Url, diag: diag::Diag) -> Notification {
    let mut diagnostics = vec![];
    for e in diag.error_iter() {
        let related = e
            .related()
            .iter()
            .map(|(r, m)| to_lsp_related(r, &url, m))
            .collect::<Vec<_>>();
        for r in related.iter() {
            diagnostics.push(Diagnostic::new(
                r.location.range,
                Some(DiagnosticSeverity::HINT),
                None,
                None,
                r.message.clone(),
                None,
                None,
            ));
        }
        diagnostics.push(Diagnostic::new(
            Range::from(e.range()),
            Some(DiagnosticSeverity::ERROR),
            None,
            None,
            e.code().to_string(),
            Some(related),
            None,
        ));
    }
    for e in diag.warning_iter() {
        diagnostics.push(Diagnostic::new(
            Range::from(e.range()),
            Some(DiagnosticSeverity::WARNING),
            None,
            None,
            e.code().to_string(),
            None,
            None,
        ));
    }
    let result = PublishDiagnosticsParams::new(url, diagnostics, None);
    let params = serde_json::to_value(&result).unwrap();
    let method = <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string();
    Notification { method, params }
}

fn cast_request<R>(r: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    r.extract(R::METHOD)
}

fn cast_notification<N>(n: Notification) -> Result<N::Params, Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    n.extract(N::METHOD)
}
