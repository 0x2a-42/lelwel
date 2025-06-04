#![cfg(feature = "lsp")]

use std::error::Error;

use lelwel::ide::Cache;
use lsp_server::{Connection, ExtractError, Message, Notification, RequestId, Response};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
    },
    request::{Completion, GotoDefinition, HoverRequest, References},
    CompletionOptions, GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability,
    InitializeParams, MarkupContent, MarkupKind, OneOf, PublishDiagnosticsParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

macro_rules! request_match {
    ( $req_ty:ty, $cache:expr, $connection:expr, $req:expr ) => {
        match cast_request::<$req_ty>($req) {
            Ok((id, params)) => {
                let result = <$req_ty>::handle(&mut $cache, params);
                let result = serde_json::to_value(&result).unwrap();
                let resp = Response::new_ok(id, result);
                $connection.sender.send(Message::Response(resp))?;
                continue;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(req)) => req,
        }
    };
}

macro_rules! notification_match {
    ( $noti_ty:ty, $cache:expr, $connection:expr, $noti:expr ) => {
        match cast_notification::<$noti_ty>($noti) {
            Ok(params) => {
                if let Some(resp) = <$noti_ty>::handle(&mut $cache, params) {
                    $connection.sender.send(Message::Notification(resp))?;
                }
                continue;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(req)) => req,
        }
    };
}

trait RequestHandler: lsp_types::request::Request {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result;
}
trait NotificationHandler: lsp_types::notification::Notification {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification>;
}

fn cast_request<R>(
    req: lsp_server::Request,
) -> Result<(RequestId, R::Params), ExtractError<lsp_server::Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
fn cast_notification<N>(noti: Notification) -> Result<N::Params, ExtractError<Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    noti.extract(N::METHOD)
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("Starting lelwel LSP server");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions::default()),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    eprintln!("Shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut cache = lelwel::ide::Cache::default();
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                request_match!(HoverRequest, cache, connection, req.clone());
                request_match!(GotoDefinition, cache, connection, req.clone());
                request_match!(References, cache, connection, req.clone());
                request_match!(Completion, cache, connection, req.clone());
            }
            Message::Response(_resp) => {}
            Message::Notification(noti) => {
                notification_match!(DidChangeTextDocument, cache, connection, noti.clone());
                notification_match!(DidOpenTextDocument, cache, connection, noti.clone());
                notification_match!(DidCloseTextDocument, cache, connection, noti.clone());
            }
        }
    }
    Ok(())
}

impl RequestHandler for HoverRequest {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        if let Some((msg, range)) = cache.hover(&uri, pos) {
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: msg,
                }),
                range: Some(range),
            })
        } else {
            None
        }
    }
}

impl RequestHandler for GotoDefinition {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        cache
            .goto_definition(&uri, pos)
            .map(GotoDefinitionResponse::Scalar)
    }
}

impl RequestHandler for References {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let with_decl = params.context.include_declaration;
        let locs = cache.references(&uri, pos, with_decl);
        Some(locs)
    }
}

impl RequestHandler for Completion {
    fn handle(cache: &mut Cache, params: Self::Params) -> Self::Result {
        cache.completion(params)
    }
}

impl NotificationHandler for DidOpenTextDocument {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification> {
        let uri = params.text_document.uri;
        eprintln!("opened document {:?}", uri);
        let text = params.text_document.text;
        let diagnostics = {
            cache.invalidate(&uri);
            cache.analyze(uri.clone(), text);
            cache.get_diagnostics(&uri)
        };
        let result = PublishDiagnosticsParams::new(uri, diagnostics, None);
        let params = serde_json::to_value(&result).unwrap();
        let method =
            <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string();
        Some(Notification { method, params })
    }
}

impl NotificationHandler for DidChangeTextDocument {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification> {
        let uri = params.text_document.uri;
        let text = params.content_changes.into_iter().next().unwrap().text;
        let diagnostics = {
            cache.invalidate(&uri);
            cache.analyze(uri.clone(), text);
            cache.get_diagnostics(&uri)
        };
        let result = PublishDiagnosticsParams::new(uri, diagnostics, None);
        let params = serde_json::to_value(&result).unwrap();
        let method =
            <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string();
        Some(Notification { method, params })
    }
}

impl NotificationHandler for DidCloseTextDocument {
    fn handle(cache: &mut Cache, params: Self::Params) -> Option<Notification> {
        let uri = params.text_document.uri;
        eprintln!("closed document {:?}", uri);
        cache.invalidate(&uri);
        None
    }
}
