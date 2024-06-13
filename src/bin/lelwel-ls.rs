#![cfg(feature = "lsp")]

use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache: RwLock::new(lelwel::ide::Cache::default()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}

struct Backend {
    client: Client,
    cache: RwLock<lelwel::ide::Cache>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
        })
    }
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let diagnostics = {
            let mut cache = self.cache.write().await;
            cache.analyze(&uri, text);
            cache.get_diagnostics(&uri).await
        };
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.content_changes.into_iter().next().unwrap().text;
        let diagnostics = {
            let mut cache = self.cache.write().await;
            cache.invalidate(&uri);
            cache.analyze(&uri, text);
            cache.get_diagnostics(&uri).await
        };
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.cache
            .write()
            .await
            .invalidate(&params.text_document.uri);
    }
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        if let Some((msg, range)) = self.cache.write().await.hover(&uri, pos).await {
            Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: msg,
                }),
                range: Some(range),
            }))
        } else {
            Ok(None)
        }
    }
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        if let Some(location) = self.cache.write().await.goto_definition(&uri, pos).await {
            Ok(Some(GotoDefinitionResponse::Scalar(location)))
        } else {
            Ok(None)
        }
    }
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let with_decl = params.context.include_declaration;
        let locs = self
            .cache
            .write()
            .await
            .references(&uri, pos, with_decl)
            .await;
        Ok(Some(locs))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let resp = self.cache.write().await.completion(params).await;
        Ok(resp)
    }
}
