use crate::{
    core::{self, IntoJsonRpcError},
    handler,
    provider::semantic_tokens::{modifiers::DDLOG_TOKEN_MODIFIERS, tokens::DDLOG_TOKEN_TYPES},
};
use lsp::{
    SemanticTokensFullOptions,
    SemanticTokensLegend,
    SemanticTokensOptions,
    SemanticTokensParams,
    SemanticTokensRangeParams,
    SemanticTokensRangeResult,
    SemanticTokensResult,
    SemanticTokensServerCapabilities,
};
use lspower::jsonrpc;
use std::sync::Arc;

pub struct Server {
    pub client: lspower::Client,
    pub session: Arc<core::Session>,
}

impl Server {
    pub fn new(client: lspower::Client) -> anyhow::Result<Self> {
        let session = Arc::new(core::Session::new(Some(client.clone()))?);
        Ok(Server { client, session })
    }
}

/// Convenience function for building [`lsp::ServerCapabilities`] for [Server].
pub fn capabilities() -> lsp::ServerCapabilities {
    let document_symbol_provider = Some(lsp::OneOf::Left(true));

    let semantic_tokens_provider = {
        let options = SemanticTokensOptions {
            legend: SemanticTokensLegend {
                token_types: DDLOG_TOKEN_TYPES.to_vec(),
                token_modifiers: DDLOG_TOKEN_MODIFIERS.to_vec(),
            },
            range: Some(true),
            full: Some(SemanticTokensFullOptions::Bool(true)),
            ..Default::default()
        };

        Some(SemanticTokensServerCapabilities::SemanticTokensOptions(options))
    };

    let text_document_sync = {
        let options = lsp::TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(lsp::TextDocumentSyncKind::Incremental),
            ..Default::default()
        };
        Some(lsp::TextDocumentSyncCapability::Options(options))
    };

    lsp::ServerCapabilities {
        text_document_sync,
        document_symbol_provider,
        semantic_tokens_provider,
        ..Default::default()
    }
}

#[lspower::async_trait]
impl lspower::LanguageServer for Server {
    async fn initialize(&self, params: lsp::InitializeParams) -> jsonrpc::Result<lsp::InitializeResult> {
        *self.session.client_capabilities.write().await = Some(params.capabilities);
        let capabilities = capabilities();
        Ok(lsp::InitializeResult {
            capabilities,
            ..lsp::InitializeResult::default()
        })
    }

    async fn initialized(&self, _: lsp::InitializedParams) {
        let typ = lsp::MessageType::Info;
        let message = "DDlog language server initialized!";
        self.client.log_message(typ, message).await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let session = self.session.clone();
        handler::text_document::did_open(session, params).await.unwrap()
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        let session = self.session.clone();
        handler::text_document::did_change(session, params).await.unwrap()
    }

    async fn did_close(&self, params: lsp::DidCloseTextDocumentParams) {
        let session = self.session.clone();
        handler::text_document::did_close(session, params).await.unwrap()
    }

    async fn document_symbol(
        &self,
        params: lsp::DocumentSymbolParams,
    ) -> jsonrpc::Result<Option<lsp::DocumentSymbolResponse>> {
        let session = self.session.clone();
        let result = handler::text_document::document_symbol(session, params).await;
        Ok(result.map_err(IntoJsonRpcError)?)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        let session = self.session.clone();
        let result = handler::text_document::semantic_tokens::full(session, params).await;

        Ok(result.map_err(IntoJsonRpcError)?)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> jsonrpc::Result<Option<SemanticTokensRangeResult>> {
        let session = self.session.clone();
        let result = handler::text_document::semantic_tokens::range(session, params).await;

        Ok(result.map_err(IntoJsonRpcError)?)
    }
}
