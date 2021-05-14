mod dl;
mod token_builder;

pub use token_builder::SemanticTokensBuilder;

use crate::core::Session;
use anyhow::Result;
use ddlog_lsp_languages::language::Language;
use lsp::{SemanticTokensRangeParams, SemanticTokensRangeResult, SemanticTokensResult};
use std::sync::Arc;

/// LSP message handler function for `textDocument/semanticTokens/full`.
pub async fn full(session: Arc<Session>, params: lsp::SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
    let text = session.get_text(&params.text_document.uri).await?;
    let response = match text.language {
        Language::DDlogDl => dl::full(session.clone(), params, &text.content).await?,
        Language::DDlogDat => None,
    };

    Ok(response)
}

/// LSP message handler function for `textDocument/semanticTokens/range`.
pub async fn range(
    session: Arc<Session>,
    params: SemanticTokensRangeParams,
) -> Result<Option<SemanticTokensRangeResult>> {
    let text = session.get_text(&params.text_document.uri).await?;
    let response = match text.language {
        Language::DDlogDl => dl::range(session.clone(), params, &text.content).await?,
        Language::DDlogDat => None,
    };

    Ok(response)
}
