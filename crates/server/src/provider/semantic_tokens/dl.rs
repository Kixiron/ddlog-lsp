//! Semantic tokens provider definitions for ".dl" files.

use super::token_builder::SemanticTokensBuilder;
use crate::core::{language::dl, node::BasicNodeWalker, Language, Session};
use anyhow::{Context, Result};
use lsp::{
    SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend, SemanticTokensParams, SemanticTokensRangeParams,
    SemanticTokensRangeResult, SemanticTokensResult,
};
use lsp_text::RopeExt;
use ropey::Rope;
use std::sync::Arc;

// Move to the next appropriate node in the syntax tree.
struct Handler<'text, 'tree> {
    builder: SemanticTokensBuilder<'text, 'tree>,
    walker: BasicNodeWalker<'tree>,
}

pub(crate) async fn full(
    session: Arc<Session>,
    params: SemanticTokensParams,
    content: &Rope,
) -> anyhow::Result<Option<SemanticTokensResult>> {
    let params = {
        let tree = session
            .get_tree(&params.text_document.uri)
            .await
            .context("failed to get token tree for `.dl` semantic token provider")?;

        let range = {
            let tree = tree.lock().await;
            let node = tree.root_node();

            content.tree_sitter_range_to_lsp_range(node.range())
        };

        SemanticTokensRangeParams {
            work_done_progress_params: params.work_done_progress_params,
            partial_result_params: params.partial_result_params,
            text_document: params.text_document,
            range,
        }
    };

    let token_range = range(session, params, content).await?;
    let result = token_range.map(|result| match result {
        SemanticTokensRangeResult::Tokens(tokens) => SemanticTokensResult::Tokens(tokens),
        SemanticTokensRangeResult::Partial(partial) => SemanticTokensResult::Partial(partial),
    });

    Ok(result)
}

pub(crate) async fn range(
    session: Arc<Session>,
    params: SemanticTokensRangeParams,
    content: &Rope,
) -> Result<Option<SemanticTokensRangeResult>> {
    let legend = session.semantic_tokens_legend().await;
    let legend = legend.as_ref();

    let tree = session.get_tree(&params.text_document.uri).await?;
    let tree = tree.lock().await;

    let root_node = {
        let range = content.lsp_range_to_tree_sitter_range(params.range)?;
        let start = range.start_point();
        let end = range.end_point();

        tree.root_node().descendant_for_point_range(start, end)
    };

    if let Some(node) = root_node {
        let mut handler = Handler::new(content, legend, node)?;

        while !handler.walker.done {
            match handler.walker.kind() {
                dl::kind::ROOT => handler.root(),

                dl::kind::ANNOTATED_ITEM => handler.annotated_item()?,

                _ => {
                    handler.walker.goto_next();
                }
            }
        }

        let tokens = handler.builder.build();
        let result = SemanticTokensRangeResult::Tokens(tokens);

        Ok(Some(result))
    } else {
        Err(anyhow::anyhow!("Could not obtain tree node for given range"))
    }
}

impl<'text, 'tree> Handler<'text, 'tree> {
    fn new(
        content: &'text Rope,
        legend: Option<&'tree SemanticTokensLegend>,
        node: tree_sitter::Node<'tree>,
    ) -> Result<Self> {
        let language = Language::DDlogDl;
        let builder = SemanticTokensBuilder::new(content, legend)?;
        let walker = BasicNodeWalker::new(language, node);
        Ok(Self { builder, walker })
    }

    fn root(&mut self) {
        self.walker.goto_next();
    }

    fn annotated_item(&mut self) -> Result<()> {
        // Attributes
        if self.walker.is(dl::kind::ATTRIBUTES) {
            self.attributes()?;
        }

        Ok(())
    }

    fn attributes(&mut self) -> Result<()> {
        // TODO: How to loop while the next token is `#[`?
        // "#["
        self.walker.goto_next();

        self.attribute()?;

        // "]"
        self.walker.goto_next();

        Ok(())
    }

    fn attribute(&mut self) -> Result<()> {
        while self.walker.is(dl::kind::NAME) {
            // $.name
            {
                let node = self.walker.node();
                let token_type = &SemanticTokenType::VARIABLE;
                let token_modifiers = None;
                self.builder.push(node, token_type, token_modifiers)?;
            }

            // TODO: if not `,` then break
        }

        Ok(())
    }
}
