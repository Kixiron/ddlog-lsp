//! Semantic tokens provider definitions for ".dl" files.

use super::token_builder::SemanticTokensBuilder;
use crate::{
    core::{language::NodeMove, node::BasicNodeWalker, Language, Session},
    provider::semantic_tokens::modifiers,
};
use anyhow::{Context, Result};
use ddlog_lsp_syntax::{
    language::dl::{kind, utils, visit::exp, Visitor},
    node::{context::basic::Context as BasicContext, SyntaxErrors},
};
use lsp::{
    SemanticTokenModifier,
    SemanticTokenType,
    SemanticTokensLegend,
    SemanticTokensParams,
    SemanticTokensRangeParams,
    SemanticTokensRangeResult,
    SemanticTokensResult,
};
use lsp_text::RopeExt;
use ropey::Rope;
use std::{result::Result as StdResult, sync::Arc};

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

    let node = {
        let range = content.lsp_range_to_tree_sitter_range(params.range)?;
        let start = range.start_point();
        let end = range.end_point();

        tree.root_node().descendant_for_point_range(start, end)
    };

    if let Some(node) = node {
        let mut handler = Handler::new(content, legend, node)?;

        while !handler.walker.done {
            match handler.walker.kind() {
                kind::ATTRIBUTE => handler.visit_attribute(NodeMove::Init),
                kind::ATTRIBUTES => handler.visit_attributes(NodeMove::Init),
                kind::ANNOTATED_ITEM => handler.visit_annotated_item(NodeMove::Init),
                kind::ITEM => handler.visit_item(NodeMove::Init),
                kind::FUNCTION => handler.visit_function(NodeMove::Init),
                kind::TYPEDEF => handler.visit_typedef(NodeMove::Init),
                kind::TYPEDEF_EXTERN => handler.visit_typedef_extern(NodeMove::Init),
                kind::TYPEDEF_NORMAL => handler.visit_typedef_normal(NodeMove::Init),

                _ => {
                    if !handler.walker.goto_next() {
                        break;
                    }

                    Ok(())
                },
            }
            .unwrap_or_else(|err| log::error!("encountered error during syntax highlighting: {:?}", err));
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

    fn any_ident(&mut self, m: NodeMove) -> StdResult<tree_sitter::Node<'tree>, SyntaxErrors> {
        utils::choice((
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler
                    .walker()
                    .rule(kind::IDENT_LOWER_SCOPED, m)
                    .map_err(SyntaxErrors::from)
            },
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler
                    .walker()
                    .rule(kind::IDENT_UPPER_SCOPED, m)
                    .map_err(SyntaxErrors::from)
            },
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler.walker().rule(kind::IDENT_LOWER, m).map_err(SyntaxErrors::from)
            },
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler.walker().rule(kind::IDENT_UPPER, m).map_err(SyntaxErrors::from)
            },
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler.walker().rule(kind::IDENT_SCOPED, m).map_err(SyntaxErrors::from)
            },
        ))(self, m)
    }
}

impl<'text, 'tree> Visitor<'tree, BasicContext<'tree>> for Handler<'text, 'tree> {
    fn walker(&mut self) -> &mut BasicNodeWalker<'tree> {
        &mut self.walker
    }

    fn node(&self) -> tree_sitter::Node<'tree> {
        self.walker.node()
    }

    fn reset(&mut self, node: tree_sitter::Node<'tree>) {
        self.walker().reset(node)
    }

    fn visit_attribute(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        let name = self.walker().rule(kind::NAME, m)?;
        self.builder
            .push(
                name,
                &SemanticTokenType::VARIABLE,
                Some(vec![&SemanticTokenModifier::MODIFICATION, &modifiers::ATTRIBUTE]),
            )
            .unwrap_or_else(|err| log::error!("error creating semantic token: {:?}", err));
        utils::optional(utils::seq((operator::EQUALS_SIGN, exp)))(self, NodeMove::Step)
    }

    fn visit_name_func(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        let node = self.any_ident(m)?;
        self.builder
            .push(
                node,
                &SemanticTokenType::FUNCTION,
                Some(vec![
                    &SemanticTokenModifier::DEFINITION,
                    &SemanticTokenModifier::DECLARATION,
                ]),
            )
            .unwrap_or_else(|err| log::error!("error creating semantic token: {:?}", err));

        Ok(())
    }

    fn visit_name_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        let node = self.any_ident(m)?;
        self.builder
            .push(
                node,
                &SemanticTokenType::TYPE,
                Some(vec![
                    &SemanticTokenModifier::DEFINITION,
                    &SemanticTokenModifier::DECLARATION,
                ]),
            )
            .unwrap_or_else(|err| log::error!("error creating semantic token: {:?}", err));

        Ok(())
    }
}

mod operator {
    use super::{Handler, NodeMove};
    use ddlog_lsp_syntax::{language::dl::Visitor, node::SyntaxErrors};
    use lsp::SemanticTokenType;

    macro_rules! operator {
        ($($name:tt),* $(,)?) => {
            $(
                #[inline]
                #[allow(non_snake_case)]
                pub(super) fn $name<'text, 'tree>(
                    handler: &mut Handler<'text, 'tree>,
                    m: NodeMove,
                ) -> Result<(), SyntaxErrors> {
                    handler.walker().token(ddlog_lsp_syntax::language::dl::token::$name, m)?;

                    let node = handler.walker().node();
                    handler
                        .builder
                        .push(node, &SemanticTokenType::OPERATOR, None)
                        .unwrap_or_else(|err| log::error!("error creating semantic token: {:?}", err));

                    Ok(())
                }
            )*
        };
    }

    operator! {
        EQUALS_SIGN,
    }
}
