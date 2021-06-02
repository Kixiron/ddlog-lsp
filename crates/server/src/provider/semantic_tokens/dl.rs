//! Semantic tokens provider definitions for ".dl" files.

use crate::{
    core::{
        language::{
            dl::{kind, utils, visit, Visitor},
            NodeMove,
        },
        node::{context::trace::Context as TraceContext, SyntaxErrors, TraceNodeWalker},
        Language,
        Session,
    },
    provider::semantic_tokens::token_builder::SemanticTokensBuilder,
};
use anyhow::Context;
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
use std::sync::Arc;

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
) -> anyhow::Result<Option<SemanticTokensRangeResult>> {
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
    log::info!("semantic tokens initial node: {:#?}", node);

    if let Some(node) = node {
        let mut handler = Handler::new(content, legend, node)?;

        let result = handler.visit();
        if let Err(err) = result {
            log::error!("encountered error during syntax highlighting: {:?}", err);
        }

        let tokens = handler.builder.build();
        let result = SemanticTokensRangeResult::Tokens(tokens);

        Ok(Some(result))
    } else {
        Err(anyhow::anyhow!("Could not obtain tree node for given range"))
    }
}

// Move to the next appropriate node in the syntax tree.
struct Handler<'text, 'tree> {
    builder: SemanticTokensBuilder<'text, 'tree>,
    walker: TraceNodeWalker<'tree>,
}

impl<'text, 'tree> Handler<'text, 'tree> {
    fn new(
        content: &'text Rope,
        legend: Option<&'tree SemanticTokensLegend>,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<Self> {
        let language = Language::DDlogDl;
        let builder = SemanticTokensBuilder::new(content, legend)?;
        let walker = TraceNodeWalker::new(language, node);

        Ok(Self { builder, walker })
    }

    fn any_ident(&mut self, mov: NodeMove) -> Result<tree_sitter::Node<'tree>, SyntaxErrors> {
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
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler.walker().rule(kind::NAME, m).map_err(SyntaxErrors::from)
            },
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler.walker().rule(kind::NAME_ARG, m).map_err(SyntaxErrors::from)
            },
            |handler: &mut Handler<'text, 'tree>, m: NodeMove| {
                handler.walker().rule(kind::NAME_FIELD, m).map_err(SyntaxErrors::from)
            },
        ))(self, mov)
    }

    fn visit_any_ident(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
        let node = self.any_ident(mov)?;
        self.builder
            .push(node, &SemanticTokenType::VARIABLE, None)
            .unwrap_or_else(|err| log::error!("error creating semantic token: {:?}", err));

        Ok(())
    }
}

macro_rules! default_ident {
    ($($ident:ident => $kind:ident),* $(,)?) => {
        $(
            fn $ident(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
                let node = self.walker().rule(
                    ::ddlog_lsp_syntax::language::dl::kind::IDENT_LOWER,
                    mov,
                )?;

                self.builder
                    .push(node, &SemanticTokenType::VARIABLE, None)
                    .unwrap_or_else(|err| log::error!("error creating semantic token: {:?}", err));

                Ok(())
            }
        )*
    };
}

impl<'text, 'tree> Visitor<'tree, TraceContext<'tree>> for Handler<'text, 'tree> {
    default_ident! {
        visit_ident_lower_scoped => IDENT_LOWER_SCOPED,
        visit_ident_upper_scoped => IDENT_UPPER_SCOPED,
        visit_ident_scoped       => IDENT_SCOPED,
        visit_ident_lower        => IDENT_LOWER,
        visit_ident_upper        => IDENT_UPPER,
        visit_name               => NAME,
        visit_name_arg           => NAME_ARG,
        visit_name_field         => NAME_FIELD,
    }

    fn walker(&mut self) -> &mut TraceNodeWalker<'tree> {
        &mut self.walker
    }

    fn node(&self) -> tree_sitter::Node<'tree> {
        self.walker.node()
    }

    fn reset(&mut self, node: tree_sitter::Node<'tree>) {
        self.walker().reset(node)
    }

    fn visit_name_func(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
        dbg!();
        self.walker().rule(kind::NAME_FUNC, mov)?;
        dbg!();
        let node = self.any_ident(NodeMove::Step)?;
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

    fn visit_name_type(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
        dbg!();
        self.walker().rule(kind::NAME_TYPE, mov)?;
        dbg!();
        let node = self.any_ident(NodeMove::Step)?;
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

    fn visit_attributes(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
        dbg!();
        self.walker().rule(kind::ATTRIBUTES, mov)?;
        dbg!();
        utils::repeat1(utils::seq((
            visit::token::NUMBER_SIGN_LEFT_SQUARE_BRACKET,
            Self::visit_attribute,
            utils::repeat(utils::seq((visit::token::COMMA, Self::visit_attribute))),
            visit::token::RIGHT_SQUARE_BRACKET,
        )))(self, NodeMove::Step)
    }

    fn visit_attribute(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
        dbg!();
        self.walker().rule(kind::ATTRIBUTE, mov)?;
        dbg!();
        utils::seq((
            Self::visit_any_ident,
            utils::optional(utils::seq((visit::token::EQUALS_SIGN, Self::visit_exp))),
        ))(self, NodeMove::Step)
    }

    fn visit_typedef_extern(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
        dbg!();
        self.walker().rule(kind::TYPEDEF_EXTERN, mov)?;
        dbg!();
        utils::seq((
            keyword::EXTERN,
            keyword::TYPE,
            Self::visit_name_type,
            utils::optional(utils::seq((
                operator::LESS_THAN_SIGN,
                Self::visit_name_var_type,
                utils::repeat(utils::seq((visit::token::COMMA, Self::visit_name_var_type))),
                operator::GREATER_THAN_SIGN,
            ))),
        ))(self, NodeMove::Step)
    }

    fn visit_function_extern(&mut self, mov: NodeMove) -> Result<(), SyntaxErrors> {
        dbg!();
        self.walker().rule(kind::FUNCTION_EXTERN, mov)?;
        dbg!();
        utils::seq((
            keyword::EXTERN,
            keyword::FUNCTION,
            Self::visit_name_func,
            visit::token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Self::visit_arg,
                utils::repeat(utils::seq((visit::token::COMMA, Self::visit_arg))),
            ))),
            visit::token::RIGHT_PARENTHESIS,
            utils::optional(utils::seq((visit::token::COLON, Self::visit_type_atom))),
        ))(self, NodeMove::Step)
    }
}

mod keyword {
    use super::{Handler, NodeMove};
    use ddlog_lsp_syntax::{language::dl::Visitor, node::SyntaxErrors};
    use lsp::SemanticTokenType;

    macro_rules! keyword {
        ($($name:tt),* $(,)?) => {
            $(
                #[inline]
                #[allow(non_snake_case)]
                pub(super) fn $name<'text, 'tree>(
                    handler: &mut Handler<'text, 'tree>,
                    mov: NodeMove,
                ) -> Result<(), SyntaxErrors> {
                    let node = handler.walker().token(
                        ::ddlog_lsp_syntax::language::dl::token::$name,
                        mov,
                    )?;

                    handler.builder
                        .push(node, &SemanticTokenType::KEYWORD, None)
                        .unwrap_or_else(|err| log::error!("error creating semantic token: {:?}", err));

                    Ok(())
                }
            )*
        };
    }

    keyword! {
        EXTERN,
        TYPE,
        FUNCTION,
    }
}

mod operator {
    // FIXME
    #![allow(unused)]

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
                    mov: NodeMove,
                ) -> Result<(), SyntaxErrors> {
                    handler.walker().token(::ddlog_lsp_syntax::language::dl::token::$name, mov)?;

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
        LESS_THAN_SIGN,
        GREATER_THAN_SIGN,
    }
}
