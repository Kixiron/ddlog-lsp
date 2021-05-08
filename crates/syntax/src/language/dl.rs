//! Functions for working with the `.dl` grammar.

use crate::node::{NodeWalker, SyntaxErrors};

pub mod field {
    #![allow(missing_docs)]

    ddlog_lsp_macros::field_ids! {
        language: "ddlog.dl",
        fields: [
        ]
    }
}

pub mod kind {
    #![allow(missing_docs)]

    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dl",
        node_kinds: [
            (ROOT, "ROOT", true),
            (ANNOTATED_ITEM, "annotated_item", true),
            (APPLY, "apply", true),
            (ARG, "arg", true),
            (ARG_OPT_TYPE, "arg_opt_type", true),
            (ARG_TRANS, "arg_trans", true),
            (ATOM, "atom", true),
            (ATOM_ELEM, "atom_elem", true),
            (ATOM_POS, "atom_pos", true),
            (ATOM_REC, "atom_rec", true),
            (ATTRIBUTE, "attribute", true),
            (ATTRIBUTES, "attributes", true),
            (COMMENT_BLOCK, "comment_block", true),
            (COMMENT_BLOCK_INNER, "comment_block_inner", true),
            (COMMENT_LINE, "comment_line", true),
            (CONS, "cons", true),
            (CONS_POS, "cons_pos", true),
            (CONS_REC, "cons_rec", true),
            (ESCAPE_SEQUENCE, "escape_sequence", true),
            (ESCAPE_SEQUENCE_INTERPOLATED, "escape_sequence_interpolated", true),
            (EXP, "exp", true),
            (EXP_ADD, "exp_add", true),
            (EXP_ASSIGN, "exp_assign", true),
            (EXP_BINDING, "exp_binding", true),
            (EXP_BIT_AND, "exp_bit_and", true),
            (EXP_BIT_NEG, "exp_bit_neg", true),
            (EXP_BIT_OR, "exp_bit_or", true),
            (EXP_BIT_SLICE, "exp_bit_slice", true),
            (EXP_BIT_XOR, "exp_bit_xor", true),
            (EXP_BLOCK, "exp_block", true),
            (EXP_BREAK, "exp_break", true),
            (EXP_CAST, "exp_cast", true),
            (EXP_CAT, "exp_cat", true),
            (EXP_COND, "exp_cond", true),
            (EXP_CONS_POS, "exp_cons_pos", true),
            (EXP_CONS_REC, "exp_cons_rec", true),
            (EXP_CONTINUE, "exp_continue", true),
            (EXP_DECL_VAR, "exp_decl_var", true),
            (EXP_DIV, "exp_div", true),
            (EXP_EQ, "exp_eq", true),
            (EXP_FIELD, "exp_field", true),
            (EXP_FOR, "exp_for", true),
            (EXP_FUN_CALL, "exp_fun_call", true),
            (EXP_FUN_CALL_DOT, "exp_fun_call_dot", true),
            (EXP_GT, "exp_gt", true),
            (EXP_GTEQ, "exp_gteq", true),
            (EXP_LAMBDA, "exp_lambda", true),
            (EXP_LIT, "exp_lit", true),
            (EXP_LOG_AND, "exp_log_and", true),
            (EXP_LOG_IMP, "exp_log_imp", true),
            (EXP_LOG_NEG, "exp_log_neg", true),
            (EXP_LOG_OR, "exp_log_or", true),
            (EXP_LT, "exp_lt", true),
            (EXP_LTEQ, "exp_lteq", true),
            (EXP_MATCH, "exp_match", true),
            (EXP_MUL, "exp_mul", true),
            (EXP_NEG, "exp_neg", true),
            (EXP_NEQ, "exp_neq", true),
            (EXP_PROJ, "exp_proj", true),
            (EXP_PROJ_DIGITS, "exp_proj_digits", true),
            (EXP_REF, "exp_ref", true),
            (EXP_REM, "exp_rem", true),
            (EXP_RETURN, "exp_return", true),
            (EXP_SEQ, "exp_seq", true),
            (EXP_SHL, "exp_shl", true),
            (EXP_SHR, "exp_shr", true),
            (EXP_SLICE, "exp_slice", true),
            (EXP_SUB, "exp_sub", true),
            (EXP_TRY, "exp_try", true),
            (EXP_TUPLE, "exp_tuple", true),
            (EXP_TYPE, "exp_type", true),
            (EXP_WILD, "exp_wild", true),
            (FIELD, "field", true),
            (FUNCTION, "function", true),
            (FUNCTION_EXTERN, "function_extern", true),
            (FUNCTION_NORMAL, "function_normal", true),
            (IDENT, "ident", true),
            (IDENT_LOWER, "ident_lower", true),
            (IDENT_LOWER_SCOPED, "ident_lower_scoped", true),
            (IDENT_SCOPED, "ident_scoped", true),
            (IDENT_UPPER, "ident_upper", true),
            (IDENT_UPPER_SCOPED, "ident_upper_scoped", true),
            (IMPORT, "import", true),
            (INDEX, "index", true),
            (INTERPOLATION, "interpolation", true),
            (ITEM, "item", true),
            (KEY_PRIMARY, "key_primary", true),
            (LIT_BOOL, "lit_bool", true),
            (LIT_MAP, "lit_map", true),
            (LIT_NUM, "lit_num", true),
            (LIT_NUM_BIN, "lit_num_bin", true),
            (LIT_NUM_DEC, "lit_num_dec", true),
            (LIT_NUM_FLOAT, "lit_num_float", true),
            (LIT_NUM_HEX, "lit_num_hex", true),
            (LIT_NUM_OCT, "lit_num_oct", true),
            (LIT_STRING, "lit_string", true),
            (LIT_VEC, "lit_vec", true),
            (MODULE_ALIAS, "module_alias", true),
            (MODULE_PATH, "module_path", true),
            (NAME, "name", true),
            (NAME_ARG, "name_arg", true),
            (NAME_CONS, "name_cons", true),
            (NAME_FIELD, "name_field", true),
            (NAME_FUNC, "name_func", true),
            (NAME_INDEX, "name_index", true),
            (NAME_REL, "name_rel", true),
            (NAME_TRANS, "name_trans", true),
            (NAME_TYPE, "name_type", true),
            (NAME_VAR_TERM, "name_var_term", true),
            (NAME_VAR_TYPE, "name_var_type", true),
            (PAT, "pat", true),
            (PAT_CONS, "pat_cons", true),
            (PAT_CONS_POS, "pat_cons_pos", true),
            (PAT_CONS_REC, "pat_cons_rec", true),
            (PAT_LIT, "pat_lit", true),
            (PAT_TERM_DECL_VAR, "pat_term_decl_var", true),
            (PAT_TUPLE, "pat_tuple", true),
            (PAT_TYPE, "pat_type", true),
            (PAT_WILD, "pat_wild", true),
            (REL, "rel", true),
            (REL_ARGS, "rel_args", true),
            (REL_ELEM, "rel_elem", true),
            (REL_ROLE, "rel_role", true),
            (REL_SEMANTICS, "rel_semantics", true),
            (RHS, "rhs", true),
            (RHS_ATOM_NEG, "rhs_atom_neg", true),
            (RHS_FLAT_MAP, "rhs_flat_map", true),
            (RHS_GROUPING, "rhs_grouping", true),
            (RHS_INSPECT, "rhs_inspect", true),
            (RULE, "rule", true),
            (RULE_END, "rule_end", true),
            (STATEMENT, "statement", true),
            (STATEMENT_ASSIGN, "statement_assign", true),
            (STATEMENT_BLOCK, "statement_block", true),
            (STATEMENT_EMPTY, "statement_empty", true),
            (STATEMENT_FOR, "statement_for", true),
            (STATEMENT_IF, "statement_if", true),
            (STATEMENT_INSERT, "statement_insert", true),
            (STATEMENT_MATCH, "statement_match", true),
            (STRING_QUOTED, "string_quoted", true),
            (STRING_QUOTED_ESCAPED, "string_quoted_escaped", true),
            (STRING_RAW, "string_raw", true),
            (STRING_RAW_INTERPOLATED, "string_raw_interpolated", true),
            (TRANSFORMER, "transformer", true),
            (TYPE, "type", true),
            (TYPE_ATOM, "type_atom", true),
            (TYPE_BIGINT, "type_bigint", true),
            (TYPE_BIT, "type_bit", true),
            (TYPE_BOOL, "type_bool", true),
            (TYPE_DOUBLE, "type_double", true),
            (TYPE_FLOAT, "type_float", true),
            (TYPE_FUN, "type_fun", true),
            (TYPE_SIGNED, "type_signed", true),
            (TYPE_STRING, "type_string", true),
            (TYPE_TRANS, "type_trans", true),
            (TYPE_TRANS_FUN, "type_trans_fun", true),
            (TYPE_TRANS_REL, "type_trans_rel", true),
            (TYPE_TUPLE, "type_tuple", true),
            (TYPE_UNION, "type_union", true),
            (TYPE_USER, "type_user", true),
            (TYPE_VAR, "type_var", true),
            (TYPEDEF, "typedef", true),
            (TYPEDEF_EXTERN, "typedef_extern", true),
            (TYPEDEF_NORMAL, "typedef_normal", true),
            (WORD, "word", true),
        ],
    }
}

pub mod token {
    #![allow(missing_docs)]

    // keywords
    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dl",
        node_kinds: [
            (AND, "and", false),
            (APPLY, "apply", false),
            (AS, "as", false),
            (BIGINT, "bigint", false),
            (BIT, "bit", false),
            (BOOL, "bool", false),
            (BREAK, "break", false),
            (CONTINUE, "continue", false),
            (DOUBLE, "double", false),
            (ELSE, "else", false),
            (EXTERN, "extern", false),
            (FALSE, "false", false),
            (FLAT_MAP, "FlatMap", false),
            (FLOAT, "float", false),
            (FOR, "for", false),
            (FUNCTION, "function", false),
            (GROUP_BY, "group_by", false),
            (IDENTIFIER, "identifier", false),
            (IF, "if", false),
            (IMPORT, "import", false),
            (IN, "in", false),
            (INDEX, "index", false),
            (INPUT, "input", false),
            (INSPECT, "Inspect", false),
            (INTERNAL, "internal", false),
            (KEY, "key", false),
            (MATCH, "match", false),
            (MULTISET, "multiset", false),
            (MUT, "mut", false),
            (NOT, "not", false),
            (ON, "on", false),
            (OR, "or", false),
            (OUTPUT, "output", false),
            (PRIMARY, "primary", false),
            (RELATION, "relation", false),
            (RETURN, "return", false),
            (SIGNED, "signed", false),
            (SKIP, "skip", false),
            (STREAM, "stream", false),
            (STRING, "string", false),
            (TRANSFORMER, "transformer", false),
            (TRUE, "true", false),
            (TYPE, "type", false),
            (TYPEDEF, "typedef", false),
            (VAR, "var", false),
        ]
    }

    // tokens
    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dl",
        node_kinds: [
            (AMPERSAND, "&", false),
            (ASTERISK, "*", false),
            (ASTERISK_SOLIDUS, "*/", false),
            (CIRCUMFLEX_ACCENT, "^", false),
            (COLON, ":", false),
            (COLON_COLON, "::", false),
            (COLON_HYPHEN_MINUS, ":-", false),
            (COMMA, ",", false),
            (COMMERCIAL_AT, "@", false),
            (DOLLAR_SIGN, "$", false),
            (DOLLAR_SIGN_LEFT_CURLY_BRACKET, "${", false),
            (EQUALS_SIGN, "=", false),
            (EQUALS_SIGN_EQUALS_SIGN, "==", false),
            (EQUALS_SIGN_GREATER_THAN_SIGN, "=>", false),
            (EXCLAMATION_MARK_EQUALS_SIGN, "!=", false),
            (FULL_STOP, ".", false),
            (GREATER_THAN_SIGN, ">", false),
            (GREATER_THAN_SIGN_GREATER_THAN_SIGN, ">>", false),
            (GREATER_THAN_SIGN_EQUALS_SIGN, ">=", false),
            (HYPHEN_MINUS, "-", false),
            (LEFT_CURLY_BRACKET, "{", false),
            (LEFT_PARENTHESIS, "(", false),
            (LEFT_SQUARE_BRACKET, "[", false),
            (LESS_THAN_SIGN, "<", false),
            (LESS_THAN_SIGN_EQUALS_SIGN, "<=", false),
            (LESS_THAN_SIGN_LESS_THAN_SIGN, "<<", false),
            (LIT_BIN, "'b", false),
            (LIT_DEC, "'d", false),
            (LIT_FLOAT, "'f", false),
            (LIT_HEX, "'h", false),
            (LIT_OCT, "'o", false),
            (LIT_S_BIN, "'sb", false),
            (LIT_S_DEC, "'sd", false),
            (LIT_S_HEX, "'sh", false),
            (LIT_S_OCT, "'so", false),
            (LOW_LINE, "_", false),
            (NUMBER_SIGN, "#", false),
            (NUMBER_SIGN_LEFT_SQUARE_BRACKET, "#[", false),
            (PERCENT_SIGN, "%", false),
            (PLUS_SIGN, "+", false),
            (PLUS_SIGN_PLUS_SIGN, "++", false),
            (QUESTION_MARK, "?", false),
            (REVERSE_SOLIDUS_REVERSE_SOLIDUS, "\\", false),
            (RIGHT_CURLY_BRACKET, "}", false),
            (RIGHT_PARENTHESIS, ")", false),
            (RIGHT_SQUARE_BRACKET, "]", false),
            (RIGHTWARDS_ARROW, "->", false),
            (SEMICOLON, ";", false),
            (SOLIDUS, "/", false),
            (SOLIDUS_ASTERISK, "/*", false),
            (SOLIDUS_SOLIDUS, "//", false),
            (TILDE, "~", false),
            (VERTICAL_LINE, "|", false),
            (VERTICAL_LINE_RIGHT_SQUARE_BRACKET, "|]", false),
        ]
    }
}

#[allow(missing_docs)]
pub trait Visitor<'tree, Ctx>
where
    Ctx: crate::node::Context<'tree> + 'tree,
{
    fn walker(&mut self) -> &mut NodeWalker<'tree, Ctx>;

    fn node(&self) -> tree_sitter::Node<'tree>;

    fn reset(&mut self, node: tree_sitter::Node<'tree>);

    // #[allow(non_snake_case)]
    // fn visit_ROOT(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::ROOT> {
    //     visit::ROOT(self, node)
    // }
}

pub mod utils {
    #![allow(missing_docs)]
    #![allow(unused)]

    use super::*;
    use crate::node::{Context, SyntaxError};

    pub trait Choice<'tree, Ctx, Vis>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        fn choice(&self, visitor: &mut Vis) -> Result<(), SyntaxErrors>;
    }

    ddlog_lsp_macros::impl_choice!(2);
    ddlog_lsp_macros::impl_choice!(3);
    ddlog_lsp_macros::impl_choice!(4);
    ddlog_lsp_macros::impl_choice!(5);
    ddlog_lsp_macros::impl_choice!(6);
    ddlog_lsp_macros::impl_choice!(7);
    ddlog_lsp_macros::impl_choice!(9);
    ddlog_lsp_macros::impl_choice!(11);
    ddlog_lsp_macros::impl_choice!(12);
    ddlog_lsp_macros::impl_choice!(50);

    #[inline]
    pub fn choice<'tree, Ctx, Vis, T>(funs: T) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
        T: Choice<'tree, Ctx, Vis>,
    {
        move |visitor| funs.choice(visitor)
    }

    #[inline]
    pub fn done<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        if visitor.walker().done {
            Ok(())
        } else {
            let mut errors = SyntaxErrors::new();
            errors.push(SyntaxError::MoreNodes);
            Err(errors)
        }
    }

    #[inline]
    pub fn optional<'tree, Ctx, Vis>(
        fun: impl Fn(&mut Vis) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor| {
            let prev = visitor.node();
            if fun(visitor).is_err() {
                visitor.reset(prev);
            }
            Ok(())
        }
    }

    pub fn repeat<'tree, Ctx, Vis>(
        fun: impl Fn(&mut Vis) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor| {
            loop {
                let prev = visitor.node();
                if visitor.walker().done {
                    break;
                }
                if fun(visitor).is_err() {
                    break;
                }
            }
            Ok(())
        }
    }

    #[inline]
    pub fn repeat1<'tree, Ctx, Vis>(
        fun: impl Fn(&mut Vis) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor| {
            let mut errors = SyntaxErrors::new();
            let mut succeeded_once = false;
            if visitor.walker().done {
                errors.push(SyntaxError::DoneEarly);
                return Err(errors);
            }
            loop {
                let prev = visitor.node();
                match fun(visitor) {
                    Ok(_) => {
                        succeeded_once = true;
                    },
                    Err(mut errs) => {
                        if succeeded_once {
                            visitor.reset(prev);
                            break;
                        } else {
                            errors.append(&mut errs);
                            return Err(errors);
                        }
                    },
                }
            }
            Ok(())
        }
    }

    pub trait Seq<'tree, Ctx, Vis>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        fn seq(&self, visitor: &mut Vis) -> Result<(), SyntaxErrors>;
    }

    ddlog_lsp_macros::impl_seq!(2);
    ddlog_lsp_macros::impl_seq!(3);
    ddlog_lsp_macros::impl_seq!(4);
    ddlog_lsp_macros::impl_seq!(5);
    ddlog_lsp_macros::impl_seq!(6);
    ddlog_lsp_macros::impl_seq!(7);
    ddlog_lsp_macros::impl_seq!(8);
    ddlog_lsp_macros::impl_seq!(9);
    ddlog_lsp_macros::impl_seq!(10);
    ddlog_lsp_macros::impl_seq!(11);

    #[inline]
    pub fn seq<'tree, Ctx, Vis, T>(funs: T) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
        T: Seq<'tree, Ctx, Vis>,
    {
        move |visitor| funs.seq(visitor)
    }

    #[inline]
    pub fn restore<'tree, Ctx, Vis>(
        fun: impl Fn(&mut Vis) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor| {
            let prev = visitor.node();
            match fun(visitor) {
                Ok(result) => Ok(result),
                Err(ref mut errs) => {
                    visitor.reset(prev);
                    let mut errors = SyntaxErrors::new();
                    errors.append(errs);
                    Err(errors)
                },
            }
        }
    }
}

#[allow(unused)]
#[allow(missing_docs)]
pub mod visit {
    use super::*;
    use crate::node::Context;

    #[allow(non_snake_case)]
    pub fn ROOT<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ROOT)?;
        utils::repeat(annotated_item)(visitor)
    }

    pub fn annotated_item<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ANNOTATED_ITEM)?;
        utils::seq((utils::optional(attributes), item))(visitor)
    }

    pub fn apply<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::APPLY)?;
        utils::seq((
            token::APPLY,
            name_trans,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::choice((name_func, name_rel))),
            utils::repeat(utils::seq((token::COMMA, utils::choice((name_func, name_rel))))),
            token::RIGHT_PARENTHESIS,
            token::RIGHTWARDS_ARROW,
            token::LEFT_PARENTHESIS,
            utils::optional(name_rel),
            utils::repeat(utils::seq((token::COMMA, name_rel))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn arg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG)?;
        utils::seq((name_arg, token::COLON, utils::optional(token::MUT), type_atom))(visitor)
    }

    pub fn arg_opt_type<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG_OPT_TYPE)?;
        utils::seq((
            name_arg,
            utils::optional(utils::seq((token::COLON, utils::optional(token::MUT), type_atom))),
            type_atom,
        ))(visitor)
    }

    pub fn arg_trans<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG_TRANS)?;
        utils::seq((name_trans, token::COLON, type_trans))(visitor)
    }

    pub fn atom<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM)?;
        utils::choice((atom_rec, atom_pos, atom_elem))(visitor)
    }

    pub fn atom_elem<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_ELEM)?;
        utils::seq((name_rel, token::LEFT_SQUARE_BRACKET, exp, token::RIGHT_SQUARE_BRACKET))(visitor)
    }

    pub fn atom_pos<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_POS)?;
        utils::seq((
            utils::optional(utils::seq((name_var_term, token::IN))),
            utils::seq((utils::optional(token::AMPERSAND), name_rel)),
            utils::optional(utils::seq((
                token::LEFT_PARENTHESIS,
                utils::optional(utils::seq((
                    exp,
                    utils::repeat(utils::seq((token::COMMA, exp))),
                    utils::optional(token::COMMA),
                ))),
                token::RIGHT_PARENTHESIS,
            ))),
        ))(visitor)
    }

    pub fn atom_rec<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_REC)?;
        utils::seq((
            utils::optional(utils::seq((name_var_term, token::IN))),
            utils::seq((utils::optional(token::AMPERSAND), name_rel)),
            token::LEFT_PARENTHESIS,
            token::FULL_STOP,
            name_arg,
            token::EQUALS_SIGN,
            exp,
            utils::repeat(utils::seq((
                token::COMMA,
                token::FULL_STOP,
                name_arg,
                token::EQUALS_SIGN,
                exp,
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn attribute<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATTRIBUTE)?;
        utils::seq((name, utils::optional(utils::seq((token::EQUALS_SIGN, exp)))))(visitor)
    }

    pub fn attributes<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATTRIBUTES)?;
        utils::repeat1(utils::seq((
            token::NUMBER_SIGN_LEFT_SQUARE_BRACKET,
            attribute,
            utils::repeat(utils::seq((token::COMMA, attribute))),
            token::RIGHT_SQUARE_BRACKET,
        )))(visitor)
    }

    pub fn comment_block<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_BLOCK)?;
        utils::seq((
            token::SOLIDUS_ASTERISK,
            utils::repeat(utils::choice((comment_block, comment_block_inner))),
            token::ASTERISK_SOLIDUS,
        ))(visitor)
    }

    pub fn comment_block_inner<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_BLOCK_INNER)?;
        Ok(())
    }

    pub fn comment_line<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_LINE)?;
        Ok(())
    }

    pub fn cons<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS)?;
        utils::choice((cons_rec, cons_pos))(visitor)
    }

    pub fn cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_POS)?;
        utils::seq((utils::optional(attributes), name_cons))(visitor)
    }

    pub fn cons_rec<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_REC)?;
        utils::seq((
            utils::optional(attributes),
            name_cons,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                field,
                utils::repeat(utils::seq((token::COMMA, field))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor)
    }

    pub fn escape_sequence<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ESCAPE_SEQUENCE)?;
        Ok(())
    }

    pub fn escape_sequence_interpolated<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ESCAPE_SEQUENCE_INTERPOLATED)?;
        Ok(())
    }

    pub fn exp<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP)?;
        utils::choice((
            exp_add,
            exp_assign,
            exp_binding,
            exp_bit_and,
            exp_bit_neg,
            exp_bit_or,
            exp_bit_slice,
            exp_bit_xor,
            exp_block,
            exp_break,
            exp_cast,
            exp_cat,
            exp_cond,
            exp_continue,
            exp_cons_pos,
            exp_cons_rec,
            exp_decl_var,
            exp_div,
            exp_eq,
            exp_field,
            exp_for,
            exp_fun_call,
            exp_fun_call_dot,
            exp_gt,
            exp_gteq,
            exp_lambda,
            exp_lit,
            exp_log_and,
            exp_log_imp,
            exp_log_neg,
            exp_log_or,
            exp_lt,
            exp_lteq,
            exp_match,
            exp_mul,
            exp_neg,
            exp_neq,
            exp_proj,
            exp_ref,
            exp_rem,
            exp_return,
            exp_seq,
            exp_shl,
            exp_shr,
            exp_slice,
            exp_sub,
            exp_try,
            exp_tuple,
            exp_type,
            exp_wild,
        ))(visitor)
    }

    pub fn exp_add<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_ADD)?;
        utils::seq((exp, token::PLUS_SIGN, exp))(visitor)
    }

    pub fn exp_assign<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_ASSIGN)?;
        utils::seq((exp, token::PLUS_SIGN, exp))(visitor)
    }

    pub fn exp_binding<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BINDING)?;
        utils::seq((name_var_term, token::COMMERCIAL_AT, exp))(visitor)
    }

    pub fn exp_bit_and<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_AND)?;
        utils::seq((exp, token::AMPERSAND, exp))(visitor)
    }

    pub fn exp_bit_neg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_NEG)?;
        utils::seq((token::TILDE, exp))(visitor)
    }

    pub fn exp_bit_or<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_OR)?;
        utils::seq((exp, token::VERTICAL_LINE, exp))(visitor)
    }

    pub fn exp_bit_slice<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_OR)?;
        utils::seq((exp, token::LEFT_SQUARE_BRACKET, token::RIGHT_SQUARE_BRACKET))(visitor)
    }

    pub fn exp_bit_xor<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_XOR)?;
        utils::seq((exp, token::CIRCUMFLEX_ACCENT, exp))(visitor)
    }

    pub fn exp_block<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BLOCK)?;
        utils::seq((
            token::LEFT_CURLY_BRACKET,
            utils::optional(exp),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor)
    }

    pub fn exp_break<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BREAK)?;
        token::BREAK(visitor)
    }

    pub fn exp_cast<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CAST)?;
        utils::seq((exp, token::AS, type_atom))(visitor)
    }

    pub fn exp_cat<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CAT)?;
        utils::seq((exp, token::PLUS_SIGN_PLUS_SIGN, exp))(visitor)
    }

    pub fn exp_cond<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_COND)?;
        utils::seq((token::IF, exp, exp, utils::optional(utils::seq((token::ELSE, exp)))))(visitor)
    }

    pub fn exp_cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CONS_POS)?;
        utils::seq((
            name_cons,
            utils::optional(utils::seq((
                token::LEFT_CURLY_BRACKET,
                utils::optional(utils::seq((
                    token::LEFT_CURLY_BRACKET,
                    utils::optional(utils::seq((exp, utils::repeat(utils::seq((token::COMMA, exp)))))),
                    token::RIGHT_CURLY_BRACKET,
                ))),
                token::RIGHT_CURLY_BRACKET,
            ))),
        ))(visitor)
    }

    pub fn exp_cons_rec<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn exp_continue<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CONTINUE)?;
        token::CONTINUE(visitor)
    }

    pub fn exp_decl_var<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DECL_VAR)?;
        utils::seq((utils::optional(token::VAR), name_var_term))(visitor)
    }

    pub fn exp_div<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DIV)?;
        utils::seq((exp, token::SOLIDUS, exp))(visitor)
    }

    pub fn exp_eq<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DIV)?;
        utils::seq((exp, token::EQUALS_SIGN_EQUALS_SIGN, exp))(visitor)
    }

    pub fn exp_field<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FIELD)?;
        utils::seq((exp, token::FULL_STOP, ident))(visitor)
    }

    pub fn exp_for<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FOR)?;
        utils::seq((
            token::FOR,
            token::LEFT_PARENTHESIS,
            name_var_term,
            token::IN,
            exp,
            token::RIGHT_PARENTHESIS,
            exp,
        ))(visitor)
    }

    pub fn exp_fun_call<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FUN_CALL)?;
        utils::seq((
            exp,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                exp,
                utils::repeat(utils::seq((token::COMMA, exp))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn exp_fun_call_dot<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FUN_CALL_DOT)?;
        utils::seq((
            exp,
            token::FULL_STOP,
            name_func,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                exp,
                utils::repeat(utils::seq((token::COMMA, exp))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn exp_gt<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_GT)?;
        utils::seq((exp, token::GREATER_THAN_SIGN, exp))(visitor)
    }

    pub fn exp_gteq<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_GTEQ)?;
        utils::seq((exp, token::GREATER_THAN_SIGN_EQUALS_SIGN, exp))(visitor)
    }

    pub fn exp_lambda<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LAMBDA)?;
        utils::choice((
            utils::seq((
                token::FUNCTION,
                token::LEFT_PARENTHESIS,
                utils::optional(utils::seq((
                    arg_opt_type,
                    utils::repeat(utils::seq((token::COMMA, arg_opt_type))),
                ))),
                token::RIGHT_PARENTHESIS,
                utils::optional(utils::seq((token::COLON, type_atom))),
                exp,
            )),
            utils::seq((
                token::VERTICAL_LINE,
                utils::optional(utils::seq((
                    arg_opt_type,
                    utils::repeat(utils::seq((token::COMMA, arg_opt_type))),
                ))),
                token::VERTICAL_LINE,
                utils::optional(utils::seq((token::COLON, type_atom))),
                exp,
            )),
        ))(visitor)
    }

    pub fn exp_lit<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LIT)?;
        utils::choice((lit_bool, lit_num, lit_map, lit_string, lit_vec))(visitor)
    }

    pub fn exp_log_and<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_AND)?;
        utils::seq((exp, token::AND, exp))(visitor)
    }

    pub fn exp_log_imp<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_IMP)?;
        utils::seq((exp, token::EQUALS_SIGN_GREATER_THAN_SIGN, exp))(visitor)
    }

    pub fn exp_log_neg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_NEG)?;
        utils::seq((token::NOT, exp))(visitor)
    }

    pub fn exp_log_or<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_OR)?;
        utils::seq((exp, token::OR, exp))(visitor)
    }

    pub fn exp_lt<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LT)?;
        utils::seq((exp, token::LESS_THAN_SIGN, exp))(visitor)
    }

    pub fn exp_lteq<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LTEQ)?;
        utils::seq((exp, token::LESS_THAN_SIGN_EQUALS_SIGN, exp))(visitor)
    }

    pub fn exp_match<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_MATCH)?;
        utils::seq((
            token::MATCH,
            token::LEFT_PARENTHESIS,
            exp,
            token::RIGHT_PARENTHESIS,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                utils::seq((pat, token::RIGHTWARDS_ARROW, exp)),
                utils::repeat(utils::seq((
                    token::COMMA,
                    utils::seq((pat, token::RIGHTWARDS_ARROW, exp)),
                ))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor)
    }

    pub fn exp_mul<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_MUL)?;
        utils::seq((exp, token::ASTERISK, exp))(visitor)
    }

    pub fn exp_neg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_NEG)?;
        utils::seq((token::HYPHEN_MINUS, exp))(visitor)
    }

    pub fn exp_neq<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_NEQ)?;
        utils::seq((exp, token::EXCLAMATION_MARK_EQUALS_SIGN, exp))(visitor)
    }

    pub fn exp_proj<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_PROJ)?;
        utils::seq((exp, token::FULL_STOP, exp_proj_digits))(visitor)
    }

    pub fn exp_proj_digits<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_PROJ_DIGITS)?;
        Ok(())
    }

    pub fn exp_ref<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_REF)?;
        utils::seq((token::AMPERSAND, exp))(visitor)
    }

    pub fn exp_rem<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_REM)?;
        utils::seq((exp, token::PERCENT_SIGN, exp))(visitor)
    }

    pub fn exp_return<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_RETURN)?;
        utils::seq((token::RETURN, utils::optional(exp)))(visitor)
    }

    pub fn exp_seq<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SEQ)?;
        utils::seq((exp, token::SEMICOLON, utils::optional(exp)))(visitor)
    }

    pub fn exp_shl<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SHL)?;
        utils::seq((exp, token::LESS_THAN_SIGN_LESS_THAN_SIGN, exp))(visitor)
    }

    pub fn exp_shr<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SHR)?;
        utils::seq((exp, token::GREATER_THAN_SIGN_GREATER_THAN_SIGN, exp))(visitor)
    }

    pub fn exp_slice<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SLICE)?;
        utils::seq((
            exp,
            token::LEFT_SQUARE_BRACKET,
            lit_num_dec,
            token::COLON,
            lit_num_dec,
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor)
    }

    pub fn exp_sub<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SUB)?;
        utils::seq((exp, token::HYPHEN_MINUS, exp))(visitor)
    }

    pub fn exp_try<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_TRY)?;
        utils::seq((exp, token::QUESTION_MARK))(visitor)
    }

    pub fn exp_tuple<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_TUPLE)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                exp,
                utils::repeat(utils::seq((token::COMMA, exp))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn exp_type<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_TYPE)?;
        utils::seq((exp, token::COLON, type_atom))(visitor)
    }

    pub fn exp_wild<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_WILD)?;
        token::LOW_LINE(visitor)
    }

    pub fn field<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FIELD)?;
        utils::seq((utils::optional(attributes), name_field, token::COLON, type_atom))(visitor)
    }

    pub fn function<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FUNCTION)?;
        utils::choice((function_normal, function_extern))(visitor)
    }

    pub fn function_extern<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FUNCTION_EXTERN)?;
        utils::seq((
            token::EXTERN,
            token::FUNCTION,
            name_func,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((arg, utils::repeat(utils::seq((token::COMMA, arg)))))),
            token::RIGHT_PARENTHESIS,
            utils::optional(utils::seq((token::COLON, type_atom))),
        ))(visitor)
    }

    pub fn function_normal<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FUNCTION_NORMAL)?;
        utils::seq((
            token::FUNCTION,
            name_func,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((arg, utils::repeat(utils::seq((token::COMMA, arg)))))),
            token::RIGHT_PARENTHESIS,
            utils::optional(utils::seq((token::COLON, type_atom))),
            utils::choice((
                utils::seq((token::EQUALS_SIGN, exp)),
                token::LEFT_CURLY_BRACKET,
                exp,
                token::RIGHT_CURLY_BRACKET,
            )),
        ))(visitor)
    }

    pub fn ident<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT)?;
        utils::choice((ident_lower, ident_upper))(visitor)
    }

    pub fn ident_lower<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_LOWER)?;
        Ok(())
    }

    pub fn ident_lower_scoped<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_LOWER_SCOPED)?;
        Ok(())
    }

    pub fn ident_scoped<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_SCOPED)?;
        Ok(())
    }

    pub fn ident_upper<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_UPPER)?;
        Ok(())
    }

    pub fn ident_upper_scoped<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_UPPER_SCOPED)?;
        Ok(())
    }

    pub fn import<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IMPORT)?;
        utils::seq((
            token::IMPORT,
            module_path,
            utils::optional(utils::seq((token::AS, module_alias))),
        ))(visitor)
    }

    pub fn index<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INDEX)?;
        utils::seq((
            token::INDEX,
            name_index,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                arg,
                utils::repeat(utils::seq((token::COMMA, arg))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
            token::ON,
            atom,
        ))(visitor)
    }

    pub fn interpolation<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INTERPOLATION)?;
        utils::seq((token::DOLLAR_SIGN_LEFT_CURLY_BRACKET, exp, token::RIGHT_CURLY_BRACKET))(visitor)
    }

    pub fn item<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ITEM)?;
        utils::choice((
            statement_for,
            apply,
            import,
            function,
            index,
            rel,
            rule,
            transformer,
            typedef,
        ))(visitor)
    }

    pub fn key_primary<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::KEY_PRIMARY)?;
        utils::seq((
            token::PRIMARY,
            token::KEY,
            token::LEFT_PARENTHESIS,
            name_var_term,
            token::RIGHT_PARENTHESIS,
            exp,
        ))(visitor)
    }

    pub fn lit_bool<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_BOOL)?;
        utils::choice((token::FALSE, token::TRUE))(visitor)
    }

    pub fn lit_map<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_MAP)?;
        utils::seq((
            token::LEFT_SQUARE_BRACKET,
            exp,
            token::RIGHTWARDS_ARROW,
            exp,
            utils::repeat(utils::seq((token::COMMA, exp, token::RIGHTWARDS_ARROW, exp))),
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor)
    }

    pub fn lit_num<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM)?;
        utils::choice((
            utils::choice((lit_num_dec, lit_num_float, lit_num_hex)),
            utils::seq((
                utils::optional(lit_num_dec),
                utils::choice((
                    utils::seq((token::LIT_BIN, lit_num_bin)),
                    utils::seq((token::LIT_DEC, lit_num_dec)),
                    utils::seq((token::LIT_FLOAT, lit_num_float)),
                    utils::seq((token::LIT_HEX, lit_num_hex)),
                    utils::seq((token::LIT_OCT, lit_num_oct)),
                    utils::seq((token::LIT_S_BIN, lit_num_bin)),
                    utils::seq((token::LIT_S_DEC, lit_num_dec)),
                    utils::seq((token::LIT_S_HEX, lit_num_hex)),
                    utils::seq((token::LIT_S_OCT, lit_num_oct)),
                )),
            )),
        ))(visitor)
    }

    pub fn lit_num_bin<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_BIN)?;
        Ok(())
    }

    pub fn lit_num_dec<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_DEC)?;
        Ok(())
    }

    pub fn lit_num_float<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_FLOAT)?;
        Ok(())
    }

    pub fn lit_num_hex<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_HEX)?;
        Ok(())
    }

    pub fn lit_num_oct<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_OCT)?;
        Ok(())
    }

    pub fn lit_string<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_STRING)?;
        utils::repeat1(utils::choice((
            string_quoted,
            string_quoted_escaped,
            string_raw,
            string_raw_interpolated,
        )))(visitor)
    }

    pub fn lit_vec<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_VEC)?;
        utils::seq((
            token::LEFT_SQUARE_BRACKET,
            exp,
            utils::repeat(utils::seq((token::COMMA, exp))),
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor)
    }

    pub fn module_alias<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MODULE_ALIAS)?;
        ident(visitor)
    }

    pub fn module_path<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MODULE_PATH)?;
        utils::seq((ident, utils::repeat(utils::seq((token::COLON_COLON, ident)))))(visitor)
    }

    pub fn name<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME)?;
        Ok(())
    }

    pub fn name_arg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_ARG)?;
        Ok(())
    }

    pub fn name_cons<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_CONS)?;
        ident_upper_scoped(visitor)
    }

    pub fn name_field<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_FIELD)?;
        Ok(())
    }

    pub fn name_func<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_FUNC)?;
        ident_lower_scoped(visitor)
    }

    pub fn name_index<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_INDEX)?;
        ident_scoped(visitor)
    }

    pub fn name_rel<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_REL)?;
        ident_upper_scoped(visitor)
    }

    pub fn name_trans<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_TRANS)?;
        ident_scoped(visitor)
    }

    pub fn name_type<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_TYPE)?;
        utils::choice((ident_lower_scoped, ident_upper_scoped))(visitor)
    }

    pub fn name_var_term<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_VAR_TERM)?;
        ident_lower_scoped(visitor)
    }

    pub fn name_var_type<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_VAR_TYPE)?;
        Ok(())
    }

    pub fn pat<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT)?;
        utils::choice((pat_cons, pat_term_decl_var, pat_lit, pat_tuple, pat_type, pat_wild))(visitor)
    }

    pub fn pat_cons<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS)?;
        utils::choice((pat_cons_rec, pat_cons_pos))(visitor)
    }

    pub fn pat_cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS_POS)?;
        utils::seq((
            name_cons,
            utils::optional(utils::seq((
                token::LEFT_CURLY_BRACKET,
                utils::optional(utils::seq((
                    pat,
                    utils::repeat(utils::seq((token::COMMA, pat))),
                    utils::optional(token::COMMA),
                ))),
                token::RIGHT_CURLY_BRACKET,
            ))),
        ))(visitor)
    }

    pub fn pat_cons_rec<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS_REC)?;
        utils::seq((
            name_cons,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                token::FULL_STOP,
                name_field,
                token::EQUALS_SIGN,
                pat,
                utils::repeat(utils::seq((
                    token::COMMA,
                    token::FULL_STOP,
                    name_field,
                    token::EQUALS_SIGN,
                    pat,
                ))),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor)
    }

    pub fn pat_lit<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_LIT)?;
        utils::choice((lit_bool, lit_num, lit_string))(visitor)
    }

    pub fn pat_term_decl_var<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TERM_DECL_VAR)?;
        utils::seq((utils::optional(token::VAR), name_var_term))(visitor)
    }

    pub fn pat_tuple<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TUPLE)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((pat, utils::repeat(utils::seq((token::COMMA, pat)))))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn pat_type<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TYPE)?;
        utils::seq((pat, token::COLON, type_atom))(visitor)
    }

    pub fn pat_wild<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_WILD)?;
        token::LOW_LINE(visitor)
    }

    pub fn rel<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL)?;
        utils::choice((rel_args, rel_elem))(visitor)
    }

    pub fn rel_args<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_ARGS)?;
        utils::seq((
            utils::optional(rel_role),
            rel_semantics,
            utils::seq((utils::optional(token::AMPERSAND), name_rel)),
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                arg,
                utils::repeat(utils::seq((token::COMMA, arg))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
            utils::optional(key_primary),
        ))(visitor)
    }

    pub fn rel_elem<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_ELEM)?;
        utils::seq((
            utils::optional(rel_role),
            rel_semantics,
            utils::seq((utils::optional(token::AMPERSAND), name_rel)),
            token::LEFT_SQUARE_BRACKET,
            type_atom,
            token::RIGHT_SQUARE_BRACKET,
            utils::optional(key_primary),
        ))(visitor)
    }

    pub fn rel_role<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_ROLE)?;
        utils::choice((token::INPUT, token::INTERNAL, token::OUTPUT))(visitor)
    }

    pub fn rel_semantics<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_SEMANTICS)?;
        utils::choice((token::RELATION, token::STREAM, token::MULTISET))(visitor)
    }

    pub fn rhs<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS)?;
        utils::choice((rhs_inspect, atom, rhs_atom_neg, exp, rhs_flat_map, rhs_grouping))(visitor)
    }

    pub fn rhs_atom_neg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_ATOM_NEG)?;
        utils::seq((token::NOT, atom))(visitor)
    }

    pub fn rhs_flat_map<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_FLAT_MAP)?;
        utils::seq((
            token::VAR,
            name_var_term,
            token::EQUALS_SIGN,
            token::FLAT_MAP,
            token::LEFT_PARENTHESIS,
            exp,
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn rhs_grouping<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_GROUPING)?;
        utils::seq((
            token::VAR,
            name_var_term,
            token::EQUALS_SIGN,
            exp,
            token::FULL_STOP,
            token::GROUP_BY,
            token::LEFT_PARENTHESIS,
            exp,
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn rhs_inspect<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_INSPECT)?;
        utils::seq((token::INSPECT, exp))(visitor)
    }

    pub fn rule<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RULE)?;
        utils::seq((
            atom,
            utils::repeat(utils::seq((token::COMMA, atom))),
            utils::optional(utils::seq((
                token::COLON_HYPHEN_MINUS,
                rhs,
                utils::repeat(utils::seq((token::COMMA, rhs))),
            ))),
            rule_end,
        ))(visitor)
    }

    pub fn rule_end<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RULE_END)?;
        Ok(())
    }

    pub fn statement<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT)?;
        utils::choice((
            statement_assign,
            statement_block,
            statement_empty,
            statement_for,
            statement_if,
            statement_insert,
            statement_match,
        ))(visitor)
    }

    pub fn statement_assign<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_ASSIGN)?;
        utils::seq((exp, token::IN, statement))(visitor)
    }

    pub fn statement_block<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_BLOCK)?;
        utils::seq((
            token::LEFT_CURLY_BRACKET,
            utils::repeat(utils::seq((
                statement,
                utils::optional(utils::seq((token::SEMICOLON, utils::optional(statement)))),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor)
    }

    pub fn statement_empty<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_EMPTY)?;
        token::SKIP(visitor)
    }

    pub fn statement_for<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_FOR)?;
        utils::seq((
            token::FOR,
            token::LEFT_PARENTHESIS,
            atom,
            utils::optional(utils::seq((token::IF, exp))),
            token::RIGHT_PARENTHESIS,
            statement,
        ))(visitor)
    }

    pub fn statement_if<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_IF)?;
        utils::seq((
            token::IF,
            token::LEFT_PARENTHESIS,
            exp,
            token::RIGHT_PARENTHESIS,
            statement,
            utils::optional(utils::seq((token::ELSE, statement))),
        ))(visitor)
    }

    pub fn statement_insert<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_INSERT)?;
        atom(visitor)
    }

    pub fn statement_match<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_MATCH)?;
        utils::seq((
            token::MATCH,
            token::LEFT_PARENTHESIS,
            exp,
            token::RIGHT_PARENTHESIS,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                utils::seq((pat, token::RIGHTWARDS_ARROW, statement)),
                utils::repeat(utils::seq((
                    token::COMMA,
                    utils::seq((pat, token::RIGHTWARDS_ARROW, statement)),
                ))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor)
    }

    pub fn string_quoted<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn string_quoted_escaped<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn string_raw<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn string_raw_interpolated<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn transformer<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TRANSFORMER)?;
        utils::seq((
            token::EXTERN,
            token::TRANSFORMER,
            name_trans,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                arg_trans,
                utils::repeat(utils::seq((token::COMMA, arg_trans))),
            ))),
            token::RIGHT_PARENTHESIS,
            token::RIGHTWARDS_ARROW,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                arg_trans,
                utils::repeat(utils::seq((token::COMMA, arg_trans))),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn r#type<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE)?;
        utils::choice((
            type_bit,
            type_signed,
            type_bigint,
            type_double,
            type_float,
            type_string,
            type_bool,
            type_union,
            type_user,
            type_var,
            type_fun,
            type_tuple,
        ))(visitor)
    }

    pub fn type_atom<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_ATOM)?;
        utils::choice((
            type_bit,
            type_signed,
            type_bigint,
            type_double,
            type_float,
            type_string,
            type_bool,
            type_user,
            type_var,
            type_fun,
            type_tuple,
        ))(visitor)
    }

    pub fn type_bigint<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_BIGINT)?;
        token::BIGINT(visitor)
    }

    pub fn type_bit<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_bool<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_BOOL)?;
        token::BOOL(visitor)
    }

    pub fn type_double<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_DOUBLE)?;
        token::DOUBLE(visitor)
    }

    pub fn type_float<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_FLOAT)?;
        token::FLOAT(visitor)
    }

    pub fn type_fun<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_signed<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_string<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_STRING)?;
        token::STRING(visitor)
    }

    pub fn type_trans<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_trans_fun<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_trans_rel<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_tuple<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_union<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_user<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn type_var<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn typedef<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn typedef_extern<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn typedef_normal<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn word<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub mod token {
        use super::*;

        macro_rules! make {
            ($name:tt) => {
                #[inline]
                #[allow(non_snake_case)]
                pub fn $name<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
                where
                    Ctx: Context<'tree> + 'tree,
                    Vis: Visitor<'tree, Ctx> + ?Sized,
                {
                    visitor.walker().token(super::super::token::$name)?;
                    Ok(())
                }
            };
        }

        // keywords
        make!(AND);
        make!(APPLY);
        make!(AS);
        make!(BIGINT);
        make!(BIT);
        make!(BOOL);
        make!(BREAK);
        make!(CONTINUE);
        make!(DOUBLE);
        make!(ELSE);
        make!(EXTERN);
        make!(FALSE);
        make!(FLAT_MAP);
        make!(FLOAT);
        make!(FOR);
        make!(FUNCTION);
        make!(GROUP_BY);
        make!(IDENTIFIER);
        make!(IF);
        make!(IMPORT);
        make!(IN);
        make!(INDEX);
        make!(INPUT);
        make!(INSPECT);
        make!(INTERNAL);
        make!(KEY);
        make!(MATCH);
        make!(MULTISET);
        make!(MUT);
        make!(NOT);
        make!(ON);
        make!(OR);
        make!(OUTPUT);
        make!(PRIMARY);
        make!(RELATION);
        make!(RETURN);
        make!(SIGNED);
        make!(SKIP);
        make!(STREAM);
        make!(STRING);
        make!(TRANSFORMER);
        make!(TRUE);
        make!(TYPE);
        make!(TYPEDEF);
        make!(VAR);

        // tokens
        make!(AMPERSAND);
        make!(ASTERISK);
        make!(ASTERISK_SOLIDUS);
        make!(CIRCUMFLEX_ACCENT);
        make!(COLON);
        make!(COLON_COLON);
        make!(COLON_HYPHEN_MINUS);
        make!(COMMA);
        make!(COMMERCIAL_AT);
        make!(DOLLAR_SIGN);
        make!(DOLLAR_SIGN_LEFT_CURLY_BRACKET);
        make!(EQUALS_SIGN);
        make!(EQUALS_SIGN_EQUALS_SIGN);
        make!(EQUALS_SIGN_GREATER_THAN_SIGN);
        make!(EXCLAMATION_MARK_EQUALS_SIGN);
        make!(FULL_STOP);
        make!(GREATER_THAN_SIGN);
        make!(GREATER_THAN_SIGN_GREATER_THAN_SIGN);
        make!(GREATER_THAN_SIGN_EQUALS_SIGN);
        make!(HYPHEN_MINUS);
        make!(LEFT_CURLY_BRACKET);
        make!(LEFT_PARENTHESIS);
        make!(LEFT_SQUARE_BRACKET);
        make!(LESS_THAN_SIGN);
        make!(LESS_THAN_SIGN_EQUALS_SIGN);
        make!(LESS_THAN_SIGN_LESS_THAN_SIGN);
        make!(LIT_BIN);
        make!(LIT_DEC);
        make!(LIT_FLOAT);
        make!(LIT_HEX);
        make!(LIT_OCT);
        make!(LIT_S_BIN);
        make!(LIT_S_DEC);
        make!(LIT_S_HEX);
        make!(LIT_S_OCT);
        make!(LOW_LINE);
        make!(NUMBER_SIGN);
        make!(NUMBER_SIGN_LEFT_SQUARE_BRACKET);
        make!(PERCENT_SIGN);
        make!(PLUS_SIGN);
        make!(PLUS_SIGN_PLUS_SIGN);
        make!(QUESTION_MARK);
        make!(REVERSE_SOLIDUS_REVERSE_SOLIDUS);
        make!(RIGHT_CURLY_BRACKET);
        make!(RIGHT_PARENTHESIS);
        make!(RIGHT_SQUARE_BRACKET);
        make!(RIGHTWARDS_ARROW);
        make!(SEMICOLON);
        make!(SOLIDUS);
        make!(SOLIDUS_ASTERISK);
        make!(SOLIDUS_SOLIDUS);
        make!(TILDE);
        make!(VERTICAL_LINE);
        make!(VERTICAL_LINE_RIGHT_SQUARE_BRACKET);
    }
}
