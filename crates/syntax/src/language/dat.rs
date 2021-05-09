//! Functions for working with the `.dat` grammar.

use crate::node::{NodeWalker, SyntaxErrors};

pub mod field {
    #![allow(missing_docs)]

    ddlog_lsp_macros::field_ids! {
        language: "ddlog.dat",
        fields: [
        ],
    }
}

pub mod kind {
    #![allow(missing_docs)]

    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dat",
        node_kinds: [
            (ROOT, "ROOT", true),
            (ARG, "arg", true),
            (ARG_OPT_TYPE, "arg_opt_type", true),
            (ATOM, "atom", true),
            (ATOM_ELEM, "atom_elem", true),
            (ATOM_POS, "atom_pos", true),
            (ATOM_REC, "atom_rec", true),
            (ATTRIBUTE, "attribute", true),
            (ATTRIBUTES, "attributes", true),
            (CLEAR, "clear", true),
            (COMMAND, "command", true),
            (COMMENT_LINE, "comment_line", true),
            (COMMIT, "commit", true),
            (CONS, "cons", true),
            (CONS_ARG, "cons_arg", true),
            (CONS_ARGS, "cons_args", true),
            (CONS_POS, "cons_pos", true),
            (CONS_REC, "cons_rec", true),
            (DELETE, "delete", true),
            (DELETE_KEY, "delete_key", true),
            (DUMP, "dump", true),
            (DUMP_INDEX, "dump_index", true),
            (ECHO, "echo", true),
            (EXIT, "exit", true),
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
            (IDENT, "ident", false),
            (IDENT_LOWER, "ident_lower", true),
            (IDENT_LOWER_SCOPED, "ident_lower_scoped", true),
            (IDENT_SCOPED, "ident_scoped", true),
            (IDENT_UPPER, "ident_upper", true),
            (IDENT_UPPER_SCOPED, "ident_upper_scoped", true),
            (INSERT, "insert", true),
            (INSERT_OR_UPDATE, "insert_or_update", true),
            (LIT_BOOL, "lit_bool", true),
            (LIT_MAP, "lit_map", true),
            (LIT_NUM, "lit_num", true),
            (LIT_NUM_BIN, "lit_num_bin", true),
            (LIT_NUM_DEC, "lit_num_dec", true),
            (LIT_NUM_FLOAT, "lit_num_float", true),
            (LIT_NUM_HEX, "lit_num_hex", true),
            (LIT_NUM_OCT, "lit_num_oct", true),
            (LIT_SERIALIZED, "lit_serialized", true),
            (LIT_STRING, "lit_string", true),
            (LIT_VEC, "lit_vec", true),
            (LOG_LEVEL, "log_level", true),
            (MISC_PAT0, "misc_pat0", true),
            (MODIFY, "modify", true),
            (NAME, "name", true),
            (NAME_ARG, "name_arg", true),
            (NAME_CONS, "name_cons", true),
            (NAME_FIELD, "name_field", true),
            (NAME_FUNC, "name_func", true),
            (NAME_INDEX, "name_index", true),
            (NAME_REL, "name_rel", true),
            (NAME_TYPE, "name_type", true),
            (NAME_VAR_TERM, "name_var_term", true),
            (PAT, "pat", true),
            (PAT_CONS, "pat_cons", true),
            (PAT_CONS_POS, "pat_cons_pos", true),
            (PAT_CONS_REC, "pat_cons_rec", true),
            (PAT_LIT, "pat_lit", true),
            (PAT_TERM_DECL_VAR, "pat_term_decl_var", true),
            (PAT_TUPLE, "pat_tuple", true),
            (PAT_TYPE, "pat_type", true),
            (PAT_WILD, "pat_wild", true),
            (PROFILE, "profile", true),
            (QUERY_INDEX, "query_index", true),
            (RECORD, "record", true),
            (RECORD_NAMED, "record_named", true),
            (ROLLBACK, "rollback", true),
            (SERDE_ENCODING, "serde_encoding", true),
            (SLEEP, "sleep", true),
            (START, "start", true),
            (STRING_QUOTED, "string_quoted", true),
            (STRING_QUOTED_ESCAPED, "string_quoted_escaped", true),
            (STRING_RAW, "string_raw", true),
            (STRING_RAW_INTERPOLATED, "string_raw_interpolated", true),
            (TIMESTAMP, "timestamp", true),
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
            (TYPE_VAR_IDENT, "type_var_ident", true),
            (UPDATE, "update", true),
            (UPDATES, "updates", true),
            (UPDATES_END, "updates_end", true),
            (VAL_ARRAY, "val_array", true),
            (VAL_STRUCT, "val_struct", true),
            (VAL_TUPLE, "val_tuple", true),
            (WORD, "word", true),
        ],
    }
}

pub mod token {
    #![allow(missing_docs)]

    // keywords
    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dat",
        node_kinds: [
            (AND, "and", false),
            (AS, "as", false),
            (BIGINT, "bigint", false),
            (BIT, "bit", false),
            (BOOL, "bool", false),
            (BREAK, "break", false),
            (CLEAR, "clear", false),
            (COMMIT, "commit", false),
            (CONTINUE, "continue", false),
            (CPU, "cpu", false),
            (DELETE, "delete", false),
            (DELETE_KEY, "delete_key", false),
            (DOUBLE, "double", false),
            (DUMP, "dump", false),
            (DUMP_CHANGES, "dump_changes", false),
            (DUMP_INDEX, "dump_index", false),
            (ECHO, "echo", false),
            (ELSE, "else", false),
            (EXIT, "exit", false),
            (FALSE, "false", false),
            (FLOAT, "float", false),
            (FOR, "for", false),
            (FUNCTION, "function", false),
            (IF, "if", false),
            (IN, "in", false),
            (INSERT, "insert", false),
            (INSERT_OR_UPDATE, "insert_or_update", false),
            (JSON, "json", false),
            (LOG_LEVEL, "log_level", false),
            (MATCH, "match", false),
            (MODIFY, "modify", false),
            (MUT, "mut", false),
            (NOT, "not", false),
            (OFF, "off", false),
            (ON, "on", false),
            (OR, "or", false),
            (PROFILE, "profile", false),
            (QUERY_INDEX, "query_index", false),
            (RELATION, "relation", false),
            (RETURN, "return", false),
            (ROLLBACK, "rollback", false),
            (SIGNED, "signed", false),
            (SLEEP, "sleep", false),
            (START, "start", false),
            (STRING, "string", false),
            (TIMESTAMP, "timestamp", false),
            (TRUE, "true", false),
            (VAR, "var", false),
        ]
    }

    // tokens
    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dat",
        node_kinds: [
            (AMPERSAND, "&", false),
            (APOSTROPHE, "'", false),
            (ASTERISK, "*", false),
            (CIRCUMFLEX_ACCENT, "^", false),
            (COLON, ":", false),
            (COMMA, ",", false),
            (COMMERCIAL_AT, "@", false),
            (DOLLAR_SIGN, "$", false),
            (EQUALS_SIGN, "=", false),
            (EQUALS_SIGN_EQUALS_SIGN, "==", false),
            (EQUALS_SIGN_GREATER_THAN_SIGN, "=>", false),
            (EXCLAMATION_MARK_EQUALS_SIGN, "!=", false),
            (FULL_STOP, ".", false),
            (GREATER_THAN_SIGN, ">", false),
            (GREATER_THAN_SIGN_EQUALS_SIGN, ">=", false),
            (GREATER_THAN_SIGN_GREATER_THAN_SIGN, ">>", false),
            (HYPHEN_MINUS, "-", false),
            (LEFT_CURLY_BRACKET, "{", false),
            (LEFT_PARENTHESIS, "(", false),
            (LEFT_SQUARE_BRACKET, "[", false),
            (LEFTWARDS_ARROW, "<-", false),
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
            (QUOTATION_MARK, "\"", false),
            (RIGHT_CURLY_BRACKET, "}", false),
            (RIGHT_PARENTHESIS, ")", false),
            (RIGHT_SQUARE_BRACKET, "]", false),
            (RIGHTWARDS_ARROW, "->", false),
            (SEMICOLON, ";", false),
            (SOLIDUS, "/", false),
            (TILDE, "~", false),
            (VERTICAL_LINE, "|", false),
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

    #[allow(non_snake_case)]
    fn visit_ROOT(&mut self) -> Result<(), SyntaxErrors> {
        visit::ROOT(self)
    }

    fn visit_atom(&mut self) -> Result<(), SyntaxErrors> {
        visit::atom(self)
    }

    fn visit_atom_elem(&mut self) -> Result<(), SyntaxErrors> {
        visit::atom_elem(self)
    }

    fn visit_atom_pos(&mut self) -> Result<(), SyntaxErrors> {
        visit::atom_pos(self)
    }

    fn visit_atom_rec(&mut self) -> Result<(), SyntaxErrors> {
        visit::atom_rec(self)
    }

    fn visit_clear(&mut self) -> Result<(), SyntaxErrors> {
        visit::clear(self)
    }

    fn visit_command(&mut self) -> Result<(), SyntaxErrors> {
        visit::command(self)
    }

    fn visit_comment_line(&mut self) -> Result<(), SyntaxErrors> {
        visit::comment_line(self)
    }

    fn visit_commit(&mut self) -> Result<(), SyntaxErrors> {
        visit::commit(self)
    }

    fn visit_cons_arg(&mut self) -> Result<(), SyntaxErrors> {
        visit::cons_arg(self)
    }

    fn visit_cons_args(&mut self) -> Result<(), SyntaxErrors> {
        visit::cons_args(self)
    }

    fn visit_delete(&mut self) -> Result<(), SyntaxErrors> {
        visit::delete(self)
    }

    fn visit_delete_key(&mut self) -> Result<(), SyntaxErrors> {
        visit::delete_key(self)
    }

    fn visit_dump(&mut self) -> Result<(), SyntaxErrors> {
        visit::dump(self)
    }

    fn visit_dump_index(&mut self) -> Result<(), SyntaxErrors> {
        visit::dump_index(self)
    }

    fn visit_echo(&mut self) -> Result<(), SyntaxErrors> {
        visit::echo(self)
    }

    fn visit_exit(&mut self) -> Result<(), SyntaxErrors> {
        visit::exit(self)
    }

    fn visit_exp(&mut self) -> Result<(), SyntaxErrors> {
        visit::exp(self)
    }

    fn visit_insert(&mut self) -> Result<(), SyntaxErrors> {
        visit::insert(self)
    }

    fn visit_insert_or_update(&mut self) -> Result<(), SyntaxErrors> {
        visit::insert_or_update(self)
    }

    fn visit_lit_num_hex(&mut self) -> Result<(), SyntaxErrors> {
        visit::lit_num_hex(self)
    }

    fn visit_lit_serialized(&mut self) -> Result<(), SyntaxErrors> {
        visit::lit_serialized(self)
    }

    fn visit_lit_string(&mut self) -> Result<(), SyntaxErrors> {
        visit::lit_string(self)
    }

    fn visit_log_level(&mut self) -> Result<(), SyntaxErrors> {
        visit::log_level(self)
    }

    fn visit_misc_pat0(&mut self) -> Result<(), SyntaxErrors> {
        visit::misc_pat0(self)
    }

    fn visit_modify(&mut self) -> Result<(), SyntaxErrors> {
        visit::modify(self)
    }

    fn visit_name_rel(&mut self) -> Result<(), SyntaxErrors> {
        visit::name_rel(self)
    }

    fn visit_profile(&mut self) -> Result<(), SyntaxErrors> {
        visit::profile(self)
    }

    fn visit_query_index(&mut self) -> Result<(), SyntaxErrors> {
        visit::query_index(self)
    }

    fn visit_record(&mut self) -> Result<(), SyntaxErrors> {
        visit::record(self)
    }

    fn visit_record_named(&mut self) -> Result<(), SyntaxErrors> {
        visit::record_named(self)
    }

    fn visit_rollback(&mut self) -> Result<(), SyntaxErrors> {
        visit::rollback(self)
    }

    fn visit_serde_encoding(&mut self) -> Result<(), SyntaxErrors> {
        visit::serde_encoding(self)
    }

    fn visit_sleep(&mut self) -> Result<(), SyntaxErrors> {
        visit::sleep(self)
    }

    fn visit_start(&mut self) -> Result<(), SyntaxErrors> {
        visit::start(self)
    }

    fn visit_timestamp(&mut self) -> Result<(), SyntaxErrors> {
        visit::timestamp(self)
    }

    fn visit_update(&mut self) -> Result<(), SyntaxErrors> {
        visit::update(self)
    }

    fn visit_updates(&mut self) -> Result<(), SyntaxErrors> {
        visit::updates(self)
    }

    fn visit_val_array(&mut self) -> Result<(), SyntaxErrors> {
        visit::val_array(self)
    }

    fn visit_val_struct(&mut self) -> Result<(), SyntaxErrors> {
        visit::val_struct(self)
    }

    fn visit_val_tuple(&mut self) -> Result<(), SyntaxErrors> {
        visit::val_tuple(self)
    }

    fn visit_word(&mut self) -> Result<(), SyntaxErrors> {
        visit::word(self)
    }
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

    ddlog_lsp_macros::impl_choice!(1);
    ddlog_lsp_macros::impl_choice!(2);
    ddlog_lsp_macros::impl_choice!(3);
    ddlog_lsp_macros::impl_choice!(4);
    ddlog_lsp_macros::impl_choice!(5);
    ddlog_lsp_macros::impl_choice!(6);
    ddlog_lsp_macros::impl_choice!(9);
    ddlog_lsp_macros::impl_choice!(11);
    ddlog_lsp_macros::impl_choice!(12);
    ddlog_lsp_macros::impl_choice!(14);
    ddlog_lsp_macros::impl_choice!(51);

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
        utils::repeat(command)(visitor)
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

    pub fn atom<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM)?;
        utils::seq((atom_rec, atom_pos, atom_elem))(visitor)
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
            name_rel,
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
            name_rel,
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

    pub fn clear<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CLEAR)?;
        utils::seq((token::CLEAR, name_rel, token::SEMICOLON))(visitor)
    }

    pub fn command<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMAND)?;
        utils::choice((
            clear,
            commit,
            dump,
            dump_index,
            echo,
            exit,
            log_level,
            profile,
            query_index,
            rollback,
            sleep,
            start,
            timestamp,
            updates,
        ))(visitor)
    }

    pub fn comment_line<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_LINE)?;
        Ok(())
    }

    pub fn commit<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMIT)?;
        utils::seq((token::COMMIT, utils::optional(token::DUMP_CHANGES), token::SEMICOLON))(visitor)
    }

    pub fn cons<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS)?;
        utils::choice((cons_rec, cons_pos))(visitor)
    }

    pub fn cons_arg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_ARG)?;
        utils::seq((cons_arg, utils::repeat(utils::seq((token::COMMA, cons_arg)))))(visitor)
    }

    pub fn cons_args<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_ARGS)?;
        utils::choice((record_named, record))(visitor)
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

    pub fn delete<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DELETE)?;
        utils::seq((token::DELETE, atom))(visitor)
    }

    pub fn delete_key<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DELETE_KEY)?;
        utils::seq((token::DELETE_KEY, name_rel, exp))(visitor)
    }

    pub fn dump<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DUMP)?;
        utils::seq((token::DUMP, utils::optional(name_rel), token::SEMICOLON))(visitor)
    }

    pub fn dump_index<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DUMP_INDEX)?;
        utils::seq((token::DUMP_INDEX, name_index, token::SEMICOLON))(visitor)
    }

    pub fn echo<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ECHO)?;
        utils::seq((token::ECHO, misc_pat0, token::SEMICOLON))(visitor)
    }

    pub fn exit<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXIT)?;
        utils::seq((token::EXIT, token::SEMICOLON))(visitor)
    }

    pub fn exp<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP)?;
        utils::choice((
            lit_serialized,
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
        utils::seq((exp, token::EQUALS_SIGN, exp))(visitor)
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
        visitor.walker().rule(kind::EXP_CONS_REC)?;
        utils::seq((
            name_cons,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                token::FULL_STOP,
                name_field,
                token::EQUALS_SIGN,
                exp,
                utils::repeat(utils::seq((
                    token::COMMA,
                    token::FULL_STOP,
                    name_field,
                    token::EQUALS_SIGN,
                    exp,
                ))),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor)
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

    pub fn insert<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INSERT)?;
        utils::seq((token::INSERT, atom))(visitor)
    }

    pub fn insert_or_update<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INSERT_OR_UPDATE)?;
        utils::seq((token::INSERT_OR_UPDATE, atom))(visitor)
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

    pub fn lit_serialized<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_SERIALIZED)?;
        utils::seq((token::COMMERCIAL_AT, serde_encoding))(visitor)
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

    pub fn log_level<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LOG_LEVEL)?;
        utils::seq((token::LOG_LEVEL, misc_pat0))(visitor)
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

    pub fn misc_pat0<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MISC_PAT0)?;
        Ok(())
    }

    pub fn modify<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MODIFY)?;
        utils::seq((token::MODIFY, name_rel, record, token::LEFTWARDS_ARROW, record))(visitor)
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

    pub fn profile<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PROFILE)?;
        utils::seq((
            token::PROFILE,
            utils::optional(utils::seq((token::CPU, utils::choice((token::ON, token::OFF))))),
            token::SEMICOLON,
        ))(visitor)
    }

    pub fn query_index<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::QUERY_INDEX)?;
        utils::seq((
            token::QUERY_INDEX,
            name_index,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                arg,
                utils::repeat(utils::seq((token::COMMA, arg))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
            token::SEMICOLON,
        ))(visitor)
    }

    pub fn record<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RECORD)?;
        utils::choice((
            lit_bool,
            lit_string,
            lit_serialized,
            val_tuple,
            val_array,
            val_struct,
            lit_num_float,
            lit_num_dec,
            lit_num_hex,
        ))(visitor)
    }

    pub fn record_named<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RECORD_NAMED)?;
        utils::seq((token::FULL_STOP, name_field, token::EQUALS_SIGN, record))(visitor)
    }

    pub fn rollback<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ROLLBACK)?;
        utils::seq((token::ROLLBACK, token::SEMICOLON))(visitor)
    }

    pub fn serde_encoding<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::SERDE_ENCODING)?;
        token::JSON(visitor)
    }

    pub fn sleep<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::SLEEP)?;
        utils::seq((token::SLEEP, misc_pat0, token::SEMICOLON))(visitor)
    }

    pub fn start<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::START)?;
        utils::seq((token::START, token::SEMICOLON))(visitor)
    }

    // NOTE: might have to descend into subnodes
    pub fn string_quoted<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_QUOTED)?;
        Ok(())
    }

    // NOTE: might have to descend into subnodes
    pub fn string_quoted_escaped<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_QUOTED_ESCAPED)?;
        Ok(())
    }

    // NOTE: might have to descend into subnodes
    pub fn string_raw<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_RAW)?;
        Ok(())
    }

    // NOTE: might have to descend into subnodes
    pub fn string_raw_interpolated<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_RAW_INTERPOLATED)?;
        Ok(())
    }

    pub fn timestamp<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TIMESTAMP)?;
        utils::seq((token::TIMESTAMP, token::SEMICOLON))(visitor)
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
        visitor.walker().rule(kind::TYPE_BIT)?;
        utils::seq((token::BIT, token::LESS_THAN_SIGN, lit_num_dec, token::GREATER_THAN_SIGN))(visitor)
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
        visitor.walker().rule(kind::TYPE_FUN)?;
        utils::choice((
            utils::seq((
                token::FUNCTION,
                token::LEFT_PARENTHESIS,
                utils::optional(utils::seq((
                    utils::optional(token::MUT),
                    r#type,
                    utils::repeat(utils::seq((token::COMMA, utils::optional(token::MUT), r#type))),
                ))),
                token::RIGHT_PARENTHESIS,
                utils::optional(utils::seq((token::COLON, r#type))),
            )),
            utils::seq((
                token::VERTICAL_LINE,
                utils::optional(utils::seq((
                    utils::optional(token::MUT),
                    r#type,
                    utils::repeat(utils::seq((token::COMMA, utils::optional(token::MUT), r#type))),
                ))),
                token::VERTICAL_LINE,
                utils::optional(utils::seq((token::COLON, r#type))),
            )),
        ))(visitor)
    }

    pub fn type_signed<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_SIGNED)?;
        utils::seq((
            token::SIGNED,
            token::LESS_THAN_SIGN,
            lit_num_dec,
            token::GREATER_THAN_SIGN,
        ))(visitor)
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
        visitor.walker().rule(kind::TYPE_TRANS)?;
        utils::choice((type_trans_fun, type_trans_rel))(visitor)
    }

    pub fn type_trans_fun<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_TRANS_FUN)?;
        utils::seq((
            token::FUNCTION,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((arg, utils::repeat(utils::seq((token::COMMA, arg)))))),
            token::RIGHT_PARENTHESIS,
            token::COLON,
            type_atom,
        ))(visitor)
    }

    pub fn type_trans_rel<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_TRANS_REL)?;
        utils::seq((
            token::RELATION,
            token::LEFT_SQUARE_BRACKET,
            type_atom,
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor)
    }

    pub fn type_tuple<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_TUPLE)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                type_atom,
                utils::repeat(utils::seq((token::COMMA, type_atom))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn type_union<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_UNION)?;
        utils::seq((utils::repeat(utils::seq((cons, token::VERTICAL_LINE))), cons))(visitor)
    }

    pub fn type_user<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_USER)?;
        utils::seq((
            name_type,
            utils::optional(utils::seq((
                token::LESS_THAN_SIGN,
                r#type,
                utils::repeat(utils::seq((token::COMMA, r#type))),
                token::GREATER_THAN_SIGN,
            ))),
        ))(visitor)
    }

    pub fn type_var<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_VAR)?;
        utils::seq((token::APOSTROPHE, type_var_ident))(visitor)
    }

    pub fn type_var_ident<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_VAR_IDENT)?;
        Ok(())
    }

    pub fn update<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::UPDATE)?;
        utils::choice((delete, delete_key, insert, insert_or_update, modify))(visitor)
    }

    pub fn updates<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::UPDATES)?;
        utils::seq((update, utils::repeat(utils::seq((token::COMMA, update))), updates_end))(visitor)
    }

    pub fn updates_end<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::UPDATES_END)?;
        Ok(())
    }

    pub fn val_array<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::VAL_ARRAY)?;
        utils::seq((
            token::LEFT_SQUARE_BRACKET,
            utils::optional(utils::seq((
                record,
                utils::repeat(utils::seq((token::COMMA, record))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor)
    }

    pub fn val_struct<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::VAL_STRUCT)?;
        utils::seq((
            name_rel,
            utils::optional(utils::seq((
                token::LEFT_CURLY_BRACKET,
                cons_args,
                token::RIGHT_CURLY_BRACKET,
            ))),
        ))(visitor)
    }

    pub fn val_tuple<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::VAL_TUPLE)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                record,
                utils::repeat(utils::seq((token::COMMA, record))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor)
    }

    pub fn word<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::WORD)?;
        Ok(())
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
        make!(AS);
        make!(BIGINT);
        make!(BIT);
        make!(BOOL);
        make!(BREAK);
        make!(CLEAR);
        make!(COMMIT);
        make!(CONTINUE);
        make!(CPU);
        make!(DELETE);
        make!(DELETE_KEY);
        make!(DOUBLE);
        make!(DUMP);
        make!(DUMP_CHANGES);
        make!(DUMP_INDEX);
        make!(ECHO);
        make!(ELSE);
        make!(EXIT);
        make!(FALSE);
        make!(FLOAT);
        make!(FOR);
        make!(FUNCTION);
        make!(IF);
        make!(IN);
        make!(INSERT);
        make!(INSERT_OR_UPDATE);
        make!(JSON);
        make!(LOG_LEVEL);
        make!(MATCH);
        make!(MODIFY);
        make!(MUT);
        make!(NOT);
        make!(OFF);
        make!(ON);
        make!(OR);
        make!(PROFILE);
        make!(QUERY_INDEX);
        make!(RELATION);
        make!(RETURN);
        make!(ROLLBACK);
        make!(SIGNED);
        make!(SLEEP);
        make!(START);
        make!(STRING);
        make!(TIMESTAMP);
        make!(TRUE);
        make!(VAR);

        // tokens
        make!(AMPERSAND);
        make!(APOSTROPHE);
        make!(ASTERISK);
        make!(CIRCUMFLEX_ACCENT);
        make!(COLON);
        make!(COMMA);
        make!(COMMERCIAL_AT);
        make!(DOLLAR_SIGN);
        make!(EQUALS_SIGN);
        make!(EQUALS_SIGN_EQUALS_SIGN);
        make!(EQUALS_SIGN_GREATER_THAN_SIGN);
        make!(EXCLAMATION_MARK_EQUALS_SIGN);
        make!(FULL_STOP);
        make!(GREATER_THAN_SIGN);
        make!(GREATER_THAN_SIGN_EQUALS_SIGN);
        make!(GREATER_THAN_SIGN_GREATER_THAN_SIGN);
        make!(HYPHEN_MINUS);
        make!(LEFT_CURLY_BRACKET);
        make!(LEFT_PARENTHESIS);
        make!(LEFT_SQUARE_BRACKET);
        make!(LEFTWARDS_ARROW);
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
        make!(QUOTATION_MARK);
        make!(RIGHT_CURLY_BRACKET);
        make!(RIGHT_PARENTHESIS);
        make!(RIGHT_SQUARE_BRACKET);
        make!(RIGHTWARDS_ARROW);
        make!(SEMICOLON);
        make!(SOLIDUS);
        make!(TILDE);
        make!(VERTICAL_LINE);
    }
}
