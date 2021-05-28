//! Functions for working with the `.dat` grammar.

use crate::{
    language::NodeMove,
    node::{NodeWalker, SyntaxErrors},
};

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
    fn visit_ROOT(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ROOT(self, m)
    }

    fn visit_arg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::arg(self, m)
    }

    fn visit_arg_opt_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::arg_opt_type(self, m)
    }

    fn visit_atom(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::atom(self, m)
    }

    fn visit_atom_elem(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::atom_elem(self, m)
    }

    fn visit_atom_pos(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::atom_pos(self, m)
    }

    fn visit_atom_rec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::atom_rec(self, m)
    }

    fn visit_attribute(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::attribute(self, m)
    }

    fn visit_attributes(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::attributes(self, m)
    }

    fn visit_clear(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::clear(self, m)
    }

    fn visit_command(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::command(self, m)
    }

    fn visit_comment_line(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::comment_line(self, m)
    }

    fn visit_commit(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::commit(self, m)
    }

    fn visit_cons(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons(self, m)
    }

    fn visit_cons_arg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons_arg(self, m)
    }

    fn visit_cons_args(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons_args(self, m)
    }

    fn visit_cons_pos(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons_pos(self, m)
    }

    fn visit_cons_rec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons_rec(self, m)
    }

    fn visit_delete(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::delete(self, m)
    }

    fn visit_delete_key(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::delete_key(self, m)
    }

    fn visit_dump(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::dump(self, m)
    }

    fn visit_dump_index(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::dump_index(self, m)
    }

    fn visit_echo(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::echo(self, m)
    }

    fn visit_exit(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exit(self, m)
    }

    fn visit_exp(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp(self, m)
    }

    fn visit_exp_add(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_add(self, m)
    }

    fn visit_exp_assign(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_assign(self, m)
    }

    fn visit_exp_binding(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_binding(self, m)
    }

    fn visit_exp_bit_and(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_bit_and(self, m)
    }

    fn visit_exp_bit_neg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_bit_neg(self, m)
    }

    fn visit_exp_bit_or(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_bit_or(self, m)
    }

    fn visit_exp_bit_slice(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_bit_slice(self, m)
    }

    fn visit_exp_bit_xor(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_bit_xor(self, m)
    }

    fn visit_exp_block(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_block(self, m)
    }

    fn visit_exp_break(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_break(self, m)
    }

    fn visit_exp_cast(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_cast(self, m)
    }

    fn visit_exp_cat(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_cat(self, m)
    }

    fn visit_exp_cond(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_cond(self, m)
    }

    fn visit_exp_cons_pos(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_cons_pos(self, m)
    }

    fn visit_exp_cons_rec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_cons_rec(self, m)
    }

    fn visit_exp_continue(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_continue(self, m)
    }

    fn visit_exp_decl_var(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_decl_var(self, m)
    }

    fn visit_exp_div(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_div(self, m)
    }

    fn visit_exp_eq(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_eq(self, m)
    }

    fn visit_exp_field(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_field(self, m)
    }

    fn visit_exp_for(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_for(self, m)
    }

    fn visit_exp_fun_call(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_fun_call(self, m)
    }

    fn visit_exp_fun_call_dot(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_fun_call_dot(self, m)
    }

    fn visit_exp_gt(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_gt(self, m)
    }

    fn visit_exp_gteq(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_gteq(self, m)
    }

    fn visit_exp_lambda(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_lambda(self, m)
    }

    fn visit_exp_lit(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_lit(self, m)
    }

    fn visit_exp_log_and(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_log_and(self, m)
    }

    fn visit_exp_log_imp(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_log_imp(self, m)
    }

    fn visit_exp_log_neg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_log_neg(self, m)
    }

    fn visit_exp_log_or(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_log_or(self, m)
    }

    fn visit_exp_lt(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_lt(self, m)
    }

    fn visit_exp_lteq(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_lteq(self, m)
    }

    fn visit_exp_match(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_match(self, m)
    }

    fn visit_exp_mul(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_mul(self, m)
    }

    fn visit_exp_neg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_neg(self, m)
    }

    fn visit_exp_neq(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_neq(self, m)
    }

    fn visit_exp_proj(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_proj(self, m)
    }

    fn visit_exp_proj_digits(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_proj_digits(self, m)
    }

    fn visit_exp_ref(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_ref(self, m)
    }

    fn visit_exp_rem(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_rem(self, m)
    }

    fn visit_exp_return(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_return(self, m)
    }

    fn visit_exp_seq(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_seq(self, m)
    }

    fn visit_exp_shl(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_shl(self, m)
    }

    fn visit_exp_shr(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_shr(self, m)
    }

    fn visit_exp_slice(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_slice(self, m)
    }

    fn visit_exp_sub(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_sub(self, m)
    }

    fn visit_exp_try(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_try(self, m)
    }

    fn visit_exp_tuple(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_tuple(self, m)
    }

    fn visit_exp_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_type(self, m)
    }

    fn visit_exp_wild(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::exp_wild(self, m)
    }

    fn visit_field(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::field(self, m)
    }

    fn visit_ident(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ident(self, m)
    }

    fn visit_ident_lower(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ident_lower(self, m)
    }

    fn visit_ident_lower_scoped(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ident_lower_scoped(self, m)
    }

    fn visit_ident_scoped(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ident_scoped(self, m)
    }

    fn visit_ident_upper(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ident_upper(self, m)
    }

    fn visit_ident_upper_scoped(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ident_upper_scoped(self, m)
    }

    fn visit_insert(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::insert(self, m)
    }

    fn visit_insert_or_update(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::insert_or_update(self, m)
    }

    fn visit_lit_bool(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_bool(self, m)
    }

    fn visit_lit_map(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_map(self, m)
    }

    fn visit_lit_num(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_num(self, m)
    }

    fn visit_lit_num_bin(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_num_bin(self, m)
    }

    fn visit_lit_num_dec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_num_dec(self, m)
    }

    fn visit_lit_num_float(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_num_float(self, m)
    }

    fn visit_lit_num_hex(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_num_hex(self, m)
    }

    fn visit_lit_num_oct(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_num_oct(self, m)
    }

    fn visit_lit_serialized(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_serialized(self, m)
    }

    fn visit_lit_string(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_string(self, m)
    }

    fn visit_lit_vec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_vec(self, m)
    }

    fn visit_log_level(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::log_level(self, m)
    }

    fn visit_misc_pat0(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::misc_pat0(self, m)
    }

    fn visit_modify(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::modify(self, m)
    }

    fn visit_name(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name(self, m)
    }

    fn visit_name_arg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_arg(self, m)
    }

    fn visit_name_cons(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_cons(self, m)
    }

    fn visit_name_field(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_field(self, m)
    }

    fn visit_name_func(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_func(self, m)
    }

    fn visit_name_index(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_index(self, m)
    }

    fn visit_name_rel(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_rel(self, m)
    }

    fn visit_name_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_type(self, m)
    }

    fn visit_name_var_term(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_var_term(self, m)
    }

    fn visit_pat(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat(self, m)
    }

    fn visit_pat_cons(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_cons(self, m)
    }

    fn visit_pat_cons_pos(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_cons_pos(self, m)
    }

    fn visit_pat_cons_rec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_cons_rec(self, m)
    }

    fn visit_pat_lit(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_lit(self, m)
    }

    fn visit_pat_term_decl_var(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_term_decl_var(self, m)
    }

    fn visit_pat_tuple(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_tuple(self, m)
    }

    fn visit_pat_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_type(self, m)
    }

    fn visit_pat_wild(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::pat_wild(self, m)
    }

    fn visit_profile(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::profile(self, m)
    }

    fn visit_query_index(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::query_index(self, m)
    }

    fn visit_record(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::record(self, m)
    }

    fn visit_record_named(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::record_named(self, m)
    }

    fn visit_rollback(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rollback(self, m)
    }

    fn visit_serde_encoding(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::serde_encoding(self, m)
    }

    fn visit_sleep(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::sleep(self, m)
    }

    fn visit_start(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::start(self, m)
    }

    fn visit_string_quoted(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::string_quoted(self, m)
    }

    fn visit_string_quoted_escaped(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::string_quoted_escaped(self, m)
    }

    fn visit_string_raw(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::string_raw(self, m)
    }

    fn visit_string_raw_interpolated(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::string_raw_interpolated(self, m)
    }

    fn visit_timestamp(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::timestamp(self, m)
    }

    fn visit_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::r#type(self, m)
    }

    fn visit_type_atom(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_atom(self, m)
    }

    fn visit_type_bigint(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_bigint(self, m)
    }

    fn visit_type_bit(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_bit(self, m)
    }

    fn visit_type_bool(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_bool(self, m)
    }

    fn visit_type_double(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_double(self, m)
    }

    fn visit_type_float(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_float(self, m)
    }

    fn visit_type_fun(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_fun(self, m)
    }

    fn visit_type_signed(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_signed(self, m)
    }

    fn visit_type_string(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_string(self, m)
    }

    fn visit_type_trans(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_trans(self, m)
    }

    fn visit_type_trans_fun(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_trans_fun(self, m)
    }

    fn visit_type_trans_rel(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_trans_rel(self, m)
    }

    fn visit_type_tuple(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_tuple(self, m)
    }

    fn visit_type_union(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_union(self, m)
    }

    fn visit_type_user(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_user(self, m)
    }

    fn visit_type_var(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_var(self, m)
    }

    fn visit_type_var_ident(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::type_var_ident(self, m)
    }

    fn visit_update(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::update(self, m)
    }

    fn visit_updates(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::updates(self, m)
    }

    fn visit_updates_end(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::updates_end(self, m)
    }

    fn visit_val_array(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::val_array(self, m)
    }

    fn visit_val_struct(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::val_struct(self, m)
    }

    fn visit_val_tuple(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::val_tuple(self, m)
    }

    fn visit_word(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::word(self, m)
    }
}

pub mod utils {
    #![allow(missing_docs)]
    #![allow(unused)]

    use super::*;
    use crate::node::{Context, SyntaxError};

    pub trait Choice<'tree, Ctx, Vis, Ret>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        fn choice(&self, visitor: &mut Vis, m: NodeMove) -> Result<Ret, SyntaxErrors>;
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
    pub fn choice<'tree, Ctx, Vis, Ret, T>(funs: T) -> impl Fn(&mut Vis, NodeMove) -> Result<Ret, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
        T: Choice<'tree, Ctx, Vis, Ret>,
    {
        move |visitor, m| funs.choice(visitor, m)
    }

    #[inline]
    pub fn optional<'tree, Ctx, Vis>(
        fun: impl Fn(&mut Vis, NodeMove) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis, NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor, m| {
            let prev = visitor.node();
            if fun(visitor, m).is_err() {
                visitor.reset(prev);
            }
            Ok(())
        }
    }

    pub fn repeat<'tree, Ctx, Vis>(
        fun: impl Fn(&mut Vis, NodeMove) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis, NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor, m| {
            loop {
                let prev = visitor.node();
                if visitor.walker().done {
                    break;
                }
                if fun(visitor, m).is_err() {
                    break;
                }
            }
            Ok(())
        }
    }

    #[inline]
    pub fn repeat1<'tree, Ctx, Vis>(
        fun: impl Fn(&mut Vis, NodeMove) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis, NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor, m| {
            let mut errors = SyntaxErrors::new();
            let mut succeeded_once = false;
            if visitor.walker().done {
                errors.push(SyntaxError::DoneEarly);
                return Err(errors);
            }
            loop {
                let prev = visitor.node();
                match fun(visitor, m) {
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
        fn seq(&self, visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>;
    }

    ddlog_lsp_macros::impl_seq!(2);
    ddlog_lsp_macros::impl_seq!(3);
    ddlog_lsp_macros::impl_seq!(4);
    ddlog_lsp_macros::impl_seq!(5);
    ddlog_lsp_macros::impl_seq!(6);
    ddlog_lsp_macros::impl_seq!(7);
    ddlog_lsp_macros::impl_seq!(8);

    #[inline]
    pub fn seq<'tree, Ctx, Vis, T>(funs: T) -> impl Fn(&mut Vis, NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
        T: Seq<'tree, Ctx, Vis>,
    {
        move |visitor, m| funs.seq(visitor, m)
    }

    #[inline]
    pub fn restore<'tree, Ctx, Vis, Ret>(
        fun: impl Fn(&mut Vis, NodeMove) -> Result<Ret, SyntaxErrors>,
    ) -> impl Fn(&mut Vis, NodeMove) -> Result<Ret, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        move |visitor, m| {
            let prev = visitor.node();
            fun(visitor, m).map_err(|ref mut errs| {
                visitor.reset(prev);
                let mut errors = SyntaxErrors::new();
                errors.append(errs);
                errors
            })
        }
    }
}

#[allow(unused)]
#[allow(missing_docs)]
pub mod visit {
    use super::*;
    use crate::node::Context;

    #[allow(non_snake_case)]
    pub fn ROOT<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ROOT, m)?;
        utils::repeat(command)(visitor, NodeMove::Step)
    }

    pub fn arg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG, m)?;
        utils::seq((name_arg, token::COLON, utils::optional(token::MUT), type_atom))(visitor, NodeMove::Step)
    }

    pub fn arg_opt_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG_OPT_TYPE, m)?;
        utils::seq((
            name_arg,
            utils::optional(utils::seq((token::COLON, utils::optional(token::MUT), type_atom))),
            type_atom,
        ))(visitor, NodeMove::Step)
    }

    pub fn atom<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM, m)?;
        utils::seq((atom_rec, atom_pos, atom_elem))(visitor, NodeMove::Step)
    }

    pub fn atom_elem<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_ELEM, m)?;
        utils::seq((name_rel, token::LEFT_SQUARE_BRACKET, exp, token::RIGHT_SQUARE_BRACKET))(visitor, NodeMove::Step)
    }

    pub fn atom_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_POS, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn atom_rec<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_REC, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn attribute<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATTRIBUTE, m)?;
        utils::seq((name, utils::optional(utils::seq((token::EQUALS_SIGN, exp)))))(visitor, NodeMove::Step)
    }

    pub fn attributes<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATTRIBUTES, m)?;
        utils::repeat1(utils::seq((
            token::NUMBER_SIGN_LEFT_SQUARE_BRACKET,
            attribute,
            utils::repeat(utils::seq((token::COMMA, attribute))),
            token::RIGHT_SQUARE_BRACKET,
        )))(visitor, NodeMove::Step)
    }

    pub fn clear<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CLEAR, m)?;
        utils::seq((token::CLEAR, name_rel, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn command<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMAND, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn comment_line<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_LINE, m)?;
        Ok(())
    }

    pub fn commit<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMIT, m)?;
        utils::seq((token::COMMIT, utils::optional(token::DUMP_CHANGES), token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn cons<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS, m)?;
        utils::choice((cons_rec, cons_pos))(visitor, NodeMove::Step)
    }

    pub fn cons_arg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_ARG, m)?;
        utils::seq((cons_arg, utils::repeat(utils::seq((token::COMMA, cons_arg)))))(visitor, NodeMove::Step)
    }

    pub fn cons_args<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_ARGS, m)?;
        utils::choice((record_named, record))(visitor, NodeMove::Step)
    }

    pub fn cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_POS, m)?;
        utils::seq((utils::optional(attributes), name_cons))(visitor, NodeMove::Step)
    }

    pub fn cons_rec<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_REC, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn delete<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DELETE, m)?;
        utils::seq((token::DELETE, atom))(visitor, NodeMove::Step)
    }

    pub fn delete_key<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DELETE_KEY, m)?;
        utils::seq((token::DELETE_KEY, name_rel, exp))(visitor, NodeMove::Step)
    }

    pub fn dump<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DUMP, m)?;
        utils::seq((token::DUMP, utils::optional(name_rel), token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn dump_index<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::DUMP_INDEX, m)?;
        utils::seq((token::DUMP_INDEX, name_index, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn echo<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ECHO, m)?;
        utils::seq((token::ECHO, misc_pat0, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn exit<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXIT, m)?;
        utils::seq((token::EXIT, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn exp<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_add<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_ADD, m)?;
        utils::seq((exp, token::PLUS_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_assign<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_ASSIGN, m)?;
        utils::seq((exp, token::EQUALS_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_binding<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BINDING, m)?;
        utils::seq((name_var_term, token::COMMERCIAL_AT, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_and<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_AND, m)?;
        utils::seq((exp, token::AMPERSAND, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_neg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_NEG, m)?;
        utils::seq((token::TILDE, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_or<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_OR, m)?;
        utils::seq((exp, token::VERTICAL_LINE, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_slice<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_OR, m)?;
        utils::seq((exp, token::LEFT_SQUARE_BRACKET, token::RIGHT_SQUARE_BRACKET))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_xor<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_XOR, m)?;
        utils::seq((exp, token::CIRCUMFLEX_ACCENT, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_block<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BLOCK, m)?;
        utils::seq((
            token::LEFT_CURLY_BRACKET,
            utils::optional(exp),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_break<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BREAK, m)?;
        token::BREAK(visitor, NodeMove::Step)
    }

    pub fn exp_cast<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CAST, m)?;
        utils::seq((exp, token::AS, type_atom))(visitor, NodeMove::Step)
    }

    pub fn exp_cat<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CAT, m)?;
        utils::seq((exp, token::PLUS_SIGN_PLUS_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_cond<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_COND, m)?;
        utils::seq((token::IF, exp, exp, utils::optional(utils::seq((token::ELSE, exp)))))(visitor, NodeMove::Step)
    }

    pub fn exp_cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CONS_POS, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_cons_rec<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CONS_REC, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_continue<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CONTINUE, m)?;
        token::CONTINUE(visitor, NodeMove::Step)
    }

    pub fn exp_decl_var<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DECL_VAR, m)?;
        utils::seq((utils::optional(token::VAR), name_var_term))(visitor, NodeMove::Step)
    }

    pub fn exp_div<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DIV, m)?;
        utils::seq((exp, token::SOLIDUS, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_eq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DIV, m)?;
        utils::seq((exp, token::EQUALS_SIGN_EQUALS_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_field<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FIELD, m)?;
        utils::seq((exp, token::FULL_STOP, ident))(visitor, NodeMove::Step)
    }

    pub fn exp_for<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FOR, m)?;
        utils::seq((
            token::FOR,
            token::LEFT_PARENTHESIS,
            name_var_term,
            token::IN,
            exp,
            token::RIGHT_PARENTHESIS,
            exp,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_fun_call<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FUN_CALL, m)?;
        utils::seq((
            exp,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                exp,
                utils::repeat(utils::seq((token::COMMA, exp))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_fun_call_dot<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FUN_CALL_DOT, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_gt<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_GT, m)?;
        utils::seq((exp, token::GREATER_THAN_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_gteq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_GTEQ, m)?;
        utils::seq((exp, token::GREATER_THAN_SIGN_EQUALS_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_lambda<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LAMBDA, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_lit<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LIT, m)?;
        utils::choice((lit_bool, lit_num, lit_map, lit_string, lit_vec))(visitor, NodeMove::Step)
    }

    pub fn exp_log_and<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_AND, m)?;
        utils::seq((exp, token::AND, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_log_imp<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_IMP, m)?;
        utils::seq((exp, token::EQUALS_SIGN_GREATER_THAN_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_log_neg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_NEG, m)?;
        utils::seq((token::NOT, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_log_or<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_OR, m)?;
        utils::seq((exp, token::OR, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_lt<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LT, m)?;
        utils::seq((exp, token::LESS_THAN_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_lteq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LTEQ, m)?;
        utils::seq((exp, token::LESS_THAN_SIGN_EQUALS_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_match<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_MATCH, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_mul<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_MUL, m)?;
        utils::seq((exp, token::ASTERISK, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_neg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_NEG, m)?;
        utils::seq((token::HYPHEN_MINUS, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_neq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_NEQ, m)?;
        utils::seq((exp, token::EXCLAMATION_MARK_EQUALS_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_proj<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_PROJ, m)?;
        utils::seq((exp, token::FULL_STOP, exp_proj_digits))(visitor, NodeMove::Step)
    }

    pub fn exp_proj_digits<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_PROJ_DIGITS, m)?;
        Ok(())
    }

    pub fn exp_ref<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_REF, m)?;
        utils::seq((token::AMPERSAND, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_rem<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_REM, m)?;
        utils::seq((exp, token::PERCENT_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_return<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_RETURN, m)?;
        utils::seq((token::RETURN, utils::optional(exp)))(visitor, NodeMove::Step)
    }

    pub fn exp_seq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SEQ, m)?;
        utils::seq((exp, token::SEMICOLON, utils::optional(exp)))(visitor, NodeMove::Step)
    }

    pub fn exp_shl<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SHL, m)?;
        utils::seq((exp, token::LESS_THAN_SIGN_LESS_THAN_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_shr<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SHR, m)?;
        utils::seq((exp, token::GREATER_THAN_SIGN_GREATER_THAN_SIGN, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_slice<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SLICE, m)?;
        utils::seq((
            exp,
            token::LEFT_SQUARE_BRACKET,
            lit_num_dec,
            token::COLON,
            lit_num_dec,
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_sub<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SUB, m)?;
        utils::seq((exp, token::HYPHEN_MINUS, exp))(visitor, NodeMove::Step)
    }

    pub fn exp_try<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_TRY, m)?;
        utils::seq((exp, token::QUESTION_MARK))(visitor, NodeMove::Step)
    }

    pub fn exp_tuple<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_TUPLE, m)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                exp,
                utils::repeat(utils::seq((token::COMMA, exp))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_TYPE, m)?;
        utils::seq((exp, token::COLON, type_atom))(visitor, NodeMove::Step)
    }

    pub fn exp_wild<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_WILD, m)?;
        token::LOW_LINE(visitor, NodeMove::Step)
    }

    pub fn field<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FIELD, m)?;
        utils::seq((utils::optional(attributes), name_field, token::COLON, type_atom))(visitor, NodeMove::Step)
    }

    pub fn ident<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT, m)?;
        utils::choice((ident_lower, ident_upper))(visitor, NodeMove::Step)
    }

    pub fn ident_lower<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_LOWER, m)?;
        Ok(())
    }

    pub fn ident_lower_scoped<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_LOWER_SCOPED, m)?;
        Ok(())
    }

    pub fn ident_scoped<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_SCOPED, m)?;
        Ok(())
    }

    pub fn ident_upper<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_UPPER, m)?;
        Ok(())
    }

    pub fn ident_upper_scoped<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT_UPPER_SCOPED, m)?;
        Ok(())
    }

    pub fn insert<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INSERT, m)?;
        utils::seq((token::INSERT, atom))(visitor, NodeMove::Step)
    }

    pub fn insert_or_update<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INSERT_OR_UPDATE, m)?;
        utils::seq((token::INSERT_OR_UPDATE, atom))(visitor, NodeMove::Step)
    }

    pub fn lit_bool<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_BOOL, m)?;
        utils::choice((token::FALSE, token::TRUE))(visitor, NodeMove::Step)
    }

    pub fn lit_map<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_MAP, m)?;
        utils::seq((
            token::LEFT_SQUARE_BRACKET,
            exp,
            token::RIGHTWARDS_ARROW,
            exp,
            utils::repeat(utils::seq((token::COMMA, exp, token::RIGHTWARDS_ARROW, exp))),
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn lit_num<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn lit_num_bin<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_BIN, m)?;
        Ok(())
    }

    pub fn lit_num_dec<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_DEC, m)?;
        Ok(())
    }

    pub fn lit_num_float<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_FLOAT, m)?;
        Ok(())
    }

    pub fn lit_num_hex<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_HEX, m)?;
        Ok(())
    }

    pub fn lit_num_oct<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_NUM_OCT, m)?;
        Ok(())
    }

    pub fn lit_serialized<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_SERIALIZED, m)?;
        utils::seq((token::COMMERCIAL_AT, serde_encoding))(visitor, NodeMove::Step)
    }

    pub fn lit_string<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_STRING, m)?;
        utils::repeat1(utils::choice((
            string_quoted,
            string_quoted_escaped,
            string_raw,
            string_raw_interpolated,
        )))(visitor, NodeMove::Step)
    }

    pub fn lit_vec<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_VEC, m)?;
        utils::seq((
            token::LEFT_SQUARE_BRACKET,
            exp,
            utils::repeat(utils::seq((token::COMMA, exp))),
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn log_level<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LOG_LEVEL, m)?;
        utils::seq((token::LOG_LEVEL, misc_pat0))(visitor, NodeMove::Step)
    }

    pub fn misc_pat0<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MISC_PAT0, m)?;
        Ok(())
    }

    pub fn modify<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MODIFY, m)?;
        utils::seq((token::MODIFY, name_rel, record, token::LEFTWARDS_ARROW, record))(visitor, NodeMove::Step)
    }

    pub fn name<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME, m)?;
        Ok(())
    }

    pub fn name_arg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_ARG, m)?;
        Ok(())
    }

    pub fn name_cons<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_CONS, m)?;
        ident_upper_scoped(visitor, NodeMove::Step)
    }

    pub fn name_field<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_FIELD, m)?;
        Ok(())
    }

    pub fn name_func<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_FUNC, m)?;
        ident_lower_scoped(visitor, NodeMove::Step)
    }

    pub fn name_index<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_INDEX, m)?;
        ident_scoped(visitor, NodeMove::Step)
    }

    pub fn name_rel<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_REL, m)?;
        ident_upper_scoped(visitor, NodeMove::Step)
    }

    pub fn name_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_TYPE, m)?;
        utils::choice((ident_lower_scoped, ident_upper_scoped))(visitor, NodeMove::Step)
    }

    pub fn name_var_term<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_VAR_TERM, m)?;
        ident_lower_scoped(visitor, NodeMove::Step)
    }

    pub fn pat<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT, m)?;
        utils::choice((pat_cons, pat_term_decl_var, pat_lit, pat_tuple, pat_type, pat_wild))(visitor, NodeMove::Step)
    }

    pub fn pat_cons<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS, m)?;
        utils::choice((pat_cons_rec, pat_cons_pos))(visitor, NodeMove::Step)
    }

    pub fn pat_cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS_POS, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn pat_cons_rec<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS_REC, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn pat_lit<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_LIT, m)?;
        utils::choice((lit_bool, lit_num, lit_string))(visitor, NodeMove::Step)
    }

    pub fn pat_term_decl_var<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TERM_DECL_VAR, m)?;
        utils::seq((utils::optional(token::VAR), name_var_term))(visitor, NodeMove::Step)
    }

    pub fn pat_tuple<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TUPLE, m)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((pat, utils::repeat(utils::seq((token::COMMA, pat)))))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn pat_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TYPE, m)?;
        utils::seq((pat, token::COLON, type_atom))(visitor, NodeMove::Step)
    }

    pub fn pat_wild<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_WILD, m)?;
        token::LOW_LINE(visitor, NodeMove::Step)
    }

    pub fn profile<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PROFILE, m)?;
        utils::seq((
            token::PROFILE,
            utils::optional(utils::seq((token::CPU, utils::choice((token::ON, token::OFF))))),
            token::SEMICOLON,
        ))(visitor, NodeMove::Step)
    }

    pub fn query_index<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::QUERY_INDEX, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn record<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RECORD, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn record_named<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RECORD_NAMED, m)?;
        utils::seq((token::FULL_STOP, name_field, token::EQUALS_SIGN, record))(visitor, NodeMove::Step)
    }

    pub fn rollback<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ROLLBACK, m)?;
        utils::seq((token::ROLLBACK, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn serde_encoding<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::SERDE_ENCODING, m)?;
        token::JSON(visitor, NodeMove::Step)
    }

    pub fn sleep<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::SLEEP, m)?;
        utils::seq((token::SLEEP, misc_pat0, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn start<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::START, m)?;
        utils::seq((token::START, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    // NOTE: might have to descend into subnodes
    pub fn string_quoted<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_QUOTED, m)?;
        Ok(())
    }

    // NOTE: might have to descend into subnodes
    pub fn string_quoted_escaped<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_QUOTED_ESCAPED, m)?;
        Ok(())
    }

    // NOTE: might have to descend into subnodes
    pub fn string_raw<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_RAW, m)?;
        Ok(())
    }

    // NOTE: might have to descend into subnodes
    pub fn string_raw_interpolated<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STRING_RAW_INTERPOLATED, m)?;
        Ok(())
    }

    pub fn timestamp<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TIMESTAMP, m)?;
        utils::seq((token::TIMESTAMP, token::SEMICOLON))(visitor, NodeMove::Step)
    }

    pub fn r#type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn type_atom<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_ATOM, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn type_bigint<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_BIGINT, m)?;
        token::BIGINT(visitor, NodeMove::Step)
    }

    pub fn type_bit<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_BIT, m)?;
        utils::seq((token::BIT, token::LESS_THAN_SIGN, lit_num_dec, token::GREATER_THAN_SIGN))(visitor, NodeMove::Step)
    }

    pub fn type_bool<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_BOOL, m)?;
        token::BOOL(visitor, NodeMove::Step)
    }

    pub fn type_double<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_DOUBLE, m)?;
        token::DOUBLE(visitor, NodeMove::Step)
    }

    pub fn type_float<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_FLOAT, m)?;
        token::FLOAT(visitor, NodeMove::Step)
    }

    pub fn type_fun<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_FUN, m)?;
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
        ))(visitor, NodeMove::Step)
    }

    pub fn type_signed<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_SIGNED, m)?;
        utils::seq((
            token::SIGNED,
            token::LESS_THAN_SIGN,
            lit_num_dec,
            token::GREATER_THAN_SIGN,
        ))(visitor, NodeMove::Step)
    }

    pub fn type_string<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_STRING, m)?;
        token::STRING(visitor, NodeMove::Step)
    }

    pub fn type_trans<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_TRANS, m)?;
        utils::choice((type_trans_fun, type_trans_rel))(visitor, NodeMove::Step)
    }

    pub fn type_trans_fun<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_TRANS_FUN, m)?;
        utils::seq((
            token::FUNCTION,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((arg, utils::repeat(utils::seq((token::COMMA, arg)))))),
            token::RIGHT_PARENTHESIS,
            token::COLON,
            type_atom,
        ))(visitor, NodeMove::Step)
    }

    pub fn type_trans_rel<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_TRANS_REL, m)?;
        utils::seq((
            token::RELATION,
            token::LEFT_SQUARE_BRACKET,
            type_atom,
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn type_tuple<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_TUPLE, m)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                type_atom,
                utils::repeat(utils::seq((token::COMMA, type_atom))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn type_union<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_UNION, m)?;
        utils::seq((utils::repeat(utils::seq((cons, token::VERTICAL_LINE))), cons))(visitor, NodeMove::Step)
    }

    pub fn type_user<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_USER, m)?;
        utils::seq((
            name_type,
            utils::optional(utils::seq((
                token::LESS_THAN_SIGN,
                r#type,
                utils::repeat(utils::seq((token::COMMA, r#type))),
                token::GREATER_THAN_SIGN,
            ))),
        ))(visitor, NodeMove::Step)
    }

    pub fn type_var<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_VAR, m)?;
        utils::seq((token::APOSTROPHE, type_var_ident))(visitor, NodeMove::Step)
    }

    pub fn type_var_ident<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_VAR_IDENT, m)?;
        Ok(())
    }

    pub fn update<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::UPDATE, m)?;
        utils::choice((delete, delete_key, insert, insert_or_update, modify))(visitor, NodeMove::Step)
    }

    pub fn updates<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::UPDATES, m)?;
        utils::seq((update, utils::repeat(utils::seq((token::COMMA, update))), updates_end))(visitor, NodeMove::Step)
    }

    pub fn updates_end<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::UPDATES_END, m)?;
        Ok(())
    }

    pub fn val_array<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::VAL_ARRAY, m)?;
        utils::seq((
            token::LEFT_SQUARE_BRACKET,
            utils::optional(utils::seq((
                record,
                utils::repeat(utils::seq((token::COMMA, record))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn val_struct<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::VAL_STRUCT, m)?;
        utils::seq((
            name_rel,
            utils::optional(utils::seq((
                token::LEFT_CURLY_BRACKET,
                cons_args,
                token::RIGHT_CURLY_BRACKET,
            ))),
        ))(visitor, NodeMove::Step)
    }

    pub fn val_tuple<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::VAL_TUPLE, m)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                record,
                utils::repeat(utils::seq((token::COMMA, record))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn word<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::WORD, m)?;
        Ok(())
    }

    pub mod token {
        use super::*;

        macro_rules! make {
            ($name:tt) => {
                #[inline]
                #[allow(non_snake_case)]
                pub fn $name<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
                where
                    Ctx: Context<'tree> + 'tree,
                    Vis: Visitor<'tree, Ctx> + ?Sized,
                {
                    visitor.walker().token(super::super::token::$name, m)?;
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
