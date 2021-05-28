//! Functions for working with the `.dl` grammar.

use crate::{
    language::NodeMove,
    node::{NodeWalker, SyntaxErrors},
};

pub mod field {
    #![allow(missing_docs)]

    ddlog_lsp_macros::field_ids! {
        language: "ddlog.dl",
        fields: [
            (IDENTIFIER, "identifier"),
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
            (MISC_PAT0, "misc_pat0", true),
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
            (APOSTROPHE, "'", false),
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
            (GREATER_THAN_SIGN_EQUALS_SIGN, ">=", false),
            (GREATER_THAN_SIGN_GREATER_THAN_SIGN, ">>", false),
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

    #[allow(non_snake_case)]
    fn visit_ROOT(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::ROOT(self, m)
    }

    fn visit_annotated_item(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::annotated_item(self, m)
    }

    fn visit_apply(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::annotated_item(self, m)
    }

    fn visit_arg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::arg(self, m)
    }

    fn visit_arg_opt_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::arg_opt_type(self, m)
    }

    fn visit_arg_trans(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::arg_trans(self, m)
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

    fn visit_comment_block(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::comment_block(self, m)
    }

    fn visit_comment_block_inner(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::comment_block_inner(self, m)
    }

    fn visit_comment_line(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::comment_line(self, m)
    }

    fn visit_cons(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons(self, m)
    }

    fn visit_cons_pos(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons_pos(self, m)
    }

    fn visit_cons_rec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::cons_rec(self, m)
    }

    fn visit_escape_sequence(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::escape_sequence(self, m)
    }

    fn visit_escape_sequence_interpolated(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::escape_sequence_interpolated(self, m)
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

    fn visit_function(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::function(self, m)
    }

    fn visit_function_extern(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::function_extern(self, m)
    }

    fn visit_function_normal(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::function_normal(self, m)
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

    fn visit_import(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::import(self, m)
    }

    fn visit_index(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::index(self, m)
    }

    fn visit_interpolation(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::interpolation(self, m)
    }

    fn visit_item(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::item(self, m)
    }

    fn visit_key_primary(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::key_primary(self, m)
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

    fn visit_lit_string(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_string(self, m)
    }

    fn visit_lit_vec(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::lit_vec(self, m)
    }

    fn visit_misc_pat0(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::misc_pat0(self, m)
    }

    fn visit_module_alias(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::module_alias(self, m)
    }

    fn visit_module_path(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::module_path(self, m)
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

    fn visit_name_trans(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_trans(self, m)
    }

    fn visit_name_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_type(self, m)
    }

    fn visit_name_var_term(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_var_term(self, m)
    }

    fn visit_name_var_type(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::name_var_type(self, m)
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

    fn visit_rel(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rel(self, m)
    }

    fn visit_rel_args(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rel_args(self, m)
    }

    fn visit_rel_elem(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rel_elem(self, m)
    }

    fn visit_rel_role(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rel_role(self, m)
    }

    fn visit_rel_semantics(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rel_semantics(self, m)
    }

    fn visit_rhs(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rhs(self, m)
    }

    fn visit_rhs_atom_neg(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rhs_atom_neg(self, m)
    }

    fn visit_rhs_flat_map(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rhs_flat_map(self, m)
    }

    fn visit_rhs_grouping(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rhs_grouping(self, m)
    }

    fn visit_rhs_inspect(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rhs_inspect(self, m)
    }

    fn visit_rule(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rule(self, m)
    }

    fn visit_rule_end(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::rule_end(self, m)
    }

    fn visit_statement(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement(self, m)
    }

    fn visit_statement_assign(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement_assign(self, m)
    }

    fn visit_statement_block(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement_block(self, m)
    }

    fn visit_statement_empty(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement_empty(self, m)
    }

    fn visit_statement_for(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement_for(self, m)
    }

    fn visit_statement_if(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement_if(self, m)
    }

    fn visit_statement_insert(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement_insert(self, m)
    }

    fn visit_statement_match(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::statement_match(self, m)
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

    fn visit_transformer(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::transformer(self, m)
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

    fn visit_typedef(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::typedef(self, m)
    }

    fn visit_typedef_extern(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::typedef_extern(self, m)
    }

    fn visit_typedef_normal(&mut self, m: NodeMove) -> Result<(), SyntaxErrors> {
        visit::typedef_normal(self, m)
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
    ddlog_lsp_macros::impl_choice!(7);
    ddlog_lsp_macros::impl_choice!(8);
    ddlog_lsp_macros::impl_choice!(9);
    ddlog_lsp_macros::impl_choice!(10);
    ddlog_lsp_macros::impl_choice!(11);
    ddlog_lsp_macros::impl_choice!(12);
    ddlog_lsp_macros::impl_choice!(13);
    ddlog_lsp_macros::impl_choice!(14);
    ddlog_lsp_macros::impl_choice!(15);
    ddlog_lsp_macros::impl_choice!(50);

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
                } else if fun(visitor, m).is_err() {
                    visitor.reset(prev);
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
                    Ok(_) => succeeded_once = true,
                    Err(mut errs) => {
                        if succeeded_once {
                            visitor.reset(prev);
                            break;
                        } else {
                            errors.append(&mut errs);
                            return Err(errors);
                        }
                    }
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

    ddlog_lsp_macros::impl_seq!(1);
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
    ddlog_lsp_macros::impl_seq!(12);
    ddlog_lsp_macros::impl_seq!(13);
    ddlog_lsp_macros::impl_seq!(14);
    ddlog_lsp_macros::impl_seq!(15);

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
        utils::repeat(Vis::visit_annotated_item)(visitor, NodeMove::Step)
    }

    pub fn annotated_item<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ANNOTATED_ITEM, m)?;
        utils::seq((utils::optional(Vis::visit_attributes), Vis::visit_item))(visitor, NodeMove::Step)
    }

    pub fn apply<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::APPLY, m)?;
        utils::seq((
            token::APPLY,
            Vis::visit_name_trans,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::choice((Vis::visit_name_func, Vis::visit_name_rel))),
            utils::repeat(utils::seq((
                token::COMMA,
                utils::choice((Vis::visit_name_func, Vis::visit_name_rel)),
            ))),
            token::RIGHT_PARENTHESIS,
            token::RIGHTWARDS_ARROW,
            token::LEFT_PARENTHESIS,
            utils::optional(Vis::visit_name_rel),
            utils::repeat(utils::seq((token::COMMA, Vis::visit_name_rel))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn arg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG, m)?;
        utils::seq((
            Vis::visit_name_arg,
            token::COLON,
            utils::optional(token::MUT),
            Vis::visit_type_atom,
        ))(visitor, NodeMove::Step)
    }

    pub fn arg_opt_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG_OPT_TYPE, m)?;
        utils::seq((
            Vis::visit_name_arg,
            utils::optional(utils::seq((
                token::COLON,
                utils::optional(token::MUT),
                Vis::visit_type_atom,
            ))),
            Vis::visit_type_atom,
        ))(visitor, NodeMove::Step)
    }

    pub fn arg_trans<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ARG_TRANS, m)?;
        utils::seq((Vis::visit_name_trans, token::COLON, Vis::visit_type_trans))(visitor, NodeMove::Step)
    }

    pub fn atom<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM, m)?;
        utils::choice((Vis::visit_atom_rec, Vis::visit_atom_pos, Vis::visit_atom_elem))(visitor, NodeMove::Step)
    }

    pub fn atom_elem<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_ELEM, m)?;
        utils::seq((
            Vis::visit_name_rel,
            token::LEFT_SQUARE_BRACKET,
            exp,
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn atom_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_POS, m)?;
        utils::seq((
            utils::optional(utils::seq((Vis::visit_name_var_term, token::IN))),
            utils::seq((utils::optional(token::AMPERSAND), Vis::visit_name_rel)),
            utils::optional(utils::seq((
                token::LEFT_PARENTHESIS,
                utils::optional(utils::seq((
                    Vis::visit_exp,
                    utils::repeat(utils::seq((token::COMMA, Vis::visit_exp))),
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
            utils::optional(utils::seq((Vis::visit_name_var_term, token::IN))),
            utils::seq((utils::optional(token::AMPERSAND), Vis::visit_name_rel)),
            token::LEFT_PARENTHESIS,
            token::FULL_STOP,
            Vis::visit_name_arg,
            token::EQUALS_SIGN,
            Vis::visit_exp,
            utils::repeat(utils::seq((
                token::COMMA,
                token::FULL_STOP,
                Vis::visit_name_arg,
                token::EQUALS_SIGN,
                Vis::visit_exp,
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
        utils::seq((
            Vis::visit_name,
            utils::optional(utils::seq((token::EQUALS_SIGN, Vis::visit_exp))),
        ))(visitor, NodeMove::Step)
    }

    pub fn attributes<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ATTRIBUTES, m)?;
        utils::repeat1(utils::seq((
            token::NUMBER_SIGN_LEFT_SQUARE_BRACKET,
            Vis::visit_attribute,
            utils::repeat(utils::seq((token::COMMA, Vis::visit_attribute))),
            token::RIGHT_SQUARE_BRACKET,
        )))(visitor, NodeMove::Step)
    }

    pub fn comment_block<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_BLOCK, m)?;
        utils::seq((
            token::SOLIDUS_ASTERISK,
            utils::repeat(utils::choice((
                Vis::visit_comment_block,
                Vis::visit_comment_block_inner,
            ))),
            token::ASTERISK_SOLIDUS,
        ))(visitor, NodeMove::Step)
    }

    pub fn comment_block_inner<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_BLOCK_INNER, m)?;
        Ok(())
    }

    pub fn comment_line<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::COMMENT_LINE, m)?;
        Ok(())
    }

    pub fn cons<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS, m)?;
        utils::choice((Vis::visit_cons_rec, Vis::visit_cons_pos))(visitor, NodeMove::Step)
    }

    pub fn cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_POS, m)?;
        utils::seq((utils::optional(Vis::visit_attributes), Vis::visit_name_cons))(visitor, NodeMove::Step)
    }

    pub fn cons_rec<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::CONS_REC, m)?;
        utils::seq((
            utils::optional(attributes),
            Vis::visit_name_cons,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                Vis::visit_field,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_field))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn escape_sequence<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ESCAPE_SEQUENCE, m)?;
        Ok(())
    }

    pub fn escape_sequence_interpolated<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ESCAPE_SEQUENCE_INTERPOLATED, m)?;
        Ok(())
    }

    pub fn exp<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP, m)?;
        utils::choice((
            Vis::visit_exp_add,
            Vis::visit_exp_assign,
            Vis::visit_exp_binding,
            Vis::visit_exp_bit_and,
            Vis::visit_exp_bit_neg,
            Vis::visit_exp_bit_or,
            Vis::visit_exp_bit_slice,
            Vis::visit_exp_bit_xor,
            Vis::visit_exp_block,
            Vis::visit_exp_break,
            Vis::visit_exp_cast,
            Vis::visit_exp_cat,
            Vis::visit_exp_cond,
            Vis::visit_exp_continue,
            Vis::visit_exp_cons_pos,
            Vis::visit_exp_cons_rec,
            Vis::visit_exp_decl_var,
            Vis::visit_exp_div,
            Vis::visit_exp_eq,
            Vis::visit_exp_field,
            Vis::visit_exp_for,
            Vis::visit_exp_fun_call,
            Vis::visit_exp_fun_call_dot,
            Vis::visit_exp_gt,
            Vis::visit_exp_gteq,
            Vis::visit_exp_lambda,
            Vis::visit_exp_lit,
            Vis::visit_exp_log_and,
            Vis::visit_exp_log_imp,
            Vis::visit_exp_log_neg,
            Vis::visit_exp_log_or,
            Vis::visit_exp_lt,
            Vis::visit_exp_lteq,
            Vis::visit_exp_match,
            Vis::visit_exp_mul,
            Vis::visit_exp_neg,
            Vis::visit_exp_neq,
            Vis::visit_exp_proj,
            Vis::visit_exp_ref,
            Vis::visit_exp_rem,
            Vis::visit_exp_return,
            Vis::visit_exp_seq,
            Vis::visit_exp_shl,
            Vis::visit_exp_shr,
            Vis::visit_exp_slice,
            Vis::visit_exp_sub,
            Vis::visit_exp_try,
            Vis::visit_exp_tuple,
            Vis::visit_exp_type,
            Vis::visit_exp_wild,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_add<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_ADD, m)?;
        utils::seq((Vis::visit_exp, token::PLUS_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_assign<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_ASSIGN, m)?;
        utils::seq((Vis::visit_exp, token::EQUALS_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_binding<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BINDING, m)?;
        utils::seq((Vis::visit_name_var_term, token::COMMERCIAL_AT, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_and<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_AND, m)?;
        utils::seq((Vis::visit_exp, token::AMPERSAND, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_neg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_NEG, m)?;
        utils::seq((token::TILDE, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_or<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_OR, m)?;
        utils::seq((Vis::visit_exp, token::VERTICAL_LINE, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_slice<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_OR, m)?;
        utils::seq((Vis::visit_exp, token::LEFT_SQUARE_BRACKET, token::RIGHT_SQUARE_BRACKET))(visitor, NodeMove::Step)
    }

    pub fn exp_bit_xor<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BIT_XOR, m)?;
        utils::seq((Vis::visit_exp, token::CIRCUMFLEX_ACCENT, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_block<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_BLOCK, m)?;
        utils::seq((
            token::LEFT_CURLY_BRACKET,
            utils::optional(Vis::visit_exp),
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
        utils::seq((Vis::visit_exp, token::AS, Vis::visit_type_atom))(visitor, NodeMove::Step)
    }

    pub fn exp_cat<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CAT, m)?;
        utils::seq((Vis::visit_exp, token::PLUS_SIGN_PLUS_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_cond<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_COND, m)?;
        utils::seq((
            token::IF,
            Vis::visit_exp,
            Vis::visit_exp,
            utils::optional(utils::seq((token::ELSE, Vis::visit_exp))),
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_CONS_POS, m)?;
        utils::seq((
            Vis::visit_name_cons,
            utils::optional(utils::seq((
                token::LEFT_CURLY_BRACKET,
                utils::optional(utils::seq((
                    token::LEFT_CURLY_BRACKET,
                    utils::optional(utils::seq((
                        Vis::visit_exp,
                        utils::repeat(utils::seq((token::COMMA, Vis::visit_exp))),
                    ))),
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
            Vis::visit_name_cons,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                token::FULL_STOP,
                Vis::visit_name_field,
                token::EQUALS_SIGN,
                Vis::visit_exp,
                utils::repeat(utils::seq((
                    token::COMMA,
                    token::FULL_STOP,
                    Vis::visit_name_field,
                    token::EQUALS_SIGN,
                    Vis::visit_exp,
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
        utils::seq((utils::optional(token::VAR), Vis::visit_name_var_term))(visitor, NodeMove::Step)
    }

    pub fn exp_div<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DIV, m)?;
        utils::seq((Vis::visit_exp, token::SOLIDUS, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_eq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_DIV, m)?;
        utils::seq((Vis::visit_exp, token::EQUALS_SIGN_EQUALS_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_field<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FIELD, m)?;
        utils::seq((Vis::visit_exp, token::FULL_STOP, Vis::visit_ident))(visitor, NodeMove::Step)
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
            Vis::visit_name_var_term,
            token::IN,
            Vis::visit_exp,
            token::RIGHT_PARENTHESIS,
            Vis::visit_exp,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_fun_call<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_FUN_CALL, m)?;
        utils::seq((
            Vis::visit_exp,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_exp,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_exp))),
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
            Vis::visit_exp,
            token::FULL_STOP,
            Vis::visit_name_func,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_exp,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_exp))),
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
        utils::seq((Vis::visit_exp, token::GREATER_THAN_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_gteq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_GTEQ, m)?;
        utils::seq((Vis::visit_exp, token::GREATER_THAN_SIGN_EQUALS_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
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
                    Vis::visit_arg_opt_type,
                    utils::repeat(utils::seq((token::COMMA, Vis::visit_arg_opt_type))),
                ))),
                token::RIGHT_PARENTHESIS,
                utils::optional(utils::seq((token::COLON, Vis::visit_type_atom))),
                Vis::visit_exp,
            )),
            utils::seq((
                token::VERTICAL_LINE,
                utils::optional(utils::seq((
                    Vis::visit_arg_opt_type,
                    utils::repeat(utils::seq((token::COMMA, Vis::visit_arg_opt_type))),
                ))),
                token::VERTICAL_LINE,
                utils::optional(utils::seq((token::COLON, Vis::visit_type_atom))),
                Vis::visit_exp,
            )),
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_lit<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LIT, m)?;
        utils::choice((
            Vis::visit_lit_bool,
            Vis::visit_lit_num,
            Vis::visit_lit_map,
            Vis::visit_lit_string,
            Vis::visit_lit_vec,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_log_and<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_AND, m)?;
        utils::seq((Vis::visit_exp, token::AND, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_log_imp<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_IMP, m)?;
        utils::seq((Vis::visit_exp, token::EQUALS_SIGN_GREATER_THAN_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_log_neg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_NEG, m)?;
        utils::seq((token::NOT, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_log_or<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LOG_OR, m)?;
        utils::seq((Vis::visit_exp, token::OR, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_lt<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LT, m)?;
        utils::seq((Vis::visit_exp, token::LESS_THAN_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_lteq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_LTEQ, m)?;
        utils::seq((Vis::visit_exp, token::LESS_THAN_SIGN_EQUALS_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
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
            Vis::visit_exp,
            token::RIGHT_PARENTHESIS,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                utils::seq((Vis::visit_pat, token::RIGHTWARDS_ARROW, Vis::visit_exp)),
                utils::repeat(utils::seq((
                    token::COMMA,
                    utils::seq((Vis::visit_pat, token::RIGHTWARDS_ARROW, Vis::visit_exp)),
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
        utils::seq((Vis::visit_exp, token::ASTERISK, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_neg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_NEG, m)?;
        utils::seq((token::HYPHEN_MINUS, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_neq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_NEQ, m)?;
        utils::seq((Vis::visit_exp, token::EXCLAMATION_MARK_EQUALS_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_proj<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_PROJ, m)?;
        utils::seq((Vis::visit_exp, token::FULL_STOP, Vis::visit_exp_proj_digits))(visitor, NodeMove::Step)
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
        utils::seq((token::AMPERSAND, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_rem<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_REM, m)?;
        utils::seq((Vis::visit_exp, token::PERCENT_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_return<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_RETURN, m)?;
        utils::seq((token::RETURN, utils::optional(Vis::visit_exp)))(visitor, NodeMove::Step)
    }

    pub fn exp_seq<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SEQ, m)?;
        utils::seq((Vis::visit_exp, token::SEMICOLON, utils::optional(Vis::visit_exp)))(visitor, NodeMove::Step)
    }

    pub fn exp_shl<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SHL, m)?;
        utils::seq((Vis::visit_exp, token::LESS_THAN_SIGN_LESS_THAN_SIGN, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_shr<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SHR, m)?;
        utils::seq((
            Vis::visit_exp,
            token::GREATER_THAN_SIGN_GREATER_THAN_SIGN,
            Vis::visit_exp,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_slice<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SLICE, m)?;
        utils::seq((
            Vis::visit_exp,
            token::LEFT_SQUARE_BRACKET,
            Vis::visit_lit_num_dec,
            token::COLON,
            Vis::visit_lit_num_dec,
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn exp_sub<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_SUB, m)?;
        utils::seq((Vis::visit_exp, token::HYPHEN_MINUS, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn exp_try<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::EXP_TRY, m)?;
        utils::seq((Vis::visit_exp, token::QUESTION_MARK))(visitor, NodeMove::Step)
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
                Vis::visit_exp,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_exp))),
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
        utils::seq((exp, token::COLON, Vis::visit_type_atom))(visitor, NodeMove::Step)
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
        utils::seq((
            utils::optional(attributes),
            Vis::visit_name_field,
            token::COLON,
            Vis::visit_type_atom,
        ))(visitor, NodeMove::Step)
    }

    pub fn function<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FUNCTION, m)?;
        utils::choice((Vis::visit_function_normal, Vis::visit_function_extern))(visitor, NodeMove::Step)
    }

    pub fn function_extern<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FUNCTION_EXTERN, m)?;
        utils::seq((
            token::EXTERN,
            token::FUNCTION,
            Vis::visit_name_func,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_arg,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_arg))),
            ))),
            token::RIGHT_PARENTHESIS,
            utils::optional(utils::seq((token::COLON, Vis::visit_type_atom))),
        ))(visitor, NodeMove::Step)
    }

    pub fn function_normal<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::FUNCTION_NORMAL, m)?;
        utils::seq((
            token::FUNCTION,
            Vis::visit_name_func,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_arg,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_arg))),
            ))),
            token::RIGHT_PARENTHESIS,
            utils::optional(utils::seq((token::COLON, Vis::visit_type_atom))),
            utils::choice((
                utils::seq((token::EQUALS_SIGN, Vis::visit_exp)),
                token::LEFT_CURLY_BRACKET,
                Vis::visit_exp,
                token::RIGHT_CURLY_BRACKET,
            )),
        ))(visitor, NodeMove::Step)
    }

    pub fn ident<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IDENT, m)?;
        utils::choice((Vis::visit_ident_lower, Vis::visit_ident_upper))(visitor, NodeMove::Step)
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

    pub fn import<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::IMPORT, m)?;
        utils::seq((
            token::IMPORT,
            Vis::visit_module_path,
            utils::optional(utils::seq((token::AS, Vis::visit_module_alias))),
        ))(visitor, NodeMove::Step)
    }

    pub fn index<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INDEX, m)?;
        utils::seq((
            token::INDEX,
            Vis::visit_name_index,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_arg,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_arg))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
            token::ON,
            Vis::visit_atom,
        ))(visitor, NodeMove::Step)
    }

    pub fn interpolation<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::INTERPOLATION, m)?;
        utils::seq((
            token::DOLLAR_SIGN_LEFT_CURLY_BRACKET,
            Vis::visit_exp,
            token::RIGHT_CURLY_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn item<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::ITEM, m)?;
        utils::choice((
            Vis::visit_statement_for,
            Vis::visit_apply,
            Vis::visit_import,
            Vis::visit_function,
            Vis::visit_index,
            Vis::visit_rel,
            Vis::visit_rule,
            Vis::visit_transformer,
            Vis::visit_typedef,
        ))(visitor, NodeMove::Step)
    }

    pub fn key_primary<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::KEY_PRIMARY, m)?;
        utils::seq((
            token::PRIMARY,
            token::KEY,
            token::LEFT_PARENTHESIS,
            Vis::visit_name_var_term,
            token::RIGHT_PARENTHESIS,
            Vis::visit_exp,
        ))(visitor, NodeMove::Step)
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
            Vis::visit_exp,
            token::RIGHTWARDS_ARROW,
            Vis::visit_exp,
            utils::repeat(utils::seq((
                token::COMMA,
                Vis::visit_exp,
                token::RIGHTWARDS_ARROW,
                Vis::visit_exp,
            ))),
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
            utils::choice((Vis::visit_lit_num_dec, Vis::visit_lit_num_float, Vis::visit_lit_num_hex)),
            utils::seq((
                utils::optional(Vis::visit_lit_num_dec),
                utils::choice((
                    utils::seq((token::LIT_BIN, Vis::visit_lit_num_bin)),
                    utils::seq((token::LIT_DEC, Vis::visit_lit_num_dec)),
                    utils::seq((token::LIT_FLOAT, Vis::visit_lit_num_float)),
                    utils::seq((token::LIT_HEX, Vis::visit_lit_num_hex)),
                    utils::seq((token::LIT_OCT, Vis::visit_lit_num_oct)),
                    utils::seq((token::LIT_S_BIN, Vis::visit_lit_num_bin)),
                    utils::seq((token::LIT_S_DEC, Vis::visit_lit_num_dec)),
                    utils::seq((token::LIT_S_HEX, Vis::visit_lit_num_hex)),
                    utils::seq((token::LIT_S_OCT, Vis::visit_lit_num_oct)),
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

    pub fn lit_string<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::LIT_STRING, m)?;
        utils::repeat1(utils::choice((
            Vis::visit_string_quoted,
            Vis::visit_string_quoted_escaped,
            Vis::visit_string_raw,
            Vis::visit_string_raw_interpolated,
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
            Vis::visit_exp,
            utils::repeat(utils::seq((token::COMMA, Vis::visit_exp))),
            token::RIGHT_SQUARE_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn misc_pat0<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MISC_PAT0, m)?;
        Ok(())
    }

    pub fn module_alias<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MODULE_ALIAS, m)?;
        visitor.visit_ident(NodeMove::Step)
    }

    pub fn module_path<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::MODULE_PATH, m)?;
        utils::seq((
            Vis::visit_ident,
            utils::repeat(utils::seq((token::COLON_COLON, Vis::visit_ident))),
        ))(visitor, NodeMove::Step)
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
        visitor.visit_ident_upper_scoped(NodeMove::Step)
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
        visitor.visit_ident_scoped(NodeMove::Step)
    }

    pub fn name_rel<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_REL, m)?;
        visitor.visit_ident_upper_scoped(NodeMove::Step)
    }

    pub fn name_trans<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_TRANS, m)?;
        visitor.visit_ident_scoped(NodeMove::Step)
    }

    pub fn name_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_TYPE, m)?;
        utils::choice((Vis::visit_ident_lower_scoped, Vis::visit_ident_upper_scoped))(visitor, NodeMove::Step)
    }

    pub fn name_var_term<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_VAR_TERM, m)?;
        visitor.visit_ident_lower_scoped(NodeMove::Step)
    }

    pub fn name_var_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::NAME_VAR_TYPE, m)?;
        Ok(())
    }

    pub fn pat<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT, m)?;
        utils::choice((
            Vis::visit_pat_cons,
            Vis::visit_pat_term_decl_var,
            Vis::visit_pat_lit,
            Vis::visit_pat_tuple,
            Vis::visit_pat_type,
            Vis::visit_pat_wild,
        ))(visitor, NodeMove::Step)
    }

    pub fn pat_cons<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS, m)?;
        utils::choice((Vis::visit_pat_cons_rec, Vis::visit_pat_cons_pos))(visitor, NodeMove::Step)
    }

    pub fn pat_cons_pos<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_CONS_POS, m)?;
        utils::seq((
            Vis::visit_name_cons,
            utils::optional(utils::seq((
                token::LEFT_CURLY_BRACKET,
                utils::optional(utils::seq((
                    Vis::visit_pat,
                    utils::repeat(utils::seq((token::COMMA, Vis::visit_pat))),
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
            Vis::visit_name_cons,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                token::FULL_STOP,
                Vis::visit_name_field,
                token::EQUALS_SIGN,
                Vis::visit_pat,
                utils::repeat(utils::seq((
                    token::COMMA,
                    token::FULL_STOP,
                    Vis::visit_name_field,
                    token::EQUALS_SIGN,
                    Vis::visit_pat,
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
        utils::choice((Vis::visit_lit_bool, Vis::visit_lit_num, Vis::visit_lit_string))(visitor, NodeMove::Step)
    }

    pub fn pat_term_decl_var<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TERM_DECL_VAR, m)?;
        utils::seq((utils::optional(token::VAR), Vis::visit_name_var_term))(visitor, NodeMove::Step)
    }

    pub fn pat_tuple<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TUPLE, m)?;
        utils::seq((
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_pat,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_pat))),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn pat_type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_TYPE, m)?;
        utils::seq((Vis::visit_pat, token::COLON, Vis::visit_type_atom))(visitor, NodeMove::Step)
    }

    pub fn pat_wild<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::PAT_WILD, m)?;
        token::LOW_LINE(visitor, NodeMove::Step)
    }

    pub fn rel<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL, m)?;
        utils::choice((Vis::visit_rel_args, Vis::visit_rel_elem))(visitor, NodeMove::Step)
    }

    pub fn rel_args<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_ARGS, m)?;
        utils::seq((
            utils::optional(Vis::visit_rel_role),
            Vis::visit_rel_semantics,
            utils::seq((utils::optional(token::AMPERSAND), Vis::visit_name_rel)),
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_arg,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_arg))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_PARENTHESIS,
            utils::optional(Vis::visit_key_primary),
        ))(visitor, NodeMove::Step)
    }

    pub fn rel_elem<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_ELEM, m)?;
        utils::seq((
            utils::optional(rel_role),
            Vis::visit_rel_semantics,
            utils::seq((utils::optional(token::AMPERSAND), Vis::visit_name_rel)),
            token::LEFT_SQUARE_BRACKET,
            Vis::visit_type_atom,
            token::RIGHT_SQUARE_BRACKET,
            utils::optional(key_primary),
        ))(visitor, NodeMove::Step)
    }

    pub fn rel_role<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_ROLE, m)?;
        utils::choice((token::INPUT, token::INTERNAL, token::OUTPUT))(visitor, NodeMove::Step)
    }

    pub fn rel_semantics<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::REL_SEMANTICS, m)?;
        utils::choice((token::RELATION, token::STREAM, token::MULTISET))(visitor, NodeMove::Step)
    }

    pub fn rhs<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS, m)?;
        utils::choice((
            // FIXME: Visit doesn't have `.visit_rhs_inspect()`
            rhs_inspect,
            Vis::visit_atom,
            Vis::visit_rhs_atom_neg,
            Vis::visit_exp,
            Vis::visit_rhs_flat_map,
            Vis::visit_rhs_grouping,
        ))(visitor, NodeMove::Step)
    }

    pub fn rhs_atom_neg<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_ATOM_NEG, m)?;
        utils::seq((token::NOT, Vis::visit_atom))(visitor, NodeMove::Step)
    }

    pub fn rhs_flat_map<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_FLAT_MAP, m)?;
        utils::seq((
            token::VAR,
            Vis::visit_name_var_term,
            token::EQUALS_SIGN,
            token::FLAT_MAP,
            token::LEFT_PARENTHESIS,
            Vis::visit_exp,
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn rhs_grouping<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_GROUPING, m)?;
        utils::seq((
            token::VAR,
            Vis::visit_name_var_term,
            token::EQUALS_SIGN,
            Vis::visit_exp,
            token::FULL_STOP,
            token::GROUP_BY,
            token::LEFT_PARENTHESIS,
            Vis::visit_exp,
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn rhs_inspect<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RHS_INSPECT, m)?;
        utils::seq((token::INSPECT, Vis::visit_exp))(visitor, NodeMove::Step)
    }

    pub fn rule<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RULE, m)?;
        utils::seq((
            Vis::visit_atom,
            utils::repeat(utils::seq((token::COMMA, Vis::visit_atom))),
            utils::optional(utils::seq((
                token::COLON_HYPHEN_MINUS,
                Vis::visit_rhs,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_rhs))),
            ))),
            Vis::visit_rule_end,
        ))(visitor, NodeMove::Step)
    }

    pub fn rule_end<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::RULE_END, m)?;
        Ok(())
    }

    pub fn statement<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT, m)?;
        utils::choice((
            Vis::visit_statement_assign,
            Vis::visit_statement_block,
            Vis::visit_statement_empty,
            Vis::visit_statement_for,
            Vis::visit_statement_if,
            Vis::visit_statement_insert,
            Vis::visit_statement_match,
        ))(visitor, NodeMove::Step)
    }

    pub fn statement_assign<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_ASSIGN, m)?;
        utils::seq((Vis::visit_exp, token::IN, Vis::visit_statement))(visitor, NodeMove::Step)
    }

    pub fn statement_block<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_BLOCK, m)?;
        utils::seq((
            token::LEFT_CURLY_BRACKET,
            utils::repeat(utils::seq((
                Vis::visit_statement,
                utils::optional(utils::seq((token::SEMICOLON, utils::optional(Vis::visit_statement)))),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor, NodeMove::Step)
    }

    pub fn statement_empty<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_EMPTY, m)?;
        token::SKIP(visitor, NodeMove::Step)
    }

    pub fn statement_for<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_FOR, m)?;
        utils::seq((
            token::FOR,
            token::LEFT_PARENTHESIS,
            Vis::visit_atom,
            utils::optional(utils::seq((token::IF, Vis::visit_exp))),
            token::RIGHT_PARENTHESIS,
            Vis::visit_statement,
        ))(visitor, NodeMove::Step)
    }

    pub fn statement_if<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_IF, m)?;
        utils::seq((
            token::IF,
            token::LEFT_PARENTHESIS,
            Vis::visit_exp,
            token::RIGHT_PARENTHESIS,
            Vis::visit_statement,
            utils::optional(utils::seq((token::ELSE, Vis::visit_statement))),
        ))(visitor, NodeMove::Step)
    }

    pub fn statement_insert<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_INSERT, m)?;
        visitor.visit_atom(NodeMove::Step)
    }

    pub fn statement_match<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::STATEMENT_MATCH, m)?;
        utils::seq((
            token::MATCH,
            token::LEFT_PARENTHESIS,
            Vis::visit_exp,
            token::RIGHT_PARENTHESIS,
            token::LEFT_CURLY_BRACKET,
            utils::optional(utils::seq((
                utils::seq((Vis::visit_pat, token::RIGHTWARDS_ARROW, Vis::visit_statement)),
                utils::repeat(utils::seq((
                    token::COMMA,
                    utils::seq((Vis::visit_pat, token::RIGHTWARDS_ARROW, Vis::visit_statement)),
                ))),
                utils::optional(token::COMMA),
            ))),
            token::RIGHT_CURLY_BRACKET,
        ))(visitor, NodeMove::Step)
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

    pub fn transformer<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TRANSFORMER, m)?;
        utils::seq((
            token::EXTERN,
            token::TRANSFORMER,
            Vis::visit_name_trans,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_arg_trans,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_arg_trans))),
            ))),
            token::RIGHT_PARENTHESIS,
            token::RIGHTWARDS_ARROW,
            token::LEFT_PARENTHESIS,
            utils::optional(utils::seq((
                Vis::visit_arg_trans,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_arg_trans))),
            ))),
            token::RIGHT_PARENTHESIS,
        ))(visitor, NodeMove::Step)
    }

    pub fn r#type<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE, m)?;
        utils::choice((
            Vis::visit_type_bit,
            Vis::visit_type_signed,
            Vis::visit_type_bigint,
            Vis::visit_type_double,
            Vis::visit_type_float,
            Vis::visit_type_string,
            Vis::visit_type_bool,
            Vis::visit_type_union,
            Vis::visit_type_user,
            Vis::visit_type_var,
            Vis::visit_type_fun,
            Vis::visit_type_tuple,
        ))(visitor, NodeMove::Step)
    }

    pub fn type_atom<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_ATOM, m)?;
        utils::choice((
            Vis::visit_type_bit,
            Vis::visit_type_signed,
            Vis::visit_type_bigint,
            Vis::visit_type_double,
            Vis::visit_type_float,
            Vis::visit_type_string,
            Vis::visit_type_bool,
            Vis::visit_type_user,
            Vis::visit_type_var,
            Vis::visit_type_fun,
            Vis::visit_type_tuple,
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
        utils::seq((
            token::BIT,
            token::LESS_THAN_SIGN,
            Vis::visit_lit_num_dec,
            token::GREATER_THAN_SIGN,
        ))(visitor, NodeMove::Step)
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
                    Vis::visit_type,
                    utils::repeat(utils::seq((token::COMMA, utils::optional(token::MUT), Vis::visit_type))),
                ))),
                token::RIGHT_PARENTHESIS,
                utils::optional(utils::seq((token::COLON, Vis::visit_type))),
            )),
            utils::seq((
                token::VERTICAL_LINE,
                utils::optional(utils::seq((
                    utils::optional(token::MUT),
                    Vis::visit_type,
                    utils::repeat(utils::seq((token::COMMA, utils::optional(token::MUT), Vis::visit_type))),
                ))),
                token::VERTICAL_LINE,
                utils::optional(utils::seq((token::COLON, Vis::visit_type))),
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
            Vis::visit_lit_num_dec,
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
        // FIXME: `Visitor` doesn't have `.visit_type_trans_rel()`
        utils::choice((Vis::visit_type_trans_fun, type_trans_rel))(visitor, NodeMove::Step)
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
            utils::optional(utils::seq((
                Vis::visit_arg,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_arg))),
            ))),
            token::RIGHT_PARENTHESIS,
            token::COLON,
            Vis::visit_type_atom,
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
            Vis::visit_type_atom,
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
                Vis::visit_type_atom,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_type_atom))),
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
        utils::seq((
            utils::repeat(utils::seq((Vis::visit_cons, token::VERTICAL_LINE))),
            Vis::visit_cons,
        ))(visitor, NodeMove::Step)
    }

    pub fn type_user<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPE_USER, m)?;
        utils::seq((
            Vis::visit_name_type,
            utils::optional(utils::seq((
                token::LESS_THAN_SIGN,
                Vis::visit_type,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_type))),
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
        utils::seq((token::APOSTROPHE, Vis::visit_misc_pat0))(visitor, NodeMove::Step)
    }

    pub fn typedef<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPEDEF, m)?;
        utils::choice((Vis::visit_typedef_normal, Vis::visit_typedef_extern))(visitor, NodeMove::Step)
    }

    pub fn typedef_extern<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPEDEF_EXTERN, m)?;
        utils::seq((
            token::EXTERN,
            token::TYPE,
            Vis::visit_name_type,
            utils::optional(utils::seq((
                token::LESS_THAN_SIGN,
                Vis::visit_name_var_type,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_name_var_type))),
                token::GREATER_THAN_SIGN,
            ))),
        ))(visitor, NodeMove::Step)
    }

    pub fn typedef_normal<'tree, Ctx, Vis>(visitor: &mut Vis, m: NodeMove) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        visitor.walker().rule(kind::TYPEDEF_NORMAL, m)?;
        utils::seq((
            token::TYPEDEF,
            Vis::visit_name_type,
            utils::optional(utils::seq((
                token::LESS_THAN_SIGN,
                Vis::visit_name_var_type,
                utils::repeat(utils::seq((token::COMMA, Vis::visit_name_var_type))),
                token::GREATER_THAN_SIGN,
            ))),
            token::EQUALS_SIGN,
            Vis::visit_type,
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
        make!(APOSTROPHE);
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
