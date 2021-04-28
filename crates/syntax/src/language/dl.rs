//! Functions for working with the `.dl` grammar.

use crate::node::{NodeWalker, SyntaxErrors};

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
            (IDENT_LOWER_SCOPED, "ident_lower_scoped", true),
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
            (STATEMENT, "statement", true),
            (STATEMENT_ASSIGN, "statement_assign", true),
            (STATEMENT_BLOCK, "statement_block", true),
            (STATEMENT_EMPTY, "statement_empty", true),
            (STATEMENT_FOR, "statement_for", true),
            (STATEMENT_IF, "statement_if", true),
            (STATEMENT_INSERT, "statement_insert", true),
            (STATEMENT_MATCH, "statement_match", true),
            (STRING_QUOTED, "string_quoted", true),
            (STRING_QUOTED_ESCAPE, "string_quoted_escape", true),
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

pub mod keyword {
    #![allow(missing_docs)]

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
}

pub mod token {
    #![allow(missing_docs)]

    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dl",
        node_kinds: [
            (AMPERSAND, "&", false),
            (ASTERISK, "*", false),
            (ASTERISK_SOLIDUS, "*/", false),
            (CIRCUMFLEX_ACCENT, "^", false),
            (COLON, ":", false),
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
pub trait AbstractSyntax<'tree> {
    type ROOT;
    type AnnotatedItem;
    type Apply;
    type Arg;
    type ArgOptType;
    type ArgTrans;
    type AtomElem;
    type AtomPos;
    type AtomRec;
    type Attribute;
    type Attributes;
    type CommentBlock;
    type CommentLine;
    type Cons;
    type ConsPos;
    type ConsRec;
    type EscapeSequence;
    type EscapeSequenceInterpolated;
    type Exp;
    type ExpAdd;
    type ExpAssign;
    type ExpBinding;
    type ExpBitAnd;
    type ExpBitNeg;
    type ExpBitOr;
    type ExpBitSlice;
    type ExpBitXor;
    type ExpBlock;
    type ExpBreak;
    type ExpCast;
    type ExpCat;
    type ExpCond;
    type ExpConsPos;
    type ExpConsRec;
    type ExpContinue;
    type ExpDeclVar;
    type ExpDiv;
    type ExpEq;
    type ExpField;
    type ExpFor;
    type ExpFunCall;
    type ExpFunCallDot;
    type ExpGt;
    type ExpGteq;
    type ExpLambda;
    type ExpLit;
    type ExpLogAnd;
    type ExpLogImp;
    type ExpLogNeg;
    type ExpLogOr;
    type ExpLt;
    type ExpLteq;
    type ExpMatch;
    type ExpMul;
    type ExpNeg;
    type ExpNeq;
    type ExpProj;
    type ExpRef;
    type ExpRem;
    type ExpReturn;
    type ExpSeq;
    type ExpShl;
    type ExpShr;
    type ExpSlice;
    type ExpSub;
    type ExpTry;
    type ExpTuple;
    type ExpType;
    type ExpWild;
    type Field;
    type Function;
    type FunctionExtern;
    type FunctionNormal;
    type Ident;
    type IdentLowerScoped;
    type IdentUpperScoped;
    type Import;
    type Index;
    type Interpolation;
    type Item;
    type KeyPrimary;
    type LitBool;
    type LitMap;
    type LitNum;
    type LitNumBin;
    type LitNumDec;
    type LitNumFloat;
    type LitNumHex;
    type LitNumOct;
    type LitString;
    type LitVec;
    type ModuleAlias;
    type ModulePath;
    type Name;
    type NameArg;
    type NameCons;
    type NameFunc;
    type NameIndex;
    type NameRel;
    type NameTrans;
    type NameType;
    type NameVarTerm;
    type NameVarType;
    type Pat;
    type PatCons;
    type PatConsPos;
    type PatConsRec;
    type PatLit;
    type PatTermDeclVar;
    type PatTuple;
    type PatType;
    type PatWild;
    type Rel;
    type RelArgs;
    type RelElem;
    type RelRole;
    type RelSemantics;
    type Rhs;
    type RhsAtomNeg;
    type RhsFlatMap;
    type RhsGrouping;
    type RhsInspect;
    type Rule;
    type Statement;
    type StatementAssign;
    type StatementBlock;
    type StatementEmpty;
    type StatementFor;
    type StatementIf;
    type StatementInsert;
    type StatementMatch;
    type StringQuoted;
    type StringQuotedEscape;
    type StringRaw;
    type StringRawInterpolated;
    type Transformer;
    type Type;
    type TypeAtom;
    type TypeBigint;
    type TypeBit;
    type TypeBool;
    type TypeDouble;
    type TypeFloat;
    type TypeFun;
    type TypeSigned;
    type TypeString;
    type TypeTrans;
    type TypeTransFun;
    type TypeTransRel;
    type TypeTuple;
    type TypeUnion;
    type TypeUser;
    type TypeVar;
    type Typedef;
    type TypedefExtern;
    type TypedefNormal;
    type Word;
}

#[allow(missing_docs)]
pub trait Visitor<'tree, Ctx, Ast>
where
    Ctx: crate::node::Context<'tree> + 'tree,
    Ast: AbstractSyntax<'tree> + 'tree,
{
    fn walker(&mut self) -> &mut NodeWalker<'tree, Ctx>;

    fn node(&self) -> tree_sitter::Node<'tree>;

    fn reset(&mut self, node: tree_sitter::Node<'tree>);

    #[allow(non_snake_case)]
    fn visit_ROOT(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::ROOT> {
        visit::ROOT(self, node)
    }
}

pub mod utils {
    #![allow(missing_docs)]
    #![allow(unused)]

    use super::*;
    use crate::node::{Context, SyntaxError};

    pub trait Choice<'tree, Ctx, Ast, Vis>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        fn choice(&self, visitor: &mut Vis) -> Result<(), SyntaxErrors>;
    }

    ddlog_lsp_macros::impl_choice!(0);

    #[inline]
    pub fn choice<'tree, Ctx, Ast, Vis, T>(funs: T) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
        T: Choice<'tree, Ctx, Ast, Vis>,
    {
        move |visitor| funs.choice(visitor)
    }

    #[inline]
    pub fn done<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
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
    pub fn optional<'tree, Ctx, Ast, Vis>(
        fun: impl Fn(&mut Vis) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        move |visitor| {
            let prev = visitor.node();
            if fun(visitor).is_err() {
                visitor.reset(prev);
            }

            Ok(())
        }
    }

    pub fn repeat<'tree, T, Ctx, Ast, Vis>(
        fun: impl Fn(&mut Vis) -> Result<T, SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<Vec<T>, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        move |visitor| {
            let mut results = vec![];

            loop {
                let prev = visitor.node();
                if visitor.walker().done {
                    break;
                }
                let item = fun(visitor);
                if item.is_err() {
                    visitor.reset(prev);
                    break;
                } else {
                    results.push(item?);
                }
            }

            Ok(results)
        }
    }

    #[inline]
    pub fn repeat1<'tree, T, Ctx, Ast, Vis>(
        fun: impl Fn(&mut Vis) -> Result<T, SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<Vec<T>, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        move |visitor| {
            let mut errors = SyntaxErrors::new();
            let mut results = vec![];

            if visitor.walker().done {
                errors.push(SyntaxError::DoneEarly);
                return Err(errors);
            }

            loop {
                let prev = visitor.node();
                match fun(visitor) {
                    Err(mut errs) => {
                        if !results.is_empty() {
                            visitor.reset(prev);
                            break;
                        }
                        errors.append(&mut errs);
                        return Err(errors);
                    },
                    Ok(item) => {
                        results.push(item);
                    },
                }
            }

            Ok(results)
        }
    }

    pub trait Seq<'tree, Ctx, Ast, Vis>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        fn seq(&self, visitor: &mut Vis) -> Result<(), SyntaxErrors>;
    }

    ddlog_lsp_macros::impl_seq!(0);

    #[inline]
    pub fn seq<'tree, Ctx, Vis, Ast, T>(funs: T) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
        T: Seq<'tree, Ctx, Ast, Vis>,
    {
        move |visitor| funs.seq(visitor)
    }

    #[inline]
    pub fn restore<'tree, Ctx, Ast, Vis>(
        fun: impl Fn(&mut Vis) -> Result<(), SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        move |visitor| {
            let prev = visitor.node();
            if let Err(mut errs) = fun(visitor) {
                visitor.reset(prev);
                let mut errors = SyntaxErrors::new();
                errors.append(&mut errs);
                return Err(errors);
            }
            Ok(())
        }
    }
}

#[allow(unused)]
#[allow(missing_docs)]
pub mod visit {
    use super::{AbstractSyntax, Visitor};
    use crate::node::Context;

    #[allow(non_snake_case)]
    pub fn ROOT<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ROOT>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn annotated_item<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::AnnotatedItem>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn apply<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Apply>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn arg<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Arg>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn arg_opt_type<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ArgOptType>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn arg_trans<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ArgTrans>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn atom_elem<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::AtomElem>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn atom_pos<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::AtomPos>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn atom_rec<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::AtomRec>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn attribute<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Attribute>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn attributes<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Attributes>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn comment_block<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::CommentBlock>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn comment_line<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::CommentLine>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn cons<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Cons>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn cons_pos<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ConsPos>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn cons_rec<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ConsRec>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn escape_sequence<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::EscapeSequence>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn escape_sequence_interpolated<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::EscapeSequenceInterpolated>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::EscapeSequenceInterpolated>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_add<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpAdd>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_assign<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpAssign>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_binding<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBinding>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_bit_and<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBitAnd>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_bit_neg<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBitNeg>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_bit_or<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBitOr>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_bit_slice<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBitSlice>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_bit_xor<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBitXor>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_block<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBlock>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_break<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpBreak>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_cast<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpCast>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_cat<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpCat>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_cond<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpCond>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_cons_pos<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpConsPos>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_cons_rec<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpConsRec>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_continue<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpContinue>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_decl_var<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpDeclVar>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_div<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpDiv>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_eq<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpEq>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_field<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpField>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_for<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpFor>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_fun_call<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpFunCall>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_fun_call_dot<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpFunCallDot>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_gt<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpGt>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_gteq<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpGteq>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_lambda<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpLambda>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_lit<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpLit>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_log_and<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpLogAnd>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_log_or<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpLogOr>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_lt<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpLt>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_lteq<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpLteq>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_match<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpMatch>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_mul<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpMul>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_neg<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpNeg>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_neq<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpNeq>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_proj<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpProj>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_ref<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpRef>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_rem<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpRem>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_return<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpReturn>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_seq<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpSeq>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_shl<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpShl>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_shr<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpShr>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_slice<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpSlice>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_sub<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpSub>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_try<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpTry>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_tuple<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpTuple>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_type<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpType>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp_wild<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ExpWild>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn field<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Field>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn function<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Function>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn function_extern<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::FunctionExtern>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn function_normal<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::FunctionNormal>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn ident<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Ident>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn ident_lower_scoped<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::IdentLowerScoped>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn ident_upper_scoped<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::IdentUpperScoped>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn import<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Import>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn index<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Index>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn interpolation<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Interpolation>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn item<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Item>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn key_primary<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::KeyPrimary>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_bool<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitBool>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_map<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitMap>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitNum>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num_bin<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitNumBin>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num_dec<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitNumDec>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num_float<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitNumFloat>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num_hex<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitNumHex>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num_oct<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitNumOct>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_string<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitString>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_vec<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitVec>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn module_alias<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ModuleAlias>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn module_path<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ModulePath>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Name>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_arg<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameArg>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_cons<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameCons>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_func<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameFunc>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_index<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameIndex>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_rel<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameRel>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_trans<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameTrans>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_type<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameType>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_var_term<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameVarTerm>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_var_type<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::NameVarType>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Pat>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_cons<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatCons>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_cons_pos<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatConsPos>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_cons_rec<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatConsRec>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_lit<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatLit>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_term_decl_var<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatTermDeclVar>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_tuple<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatTuple>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_type<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatType>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn pat_wild<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::PatWild>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rel<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Rel>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rel_args<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RelArgs>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rel_elem<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RelElem>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rel_role<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RelRole>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rel_semantics<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RelSemantics>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rhs<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Rhs>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rhs_atom_neg<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RhsAtomNeg>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rhs_flat_map<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RhsFlatMap>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rhs_grouping<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RhsGrouping>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rhs_inspect<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RhsInspect>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rule<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Rule>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Statement>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement_assign<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StatementAssign>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement_block<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StatementBlock>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement_empty<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StatementEmpty>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement_for<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StatementFor>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement_if<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StatementIf>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement_insert<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StatementInsert>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn statement_match<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StatementMatch>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn string_quoted<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StringQuoted>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn string_quoted_escape<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StringQuotedEscape>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn string_raw<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StringRaw>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn string_raw_interpolated<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::StringRawInterpolated>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn transformer<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Transformer>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn r#type<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Type>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_atom<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeAtom>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_bigint<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeBigint>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_bit<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeBit>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_bool<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeBool>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_double<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeDouble>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_float<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeFloat>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_fun<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeFun>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_signed<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeSigned>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_string<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeString>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_trans<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeTrans>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_trans_fun<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeTransFun>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_trans_rel<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeTransRel>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_tuple<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeTuple>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_union<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeUnion>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_user<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeUser>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn type_var<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::TypeVar>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn typedef<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Typedef>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn typedef_extern<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Typedef>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn typedef_normal<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Typedef>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn word<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Typedef>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }
}
