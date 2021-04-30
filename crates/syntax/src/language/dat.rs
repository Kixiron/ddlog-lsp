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
            (ATOM, "atom", true),
            (ATOM_ELEM, "atom_elem", true),
            (ATOM_POS, "atom_pos", true),
            (ATOM_REC, "atom_rec", true),
            (CLEAR, "clear", true),
            (COMMAND, "command", true),
            (COMMENT_LINE, "comment_line", true),
            (COMMIT, "commit", true),
            (CONS_ARG, "cons_arg", true),
            (CONS_ARGS, "cons_args", true),
            (DELETE, "delete", true),
            (DELETE_KEY, "delete_key", true),
            (DUMP, "dump", true),
            (DUMP_INDEX, "dump_index", true),
            (ECHO, "echo", true),
            (EXIT, "exit", true),
            (EXP, "exp", true),
            (INSERT, "insert", true),
            (INSERT_OR_UPDATE, "insert_or_update", true),
            (LIT_NUM_HEX, "lit_num_hex", true),
            (LIT_SERIALIZED, "lit_serialized", true),
            (LIT_STRING, "lit_string", true),
            (LOG_LEVEL, "log_level", true),
            (MODIFY, "modify", true),
            (PROFILE, "profile", true),
            (QUERY_INDEX, "query_index", true),
            (RECORD, "record", true),
            (RECORD_NAMED, "record_named", true),
            (ROLLBACK, "rollback", true),
            (SERDE_ENCODING, "serde_encoding", true),
            (SLEEP, "sleep", true),
            (START, "start", true),
            (TIMESTAMP, "timestamp", true),
            (UPDATE, "update", true),
            (UPDATES, "updates", true),
            (VAL_ARRAY, "val_array", true),
            (VAL_STRUCT, "val_struct", true),
            (VAL_TUPLE, "val_tuple", true),
            (WORD, "word", true),
        ],
    }
}

pub mod keyword {
    #![allow(missing_docs)]

    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dat",
        node_kinds: [
            (CLEAR, "clear", false),
            (COMMIT, "commit", false),
            (CPU, "cpu", false),
            (DELETE, "delete", false),
            (DELETE_KEY, "delete_key", false),
            (DUMP, "dump", false),
            (DUMP_CHANGES, "dump_changes", false),
            (DUMP_INDEX, "dump_index", false),
            (ECHO, "echo", false),
            (EXIT, "exit", false),
            (INSERT, "insert", false),
            (INSERT_OR_UPDATE, "insert_or_update", false),
            (JSON, "json", false),
            (LOG_LEVEL, "log_level", false),
            (MODIFY, "modify", false),
            (OFF, "off", false),
            (ON, "on", false),
            (PROFILE, "profile", false),
            (QUERY_INDEX, "query_index", false),
            (ROLLBACK, "rollback", false),
            (SLEEP, "sleep", false),
            (START, "start", false),
            (TIMESTAMP, "timestamp", false),
        ]
    }
}

pub mod token {
    #![allow(missing_docs)]

    ddlog_lsp_macros::node_kind_ids! {
        language: "ddlog.dat",
        node_kinds: [
            (COMMA, ",", false),
            (COMMERCIAL_AT, "@", false),
            (DOLLAR_SIGN, "$", false),
            (EQUALS_SIGN, "=", false),
            (FULL_STOP, ".", false),
            (LEFT_CURLY_BRACKET, "{", false),
            (LEFT_PARENTHESIS, "(", false),
            (LEFT_SQUARE_BRACKET, "[", false),
            (LEFTWARDS_ARROW, "<-", false),
            (NUMBER_SIGN, "#", false),
            (QUOTATION_MARK, "\"", false),
            (RIGHT_CURLY_BRACKET, "}", false),
            (RIGHT_PARENTHESIS, ")", false),
            (RIGHT_SQUARE_BRACKET, "]", false),
            (SEMICOLON, ";", false),
        ]
    }
}

#[allow(missing_docs)]
pub trait AbstractSyntax<'tree> {
    type ROOT;
    type Atom;
    type AtomElem;
    type AtomPos;
    type AtomRec;
    type Clear;
    type Command;
    type CommentLine;
    type Commit;
    type ConsArg;
    type ConsArgs;
    type Delete;
    type DeleteKey;
    type Dump;
    type DumpIndex;
    type Echo;
    type Exit;
    type Exp;
    type Insert;
    type InsertOrUpdate;
    type LitNumHex;
    type LitSerialized;
    type LitString;
    type LogLevel;
    type Modify;
    type Profile;
    type QueryIndex;
    type Record;
    type RecordNamed;
    type Rollback;
    type SerdeEncoding;
    type Sleep;
    type Start;
    type Timestamp;
    type Update;
    type Updates;
    type ValArray;
    type ValStruct;
    type ValTuple;
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

    fn visit_atom(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Atom> {
        visit::atom(self, node)
    }

    fn visit_atom_elem(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::AtomElem> {
        visit::atom_elem(self, node)
    }

    fn visit_atom_pos(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::AtomPos> {
        visit::atom_pos(self, node)
    }

    fn visit_atom_rec(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::AtomRec> {
        visit::atom_rec(self, node)
    }

    fn visit_clear(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Clear> {
        visit::clear(self, node)
    }

    fn visit_command(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Command> {
        visit::command(self, node)
    }

    fn visit_comment_line(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::CommentLine> {
        visit::comment_line(self, node)
    }

    fn visit_commit(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Commit> {
        visit::commit(self, node)
    }

    fn visit_cons_arg(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::ConsArg> {
        visit::cons_arg(self, node)
    }

    fn visit_cons_args(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::ConsArgs> {
        visit::cons_args(self, node)
    }

    fn visit_delete(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Delete> {
        visit::delete(self, node)
    }

    fn visit_delete_key(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::DeleteKey> {
        visit::delete_key(self, node)
    }

    fn visit_dump(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Dump> {
        visit::dump(self, node)
    }

    fn visit_dump_index(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::DumpIndex> {
        visit::dump_index(self, node)
    }

    fn visit_echo(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Echo> {
        visit::echo(self, node)
    }

    fn visit_exit(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Exit> {
        visit::exit(self, node)
    }

    fn visit_exp(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Exp> {
        visit::exp(self, node)
    }

    fn visit_insert(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Insert> {
        visit::insert(self, node)
    }

    fn visit_insert_or_update(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::InsertOrUpdate> {
        visit::insert_or_update(self, node)
    }

    fn visit_lit_num_hex(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::LitNumHex> {
        visit::lit_num_hex(self, node)
    }

    fn visit_lit_serialized(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::LitSerialized> {
        visit::lit_serialized(self, node)
    }

    fn visit_lit_string(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::LitString> {
        visit::lit_string(self, node)
    }

    fn visit_log_level(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::LogLevel> {
        visit::log_level(self, node)
    }

    fn visit_modify(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Modify> {
        visit::modify(self, node)
    }

    fn visit_profile(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Profile> {
        visit::profile(self, node)
    }

    fn visit_query_index(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::QueryIndex> {
        visit::query_index(self, node)
    }

    fn visit_record(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Record> {
        visit::record(self, node)
    }

    fn visit_record_named(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::RecordNamed> {
        visit::record_named(self, node)
    }

    fn visit_rollback(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Rollback> {
        visit::rollback(self, node)
    }

    fn visit_serde_encoding(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::SerdeEncoding> {
        visit::serde_encoding(self, node)
    }

    fn visit_sleep(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Sleep> {
        visit::sleep(self, node)
    }

    fn visit_start(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Start> {
        visit::start(self, node)
    }

    fn visit_timestamp(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Timestamp> {
        visit::timestamp(self, node)
    }

    fn visit_update(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Update> {
        visit::update(self, node)
    }

    fn visit_updates(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Updates> {
        visit::updates(self, node)
    }

    fn visit_val_array(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::ValArray> {
        visit::val_array(self, node)
    }

    fn visit_val_struct(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::ValStruct> {
        visit::val_struct(self, node)
    }

    fn visit_val_tuple(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::ValTuple> {
        visit::val_tuple(self, node)
    }

    fn visit_word(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<Ast::Word> {
        visit::word(self, node)
    }
}

pub mod utils {
    #![allow(missing_docs)]
    #![allow(unused)]

    use super::*;
    use crate::node::{Context, SyntaxError};

    pub trait Alt<'tree, Ctx, Ast, Vis>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        type Output;

        fn alt(&self, visitor: &mut Vis) -> Result<Self::Output, SyntaxErrors>;
    }

    ddlog_lsp_macros::enum_alt!(1);
    ddlog_lsp_macros::enum_alt!(2);
    ddlog_lsp_macros::enum_alt!(3);

    ddlog_lsp_macros::impl_alt!(1);
    ddlog_lsp_macros::impl_alt!(2);
    ddlog_lsp_macros::impl_alt!(3);

    #[inline]
    pub fn alt<'tree, Ctx, Ast, Vis, T, R>(funs: T) -> impl Fn(&mut Vis) -> Result<R, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
        T: Alt<'tree, Ctx, Ast, Vis, Output = R>,
    {
        move |visitor| funs.alt(visitor)
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
    pub fn optional<'tree, Ctx, Ast, Vis, R>(
        fun: impl Fn(&mut Vis) -> Result<R, SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<Option<R>, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        move |visitor| {
            let prev = visitor.node();
            match fun(visitor) {
                Ok(result) => Ok(Some(result)),
                Err(_) => {
                    visitor.reset(prev);
                    Ok(None)
                },
            }
        }
    }

    pub fn repeat<'tree, Ctx, Ast, Vis, R>(
        fun: impl Fn(&mut Vis) -> Result<R, SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<Vec<R>, SyntaxErrors>
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
                match fun(visitor) {
                    Ok(result) => {
                        results.push(result);
                    },
                    Err(_) => {
                        visitor.reset(prev);
                        break;
                    },
                }
            }

            Ok(results)
        }
    }

    #[inline]
    pub fn repeat1<'tree, Ctx, Ast, Vis, R>(
        fun: impl Fn(&mut Vis) -> Result<R, SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<Vec<R>, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        move |visitor| {
            let mut results = vec![];
            let mut errors = SyntaxErrors::new();

            if visitor.walker().done {
                errors.push(SyntaxError::DoneEarly);
                return Err(errors);
            }

            loop {
                let prev = visitor.node();
                match fun(visitor) {
                    Ok(result) => {
                        results.push(result);
                    },
                    Err(mut errs) => {
                        if !results.is_empty() {
                            visitor.reset(prev);
                            break;
                        }
                        errors.append(&mut errs);
                        return Err(errors);
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
        type Output;

        fn seq(&self, visitor: &mut Vis) -> Result<Self::Output, SyntaxErrors>;
    }

    ddlog_lsp_macros::impl_seq!(1);
    ddlog_lsp_macros::impl_seq!(2);
    ddlog_lsp_macros::impl_seq!(3);

    #[inline]
    pub fn seq<'tree, Ctx, Vis, Ast, T, R>(funs: T) -> impl Fn(&mut Vis) -> Result<R, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
        T: Seq<'tree, Ctx, Ast, Vis, Output = R>,
    {
        move |visitor| funs.seq(visitor)
    }

    #[inline]
    pub fn restore<'tree, Ctx, Ast, Vis, O>(
        fun: impl Fn(&mut Vis) -> Result<O, SyntaxErrors>,
    ) -> impl Fn(&mut Vis) -> Result<O, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
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

    pub fn atom<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Atom>
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

    pub fn clear<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Clear>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn command<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Command>
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

    pub fn commit<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Commit>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn cons_arg<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ConsArg>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn cons_args<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ConsArgs>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn delete<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Delete>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn delete_key<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::DeleteKey>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn dump<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Dump>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn dump_index<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::DumpIndex>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn echo<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Echo>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exit<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Exit>
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
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Exp>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn insert<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Insert>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn insert_or_update<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::InsertOrUpdate>
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

    pub fn lit_serialized<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LitSerialized>
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

    pub fn log_level<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::LogLevel>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn modify<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Modify>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn profile<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Profile>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn query_index<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::QueryIndex>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn record<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Record>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn record_named<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::RecordNamed>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rollback<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Rollback>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn serde_encoding<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::SerdeEncoding>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn sleep<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Sleep>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn start<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Start>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn timestamp<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Timestamp>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn update<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Update>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn updates<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Updates>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn val_array<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ValArray>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn val_struct<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ValStruct>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn val_tuple<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
        node: tree_sitter::Node<'tree>,
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::ValTuple>
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
    ) -> anyhow::Result<<Ast as AbstractSyntax<'tree>>::Word>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }
}
