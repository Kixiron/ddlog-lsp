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
            (NAME_REL, "name_rel", true),
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
    type NameRel;
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

    #[allow(non_snake_case)]
    fn ROOT(commands: Vec<Self::Command>) -> Self::ROOT;

    fn atom(atom_rec: Self::AtomRec, atom_pos: Self::AtomPos, atom_elem: Self::AtomElem) -> Self::Atom;

    fn atom_elem(name_rel: Self::NameRel, exp: Self::Exp) -> Self::AtomElem;

    fn atom_pos(name_rel: Self::NameRel, exps: Vec<Self::Exp>) -> Self::AtomPos;
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
    fn visit_ROOT(&mut self) -> Result<Ast::ROOT, SyntaxErrors> {
        visit::ROOT(self)
    }

    fn visit_atom(&mut self) -> Result<Ast::Atom, SyntaxErrors> {
        visit::atom(self)
    }

    fn visit_atom_elem(&mut self) -> Result<Ast::AtomElem, SyntaxErrors> {
        visit::atom_elem(self)
    }

    fn visit_atom_pos(&mut self) -> Result<Ast::AtomPos, SyntaxErrors> {
        visit::atom_pos(self)
    }

    fn visit_atom_rec(&mut self) -> Result<Ast::AtomRec, SyntaxErrors> {
        visit::atom_rec(self)
    }

    fn visit_clear(&mut self) -> Result<Ast::Clear, SyntaxErrors> {
        visit::clear(self)
    }

    fn visit_command(&mut self) -> Result<Ast::Command, SyntaxErrors> {
        visit::command(self)
    }

    fn visit_comment_line(&mut self) -> Result<Ast::CommentLine, SyntaxErrors> {
        visit::comment_line(self)
    }

    fn visit_commit(&mut self) -> Result<Ast::Commit, SyntaxErrors> {
        visit::commit(self)
    }

    fn visit_cons_arg(&mut self) -> Result<Ast::ConsArg, SyntaxErrors> {
        visit::cons_arg(self)
    }

    fn visit_cons_args(&mut self) -> Result<Ast::ConsArgs, SyntaxErrors> {
        visit::cons_args(self)
    }

    fn visit_delete(&mut self) -> Result<Ast::Delete, SyntaxErrors> {
        visit::delete(self)
    }

    fn visit_delete_key(&mut self) -> Result<Ast::DeleteKey, SyntaxErrors> {
        visit::delete_key(self)
    }

    fn visit_dump(&mut self) -> Result<Ast::Dump, SyntaxErrors> {
        visit::dump(self)
    }

    fn visit_dump_index(&mut self) -> Result<Ast::DumpIndex, SyntaxErrors> {
        visit::dump_index(self)
    }

    fn visit_echo(&mut self) -> Result<Ast::Echo, SyntaxErrors> {
        visit::echo(self)
    }

    fn visit_exit(&mut self) -> Result<Ast::Exit, SyntaxErrors> {
        visit::exit(self)
    }

    fn visit_exp(&mut self) -> Result<Ast::Exp, SyntaxErrors> {
        visit::exp(self)
    }

    fn visit_insert(&mut self) -> Result<Ast::Insert, SyntaxErrors> {
        visit::insert(self)
    }

    fn visit_insert_or_update(&mut self) -> Result<Ast::InsertOrUpdate, SyntaxErrors> {
        visit::insert_or_update(self)
    }

    fn visit_lit_num_hex(&mut self) -> Result<Ast::LitNumHex, SyntaxErrors> {
        visit::lit_num_hex(self)
    }

    fn visit_lit_serialized(&mut self) -> Result<Ast::LitSerialized, SyntaxErrors> {
        visit::lit_serialized(self)
    }

    fn visit_lit_string(&mut self) -> Result<Ast::LitString, SyntaxErrors> {
        visit::lit_string(self)
    }

    fn visit_log_level(&mut self) -> Result<Ast::LogLevel, SyntaxErrors> {
        visit::log_level(self)
    }

    fn visit_modify(&mut self) -> Result<Ast::Modify, SyntaxErrors> {
        visit::modify(self)
    }

    fn visit_name_rel(&mut self) -> Result<Ast::NameRel, SyntaxErrors> {
        visit::name_rel(self)
    }

    fn visit_profile(&mut self) -> Result<Ast::Profile, SyntaxErrors> {
        visit::profile(self)
    }

    fn visit_query_index(&mut self) -> Result<Ast::QueryIndex, SyntaxErrors> {
        visit::query_index(self)
    }

    fn visit_record(&mut self) -> Result<Ast::Record, SyntaxErrors> {
        visit::record(self)
    }

    fn visit_record_named(&mut self) -> Result<Ast::RecordNamed, SyntaxErrors> {
        visit::record_named(self)
    }

    fn visit_rollback(&mut self) -> Result<Ast::Rollback, SyntaxErrors> {
        visit::rollback(self)
    }

    fn visit_serde_encoding(&mut self) -> Result<Ast::SerdeEncoding, SyntaxErrors> {
        visit::serde_encoding(self)
    }

    fn visit_sleep(&mut self) -> Result<Ast::Sleep, SyntaxErrors> {
        visit::sleep(self)
    }

    fn visit_start(&mut self) -> Result<Ast::Start, SyntaxErrors> {
        visit::start(self)
    }

    fn visit_timestamp(&mut self) -> Result<Ast::Timestamp, SyntaxErrors> {
        visit::timestamp(self)
    }

    fn visit_update(&mut self) -> Result<Ast::Update, SyntaxErrors> {
        visit::update(self)
    }

    fn visit_updates(&mut self) -> Result<Ast::Updates, SyntaxErrors> {
        visit::updates(self)
    }

    fn visit_val_array(&mut self) -> Result<Ast::ValArray, SyntaxErrors> {
        visit::val_array(self)
    }

    fn visit_val_struct(&mut self) -> Result<Ast::ValStruct, SyntaxErrors> {
        visit::val_struct(self)
    }

    fn visit_val_tuple(&mut self) -> Result<Ast::ValTuple, SyntaxErrors> {
        visit::val_tuple(self)
    }

    fn visit_word(&mut self) -> Result<Ast::Word, SyntaxErrors> {
        visit::word(self)
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

    ddlog_lsp_macros::impl_seq!(2);
    ddlog_lsp_macros::impl_seq!(3);
    ddlog_lsp_macros::impl_seq!(4);

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
    use super::*;
    use crate::node::Context;

    #[allow(non_snake_case)]
    pub fn ROOT<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::ROOT, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        visitor.walker().rule(kind::ROOT)?;
        let commands = utils::repeat(command)(visitor)?;
        Ok(Ast::ROOT(commands))
    }

    pub fn atom<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Atom, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM)?;
        let (atom_rec, atom_pos, atom_elem) = utils::seq((atom_rec, atom_pos, atom_elem))(visitor)?;
        Ok(Ast::atom(atom_rec, atom_pos, atom_elem))
    }

    pub fn atom_elem<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::AtomElem, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_ELEM)?;
        let (name_rel, _, exp, _) =
            utils::seq((name_rel, token::LEFT_SQUARE_BRACKET, exp, token::RIGHT_SQUARE_BRACKET))(visitor)?;
        Ok(Ast::atom_elem(name_rel, exp))
    }

    pub fn atom_pos<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::AtomPos, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        visitor.walker().rule(kind::ATOM_POS)?;
        let (name_rel, pat1) = utils::seq((
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
        ))(visitor)?;
        let exps = match pat1 {
            None => vec![],
            Some((_, pat10, _)) => match pat10 {
                None => vec![],
                Some((exp, pat101, _)) => {
                    let mut exps = vec![exp];
                    for ((_, exp)) in pat101 {
                        exps.push(exp);
                    }
                    exps
                },
            },
        };
        Ok(Ast::atom_pos(name_rel, exps))
    }

    pub fn atom_rec<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::AtomRec, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn clear<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Clear, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn command<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Command, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn comment_line<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::CommentLine, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn commit<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Commit, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn cons_arg<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::ConsArg, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn cons_args<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::ConsArgs, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn delete<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Delete, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn delete_key<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::DeleteKey, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn dump<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Dump, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn dump_index<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::DumpIndex, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn echo<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Echo, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exit<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Exit, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn exp<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Exp, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn insert<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Insert, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn insert_or_update<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::InsertOrUpdate, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num_hex<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::LitNumHex, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_serialized<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::LitSerialized, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn lit_string<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::LitString, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn log_level<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::LogLevel, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn modify<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Modify, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn name_rel<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::NameRel, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn profile<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Profile, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn query_index<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::QueryIndex, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn record<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Record, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn record_named<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::RecordNamed, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn rollback<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Rollback, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn serde_encoding<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::SerdeEncoding, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn sleep<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Sleep, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn start<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Start, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn timestamp<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Timestamp, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn update<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Update, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn updates<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::Updates, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn val_array<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::ValArray, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn val_struct<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::ValStruct, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn val_tuple<'tree, Ctx, Ast, Vis>(
        visitor: &mut Vis,
    ) -> Result<<Ast as AbstractSyntax<'tree>>::ValTuple, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub fn word<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<<Ast as AbstractSyntax<'tree>>::Word, SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Ast: AbstractSyntax<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
    {
        todo!()
    }

    pub mod token {
        use super::*;

        macro_rules! make {
            ($name:tt) => {
                #[inline]
                #[allow(non_snake_case)]
                pub fn $name<'tree, Ctx, Ast, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
                where
                    Ctx: Context<'tree> + 'tree,
                    Ast: AbstractSyntax<'tree> + 'tree,
                    Vis: Visitor<'tree, Ctx, Ast> + ?Sized,
                {
                    visitor.walker().token(super::super::token::$name)?;
                    Ok(())
                }
            };
        }

        make!(COMMA);
        make!(COMMERCIAL_AT);
        make!(DOLLAR_SIGN);
        make!(EQUALS_SIGN);
        make!(FULL_STOP);
        make!(LEFT_CURLY_BRACKET);
        make!(LEFT_PARENTHESIS);
        make!(LEFT_SQUARE_BRACKET);
        make!(LEFTWARDS_ARROW);
        make!(NUMBER_SIGN);
        make!(QUOTATION_MARK);
        make!(RIGHT_CURLY_BRACKET);
        make!(RIGHT_PARENTHESIS);
        make!(RIGHT_SQUARE_BRACKET);
        make!(SEMICOLON);
    }
}
