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
            (NAME_ARG, "name_arg", true),
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

pub mod token {
    #![allow(missing_docs)]

    // keywords
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

    // tokens
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
    ddlog_lsp_macros::impl_choice!(14);

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
        todo!()
    }

    pub fn commit<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn cons_arg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn cons_args<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn delete<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn delete_key<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn dump<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn dump_index<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn echo<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn exit<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn exp<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn insert<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn insert_or_update<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn lit_num_hex<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn lit_serialized<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn lit_string<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn log_level<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn modify<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn name_arg<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn name_rel<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn profile<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn query_index<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn record<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn record_named<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn rollback<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn serde_encoding<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn sleep<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn start<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn timestamp<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn update<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn updates<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn val_array<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn val_struct<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
    where
        Ctx: Context<'tree> + 'tree,
        Vis: Visitor<'tree, Ctx> + ?Sized,
    {
        todo!()
    }

    pub fn val_tuple<'tree, Ctx, Vis>(visitor: &mut Vis) -> Result<(), SyntaxErrors>
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

        make!(CLEAR);

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
