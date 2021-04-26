//! Functions for working with the `.dat` grammar.

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
            (ATOM_ELEM, "atom_elem", true),
            (ATOM_POS, "atom_pos", true),
            (ATOM_REC, "atom_rec", true),
            (ATOM, "atom", true),
            (CLEAR, "clear", true),
            (COMMAND, "command", true),
            (COMMENT_LINE, "comment_line", true),
            (COMMIT, "commit", true),
            (CONS_ARG, "cons_arg", true),
            (CONS_ARGS, "cons_args", true),
            (DELETE_KEY, "delete_key", true),
            (DELETE, "delete", true),
            (DUMP_INDEX, "dump_index", true),
            (DUMP, "dump", true),
            (ECHO, "echo", true),
            (EXIT, "exit", true),
            (EXP, "exp", true),
            (INSERT_OR_UPDATE, "insert_or_update", true),
            (INSERT, "insert", true),
            (LIT_NUM_HEX, "lit_num_hex", true),
            (LIT_SERIALIZED, "lit_serialized", true),
            (LIT_STRING, "lit_string", true),
            (LOG_LEVEL, "log_level", true),
            (MODIFY, "modify", true),
            (PROFILE, "profile", true),
            (QUERY_INDEX, "query_index", true),
            (RECORD_NAMED, "record_named", true),
            (RECORD, "record", true),
            (ROLLBACK, "rollback", true),
            (ROOT, "root", true),
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
pub trait Visit<'tree> {
    #[allow(non_snake_case)]
    fn visit_ROOT(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::ROOT(self, node)
    }

    fn visit_atom_elem(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::atom_elem(self, node)
    }

    fn visit_atom_pos(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::atom_pos(self, node)
    }

    fn visit_atom_rec(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::atom_rec(self, node)
    }

    fn visit_atom(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::atom(self, node)
    }

    fn visit_clear(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::clear(self, node)
    }

    fn visit_command(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::command(self, node)
    }

    fn visit_comment_line(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::comment_line(self, node)
    }

    fn visit_commit(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::commit(self, node)
    }

    fn visit_cons_arg(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::cons_arg(self, node)
    }

    fn visit_cons_args(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::cons_args(self, node)
    }

    fn visit_delete_key(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::delete_key(self, node)
    }

    fn visit_dump_index(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::dump_index(self, node)
    }

    fn visit_dump(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::dump(self, node)
    }

    fn visit_echo(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::echo(self, node)
    }

    fn visit_exit(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::exit(self, node)
    }

    fn visit_exp(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::exp(self, node)
    }

    fn visit_insert_or_update(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::insert_or_update(self, node)
    }

    fn visit_insert(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::insert(self, node)
    }

    fn visit_lit_num_hex(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::lit_num_hex(self, node)
    }

    fn visit_lit_serialized(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::lit_serialized(self, node)
    }

    fn visit_lit_string(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::lit_string(self, node)
    }

    fn visit_log_level(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::log_level(self, node)
    }

    fn visit_modify(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::modify(self, node)
    }

    fn visit_profile(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::profile(self, node)
    }

    fn visit_query_index(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::query_index(self, node)
    }

    fn visit_record_named(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::record_named(self, node)
    }

    fn visit_record(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::record(self, node)
    }

    fn visit_rollback(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::rollback(self, node)
    }

    fn visit_serde_encoding(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::serde_encoding(self, node)
    }

    fn visit_sleep(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::sleep(self, node)
    }

    fn visit_start(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::start(self, node)
    }

    fn visit_timestamp(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::timestamp(self, node)
    }

    fn visit_update(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::update(self, node)
    }

    fn visit_updates(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::updates(self, node)
    }

    fn visit_val_array(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::val_array(self, node)
    }

    fn visit_val_struct(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::val_struct(self, node)
    }

    fn visit_val_tuple(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::val_tuple(self, node)
    }

    fn visit_word(&mut self, node: tree_sitter::Node<'tree>) -> anyhow::Result<()> {
        visit::word(self, node)
    }
}

#[allow(unused)]
#[allow(missing_docs)]
pub mod visit {
    use super::Visit;

    #[allow(non_snake_case)]
    pub fn ROOT<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn atom<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn atom_elem<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn atom_pos<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn atom_rec<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn clear<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn command<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn comment_line<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn commit<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn cons_arg<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn cons_args<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn delete<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn delete_key<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn dump<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn dump_index<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn echo<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn exit<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn exp<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn insert<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn insert_or_update<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn lit_num_hex<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn lit_serialized<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn lit_string<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn log_level<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn modify<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn profile<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn query_index<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn record<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn record_named<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn rollback<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn serde_encoding<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn sleep<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn start<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn timestamp<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn update<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn updates<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn val_array<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn val_struct<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn val_tuple<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }

    pub fn word<'tree, V>(visitor: &mut V, node: tree_sitter::Node<'tree>) -> anyhow::Result<()>
    where
        V: Visit<'tree> + ?Sized,
    {
        Ok(())
    }
}
