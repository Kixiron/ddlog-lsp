use crate::node::{NodeError, NodeErrorData, SyntaxError};
use ddlog_lsp_languages::language::Language;

#[allow(missing_docs)]
pub mod context {
    use tree_sitter::Node;

    pub trait Context<'tree> {
        type Level;

        fn new() -> Self;

        fn pop(&mut self) -> Option<Self::Level>;

        fn push(&mut self, level: Self::Level);

        fn push_ancestor(&mut self, ancestor: Node<'tree>, prefixed: Vec<Node<'tree>>);

        fn push_prefix(&mut self, prefix: Node<'tree>);

        fn reverse(&mut self);
    }

    pub mod basic {
        use std::convert::Infallible;
        use tree_sitter::Node;

        #[derive(Debug, Clone, Eq, Hash, PartialEq)]
        pub struct Level<'tree> {
            phantom: std::marker::PhantomData<&'tree Infallible>,
        }

        #[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
        pub struct Context<'tree> {
            phantom: std::marker::PhantomData<&'tree Infallible>,
        }

        impl<'tree> super::Context<'tree> for Context<'tree> {
            type Level = Level<'tree>;

            #[inline]
            fn new() -> Self {
                Self::default()
            }

            #[inline]
            fn pop(&mut self) -> Option<Self::Level> {
                None
            }

            #[inline]
            fn push(&mut self, _: Self::Level) {
            }

            #[inline]
            fn push_ancestor(&mut self, _: Node<'tree>, _: Vec<Node<'tree>>) {
            }

            #[inline]
            fn push_prefix(&mut self, _: Node<'tree>) {
            }

            #[inline]
            fn reverse(&mut self) {
            }
        }
    }

    pub mod trace {
        use tree_sitter::Node;

        #[derive(Debug, Clone, Eq, Hash, PartialEq)]
        pub struct Level<'tree> {
            ancestor: Node<'tree>,
            prefixed: Vec<Node<'tree>>,
        }

        /// The current node context.
        #[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
        pub struct Context<'tree> {
            stack: Vec<Level<'tree>>,
        }

        impl<'tree> super::Context<'tree> for Context<'tree> {
            type Level = Level<'tree>;

            #[inline]
            fn new() -> Self {
                Self::default()
            }

            #[inline]
            fn pop(&mut self) -> Option<Self::Level> {
                self.stack.pop()
            }

            #[inline]
            fn push(&mut self, level: Self::Level) {
                self.stack.push(level);
            }

            #[inline]
            fn push_ancestor(&mut self, ancestor: Node<'tree>, prefixed: Vec<Node<'tree>>) {
                let level = Level { ancestor, prefixed };
                self.stack.push(level);
            }

            #[inline]
            fn push_prefix(&mut self, prefix: Node<'tree>) {
                if let Some(level) = self.stack.last_mut() {
                    level.prefixed.push(prefix);
                } else {
                    unreachable!("NodeWalkerContext::push_prefix should never be callable wihout an active level");
                }
            }

            #[inline]
            fn reverse(&mut self) {
                self.stack.reverse();
            }
        }
    }
}

pub use context::Context;

#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum StepValue<'tree> {
    Done,
    None,
    Some(tree_sitter::Node<'tree>),
}

#[allow(missing_docs)]
pub struct NodeWalker<'tree, C> {
    language: Language,
    pub context: C,
    cursor: tree_sitter::TreeCursor<'tree>,
    pub done: bool,
    pub error_state: Vec<u16>,
}

impl<'tree, C: Context<'tree>> NodeWalker<'tree, C> {
    /// Create a new [NodeWalker].
    #[inline]
    pub fn new(language: Language, node: tree_sitter::Node<'tree>) -> Self {
        let context = C::new();
        let cursor = node.walk();
        let done = Default::default();
        let error_state = Default::default();
        let mut walker = Self {
            language,
            context,
            cursor,
            done,
            error_state,
        };
        walker.reconstruct_stack();
        walker
    }

    /// Move the cursor to the first child node.
    #[inline]
    pub fn goto_first_child(&mut self) -> bool {
        log::info!("goto_first_child >>");

        let ancestor = self.cursor.node();
        let moved = self.cursor.goto_first_child();
        if moved {
            let prefixed = Default::default();
            self.context.push_ancestor(ancestor, prefixed);
        }

        log::info!("goto_first_child << {}", moved);
        moved
    }

    /// Move the cursor to the next sibling node.
    #[inline]
    pub fn goto_next_sibling(&mut self) -> bool {
        log::info!("goto_next_sibling >>");

        let prefix = self.cursor.node();
        let moved = self.cursor.goto_next_sibling();
        if moved {
            self.context.push_prefix(prefix);
        }

        log::info!("goto_next_sibling << {}", moved);
        moved
    }

    /// Move cursor to the next accessible node.
    #[inline]
    pub fn goto_next(&mut self) -> bool {
        let mut moved;

        // First try to descend to the first child node.
        moved = self.goto_first_child();
        if !moved {
            // Otherwise try to move to the next sibling node.
            moved = self.goto_next_sibling();
            if !moved {
                moved = self.goto_next_ancestor_sibling();
            }
        }

        moved
    }

    /// Move cursor to the next accessible node that has an error.
    #[inline]
    pub fn goto_next_has_error(&mut self) -> bool {
        let node = self.cursor.node();
        let mut moved;

        // Only descend if the current node has an error in the subtree.
        if node.has_error() {
            moved = self.goto_next();
        } else {
            // Otherwise try to move to the next sibling node.
            moved = self.goto_next_sibling();
            if !moved {
                moved = self.goto_next_ancestor_sibling();
            }
        }

        moved
    }

    /// Move the cursor to the next ancestor sibling node.
    #[inline]
    pub fn goto_next_ancestor_sibling(&mut self) -> bool {
        log::info!("goto_next_ancestor_sibling >>");

        let mut moved;
        let mut finished = true;

        // Otherwise continue to ascend to parent nodes...
        loop {
            moved = self.goto_parent();
            if moved {
                // ... until we can move to a sibling node.
                if self.goto_next_sibling() {
                    finished = false;
                    break;
                }
            } else {
                break;
            }
        }

        self.done = finished;

        log::info!("goto_next_ancestor_sibling << {}", moved);
        moved
    }

    /// Move the cursor to the parent node.
    #[inline]
    pub fn goto_parent(&mut self) -> bool {
        log::info!("goto_parent >>");

        let moved = self.cursor.goto_parent();
        if moved {
            self.context.pop();
        }

        log::info!("goto_parent << {}", moved);
        moved
    }

    /// Return the current node's kind id.
    #[inline]
    pub fn kind(&self) -> u16 {
        self.cursor.node().kind_id()
    }

    /// Return the current node for the cursor.
    #[inline]
    pub fn node(&self) -> tree_sitter::Node<'tree> {
        self.cursor.node()
    }

    /// Reconstruct the context stack from the current node position.
    #[inline]
    fn reconstruct_stack(&mut self) {
        use crate::language::{dat, dl};
        use Language::{DDlogDat, DDlogDl};

        let language = self.language;
        let node = self.node();
        let kind = node.kind_id();

        // Reconstruct the stack by traversing upward if the current node isn't ROOT.
        if (language == DDlogDat && dat::kind::ROOT != kind) || (language == DDlogDl && dl::kind::ROOT != kind) {
            let cursor = &mut node.walk();
            loop {
                let previous = self.node();
                if self.goto_parent() {
                    let ancestor = self.node();
                    let prefixed = ancestor
                        .children(cursor)
                        .take_while(|node| node.id() != previous.id())
                        .collect();
                    self.context.push_ancestor(ancestor, prefixed)
                } else {
                    break;
                }
            }

            self.context.reverse();
            self.cursor.reset(node);
        }
    }

    #[allow(missing_docs)]
    #[inline]
    pub fn reset(&mut self, node: tree_sitter::Node<'tree>) {
        self.cursor.reset(node);
    }

    #[inline]
    fn step(&mut self, that_kind_id: u16, _descend_into_error: bool) -> Result<(), SyntaxError> {
        let prev = self.node();

        let language: tree_sitter::Language = self.language.into();
        let this = prev.clone();
        let this_id = this.id();
        let this_kind_id = this.kind_id();
        let this_kind = language.node_kind_for_id(this_kind_id).unwrap();
        log::info!("stepping from: {}@{}", this_kind, this_id);

        let expected = language.node_kind_for_id(that_kind_id).unwrap();
        log::info!("expected: {}", expected);

        if self.goto_next() {
            let next = self.node();
            let next_kind_id = next.kind_id();
            let found = next.kind();
            log::info!("found: {}\n", found);

            if next.is_missing() {
                self.reset(prev);
                let data = NodeErrorData::new(next, self.error_state.clone());
                let error = SyntaxError::MissingNode(data);
                return Err(error);
            }

            if that_kind_id != next_kind_id {
                let language = self.language.into();
                let expected = vec![that_kind_id];
                let found = Some(NodeErrorData::new(next, self.error_state.clone()));
                let error = NodeError {
                    language,
                    expected,
                    found,
                }
                .into();
                return Err(error);
            }

            Ok(())
        } else {
            let expected = vec![that_kind_id];
            let found = None;
            let error = NodeError {
                language,
                expected,
                found,
            }
            .into();
            Err(error)
        }
    }

    #[allow(missing_docs)]
    #[inline]
    pub fn rule(&mut self, that_id: u16) -> Result<(), SyntaxError> {
        let descend_into_error = true;
        self.step(that_id, descend_into_error)
    }

    #[allow(missing_docs)]
    #[inline]
    pub fn token(&mut self, that_id: u16) -> Result<(), SyntaxError> {
        let descend_into_error = false;
        self.step(that_id, descend_into_error)
    }

    #[allow(missing_docs)]
    #[inline]
    pub fn within_error(&self) -> bool {
        !self.error_state.is_empty()
    }
}

#[allow(missing_docs)]
pub type BasicNodeWalker<'tree> = NodeWalker<'tree, context::basic::Context<'tree>>;

#[allow(missing_docs)]
pub type TraceNodeWalker<'tree> = NodeWalker<'tree, context::trace::Context<'tree>>;
