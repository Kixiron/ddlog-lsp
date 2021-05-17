//! Functionality related to [`tree-sitter::Language`].

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub enum NodeMove {
    Init,
    Step,
}

/// Functions for working with the `.dat` grammar.
pub mod dat;

/// Functions for working with the `.dl` grammar.
pub mod dl;
