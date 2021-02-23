//! Parsers for the DDlog language server.

#![deny(clippy::all)]
#![deny(missing_docs)]
#![deny(unsafe_code)]
#![recursion_limit = "512"]

/// Functionality related to runtime errors.
pub mod error;

/// Functionality related to [`tree-sitter::Language`].
pub mod language;

/// Functionality related to [`tree-sitter::Node`].
pub mod node;

/// Functionality related to [`tree-sitter::Parser`].
pub mod parser;

// The external C-based parsers generated by ddlog-lsp/tree-sitter-ddlog.
#[cfg(not(target_arch = "wasm32"))]
extern {
    #[allow(dead_code)]
    #[doc(hidden)]
    fn tree_sitter_ddlog_dat() -> tree_sitter_sys::Language;

    #[allow(dead_code)]
    #[doc(hidden)]
    fn tree_sitter_ddlog_dl() -> tree_sitter_sys::Language;
}