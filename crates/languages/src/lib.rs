//! Tree-sitter languages for the WebAssembly language server.

#![deny(clippy::all)]
#![deny(missing_docs)]
#![deny(unsafe_code)]

#[cfg(not(target_arch = "wasm32"))]
extern {
    #[allow(dead_code)]
    #[doc(hidden)]
    fn tree_sitter_dat() -> tree_sitter_sys::Language;

    #[allow(dead_code)]
    #[doc(hidden)]
    fn tree_sitter_dl() -> tree_sitter_sys::Language;
}

mod error;

/// Functions for creating [`tree-sitter::Language`].
pub mod language;

/// Functions for creating [`tree-sitter::Parser`].
pub mod parser;
