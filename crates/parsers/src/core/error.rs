//! Core definitions related to runtime errors.

use std::path::PathBuf;
use thiserror::Error;

/// Runtime errors for the DDlog parsers.
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Error)]
pub enum Error {
    /// Error that occurs when parsing an invalid language-id string.
    #[error("InvalidLanguageId: {0}")]
    InvalidLanguageId(String),
    /// Error that occurs when `OsStr::to_str()` returns `None`.
    #[error("OsStrToStrFailed")]
    OsStrToStrFailed,
    /// Error that occurs when `Path::extension()` returns `None`.
    #[error("PathExtensionFailed: {0}")]
    PathExtensionFailed(PathBuf),
    /// Error that occurs when attempting to set an invalid language for a tree-sitter parser.
    #[error("tree_sitter::LanguageError: {0}")]
    TreeSitterLanguageError(tree_sitter::LanguageError),
}
