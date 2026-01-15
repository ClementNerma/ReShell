#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]
// TODO: remove once stabilized (nightly)
#![feature(type_alias_impl_trait)]
#![feature(const_trait_impl)]

pub mod ast;
pub mod files_map;
mod impls;
mod parsers;

use self::ast::AstScopeId;
pub use self::parsers::{DELIMITER_CHARS, PROGRAM, ParserContext};

pub static NATIVE_LIB_AST_SCOPE_ID: AstScopeId = AstScopeId(0);
