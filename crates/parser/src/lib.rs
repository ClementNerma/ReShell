#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![forbid(unused_crate_dependencies)]

pub mod ast;
pub mod files_map;
mod impls;
mod parsers;

use self::ast::AstScopeId;
pub use self::parsers::{DELIMITER_CHARS, PROGRAM, ParserContext};

pub static NATIVE_LIB_AST_SCOPE_ID: AstScopeId = AstScopeId(0);
