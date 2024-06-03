#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![forbid(unused_crate_dependencies)]

pub mod ast;
pub mod files;
mod impls;
mod parser;
pub mod scope;

pub use parser::{program, DELIMITER_CHARS};
