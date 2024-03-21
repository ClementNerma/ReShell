#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![forbid(unused_crate_dependencies)]
// TODO: NIGHTLY
#![feature(lazy_cell)]

pub mod ast;
pub mod files;
mod impls;
mod program;
pub mod scope;

pub use program::{program, DELIMITER_CHARS};
