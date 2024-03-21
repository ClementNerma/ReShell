#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![forbid(unused_crate_dependencies)]

pub mod ast;
pub mod files;
mod program;
mod utils;

pub use program::{delimiter_chars, program};
