#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![forbid(unused_crate_dependencies)]

pub mod ast;
mod program;

pub use program::program;
