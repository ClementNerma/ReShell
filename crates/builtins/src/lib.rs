#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]
// TODO: fix
#![allow(clippy::result_large_err)]

mod content;
mod helper;
mod type_handlers;
mod utils;

pub mod builder;
pub mod prompt;