#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod content;
mod errors;
mod helper;
mod type_handlers;
mod utils;

pub mod builder;
pub mod prompt;
