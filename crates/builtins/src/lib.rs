//!
//! ReShell's native library
//!
//! This crate contains utilities to generate the native library as well as
//! tools to render the REPL's prompt

// TODO: Restore this forbid() once time has a sound way to get local offset
// #![forbid(unsafe_code)]

#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod content;
mod errors;
mod helper;
mod type_handlers;
mod utils;

pub mod builder;
pub mod on_dir_jump;
pub mod prompt;
