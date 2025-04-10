//!
//! ReShell's native library
//!
//! This crate contains utilities to generate the native library as well as
//! tools to render the REPL's prompt
#![deny(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod functions;
mod helpers;
mod methods;
mod types;
mod utils;
mod vars;

pub mod builder;
pub use crate::vars::repl;
