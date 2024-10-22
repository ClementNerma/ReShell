//!
//! ReShell's native library
//!
//! This crate contains utilities to generate the native library as well as
//! tools to render the REPL's prompt
#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]
// TODO: remove this line when trait upscating is not feature-gated anymore
#![feature(trait_upcasting)]

mod content;
mod functions;
mod helpers;
mod methods;
mod utils;

pub mod builder;
pub mod repl;
