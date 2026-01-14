//!
//! ReShell's native library
//!
//! This crate contains utilities to generate the native library as well as
//! tools to render the REPL's prompt
#![deny(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod builder;
mod functions;
mod helpers;
mod methods;
mod types;
mod utils;
mod vars;

pub use self::{
    builder::*,
    helpers::{
        args::{TypedValueEncoder, TypedValueParser},
        fns::InternalFunction,
        types as type_helpers,
    },
    types::*,
    vars::repl,
};
