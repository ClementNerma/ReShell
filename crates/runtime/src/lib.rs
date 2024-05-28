#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]
// TEMPORARY
#![allow(clippy::result_large_err)]

pub mod builtins;
pub mod cmd;
pub mod conf;
pub mod context;
pub mod display;
pub mod errors;
pub mod exec;
pub mod expr;
pub mod files_map;
pub mod functions;
pub mod gc;
pub mod pretty;
pub mod props;
pub mod typechecker;
pub mod values;
