#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

pub mod bin_resolver;
pub mod cmd;
pub mod compat;
pub mod conf;
pub mod context;
pub mod display;
pub mod errors;
pub mod exec;
pub mod expr;
pub mod functions;
pub mod gc;
pub mod pretty;
pub mod props;
pub mod size;
pub mod typechecker;
pub mod values;
