#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![forbid(unused_crate_dependencies)]

pub mod cmd;
pub mod context;
pub mod display;
pub mod errors;
pub mod exec;
pub mod expr;
pub mod files_map;
pub mod functions;
pub mod native_lib;
pub mod typechecker;
pub mod values;
