//!
//! This module contains all native functions and variables
//!
//! Each function has its own dedicated module.
//!

mod approx_int_div;
mod ask;
mod basename;
mod canonicalize;
mod cd;
mod cmd_args;
mod current_dir;
mod current_script_path;
mod dbg;
mod dbg_type;
mod dir_exists;
mod echo;
mod env;
mod error;
mod exit;
mod file_exists;
mod glob;
mod human_size;
mod inspect;
mod map;
mod mkdir;
mod parent_dir;
mod parse_json;
mod parse_toml;
mod path_exists;
mod range;
mod read_dir;
mod read_file;
mod rm;
mod run;
mod run_fn;
mod runtime;
mod set_env;
mod struct_to_map;
mod take;
mod term_cols;
mod term_rows;
mod to_json;
mod to_string;
mod transform;
mod typemakers;
mod values;
mod whereis;
mod which;
mod write_file;

pub(crate) use self::typemakers::types::*;

use crate::helper::InternalFunction;

/// List all native functions
pub fn native_functions() -> Vec<InternalFunction> {
    vec![
        // Collect function from individual modules
        self::approx_int_div::build_fn(),
        self::ask::build_fn(),
        self::basename::build_fn(),
        self::canonicalize::build_fn(),
        self::cd::build_fn(),
        self::cmd_args::build_fn(),
        self::current_dir::build_fn(),
        self::current_script_path::build_fn(),
        self::dbg::build_fn(),
        self::dbg_type::build_fn(),
        self::dir_exists::build_fn(),
        self::echo::build_fn(),
        self::env::build_fn(),
        self::error::build_fn(),
        self::exit::build_fn(),
        self::file_exists::build_fn(),
        self::glob::build_fn(),
        self::human_size::build_fn(),
        self::inspect::build_fn(),
        self::map::build_fn(),
        self::mkdir::build_fn(),
        self::parent_dir::build_fn(),
        self::parse_json::build_fn(),
        self::parse_toml::build_fn(),
        self::path_exists::build_fn(),
        self::range::build_fn(),
        self::read_dir::build_fn(),
        self::read_file::build_fn(),
        self::rm::build_fn(),
        self::run_fn::build_fn(),
        self::set_env::build_fn(),
        self::run::build_fn(),
        self::runtime::build_fn(),
        self::struct_to_map::build_fn(),
        self::take::build_fn(),
        self::term_cols::build_fn(),
        self::term_rows::build_fn(),
        self::to_json::build_fn(),
        self::to_string::build_fn(),
        self::transform::build_fn(),
        self::values::build_fn(),
        self::whereis::build_fn(),
        self::which::build_fn(),
        self::write_file::build_fn(),
    ]
    .into_iter()
    .chain(
        // Add typemaking functions
        self::typemakers::native_typemaking_functions(),
    )
    .collect()
}
