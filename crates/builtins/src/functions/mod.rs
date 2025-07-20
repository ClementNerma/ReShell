//!
//! This module contains all native functions and variables
//!
//! Each function has its own dedicated module.
//!

mod approx_int_div;
mod ask;
mod assert;
mod basename;
mod canonicalize;
mod cd;
mod cmd_arg;
mod cmd_flag;
mod ctime;
mod current_dir;
mod current_script_path;
mod datetime;
mod dbg;
mod dbg_type;
mod dir_exists;
mod echo;
mod env;
mod error;
mod exit;
mod extname;
mod file_exists;
mod file_size;
mod get_env;
mod glob;
mod human_size;
mod in_dir;
mod instant;
mod mkdir;
mod mtime;
mod parallel;
mod parent_dir;
mod path_exists;
mod pomsky;
mod progress_bar;
mod rand_int;
mod read_dir;
mod read_file;
mod regex;
mod rm;
mod set_env;
mod term_cols;
mod term_rows;
mod whereis;
mod which;
mod write_file;
mod write_to_file;

use crate::helpers::fns::InternalFunction;

/// List all native functions
pub fn native_functions() -> Vec<InternalFunction> {
    vec![
        // Collect function from individual modules
        self::approx_int_div::build_fn(),
        self::ask::build_fn(),
        self::assert::build_fn(),
        self::basename::build_fn(),
        self::canonicalize::build_fn(),
        self::cd::build_fn(),
        self::cmd_arg::build_fn(),
        self::cmd_flag::build_fn(),
        self::current_dir::build_fn(),
        self::current_script_path::build_fn(),
        self::ctime::build_fn(),
        self::datetime::build_fn(),
        self::dbg::build_fn(),
        self::dbg_type::build_fn(),
        self::dir_exists::build_fn(),
        self::echo::build_fn(),
        self::env::build_fn(),
        self::error::build_fn(),
        self::exit::build_fn(),
        self::extname::build_fn(),
        self::file_exists::build_fn(),
        self::file_size::build_fn(),
        self::get_env::build_fn(),
        self::glob::build_fn(),
        self::human_size::build_fn(),
        self::in_dir::build_fn(),
        self::instant::build_fn(),
        self::mkdir::build_fn(),
        self::mtime::build_fn(),
        self::parent_dir::build_fn(),
        self::path_exists::build_fn(),
        self::parallel::build_fn(),
        self::pomsky::build_fn(),
        self::progress_bar::build_fn(),
        self::rand_int::build_fn(),
        self::read_dir::build_fn(),
        self::read_file::build_fn(),
        self::regex::build_fn(),
        self::rm::build_fn(),
        self::set_env::build_fn(),
        self::term_cols::build_fn(),
        self::term_rows::build_fn(),
        self::whereis::build_fn(),
        self::which::build_fn(),
        self::write_file::build_fn(),
        self::write_to_file::build_fn(),
    ]
}
