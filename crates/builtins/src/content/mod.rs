//!
//! This module contains all native functions and variables
//!
//! Each function has its own dedicated module.

mod approx_int_div;
mod cd;
mod chars;
mod clone;
mod current_dir;
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
mod join;
mod len;
mod list_filter;
mod list_map;
mod ls;
mod map;
mod map_keys;
mod map_values;
mod mkdir;
mod path_exists;
mod pop;
mod print_runtime_stats;
mod range;
mod read_file;
mod replace;
mod reverse;
mod rm;
mod set_env;
mod size_of;
mod slice;
mod split;
mod struct_to_map;
mod take;
mod term_cols;
mod term_rows;
mod write_file;

use crate::builder::BuiltinVar;

use reshell_runtime::values::RuntimeValue;

use super::{builder::NativeLibDefinition, prompt::GEN_PROMPT_VAR_NAME};

/// Generate definitions of the native library
pub fn define_native_lib() -> NativeLibDefinition {
    NativeLibDefinition {
        functions: vec![
            // Collect function from individual modules
            self::cd::build_fn(),
            self::chars::build_fn(),
            self::clone::build_fn(),
            self::current_dir::build_fn(),
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
            self::join::build_fn(),
            self::len::build_fn(),
            self::list_filter::build_fn(),
            self::list_map::build_fn(),
            self::ls::build_fn(),
            self::map::build_fn(),
            self::map_keys::build_fn(),
            self::map_values::build_fn(),
            self::mkdir::build_fn(),
            self::path_exists::build_fn(),
            self::pop::build_fn(),
            self::approx_int_div::build_fn(),
            self::print_runtime_stats::build_fn(),
            self::range::build_fn(),
            self::read_file::build_fn(),
            self::replace::build_fn(),
            self::reverse::build_fn(),
            self::rm::build_fn(),
            self::set_env::build_fn(),
            self::size_of::build_fn(),
            self::slice::build_fn(),
            self::split::build_fn(),
            self::struct_to_map::build_fn(),
            self::take::build_fn(),
            self::term_cols::build_fn(),
            self::term_rows::build_fn(),
            self::write_file::build_fn(),
        ],

        vars: vec![
            // Prompt generation variable
            BuiltinVar {
                name: GEN_PROMPT_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
            },
        ],
    }
}
