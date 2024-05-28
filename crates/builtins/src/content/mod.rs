//!
//! This module contains all native functions and variables
//!
//! Each function has its own dedicated module.

mod append;
mod approx_int_div;
mod at;
mod basename;
mod captures;
mod cd;
mod chars;
mod clone;
mod contains;
mod contains_str;
mod current_dir;
mod current_script_path;
mod datetime;
mod dbg;
mod dbg_type;
mod dir_exists;
mod echo;
mod ends_with;
mod env;
mod error;
mod exit;
mod file_exists;
mod filename;
mod filter;
mod find_str;
mod fold;
mod for_each;
mod get;
mod glob;
mod human_size;
mod inspect;
mod join;
mod keys;
mod len;
mod lines;
mod ls;
mod make_map;
mod map;
mod matches;
mod mkdir;
mod parent_dir;
mod parse_json;
mod path_exists;
mod pop;
mod prepend;
mod range;
mod read_dir;
mod read_file;
mod reduce;
mod regex;
mod replace;
mod reverse;
mod rm;
mod runtime;
mod set_env;
mod slice;
mod sort;
mod split;
mod starts_with;
mod stringify;
mod struct_to_map;
mod substr;
mod succeeds;
mod take;
mod term_cols;
mod term_rows;
mod to_json;
mod to_lowercase;
mod to_uppercase;
mod transform;
mod trim;
mod values;
mod which;
mod write_file;

use std::path::MAIN_SEPARATOR;

use reshell_runtime::{compat::PATH_VAR_SEP, values::RuntimeValue};

use crate::{
    builder::{BuiltinVar, NativeLibDefinition, NativeLibParams},
    on_dir_jump::ON_DIR_JUMP_VAR_NAME,
    prompt::GEN_PROMPT_VAR_NAME,
};

/// Generate definitions of the native library
pub fn define_native_lib(params: NativeLibParams) -> NativeLibDefinition {
    let NativeLibParams { home_dir } = params;

    NativeLibDefinition {
        functions: vec![
            // Collect function from individual modules
            self::append::build_fn(),
            self::approx_int_div::build_fn(),
            self::at::build_fn(),
            self::basename::build_fn(),
            self::captures::build_fn(),
            self::cd::build_fn(),
            self::chars::build_fn(),
            self::clone::build_fn(),
            self::contains::build_fn(),
            self::contains_str::build_fn(),
            self::current_dir::build_fn(),
            self::current_script_path::build_fn(),
            self::datetime::build_fn(),
            self::dbg::build_fn(),
            self::dbg_type::build_fn(),
            self::dir_exists::build_fn(),
            self::echo::build_fn(),
            self::ends_with::build_fn(),
            self::env::build_fn(),
            self::error::build_fn(),
            self::exit::build_fn(),
            self::file_exists::build_fn(),
            self::filename::build_fn(),
            self::fold::build_fn(),
            self::for_each::build_fn(),
            self::get::build_fn(),
            self::glob::build_fn(),
            self::human_size::build_fn(),
            self::inspect::build_fn(),
            self::join::build_fn(),
            self::parse_json::build_fn(),
            self::len::build_fn(),
            self::lines::build_fn(),
            self::filter::build_fn(),
            self::ls::build_fn(),
            self::keys::build_fn(),
            self::values::build_fn(),
            self::make_map::build_fn(),
            self::map::build_fn(),
            self::matches::build_fn(),
            self::mkdir::build_fn(),
            self::parent_dir::build_fn(),
            self::path_exists::build_fn(),
            self::transform::build_fn(),
            self::prepend::build_fn(),
            self::pop::build_fn(),
            self::range::build_fn(),
            self::read_dir::build_fn(),
            self::read_file::build_fn(),
            self::reduce::build_fn(),
            self::regex::build_fn(),
            self::replace::build_fn(),
            self::reverse::build_fn(),
            self::rm::build_fn(),
            self::set_env::build_fn(),
            self::slice::build_fn(),
            self::sort::build_fn(),
            self::split::build_fn(),
            self::starts_with::build_fn(),
            self::stringify::build_fn(),
            self::find_str::build_fn(),
            self::struct_to_map::build_fn(),
            self::substr::build_fn(),
            self::succeeds::build_fn(),
            self::take::build_fn(),
            self::runtime::build_fn(),
            self::term_cols::build_fn(),
            self::term_rows::build_fn(),
            self::to_json::build_fn(),
            self::to_lowercase::build_fn(),
            self::to_uppercase::build_fn(),
            self::trim::build_fn(),
            self::which::build_fn(),
            self::write_file::build_fn(),
        ],

        vars: vec![
            // Prompt generation variable
            BuiltinVar {
                name: GEN_PROMPT_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
            },
            // Directory jump listener
            BuiltinVar {
                name: ON_DIR_JUMP_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
            },
            // Platform-specific PATH variable separator
            BuiltinVar {
                name: "PATH_VAR_SEP",
                is_mut: false,
                init_value: RuntimeValue::String(PATH_VAR_SEP.to_string()),
            },
            // Platform-specific path separator
            BuiltinVar {
                name: "PATH_SEP",
                is_mut: false,
                init_value: RuntimeValue::String(MAIN_SEPARATOR.to_string()),
            },
            // Path to the current user's home directory
            BuiltinVar {
                name: "HOME",
                is_mut: false,
                init_value: match home_dir.and_then(|dir| dir.to_str().map(str::to_owned)) {
                    Some(dir) => RuntimeValue::String(dir),
                    None => RuntimeValue::Null,
                },
            },
        ],
    }
}
