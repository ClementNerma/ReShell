//!
//! This module contains all native functions and variables
//!
//! Each function has its own dedicated module.
//!

use crate::functions_set;

functions_set! {
    // List all native functions
    fn native_functions => {
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
        mod duration;
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
        mod max;
        mod min;
        mod mkdir;
        mod mtime;
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
    }
}
