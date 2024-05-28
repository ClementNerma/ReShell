#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![forbid(unused_crate_dependencies)]

use clap::Parser;
use colored::Colorize;
use reshell_runtime::files_map::ScopableFilePath;
use state::{with_writable_rt_ctx, RUNTIME_CONTEXT};
use std::fs;

mod cmd;
mod completer;
mod editor;
mod highlighter;
mod highlighting;
mod hinter;
mod history;
mod prompt;
mod repl;
mod reports;
mod state;
mod validator;

fn main() {
    let args = cmd::Args::parse();

    match dirs::home_dir() {
        Some(home_dir) => {
            if home_dir.is_dir() {
            RUNTIME_CONTEXT.write().unwrap().set_home_dir(home_dir);
            } else {
                print_warn(&format!("Determined path to home directory was {} but it does not exist", home_dir.to_string_lossy().bright_magenta()));
            }
        }

        None => {
            print_warn("Failed to determine path to home directory");
        }
    }

    if let Some(file_path) = args.exec_file {
        if !file_path.exists() {
            fail("Error: provided file was not found");
        }

        if !file_path.is_file() {
            fail("Error: provided file path is a directory");
        }

        let Ok(content) = fs::read_to_string(&file_path) else {
            fail("Failed to read thep rovided path");
        };

        return run_script(ScopableFilePath::RealFile(file_path.clone()), &content);
    }

    if let Some(input) = args.eval {
        return run_script(ScopableFilePath::InMemory("input"), &input);
    }

    if !args.skip_init_script {
        run_init_script();
    }

    repl::start();
}

fn run_init_script() {
    let Some(home_dir) = dirs::home_dir() else {
        return print_warn(
            "Cannot run init script: failed to determine path to the user's home directory"
        );
    };

    let init_file = home_dir.join(INIT_SCRIPT_FILE_NAME);

    if !init_file.exists() {
        return print_warn(&format!(
            "Init script was not found at path {}",
            init_file.to_string_lossy().bright_magenta(),
        ));
    }

    if !init_file.is_file() {
        return print_err(&format!(
            "Init script path ({}) exists but is not a file",
            init_file.to_string_lossy().bright_magenta()
        ));
    }

    match fs::read_to_string(&init_file) {
        Ok(source) => run_script(ScopableFilePath::RealFile(init_file), &source),
        Err(err) => print_err(&format!(
            "Failed to read init script at path {}: {err}",
            init_file.to_string_lossy().bright_magenta()
        )),
    };
}

fn run_script(file: ScopableFilePath, content: &str) {
    with_writable_rt_ctx(|ctx| {
        ctx.push_file_scope(file, content.to_string());
    });

    if let Err(err) = repl::eval(content, &reshell_parser::program()) {
        reports::print_error_report(reports::repl_error_report(
            &err,
            &RUNTIME_CONTEXT.read().unwrap().files_map(),
        ));
        std::process::exit(1);
    }
}

fn print_warn(msg: &str) {
    eprintln!("{}", msg.bright_yellow());
}

fn print_err(msg: &str) {
    eprintln!("{}", msg.bright_red());
}

fn fail(msg: &str) -> ! {
    print_err(msg);
    std::process::exit(1);
}

static INIT_SCRIPT_FILE_NAME: &str = "init.rsh";
