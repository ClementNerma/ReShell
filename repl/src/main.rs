#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]
// TEMPORARY
#![allow(clippy::result_large_err)]

use std::fs;
use std::process::ExitCode;

use clap::Parser as _;
use colored::Colorize;
use parsy::Parser;
use reshell_parser::ast::Program;
use reshell_parser::program;
use reshell_runtime::files_map::ScopableFilePath;

use self::exec::{run_script, ExecOptions};
use self::state::RUNTIME_CONTEXT;

mod cmd;
mod completer;
mod edit_mode;
mod exec;
mod highlighter;
mod hinter;
mod history;
mod logic;
mod prompt;
mod repl;
mod reports;
mod state;
mod validator;

fn main() -> ExitCode {
    let args = cmd::Args::parse();

    match dirs::home_dir() {
        Some(home_dir) => {
            if home_dir.is_dir() {
                RUNTIME_CONTEXT.write().unwrap().set_home_dir(home_dir);
            } else {
                print_warn(&format!(
                    "Determined path to home directory was {} but it does not exist",
                    home_dir.to_string_lossy().bright_magenta()
                ));
            }
        }

        None => {
            print_warn("Failed to determine path to home directory");
        }
    }

    let eval_opts = ExecOptions {
        check_only: args.check_only,
        dbg_code_checking: args.dbg_code_checking,
    };

    let parser = program();

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

        let result = run_script(
            &content,
            ScopableFilePath::RealFile(file_path),
            &parser,
            eval_opts,
        );

        return match result {
            Ok(()) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        };
    }

    if let Some(input) = args.eval {
        let result = run_script(
            &input,
            ScopableFilePath::InMemory("eval"),
            &parser,
            eval_opts,
        );

        return match result {
            Ok(()) => ExitCode::SUCCESS,
            Err(_) => ExitCode::FAILURE,
        };
    }

    if !args.skip_init_script && run_init_script(&parser).is_err() {
        return ExitCode::FAILURE;
    }

    repl::start();

    ExitCode::SUCCESS
}

fn run_init_script(parser: &impl Parser<Program>) -> Result<(), ()> {
    let Some(home_dir) = dirs::home_dir() else {
        print_warn("Cannot run init script: failed to determine path to the user's home directory");

        return Ok(());
    };

    let init_file = home_dir.join(INIT_SCRIPT_FILE_NAME);

    if !init_file.exists() {
        //  print_warn(&format!(
        //     "Init script was not found at path {}",
        //     init_file.to_string_lossy().bright_magenta(),
        // ));
        return Ok(());
    }

    if !init_file.is_file() {
        print_err(&format!(
            "Init script path ({}) exists but is not a file",
            init_file.to_string_lossy().bright_magenta()
        ));

        return Ok(());
    }

    match fs::read_to_string(&init_file) {
        Ok(source) => run_script(
            &source,
            ScopableFilePath::RealFile(init_file),
            parser,
            ExecOptions::default(),
        )
        .map_err(|_| ()),

        Err(err) => {
            print_err(&format!(
                "Failed to read init script at path {}: {err}",
                init_file.to_string_lossy().bright_magenta()
            ));

            Err(())
        }
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
