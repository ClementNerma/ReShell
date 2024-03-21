#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]
// TEMPORARY
#![allow(clippy::result_large_err)]

use std::fs;
use std::process::ExitCode;

use clap::Parser as _;
use colored::Colorize;
use reshell_parser::program;
use reshell_runtime::{exec::ProgramExitStatus, files_map::ScopableFilePath};

use self::{exec::run_script, state::RUNTIME_CONTEXT};

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
    match inner_main() {
        Ok(code) => code,
        Err(err) => {
            print_err(err);
            ExitCode::FAILURE
        }
    }
}

fn inner_main() -> Result<ExitCode, &'static str> {
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

    let parser = program();

    if let Some(file_path) = args.exec_file {
        if !file_path.exists() {
            return Err("Error: provided file was not found");
        }

        if !file_path.is_file() {
            return Err("Error: provided file path is a directory");
        }

        let Ok(content) = fs::read_to_string(&file_path) else {
            return Err("Failed to read thep rovided path");
        };

        return match run_script(&content, ScopableFilePath::RealFile(file_path), &parser) {
            Ok(exit_status) => match exit_status {
                ProgramExitStatus::Normal => Ok(ExitCode::SUCCESS),
                ProgramExitStatus::ExitRequested { code } => Ok(ExitCode::from(code)),
            },
            Err(err) => {
                reports::print_error(&err, RUNTIME_CONTEXT.read().unwrap().files_map());
                Ok(ExitCode::FAILURE)
            }
        };
    }

    if let Some(input) = args.eval {
        return match run_script(&input, ScopableFilePath::InMemory("eval"), &parser) {
            Ok(exit_status) => match exit_status {
                ProgramExitStatus::Normal => Ok(ExitCode::SUCCESS),
                ProgramExitStatus::ExitRequested { code } => Ok(ExitCode::from(code)),
            },
            Err(err) => {
                reports::print_error(&err, RUNTIME_CONTEXT.read().unwrap().files_map());
                Ok(ExitCode::FAILURE)
            }
        };
    }

    if !args.skip_init_script {
        match dirs::home_dir() {
            None => print_warn(
                "Cannot run init script: failed to determine path to the user's home directory",
            ),

            Some(home_dir) => {
                let init_file = home_dir.join(INIT_SCRIPT_FILE_NAME);

                if init_file.is_file() {
                    match fs::read_to_string(&init_file) {
                        Err(err) => {
                            print_err(&format!(
                                "Failed to read init script at path {}: {err}",
                                init_file.to_string_lossy().bright_magenta()
                            ));

                            return Ok(ExitCode::FAILURE);
                        }

                        Ok(source) => {
                            let init_script_result =
                                run_script(&source, ScopableFilePath::RealFile(init_file), &parser);

                            match init_script_result {
                                Ok(instr_ret) => match instr_ret {
                                    ProgramExitStatus::Normal => {}
                                    ProgramExitStatus::ExitRequested { code } => {
                                        return Ok(ExitCode::from(code))
                                    }
                                },
                                Err(err) => reports::print_error(
                                    &err,
                                    RUNTIME_CONTEXT.read().unwrap().files_map(),
                                ),
                            }
                        }
                    }
                }
            }
        }
    }

    match repl::start() {
        Some(exit_code) => Ok(exit_code),
        None => Ok(ExitCode::SUCCESS),
    }
}

fn print_warn(msg: &str) {
    eprintln!("{}", msg.bright_yellow());
}

fn print_err(msg: &str) {
    eprintln!("{}", msg.bright_red());
}

static INIT_SCRIPT_FILE_NAME: &str = "init.rsh";
