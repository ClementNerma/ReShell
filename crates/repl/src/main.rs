#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]
// TEMPORARY
#![allow(clippy::result_large_err)]

use std::process::ExitCode;
use std::{fs, time::Instant};

use clap::Parser as _;
use colored::Colorize;
use reshell_parser::program;
use reshell_runtime::{
    conf::RuntimeConf, context::Context, exec::ProgramExitStatus, files_map::ScopableFilePath,
};

use self::cmd::Args;
use self::exec::run_script;

mod cmd;
mod completer;
mod edit_mode;
mod exec;
mod highlighter;
mod hinter;
mod history;
mod prompt;
mod repl;
mod reports;
mod syntax;
mod validator;

fn main() -> ExitCode {
    let now = Instant::now();

    match inner_main(now) {
        Ok(code) => code,
        Err(err) => {
            print_err(err);
            ExitCode::FAILURE
        }
    }
}

fn inner_main(started: Instant) -> Result<ExitCode, &'static str> {
    let Args {
        exec_file,
        eval,
        timings,
        skip_init_script,
    } = Args::parse();

    // TODO: allow to configure through CLI
    let mut ctx = Context::new(RuntimeConf::default());

    match dirs::home_dir() {
        Some(home_dir) => {
            if home_dir.is_dir() {
                ctx.set_home_dir(home_dir);
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

    if let Some(file_path) = exec_file {
        if !file_path.exists() {
            return Err("Error: provided file was not found");
        }

        if !file_path.is_file() {
            return Err("Error: provided file path is a directory");
        }

        let Ok(content) = fs::read_to_string(&file_path) else {
            return Err("Failed to read thep rovided path");
        };

        return match run_script(
            &content,
            ScopableFilePath::RealFile(file_path),
            &parser,
            &mut ctx,
        ) {
            Ok(exit_status) => match exit_status {
                ProgramExitStatus::Normal => Ok(ExitCode::SUCCESS),
                ProgramExitStatus::ExitRequested { code } => Ok(ExitCode::from(code)),
            },
            Err(err) => {
                reports::print_error(&err, ctx.files_map());
                Ok(ExitCode::FAILURE)
            }
        };
    }

    if let Some(input) = eval {
        return match run_script(
            &input,
            ScopableFilePath::InMemory("eval"),
            &parser,
            &mut ctx,
        ) {
            Ok(exit_status) => match exit_status {
                ProgramExitStatus::Normal => Ok(ExitCode::SUCCESS),
                ProgramExitStatus::ExitRequested { code } => Ok(ExitCode::from(code)),
            },
            Err(err) => {
                reports::print_error(&err, ctx.files_map());
                Ok(ExitCode::FAILURE)
            }
        };
    }

    let before_init_script = Instant::now();

    if !skip_init_script {
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
                            let init_script_result = run_script(
                                &source,
                                ScopableFilePath::RealFile(init_file),
                                &parser,
                                &mut ctx,
                            );

                            match init_script_result {
                                Ok(instr_ret) => match instr_ret {
                                    ProgramExitStatus::Normal => {}
                                    ProgramExitStatus::ExitRequested { code } => {
                                        return Ok(ExitCode::from(code))
                                    }
                                },
                                Err(err) => reports::print_error(&err, ctx.files_map()),
                            }
                        }
                    }
                }
            }
        }
    }

    let show_timings = timings;

    let timings = Timings {
        started,
        before_init_script,
        before_repl: Instant::now(),
    };

    match repl::start(&mut ctx, timings, show_timings) {
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

pub struct Timings {
    pub started: Instant,
    pub before_init_script: Instant,
    pub before_repl: Instant,
}
