#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

use std::path::PathBuf;
use std::process::ExitCode;
use std::{fs, time::Instant};

use clap::Parser as _;
use colored::Colorize;
use parsy::FileId;
use reshell_builtins::builder::build_native_lib_content;
use reshell_parser::files::{FilesMap, SourceFileLocation};
use reshell_parser::program;
use reshell_runtime::errors::ExecErrorNature;
use reshell_runtime::{conf::RuntimeConf, context::Context};

use self::cmd::Args;
use self::exec::run_script;
use self::paths::{HOME_DIR, INIT_SCRIPT_PATH, SHELL_DATA_DIR};
use self::reports::ReportableError;
use self::utils::threads::{setup_ctrl_c_handler, take_pending_ctrl_c_request};

mod cmd;
mod compat;
mod completer;
mod edit_mode;
mod exec;
mod highlighter;
mod hinter;
mod history;
mod paths;
mod prompt;
mod repl;
mod reports;
mod utils;

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

fn inner_main(started: Instant) -> Result<ExitCode, String> {
    let Args {
        exec_file,
        eval,
        timings,
        skip_init_script,
    } = Args::parse();

    // Set up Ctrl+C handler
    setup_ctrl_c_handler().map_err(|err| format!("Failed to setup Ctrl+C handler: {err}"))?;

    // Create shell's data directory
    if let Some(dir) = &*SHELL_DATA_DIR {
        if !dir.exists() {
            if let Err(err) = fs::create_dir_all(dir) {
                print_err(format!(
                    "Failed to create data directory for the shell at path '{}': {err}",
                    dir.display()
                ));
            }
        }
    }

    // Create a files map
    let files_map = FilesMap::new(Box::new(|path, relative_to, files_map| {
        let mut path = PathBuf::from(path);

        if !path.is_absolute() {
            match relative_to {
                FileId::None | FileId::Internal | FileId::Custom(_) => {}
                FileId::SourceFile(id) => match files_map.get_file(id).unwrap().location {
                    SourceFileLocation::CustomName(_) => {}
                    SourceFileLocation::RealFile(relative_to) => {
                        path = relative_to.parent().unwrap().join(path);
                    }
                },
            }
        };

        let content = std::fs::read_to_string(&path).map_err(|err| format!("{err}"))?;

        Ok((SourceFileLocation::RealFile(path), content))
    }));

    let mut ctx = Context::new(
        // TODO: allow to configure through CLI
        RuntimeConf::default(),
        files_map.clone(),
        build_native_lib_content(),
        take_pending_ctrl_c_request,
    );

    match &*HOME_DIR {
        Some(home_dir) => {
            if home_dir.is_dir() {
                ctx.set_home_dir(home_dir.clone());
            } else {
                print_warn(format!(
                    "Determined path to home directory was {} but it does not exist",
                    home_dir.to_string_lossy().bright_magenta()
                ));
            }
        }

        None => {
            print_warn("Failed to determine path to home directory");
        }
    }

    let parser = program(move |path, relative| files_map.load_file(&path, relative));

    if let Some(file_path) = exec_file {
        if !file_path.exists() {
            return Err("Error: provided file was not found".to_owned());
        }

        if !file_path.is_file() {
            return Err("Error: provided file path is a directory".to_owned());
        }

        let Ok(content) = fs::read_to_string(&file_path) else {
            return Err("Failed to read thep rovided path".to_owned());
        };

        return match run_script(
            &content,
            SourceFileLocation::RealFile(file_path),
            &parser,
            &mut ctx,
        ) {
            Ok(()) => Ok(ExitCode::SUCCESS),
            Err(err) => {
                reports::print_error(&err, ctx.files_map());
                Ok(loose_exit_code(err.exit_code()))
            }
        };
    }

    if let Some(input) = eval {
        return match run_script(
            &input,
            SourceFileLocation::CustomName("eval".to_owned()),
            &parser,
            &mut ctx,
        ) {
            Ok(()) => Ok(ExitCode::SUCCESS),
            Err(err) => {
                reports::print_error(&err, ctx.files_map());
                Ok(loose_exit_code(err.exit_code()))
            }
        };
    }

    let before_init_script = Instant::now();

    if !skip_init_script {
        match &*INIT_SCRIPT_PATH {
            None => print_warn(
                "Cannot run init script: failed to determine path to the user's home directory",
            ),

            Some(init_file) => {
                if init_file.is_file() {
                    match fs::read_to_string(init_file) {
                        Err(err) => {
                            print_err(format!(
                                "Failed to read init script at path {}: {err}",
                                init_file.to_string_lossy().bright_magenta()
                            ));

                            return Ok(ExitCode::FAILURE);
                        }

                        Ok(source) => {
                            let init_script_result = run_script(
                                &source,
                                SourceFileLocation::RealFile(init_file.clone()),
                                &parser,
                                &mut ctx,
                            );

                            if let Err(err) = init_script_result {
                                if let ReportableError::Runtime(err, _) = &err {
                                    if let ExecErrorNature::Exit { code } = err.nature {
                                        return Ok(loose_exit_code(code.map(i32::from)));
                                    }
                                }

                                reports::print_error(&err, ctx.files_map());
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

    repl::start(&mut ctx, timings, show_timings)
        .map(|code| code.unwrap_or(ExitCode::SUCCESS))
        .map_err(|err| format!("REPL crashed: {err:?}"))
}

fn print_warn(msg: impl AsRef<str>) {
    eprintln!("{}", msg.as_ref().bright_yellow());
}

fn print_err(msg: impl AsRef<str>) {
    eprintln!("{}", msg.as_ref().bright_red());
}

pub struct Timings {
    pub started: Instant,
    pub before_init_script: Instant,
    pub before_repl: Instant,
}

pub fn loose_exit_code(code: Option<i32>) -> ExitCode {
    match code {
        Some(code) => match u8::try_from(code) {
            Ok(code) => ExitCode::from(code),
            Err(_) => ExitCode::FAILURE,
        },
        None => ExitCode::FAILURE,
    }
}
