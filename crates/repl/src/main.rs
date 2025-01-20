#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

use std::{fs, path::PathBuf, process::ExitCode, time::Instant};

use clap::Parser as _;
use colored::Colorize;
use parsy::FileId;
use reshell_builtins::{
    builder::{build_native_lib_content, NativeLibParams},
    repl::on_dir_jump::trigger_directory_jump_event,
};
use reshell_parser::{
    ast::RuntimeCodeRange,
    files::{FilesMap, SourceFileLocation},
    program,
};
use reshell_runtime::{
    bin_resolver::BinariesResolver,
    conf::RuntimeConf,
    context::{Context, ContextCreationParams},
    errors::ExecResult,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use self::{
    args::Args,
    exec::run_script,
    paths::{HOME_DIR, INIT_SCRIPT_PATH, SHELL_CONFIG_DIR, SHELL_LOCAL_DATA_DIR},
    utils::ctrl_c::{setup_ctrl_c_handler, take_pending_ctrl_c_request},
};

mod args;
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

fn inner_main(started: Instant) -> Result<ExitCode, String> {
    let args = std::env::args_os().collect::<Vec<_>>();

    let (base_args, script_args) = match args.iter().position(|arg| arg == "--") {
        Some(pos) => (&args[0..pos], &args[pos + 1..]),
        None => (args.as_slice(), &[] as &[_]),
    };

    let Args {
        exec_file,
        eval,
        skip_init_script,
        timings,
        exec_args,
    } = Args::parse_from(base_args);

    // Set up Ctrl+C handler
    setup_ctrl_c_handler().map_err(|err| format!("Failed to setup Ctrl+C handler: {err}"))?;

    // Create shell's config directory
    if let Some(dir) = &*SHELL_CONFIG_DIR {
        if !dir.exists() {
            if let Err(err) = fs::create_dir_all(dir) {
                print_err(format!(
                    "Failed to create config directory for the shell at path '{}': {err}",
                    dir.display()
                ));
            }
        }
    }

    // Create shell's local data directory
    if let Some(dir) = &*SHELL_LOCAL_DATA_DIR {
        if !dir.exists() {
            if let Err(err) = fs::create_dir_all(dir) {
                print_err(format!(
                    "Failed to create local data directory for the shell at path '{}': {err}",
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

        let canon = dunce::canonicalize(&path)
            .map_err(|err| format!("failed to include file at path '{}': {err}", path.display()))?;

        let content = fs::read_to_string(&path).map_err(|err| match std::env::current_dir() {
            Err(_) => format!("failed to include file at path: '{}'", path.display()),
            Ok(curr_dir) => {
                format!(
                    "failed to include file at path '{}' from directory '{}': {err}",
                    path.display(),
                    curr_dir.display()
                )
            }
        })?;

        Ok((SourceFileLocation::RealFile(canon), content))
    }));

    match &*HOME_DIR {
        Some(home_dir) => {
            if !home_dir.is_dir() {
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

    let bin_resolver = BinariesResolver::new().unwrap_or_else(|err| {
        print_err(format!("{err}"));
        BinariesResolver::empty()
    });

    let mut ctx = Context::new(
        ContextCreationParams {
            // TODO: allow to configure through CLI
            runtime_conf: RuntimeConf::default(),
            files_map: files_map.clone(),
            take_ctrl_c_indicator: take_pending_ctrl_c_request,
            home_dir: HOME_DIR.clone(),
            on_dir_jump,
            script_args: script_args.to_vec(),
        },
        bin_resolver,
        build_native_lib_content(NativeLibParams {
            home_dir: HOME_DIR.clone(),
            script_args: script_args.to_vec(),
        }),
    );

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

        let result = run_script(
            &content,
            SourceFileLocation::RealFile(file_path),
            &parser,
            exec_args,
            &mut ctx,
        );

        if let Err(err) = result {
            if err.is_actual_error() {
                reports::print_error(&err, ctx.files_map());
            }

            return Ok(err
                .exit_code()
                .map(ExitCode::from)
                .unwrap_or(ExitCode::FAILURE));
        }

        return Ok(ExitCode::SUCCESS);
    }

    if let Some(input) = eval {
        return match run_script(
            &input,
            SourceFileLocation::CustomName("eval".to_owned()),
            &parser,
            exec_args,
            &mut ctx,
        ) {
            Ok(wandering_value) => {
                if let Some(loc_val) = wandering_value {
                    println!(
                        "{}",
                        loc_val
                            .value
                            .render_colored(&ctx, PrettyPrintOptions::multiline())
                    )
                }

                Ok(ExitCode::SUCCESS)
            }

            Err(err) => {
                if err.is_actual_error() {
                    reports::print_error(&err, ctx.files_map());
                }

                Ok(err
                    .exit_code()
                    .map(ExitCode::from)
                    .unwrap_or(ExitCode::FAILURE))
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
                                exec_args,
                                &mut ctx,
                            );

                            if let Err(err) = init_script_result {
                                if err.is_actual_error() {
                                    reports::print_error(&err, ctx.files_map());
                                } else if let Some(code) = err.exit_code() {
                                    return Ok(ExitCode::from(code));
                                }
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

    repl::start(ctx, parser, exec_args, timings, show_timings)
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

fn on_dir_jump(ctx: &mut Context, at: RuntimeCodeRange) -> ExecResult<()> {
    let current_dir = std::env::current_dir().map_err(|err| {
        ctx.error(
            at,
            format!("failed to get path to new current directory: {err}"),
        )
    })?;

    trigger_directory_jump_event(ctx, &current_dir)
}
