#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

use std::{any::Any, fs, path::PathBuf, process::ExitCode, sync::LazyLock, time::Instant};

use clap::Parser as _;
use colored::Colorize;
use parsy::{FileId, Parser, ParserInput, ParserResult};
use reshell_builtins::{
    NativeLibParams, build_native_lib_content, repl::on_dir_jump::trigger_directory_jump_event,
};
use reshell_parser::{
    PROGRAM, ParserContext,
    ast::{Program, RuntimeCodeRange},
    files_map::{FilesMap, SourceFileLocation},
};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::{
    bin_resolver::BinariesResolver,
    conf::{HistoryConf, RuntimeConf},
    context::{Context, ContextCreationParams},
    errors::ExecResult,
};

use self::{
    args::{Args, RuntimeConfArgs},
    exec::{ProgramResult, run_script},
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
        runtime_conf_args,
    } = Args::parse_from(base_args);

    // Set up Ctrl+C handler
    setup_ctrl_c_handler().map_err(|err| format!("Failed to setup Ctrl+C handler: {err}"))?;

    // Create shell's config directory
    if let Some(dir) = &*SHELL_CONFIG_DIR
        && !dir.exists()
        && let Err(err) = fs::create_dir_all(dir)
    {
        print_err(format!(
            "Failed to create config directory for the shell at path '{}': {err}",
            dir.display()
        ));
    }

    // Create shell's local data directory
    if let Some(dir) = &*SHELL_LOCAL_DATA_DIR
        && !dir.exists()
        && let Err(err) = fs::create_dir_all(dir)
    {
        print_err(format!(
            "Failed to create local data directory for the shell at path '{}': {err}",
            dir.display()
        ));
    }

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
            runtime_conf: convert_runtime_conf(runtime_conf_args),
            files_map: FILES_MAP.clone(),
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
            exec_args,
            &mut ctx,
        );

        return Ok(match result {
            Ok(ProgramResult::GracefullyExit | ProgramResult::Success(_)) => ExitCode::SUCCESS,
            Err(err) => {
                reshell_reports::print_error(&err, ctx.files_map());
                err.exit_code().map_or(ExitCode::FAILURE, ExitCode::from)
            }
        });
    }

    if let Some(input) = eval {
        return match run_script(
            &input,
            SourceFileLocation::CustomName("eval".to_owned()),
            exec_args,
            &mut ctx,
        ) {
            Ok(result) => {
                match result {
                    ProgramResult::Success(wandering_value) => {
                        if let Some(wandering_value) = wandering_value {
                            println!(
                                "{}",
                                wandering_value
                                    .value
                                    .display(&ctx, PrettyPrintOptions::multiline())
                            )
                        }
                    }

                    ProgramResult::GracefullyExit => todo!(),
                }

                Ok(ExitCode::SUCCESS)
            }

            Err(err) => {
                reshell_reports::print_error(&err, ctx.files_map());

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
                                exec_args,
                                &mut ctx,
                            );

                            match init_script_result {
                                Ok(ProgramResult::Success(_)) => {}

                                // TODO: forbid exiting from init script?
                                Ok(ProgramResult::GracefullyExit) => {
                                    return Ok(ExitCode::SUCCESS);
                                }

                                Err(err) => {
                                    reshell_reports::print_error(&err, ctx.files_map());
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

    repl::start(ctx, exec_args, timings, show_timings)
        .map(|code| code.unwrap_or(ExitCode::SUCCESS))
        .map_err(|err| format!("REPL crashed: {err:?}"))
}

// It would be nice to use a builder pattern akin to the `bon` crate
// But it would be pretty heavy, so for now we're using this pretty ugly code
fn convert_runtime_conf(args: RuntimeConfArgs) -> RuntimeConf {
    let RuntimeConfArgs {
        call_stack_limit,
        disable_history,
        custom_history_path,
    } = args;

    let mut runtime_conf = RuntimeConf::default();

    if let Some(call_stack_limit) = call_stack_limit {
        runtime_conf.call_stack_limit = call_stack_limit;
    }

    if disable_history {
        runtime_conf.history = HistoryConf {
            enabled: false,
            custom_location: None,
        };
    } else if let Some(custom_history_path) = custom_history_path {
        runtime_conf.history = HistoryConf {
            enabled: true,
            custom_location: Some(custom_history_path),
        };
    }

    runtime_conf
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
        ctx.hard_error(
            at,
            format!("failed to get path to new current directory: {err}"),
        )
    })?;

    trigger_directory_jump_event(ctx, &current_dir)
}

fn parse_program(source: &str, file_id: FileId) -> ParserResult<Program> {
    PROGRAM.parse(&mut ParserInput::new_with_ctx(
        source,
        file_id,
        get_parser_ctx,
    ))
}

fn get_parser_ctx() -> Box<dyn Any> {
    Box::new(ParserContext {
        load_file: Box::new(|path, relative| FILES_MAP.load_file(&path, relative)),
    })
}

static FILES_MAP: LazyLock<FilesMap> = LazyLock::new(|| {
    FilesMap::new(Box::new(|path, relative_to, files_map| {
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
    }))
});
