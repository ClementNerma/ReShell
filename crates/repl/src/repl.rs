//!
//! REPL core program.
//!
//! Provide the REPL and connects it with different components.
//!
//! Indirectly handles completion, syntax highlighting, history, keyboard shortcuts, and more.
//!

use std::{
    error::Error,
    process::ExitCode,
    sync::{Arc, LazyLock, Mutex},
    time::Instant,
};

use colored::Colorize;
use reedline::{Reedline, Signal};
use reshell_builtins::repl::{
    completer::{CompletionStringSegment, generate_completions},
    prompt::{LastCmdStatus, PromptRendering, render_prompt},
};
use reshell_parser::{ast::Instruction, files_map::SourceFileLocation};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_reports::ReportableError;
use reshell_runtime::{
    context::Context,
    errors::{ExecActualErrorNature, ExecError, ExecTopPropagation},
};

use crate::{
    Timings,
    args::ExecArgs,
    completer::{self, ExternalCompletion, UnescapedSegment},
    edit_mode,
    exec::{ProgramResult, run_script},
    highlighter::{self},
    hinter, history,
    prompt::Prompt,
    utils::cmd_checker::COMMANDS_CHECKER,
    validator,
};

pub fn start(
    mut ctx: Context,
    exec_args: ExecArgs,
    timings: Timings,
    show_timings: bool,
) -> Result<Option<ExitCode>, Box<dyn Error>> {
    // Create a line editor
    let mut line_editor = Reedline::create()
        .with_history(history::create_history(ctx.runtime_conf()))
        .with_menu(history::create_history_menu())
        .with_highlighter(highlighter::create_highlighter())
        .with_hinter(hinter::create_hinter())
        .with_validator(validator::create_validator())
        .with_menu(completer::create_completion_menu())
        .with_completer(completer::create_completer(Some(Box::new(comp_gen))))
        .with_edit_mode(edit_mode::create_edit_mode())
        .with_quick_completions(true)
        .with_partial_completions(true);

    // Programs counter in REPL
    let mut counter = 0;

    // Status of the last command run in the REPL (if any)
    let mut last_cmd_status = None;

    // Display timings is asked to
    if show_timings {
        display_timings(timings, Instant::now());
    }

    // This is the REPL's actual loop
    loop {
        // Measure start time for performance
        let line_start = Instant::now();

        // Render prompt
        let prompt_rendering = match render_prompt(&mut ctx, last_cmd_status.take()) {
            Ok(prompt) => prompt.unwrap_or_default(),
            Err(err) => {
                let err = match err {
                    ExecError::ActualError(err) => err,

                    // TODO: forbid exiting from the prompt rendering function?
                    ExecError::TopPropagation(err) => match err {
                        ExecTopPropagation::SuccessfulExit => return Ok(Some(ExitCode::SUCCESS)),
                        ExecTopPropagation::FailureExit { code } => {
                            return Ok(Some(ExitCode::from(code.get())));
                        }
                    },

                    ExecError::InternalPropagation(_) => unreachable!(),
                };

                let err = ReportableError::Runtime(err);

                if let Some(code) = err.exit_code() {
                    return Ok(Some(ExitCode::from(code)));
                }

                reshell_reports::print_error(&err, ctx.files_map());
                PromptRendering::default()
            }
        };

        let prompt = Prompt::new(prompt_rendering);

        if show_timings {
            println!(
                "* REPL rendering took       : {:>3} ms",
                line_start.elapsed().as_millis()
            );
        }

        // Prepare line reading
        COMMANDS_CHECKER.lock().unwrap().refresh(&mut ctx);

        let prev = SHARED_CONTEXT.lock().unwrap().replace(ctx);
        assert!(prev.is_none());

        // Perform a line reading
        let line_reading_result = line_editor.read_line(&prompt);

        // Retake the context
        ctx = SHARED_CONTEXT.lock().unwrap().take().unwrap();

        // Handle the return signal of the line editor
        let input = match line_reading_result {
            Ok(Signal::Success(buffer)) => buffer,
            Ok(Signal::CtrlC) => continue,
            Ok(Signal::CtrlD) => break Ok(None),
            Err(err) => {
                eprintln!("> Failed to read line: {err}");
                continue;
            }
        };

        // Measure program start time for performance
        let start = Instant::now();

        counter += 1;

        // Run the input program
        let ret = run_script(
            &input,
            SourceFileLocation::CustomName(format!("repl[{counter}]")),
            exec_args,
            &mut ctx,
        );

        // Keep the last command status (used for prompt generation)
        last_cmd_status = Some(LastCmdStatus {
            success: ret.is_ok(),
            duration_ms: i64::try_from(start.elapsed().as_millis()).unwrap_or(i64::MAX),
            exit_code: ret.as_ref().err().and_then(|(err, _)| err.exit_code()),
        });

        if show_timings {
            println!(
                "\n*** Timings ***\n* Command duration took     : {:>3} ms",
                start.elapsed().as_millis()
            );
        }

        match &ret {
            // If the program succeeded and has a wandering value, pretty-print it
            Ok(result) => match result {
                ProgramResult::Success(located_value) => {
                    if let Some(located_value) = located_value {
                        println!(
                            "{}",
                            located_value
                                .value
                                .display(&ctx, PrettyPrintOptions::multiline())
                        );
                    }
                }

                ProgramResult::GracefullyExit => return Ok(Some(ExitCode::SUCCESS)),

                ProgramResult::ExitWithErrorCode(code) => {
                    return Ok(Some(ExitCode::from(code.get())));
                }
            },

            // If the program failed, display the error
            Err((err, program)) => {
                if let ReportableError::Runtime(err) = &err {
                    let program = program.as_ref().unwrap();

                    let program_content = &program.data.content.data.instructions;

                    let is_single_cmd_call = program_content.len() == 1
                        && matches!(program_content[0].data, Instruction::CmdCall(_))
                        && err.at.parsed_range()
                            == Some(program.data.content.data.instructions[0].at);

                    // If we only run a single command (not more, no pipes, etc.) and it failed to start or run,
                    // display a simpler error.
                    if is_single_cmd_call {
                        match &err.nature {
                            ExecActualErrorNature::CommandFailedToStart { message } => {
                                eprintln!("{}", format!("ERROR: {message}").bright_red());
                                continue;
                            }

                            ExecActualErrorNature::CommandFailed {
                                message: _,
                                exit_status: _,
                            } => {
                                continue;
                            }

                            ExecActualErrorNature::ParsingErr(_)
                            | ExecActualErrorNature::CheckingErr(_)
                            | ExecActualErrorNature::Thrown { at: _, message: _ }
                            | ExecActualErrorNature::CtrlC
                            | ExecActualErrorNature::Custom(_) => {}
                        }
                    }
                }

                // In any other case, print the full error
                reshell_reports::print_error(err, ctx.files_map())
            }
        }
    }
}

/// Display timings
fn display_timings(timings: Timings, now: Instant) {
    let Timings {
        started,
        before_init_script,
        before_repl,
    } = timings;

    println!("*** Timings ***");

    println!(
        "* Initialization took       : {:>3} ms",
        (before_init_script - started).as_millis(),
    );

    println!(
        "* Shell init script took    : {:>3} ms",
        (before_repl - before_init_script).as_millis(),
    );

    println!(
        "* REPL initialization took  : {:>3} ms",
        (now - before_repl).as_millis(),
    );
}

fn comp_gen(pieces: &[Vec<UnescapedSegment>], ctx: &mut Context) -> Vec<ExternalCompletion> {
    let pieces = pieces
        .iter()
        .map(|segments| {
            segments
                .iter()
                .map(|segment| match segment {
                    UnescapedSegment::VariableName(name) => {
                        CompletionStringSegment::VariableName(name.clone())
                    }
                    UnescapedSegment::String(string) => {
                        CompletionStringSegment::String(string.clone())
                    }
                })
                .collect()
        })
        .collect::<Vec<_>>();

    match generate_completions(&pieces, ctx) {
        Ok(None) => vec![],

        Ok(Some(completions)) => completions
            .into_iter()
            .map(ExternalCompletion::from)
            .collect(),

        Err(err) => {
            match err {
                ExecError::ActualError(err) => {
                    reshell_reports::print_error(&ReportableError::Runtime(err), ctx.files_map());

                    panic!();
                    // TODO: find a way to display error
                }

                ExecError::TopPropagation(err) => match err {
                    ExecTopPropagation::SuccessfulExit
                    | ExecTopPropagation::FailureExit { code: _ } => {
                        // TODO: report properly instead of panicking
                        panic!("Cannot exit from completions generation function");
                    }
                },

                ExecError::InternalPropagation(_) => unreachable!(),
            }
        }
    }
}

pub static SHARED_CONTEXT: LazyLock<Arc<Mutex<Option<Context>>>> =
    LazyLock::new(|| Arc::new(Mutex::new(None)));
