use std::{
    error::Error,
    process::ExitCode,
    sync::{
        mpsc::{channel, TryRecvError},
        Arc, Mutex,
    },
    time::{Duration, Instant},
};

use colored::Colorize;
use parsy::Parser;
use reedline::{Reedline, Signal, Suggestion};
use reshell_builtins::prompt::{render_prompt, LastCmdStatus, PromptRendering};
use reshell_parser::{
    ast::{Instruction, Program},
    files::SourceFileLocation,
};
use reshell_runtime::{
    context::Context,
    errors::ExecErrorNature,
    pretty::{PrettyPrintOptions, PrettyPrintable},
};

use crate::{
    args::ExecArgs,
    completer::{self, generate_completions, CompletionContext},
    edit_mode,
    exec::run_script,
    highlighter, hinter, history,
    prompt::Prompt,
    reports::{self, ReportableError},
    utils::validator,
    Timings,
};

pub fn start(
    ctx: &mut Context,
    parser: impl Parser<Program>,
    exec_args: ExecArgs,
    timings: Timings,
    show_timings: bool,
) -> Result<Option<ExitCode>, Box<dyn Error>> {
    // These channels are used to receive completion requests from the completer (completer.rs)
    // And send the generated suggestions back
    let (sx_req, rx_req) = channel::<CompletionContext>();
    let (sx_res, rx_res) = channel::<Vec<Suggestion>>();

    // Create a line editor
    let line_editor = Reedline::create()
        .with_history(history::create_history(ctx.runtime_conf()))
        .with_menu(history::create_history_menu())
        .with_highlighter(highlighter::create_highlighter())
        .with_hinter(hinter::create_hinter())
        .with_validator(validator::create_validator())
        .with_menu(completer::create_completion_menu())
        .with_completer(completer::create_completer(move |data| {
            sx_req.send(data).unwrap();
            rx_res.recv().unwrap()
        }))
        .with_edit_mode(edit_mode::create_edit_mode())
        .with_quick_completions(true)
        .with_partial_completions(true);

    // Wrap the line editor in a sharable type
    let line_editor = Arc::new(Mutex::new(line_editor));

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
        let prompt_rendering = match render_prompt(ctx, last_cmd_status.take()) {
            Ok(prompt) => prompt.unwrap_or_default(),
            Err(err) => {
                if let ExecErrorNature::Exit { code } = err.nature {
                    return Ok(Some(code.map(ExitCode::from).unwrap_or(ExitCode::SUCCESS)));
                }

                reports::print_error(&ReportableError::Runtime(err, None), ctx.files_map());
                PromptRendering::default()
            }
        };

        let prompt = Prompt::new(prompt_rendering);

        if show_timings {
            println!("* Time to interaction: {:?}", line_start.elapsed());
        }

        // Spawn a line editor thread
        // This is required in order to listen to other events in paralle, such as completion requests
        let line_editor = Arc::clone(&line_editor);
        let child = std::thread::spawn(move || line_editor.lock().unwrap().read_line(&prompt));

        // Wait for the line editing to complete
        let signal = loop {
            // If the thread is done, get the result
            if child.is_finished() {
                break child.join().map_err(|_| "Line reading thread panicked!")?;
            }

            // Otherwise, check if a completion request has been sent
            match rx_req.try_recv() {
                Ok(completion_context) => {
                    // If yes, generate suggestions
                    // As you can see, we could not do that in the completer itself as we borrow the runtime context,
                    // which cannot be sent between threads
                    let suggestions = generate_completions(completion_context, ctx);

                    // Send the completion results back to the completer
                    sx_res.send(suggestions).map_err(|err| {
                        format!("Failed to send completions result to line reading thread: {err}")
                    })?;
                }

                Err(err) => match err {
                    TryRecvError::Empty => {}
                    TryRecvError::Disconnected => panic!("Completer's thread disconnected"),
                },
            };

            // Don't wait too much CPU while idleing
            yield_for_at_least(Duration::from_millis(10));
        };

        // Handle the return signal of the line editor
        let input = match signal {
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
            &parser,
            exec_args,
            ctx,
        );

        // Keep the last command status (used for prompt generation)
        last_cmd_status = Some(LastCmdStatus {
            success: ret.is_ok(),
            duration_ms: start.elapsed().as_millis(),
            exit_code: ret.as_ref().err().and_then(|err| err.exit_code()),
        });

        if show_timings {
            println!("* Command duration: {} ms", start.elapsed().as_millis());
        }

        match &ret {
            // If the program succeeded and has a wandering value, pretty-print it
            Ok(()) => {
                if let Some(value) = ctx.take_wandering_value() {
                    println!(
                        "{}",
                        value.render_colored(ctx, PrettyPrintOptions::multiline())
                    )
                }
            }

            // If the program failed, display the error
            Err(err) => {
                if let ReportableError::Runtime(err, program) = &err {
                    let program = program.as_ref().unwrap();

                    // Except in case of Exit request, which makes the REPL itself quit
                    if let ExecErrorNature::Exit { code } = err.nature {
                        return Ok(Some(code.map(ExitCode::from).unwrap_or(ExitCode::SUCCESS)));
                    }

                    let program_content = &program.data.content.data.instructions;

                    let is_single_cmd_call = program_content.len() == 1
                        && matches!(program_content[0].data, Instruction::CmdCall(_))
                        && err.at.parsed_range()
                            == Some(program.data.content.data.instructions[0].at);

                    match &err.nature {
                        ExecErrorNature::CommandFailedToStart { message } => {
                            // If we only run a single command (not more, no pipes, etc.) and it failed to start,
                            // display a simpler error.
                            if is_single_cmd_call {
                                eprintln!("{} {message}", "ERROR:".bright_red());
                                continue;
                            }
                        }

                        ExecErrorNature::CommandFailed {
                            message: _,
                            exit_status: _,
                        } => {
                            // If we only run a single command (not more, no pipes, etc.) and it failed to start,
                            // don't display any error
                            continue;
                        }

                        ExecErrorNature::ParsingErr(_)
                        | ExecErrorNature::CheckingErr(_)
                        | ExecErrorNature::Thrown { at: _, message: _ }
                        | ExecErrorNature::Exit { code: _ }
                        | ExecErrorNature::CtrlC
                        | ExecErrorNature::Custom(_) => {}
                    }
                }

                // In any other case, print the full error
                reports::print_error(err, ctx.files_map());
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
        "* Time to init script: {:.1?} ({:.1?})",
        before_init_script - started,
        before_init_script - started,
    );

    println!(
        "* Time to REPL       : {:.1?} ({:.1?})",
        before_repl - before_init_script,
        before_repl - started,
    );

    println!(
        "* Time to REPL ready : {:.1?} ({:.1?})",
        now - before_repl,
        now - started,
    );
}

/// Yield to the operating system for *at least* a provided duration
pub fn yield_for_at_least(at_least: Duration) {
    let started_waiting = Instant::now();

    std::thread::yield_now();

    let yielded_for = started_waiting.elapsed();

    if yielded_for < at_least {
        std::thread::sleep(at_least - yielded_for)
    }
}
