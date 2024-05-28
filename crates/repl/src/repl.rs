use std::{
    error::Error,
    process::ExitCode,
    sync::{
        mpsc::{channel, TryRecvError},
        Arc, Mutex,
    },
    time::Instant,
};

use colored::Colorize;
use reedline::{Reedline, Signal, Suggestion};
use reshell_builtins::prompt::{render_prompt, LastCmdStatus, PromptRendering};
use reshell_parser::files::SourceFileLocation;
use reshell_runtime::{
    context::Context,
    errors::ExecErrorNature,
    pretty::{PrettyPrintOptions, PrettyPrintable},
};

use crate::{
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
    timings: Timings,
    show_timings: bool,
) -> Result<Option<ExitCode>, Box<dyn Error>> {
    // These channels are used to receive completion requests from the completer (completer.rs)
    // And send the generated suggestions back
    let (sx_req, rx_req) = channel::<CompletionContext>();
    let (sx_res, rx_res) = channel::<Vec<Suggestion>>();

    // Create a line editor
    let line_editor = Reedline::create()
        .with_history(history::create_history(ctx.conf()))
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

    // Prepare a program parser with access to the shared files map
    let files_map = ctx.files_map().clone();

    let parser =
        reshell_parser::program(move |path, relative_to| files_map.load_file(&path, relative_to));

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
            // If the thread is done, get with the result
            if child.is_finished() {
                break child
                    .join()
                    .map_err(|err| format!("Line reading thread panicked: {err:?}"))?;
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
                    TryRecvError::Disconnected => panic!(),
                },
            };
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
            ctx,
        );

        // Keep the last command status (used for prompt generation)
        last_cmd_status = Some(LastCmdStatus {
            success: ret.is_ok(),
            duration_ms: start.elapsed().as_millis(),
            exit_code: ret.as_ref().err().and_then(|err| err.exit_code()),
        });

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

                    // If we only run a single command (not more, no pipes, etc.) and it failed,
                    // display a simpler error.
                    if let ExecErrorNature::CommandFailed {
                        message,
                        exit_status: _,
                    } = &err.nature
                    {
                        if program.data.content.data.instructions.len() == 1 {
                            eprintln!("{} {message}", "ERROR:".bright_red());
                            continue;
                        }
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
