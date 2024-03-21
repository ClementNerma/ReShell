use std::{
    process::ExitCode,
    sync::{Arc, Mutex},
    time::Instant,
};

use reedline::{Reedline, Signal};
use reshell_builtins::prompt::{render_prompt, LastCmdStatus, PromptRendering};
use reshell_runtime::{context::Context, errors::ExecErrorNature, files_map::ScopableFilePath};

use crate::{
    completer::{self, CompletionData},
    edit_mode,
    exec::run_script,
    highlighter, hinter, history,
    prompt::Prompt,
    reports::{self, ReportableError},
    validator, Timings,
};

pub fn start(ctx: &mut Context, timings: Timings, show_timings: bool) -> Option<ExitCode> {
    let completion_data = Arc::new(Mutex::new(CompletionData::generate_from_context(ctx)));

    let mut line_editor = Reedline::create()
        .with_history(history::create_history(ctx.conf()))
        .with_menu(history::create_history_menu())
        .with_highlighter(highlighter::create_highlighter())
        .with_hinter(hinter::create_hinter())
        .with_validator(validator::create_validator())
        .with_menu(completer::create_completion_menu())
        .with_completer(completer::create_completer(Arc::clone(&completion_data)))
        .with_edit_mode(edit_mode::create_edit_mode())
        .with_quick_completions(true)
        .with_partial_completions(true);

    let parser = reshell_parser::program();

    let mut counter = 0;

    let mut last_cmd_status = None;

    if show_timings {
        display_timings(timings, Instant::now());
    }

    loop {
        let line_start = Instant::now();

        let prompt_rendering = match render_prompt(ctx, last_cmd_status.take()) {
            Ok(prompt) => prompt.unwrap_or_default(),
            Err(err) => {
                reports::print_error(&ReportableError::Runtime(err), ctx.files_map());
                PromptRendering::default()
            }
        };

        let prompt = Prompt::new(prompt_rendering);

        completion_data.lock().unwrap().update_with(ctx);

        if show_timings {
            println!("* Time to interaction: {:?}", line_start.elapsed());
        }

        let input = match line_editor.read_line(&prompt) {
            Ok(Signal::Success(buffer)) => buffer,
            Ok(Signal::CtrlC) => continue,
            Ok(Signal::CtrlD) => break None,
            Err(err) => {
                eprintln!("> Failed to read line: {err}");
                continue;
            }
        };

        let start = Instant::now();

        counter += 1;

        let ret = run_script(
            &input,
            ScopableFilePath::InMemoryWithCounter("repl", counter),
            &parser,
            ctx,
        );

        match &ret {
            Ok(()) => todo!(),
            Err(err) => {
                if let ReportableError::Runtime(err) = &err {
                    if let ExecErrorNature::Exit { code } = err.nature {
                        return Some(code.map(ExitCode::from).unwrap_or(ExitCode::SUCCESS));
                    }
                }

                reports::print_error(err, ctx.files_map());
            }
        }

        last_cmd_status = Some(LastCmdStatus {
            success: ret.is_ok(),
            duration_ms: start.elapsed().as_millis(),
            exit_code: ret.err().and_then(|err| {
                err.exit_code().map(|code| {
                    code.unwrap_or(
                        // NOTE: fallback exit code
                        1,
                    )
                })
            }),
        });
    }
}

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
