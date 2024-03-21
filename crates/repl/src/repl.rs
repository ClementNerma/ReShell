use std::{
    process::ExitCode,
    sync::{Arc, Mutex},
    time::Instant,
};

use reedline::{Reedline, Signal};
use reshell_runtime::{
    context::Context,
    errors::ExecErrorContent,
    exec::ProgramExitStatus,
    files_map::ScopableFilePath,
    native_lib::{render_prompt, LastCmdStatus, PromptRendering},
    pretty::{PrettyPrintOptions, PrettyPrintable},
};

use crate::{
    completer::{self, CompletionData, CompletionDataScope},
    edit_mode,
    exec::run_script,
    highlighter, hinter, history,
    prompt::Prompt,
    reports::{self, ReportableError},
    validator,
};

pub fn start(ctx: &mut Context) -> Option<ExitCode> {
    let completion_data = Arc::new(Mutex::new(CompletionData::new()));

    let mut line_editor = Reedline::create()
        .with_history(history::create_history())
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

    loop {
        let prompt_rendering = match render_prompt(ctx, last_cmd_status.take()) {
            Ok(prompt) => prompt.unwrap_or_default(),
            Err(err) => {
                reports::print_error(&ReportableError::Runtime(err), ctx.files_map());
                PromptRendering::default()
            }
        };

        let prompt = Prompt::new(prompt_rendering);

        completion_data.lock().unwrap().replace_with(
            ctx.visible_scopes()
                .map(|scope| CompletionDataScope {
                    fns: scope
                        .fns
                        .iter()
                        .map(|(name, func)| {
                            (
                                name.clone(),
                                func.value
                                    .signature
                                    .render_colored(ctx, PrettyPrintOptions::inline()),
                            )
                        })
                        .collect(),

                    vars: scope
                        .vars
                        .iter()
                        .map(|(name, var)| {
                            (
                                name.clone(),
                                var.value.read().as_ref().map(|loc_val| {
                                    loc_val
                                        .value
                                        .render_colored(ctx, PrettyPrintOptions::inline())
                                }),
                            )
                        })
                        .collect(),
                })
                .collect(),
        );

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

        match ret {
            Ok(exit_status) => match exit_status {
                ProgramExitStatus::Normal => {}
                ProgramExitStatus::ExitRequested { code } => {
                    return Some(ExitCode::from(code));
                }
            },

            Err(ref err) => {
                reports::print_error(err, ctx.files_map());
            }
        }

        last_cmd_status = Some(LastCmdStatus {
            success: ret.is_ok(),
            duration_ms: start.elapsed().as_millis(),
            exit_code: ret.as_ref().err().and_then(|err| match err {
                ReportableError::Parsing(_) => None,
                ReportableError::Checking(_) => None,
                ReportableError::Runtime(err) => match err.content {
                    ExecErrorContent::Str(_) => None,
                    ExecErrorContent::String(_) => None,
                    ExecErrorContent::ParsingErr(_) => None,
                    ExecErrorContent::CommandFailed {
                        message: _,
                        exit_status,
                    } => exit_status,
                },
            }),
        });
    }
}
