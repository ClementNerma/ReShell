use std::time::Instant;

use reedline::{Reedline, Signal};
use reshell_runtime::{
    errors::{ExecErrorContent, ExecResult},
    files_map::ScopableFilePath,
    native_lib::{render_prompt, LastCmdStatus, PromptRendering},
};

use crate::{
    completer, edit_mode,
    exec::run_script,
    highlighter, hinter, history,
    prompt::Prompt,
    reports::{self, ReportableError},
    state::RUNTIME_CONTEXT,
    validator,
};

pub fn start() {
    let mut line_editor = Reedline::create()
        .with_history(history::create_history())
        .with_menu(history::create_history_menu())
        .with_highlighter(highlighter::create_highlighter())
        .with_hinter(hinter::create_hinter())
        .with_validator(validator::create_validator())
        .with_menu(completer::create_completion_menu())
        .with_completer(completer::create_completer())
        .with_edit_mode(edit_mode::create_edit_mode())
        .with_quick_completions(true)
        .with_partial_completions(true);

    let parser = reshell_parser::program();

    let mut counter = 0;

    let mut last_cmd_status = None;

    loop {
        let prompt_rendering = match repl_try_render_prompt(last_cmd_status.take()) {
            Ok(prompt) => prompt.unwrap_or_default(),
            Err(err) => {
                reports::print_error(
                    &ReportableError::Runtime(err),
                    RUNTIME_CONTEXT.read().unwrap().files_map(),
                );
                PromptRendering::default()
            }
        };

        let prompt = Prompt::new(prompt_rendering);

        let input = match line_editor.read_line(&prompt) {
            Ok(Signal::Success(buffer)) => buffer,
            Ok(Signal::CtrlC) => continue,
            Ok(Signal::CtrlD) => break,
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
        );

        if let Err(err) = &ret {
            reports::print_error(err, RUNTIME_CONTEXT.read().unwrap().files_map());
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

fn repl_try_render_prompt(
    last_cmd_status: Option<LastCmdStatus>,
) -> ExecResult<Option<PromptRendering>> {
    render_prompt(&mut RUNTIME_CONTEXT.write().unwrap(), last_cmd_status)
}
