use std::time::Instant;

use parsy::{CodeRange, FileId, Location, Parser};
use reedline::{Reedline, Signal};
use reshell_parser::ast::Program;
use reshell_runtime::{
    context::{ScopeContent, ScopeRange},
    errors::{ExecErrorContent, ExecResult},
    exec::run_program_in_current_scope,
    files_map::ScopableFilePath,
    native_lib::{render_prompt, LastCmdStatus, PromptRendering},
};

use crate::{
    completer, edit_mode, highlighter, hinter, history,
    prompt::Prompt,
    reports::{self, ReportableError},
    state::{with_writable_rt_ctx, RUNTIME_CONTEXT},
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
                    &ReportableError::ExecError(err),
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

        with_writable_rt_ctx(|ctx| {
            counter += 1;

            let file_id = ctx.register_file(
                ScopableFilePath::InMemoryWithCounter("repl", counter),
                input.clone(),
            );

            ctx.create_and_push_scope(
                ScopeRange::CodeRange(CodeRange::new(
                    Location {
                        file_id: FileId::Id(file_id),
                        offset: 0,
                    },
                    input.len(),
                )),
                ScopeContent::new(),
                None,
            );
        });

        let start = Instant::now();

        let ret = eval(&input, &parser);

        last_cmd_status = Some(LastCmdStatus {
            success: ret.is_ok(),
            duration_ms: start.elapsed().as_millis(),
            exit_code: ret.as_ref().err().and_then(|err| match err {
                ReportableError::ParsingError(_) => None,
                ReportableError::ExecError(err) => match err.content {
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

        if let Err(err) = ret {
            reports::print_error(&err, RUNTIME_CONTEXT.read().unwrap().files_map());
        }
    }
}

fn repl_try_render_prompt(
    last_cmd_status: Option<LastCmdStatus>,
) -> ExecResult<Option<PromptRendering>> {
    render_prompt(&mut RUNTIME_CONTEXT.write().unwrap(), last_cmd_status)
}

pub fn eval(input: &str, parser: &impl Parser<Program>) -> Result<(), ReportableError> {
    let ctx = &mut RUNTIME_CONTEXT.write().unwrap();

    let parsed = parser
        .parse_str_as_file(
            input,
            FileId::Id(ctx.current_scope().source_file_id().unwrap()),
        )
        .map_err(ReportableError::ParsingError)?;

    run_program_in_current_scope(&parsed.data, ctx).map_err(ReportableError::ExecError)
}
