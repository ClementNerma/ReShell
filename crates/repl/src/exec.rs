use parsy::{FileId, Span};
use reshell_parser::{ast::Program, files_map::SourceFileLocation};
use reshell_reports::ReportableError;
use reshell_runtime::{
    context::Context,
    errors::{ExecError, ExecResult, ExecTopPropagation},
    exec::run_program,
    values::LocatedValue,
};

use crate::{args::ExecArgs, parse_program};

/// Run a ReShell script
pub fn run_script(
    input: &str,
    file_loc: SourceFileLocation,
    exec_args: ExecArgs,
    ctx: &mut Context,
) -> Result<ProgramResult, ReportableError> {
    let ExecArgs {
        print_ast,
        only_check,
    } = exec_args;

    let file_id = ctx.files_map().register_file(file_loc, input.to_string());

    let parsed =
        parse_program(input, FileId::SourceFile(file_id)).map_err(ReportableError::Parsing)?;

    if print_ast {
        println!("AST: {parsed:#?}");
    }

    if only_check {
        reshell_checker::check(
            &parsed.data,
            ctx.generate_checker_scopes(),
            &mut ctx.checker_output().clone(),
        )
        .map_err(ReportableError::Checking)?;

        println!("The provided program is valid (no error detected).");
        return Ok(ProgramResult::Success(None));
    }

    handle_ret_value(run_program(&parsed, ctx), Some(parsed))
}

pub fn handle_ret_value(
    ret_value: ExecResult<Option<LocatedValue>>,
    parsed: Option<Span<Program>>,
) -> Result<ProgramResult, ReportableError> {
    match ret_value {
        Ok(ret_val) => Ok(ProgramResult::Success(ret_val)),
        Err(err) => match err {
            ExecError::ActualError(err) => Err(ReportableError::Runtime(err, parsed)),
            ExecError::TopPropagation(err) => match err {
                ExecTopPropagation::SuccessfulExit => Ok(ProgramResult::GracefullyExit),
            },
            ExecError::InternalPropagation(_) => unreachable!(),
        },
    }
}

pub enum ProgramResult {
    Success(Option<LocatedValue>),
    GracefullyExit,
}
