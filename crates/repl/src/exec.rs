use parsy::{FileId, Parser};
use reshell_parser::{ast::Program, files::SourceFileLocation};
use reshell_runtime::{context::Context, exec::run_program, values::LocatedValue};

use crate::{args::ExecArgs, reports::ReportableError};

/// Run a ReShell script
pub fn run_script(
    input: &str,
    file_loc: SourceFileLocation,
    parser: &impl Parser<Program>,
    exec_args: ExecArgs,
    ctx: &mut Context,
) -> Result<Option<LocatedValue>, ReportableError> {
    let ExecArgs {
        print_ast,
        only_check,
    } = exec_args;

    let file_id = ctx.files_map().register_file(file_loc, input.to_string());

    let parsed = parser
        .parse_str_with_file_id(input, FileId::SourceFile(file_id))
        .map_err(ReportableError::Parsing)?;

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
        return Ok(None);
    }

    run_program(&parsed, ctx).map_err(|err| ReportableError::Runtime(err, Some(parsed)))
}
