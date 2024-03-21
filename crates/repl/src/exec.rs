use parsy::{FileId, Parser};
use reshell_parser::{ast::Program, files::SourceFileLocation};
use reshell_runtime::{context::Context, exec::run_program};

use crate::{args::ExecArgs, reports::ReportableError};

pub fn run_script(
    input: &str,
    file_loc: SourceFileLocation,
    parser: &impl Parser<Program>,
    exec_args: ExecArgs,
    ctx: &mut Context,
) -> Result<(), ReportableError> {
    let ExecArgs {
        print_ast,
        print_checker_output,
        only_check,
    } = exec_args;

    let file_id = ctx.files_map().register_file(file_loc, input.to_string());

    let parsed = parser
        .parse_str_as_file(input, FileId::SourceFile(file_id))
        .map_err(ReportableError::Parsing)?;

    if print_ast {
        println!("AST: {parsed:#?}");
    }

    let checker_output = reshell_checker::check(
        &parsed.data,
        ctx.generate_checker_scopes(),
        Some(ctx.generate_checker_output()),
    )
    .map_err(ReportableError::Checking)?;

    if print_checker_output {
        println!("Checker output: {checker_output:#?}");
    }

    if only_check {
        return Ok(());
    }

    run_program(&parsed, checker_output, ctx)
        .map_err(|err| ReportableError::Runtime(err, Some(parsed)))
}
