use parsy::{Eaten, FileId, Parser};
use reshell_checker::CheckerOutput;
use reshell_parser::{ast::Program, files::SourceFileLocation};
use reshell_runtime::{context::Context, exec::run_program};

use crate::reports::ReportableError;

pub fn code_check_script(
    input: &str,
    loc: SourceFileLocation,
    parser: &impl Parser<Program>,
    ctx: &mut Context,
) -> Result<(Eaten<Program>, CheckerOutput), ReportableError> {
    let file_id = ctx.files_map().register_file(loc, input.to_string());

    let parsed = parser
        .parse_str_as_file(input, FileId::SourceFile(file_id))
        .map_err(ReportableError::Parsing)?;

    reshell_checker::check(
        &parsed.data,
        ctx.generate_checker_scopes(),
        Some(ctx.generate_checker_output()),
    )
    .map(|checker_out| (parsed, checker_out))
    .map_err(ReportableError::Checking)
}

pub fn run_script(
    input: &str,
    file_loc: SourceFileLocation,
    parser: &impl Parser<Program>,
    ctx: &mut Context,
) -> Result<(), ReportableError> {
    let (parsed, checker_output) = code_check_script(input, file_loc, parser, ctx)?;

    run_program(&parsed, checker_output, ctx)
        .map_err(|err| ReportableError::Runtime(err, Some(parsed)))
}
