use once_cell::sync::OnceCell;
use parsy::{Eaten, FileId, Parser};
use reshell_checker::{CheckerOutput, CheckerScope};
use reshell_parser::{ast::Program, files::SourceFileLocation};
use reshell_runtime::{context::Context, exec::run_program};

use crate::reports::ReportableError;

static NATIVE_LIB_FOR_CHECKER: OnceCell<CheckerScope> = OnceCell::new();

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

    let native_lib_for_checker =
        NATIVE_LIB_FOR_CHECKER.get_or_init(|| ctx.native_lib_scope_for_checker());

    reshell_checker::check(
        &parsed.data,
        native_lib_for_checker.clone(),
        ctx.first_scope_for_checker(),
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

    run_program(&parsed.data, checker_output, ctx)
        .map_err(|err| ReportableError::Runtime(err, Some(parsed)))
}
