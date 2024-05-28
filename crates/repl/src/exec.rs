// TODO: if exec returns an error, reset the current scope's ID to the one just after the native lib's one
// TODO: if exec returns an error instead of a popped scope, get the current scope from the ID mentioned above

use once_cell::sync::OnceCell;
use parsy::{Eaten, FileId, Parser};
use reshell_checker::{CheckerOutput, CheckerScope};
use reshell_parser::ast::Program;
use reshell_runtime::{
    exec::run_program, files_map::ScopableFilePath, native_lib::generate_native_lib,
};

use crate::{reports::ReportableError, state::RUNTIME_CONTEXT};

static NATIVE_LIB_FOR_CHECKER: OnceCell<CheckerScope> = OnceCell::new();

pub fn code_check_script(
    input: &str,
    file_path: ScopableFilePath,
    parser: &impl Parser<Program>,
) -> Result<(Eaten<Program>, CheckerOutput), ReportableError> {
    let ctx = &mut RUNTIME_CONTEXT.write().unwrap();

    let file_id = ctx.register_file(file_path, input.to_string());

    let parsed = parser
        .parse_str_as_file(input, FileId::SourceFile(file_id))
        .map_err(ReportableError::Parsing)?;

    let native_lib_for_checker =
        NATIVE_LIB_FOR_CHECKER.get_or_init(|| generate_native_lib().to_checker_scope(ctx));

    reshell_checker::check(
        &parsed.data,
        native_lib_for_checker.clone(),
        ctx.first_scope().to_checker_scope(ctx),
    )
    .map(|checker_out| (parsed, checker_out))
    .map_err(ReportableError::Checking)
}

pub fn run_script(
    input: &str,
    file_path: ScopableFilePath,
    parser: &impl Parser<Program>,
) -> Result<(), ReportableError> {
    let (parsed, checker_output) = code_check_script(input, file_path, parser)?;

    let ctx = &mut RUNTIME_CONTEXT.write().unwrap();

    run_program(&parsed.data, checker_output, ctx).map_err(ReportableError::Runtime)
}
