use once_cell::sync::Lazy;
use parsy::{FileId, Parser};
use reshell_parser::ast::Program;
use reshell_runtime::context::ScopeContent;
use reshell_runtime::exec::run_program;
use reshell_runtime::files_map::ScopableFilePath;
use reshell_runtime::native_lib::generate_native_lib_for_checker;

use crate::reports::{self, ReportableError};
use crate::state::RUNTIME_CONTEXT;

static NATIVE_LIB_FOR_CHECKER: Lazy<reshell_checker::Scope> =
    Lazy::new(generate_native_lib_for_checker);

#[derive(Debug, Default, Clone, Copy)]
pub struct ExecOptions {
    pub check_only: bool,
    pub dbg_code_checking: bool,
}

pub fn run_script(
    input: &str,
    file_id: ScopableFilePath,
    parser: &impl Parser<Program>,
    opts: ExecOptions,
) -> Result<(), ReportableError> {
    run_script_inner(input, file_id, parser, opts).map_err(|err| {
        reports::print_error(&err, RUNTIME_CONTEXT.read().unwrap().files_map());
        err
    })
}

// TODO: keep previous scopes in memory
// TODO: use previous scopes' declarations in reshell_checker::check
fn run_script_inner(
    input: &str,
    file_path: ScopableFilePath,
    parser: &impl Parser<Program>,
    opts: ExecOptions,
) -> Result<(), ReportableError> {
    let ExecOptions {
        check_only,
        dbg_code_checking,
    } = opts;

    let ctx = &mut RUNTIME_CONTEXT.write().unwrap();

    let file_id = ctx.register_file(file_path, input.to_string());

    let parsed = parser
        .parse_str_as_file(input, FileId::SourceFile(file_id))
        .map_err(ReportableError::Parsing)?;

    let checker_output = reshell_checker::check(&parsed.data, NATIVE_LIB_FOR_CHECKER.clone())
        .map_err(ReportableError::Checking)?;

    if dbg_code_checking {
        println!("{checker_output:#?}");
    }

    if check_only {
        return Ok(());
    }

    run_program(&parsed.data, checker_output, ScopeContent::new(), ctx)
        .map_err(ReportableError::Runtime)
}
