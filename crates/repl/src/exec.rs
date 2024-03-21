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
        only_check: _, // TODO
    } = exec_args;

    let file_id = ctx.files_map().register_file(file_loc, input.to_string());

    let parsed = parser
        .parse_str_as_file(input, FileId::SourceFile(file_id))
        .map_err(ReportableError::Parsing)?;

    if print_ast {
        println!("AST: {parsed:#?}");
    }

    run_program(&parsed, ctx).map_err(|err| ReportableError::Runtime(err, Some(parsed)))
}
