use ::std::any::Any;

use parsy::{FileId, Parser, ParserInput, Span};
use reshell_parser::{
    PROGRAM, ParserContext,
    ast::{Program, RuntimeCodeRange},
    files_map::{FilesMap, SourceFileLocation},
};

use reshell_builtins::{NativeLibParams, TypedValueParser, build_native_lib_content};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_reports::ReportableError;
use reshell_runtime::{
    bin_resolver::BinariesResolver,
    conf::RuntimeConf,
    context::{Context, ContextCreationParams},
    errors::{ExecError, ExecResult, ExecTopPropagation},
    exec::run_program,
    values::LocatedValue,
};

mod basics;
mod std;

pub fn run(
    source: impl Into<String>,
) -> Result<(Option<LocatedValue>, Context), (ExecError, Span<Program>)> {
    let source = source.into();

    let files_map = FilesMap::new(Box::new(|_, _, _| unreachable!()));

    let source_file_id = files_map.register_file(
        SourceFileLocation::CustomName("<test>".to_owned()),
        source.clone(),
    );

    let program = PROGRAM
        .parse(&mut ParserInput::new_with_ctx(
            &source,
            FileId::SourceFile(source_file_id),
            get_parser_context,
        ))
        .unwrap_or_else(|err| {
            reshell_reports::print_error(&ReportableError::Parsing(err), &files_map);
            panic!("Invalid syntax in program");
        });

    let mut ctx = Context::new(
        ContextCreationParams {
            files_map,
            home_dir: None,
            on_dir_jump,
            runtime_conf: RuntimeConf::default(),
            script_args: vec![],
            take_ctrl_c_indicator: no_ctrl_c_indicator,
        },
        BinariesResolver::empty(),
        build_native_lib_content(NativeLibParams {
            home_dir: None,
            script_args: vec![],
        }),
    );

    run_program(&program, &mut ctx)
        .map(|ret| (ret, ctx))
        .map_err(|err| (err, program))
}

pub fn run_expect_success(source: &str) -> (Option<LocatedValue>, Context) {
    match run(source) {
        Ok((value, ctx)) => (value, ctx),

        Err((err, program)) => match err {
            ExecError::ActualError(err) => {
                reshell_reports::print_error(
                    &ReportableError::Runtime(err, Some(program)),
                    &FilesMap::new(Box::new(|_, _, _| unreachable!())),
                );

                panic!("Program failed")
            }

            ExecError::InternalPropagation(_) => unreachable!(),

            ExecError::TopPropagation(err) => match err {
                ExecTopPropagation::SuccessfulExit => {
                    panic!("Program exited manually ; expected a value instead")
                }
            },
        },
    }
}

pub fn run_expect_value(source: &str) -> (LocatedValue, Context) {
    let (ret_val, ctx) = run_expect_success(source);

    (
        ret_val.expect("Expected the program to return a value, but it returned nothing"),
        ctx,
    )
}

pub fn run_expect_typed_value<T: TypedValueParser>(source: &str) -> T::Parsed {
    let (value, ctx) = run_expect_value(source);

    T::parse(value.value.clone()).unwrap_or_else(|err| {
        panic!(
            "Program did not return the expected value type: {err}\n\n=> expected: {}\n=>got: {}",
            T::value_type().display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
            value
                .value
                .compute_type()
                .display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
        )
    })
}

pub fn run_expect_exit(source: &str) {
    match run(source) {
        Ok(_) => panic!("Program exited automatically, expected it to exit manually."),

        Err((err, program)) => match err {
            ExecError::ActualError(err) => {
                reshell_reports::print_error(
                    &ReportableError::Runtime(err, Some(program)),
                    &FilesMap::new(Box::new(|_, _, _| unreachable!())),
                );

                panic!("Program failed")
            }

            ExecError::InternalPropagation(_) => unreachable!(),

            ExecError::TopPropagation(err) => match err {
                ExecTopPropagation::SuccessfulExit => {
                    // OK!
                }
            },
        },
    }
}

fn no_ctrl_c_indicator() -> bool {
    false
}

fn on_dir_jump(_: &mut Context, _: RuntimeCodeRange) -> ExecResult<()> {
    Ok(())
}

fn get_parser_context() -> Box<dyn Any> {
    Box::new(ParserContext {
        load_file: Box::new(|_, _| unreachable!()),
    })
}
