use ::std::{any::Any, fmt::Debug};
use parsy::{FileId, ParserInput, ParserNonConstUtils};
use reshell_builtins::{NativeLibParams, TypedValueParser, build_native_lib_content};
use reshell_parser::{
    PROGRAM, ParserContext,
    ast::RuntimeCodeRange,
    files_map::{FilesMap, SourceFileLocation},
};
use reshell_prettify::PrettyPrintable;
use reshell_reports::ReportableError;
use reshell_runtime::{
    bin_resolver::BinariesResolver,
    conf::RuntimeConf,
    context::{Context, ContextCreationParams},
    errors::{ExecActualErrorNature, ExecError, ExecResult, ExecTopPropagation},
    exec::run_program,
    values::LocatedValue,
};

#[cfg(test)]
mod basics;

#[cfg(test)]
mod std;

#[allow(clippy::result_large_err)]
pub fn run(source: impl Into<String>) -> Result<Option<LocatedValue>, (ExecError, Context)> {
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

    run_program(&program, &mut ctx).map_err(|err| (err, ctx))
}

pub fn run_expect_success(source: &str) -> Option<LocatedValue> {
    match run(source) {
        Ok(value) => value,

        Err((err, ctx)) => match err {
            ExecError::ActualError(err) => {
                reshell_reports::print_error(&ReportableError::Runtime(err), ctx.files_map());

                panic!("Program failed")
            }

            ExecError::InternalPropagation(_) => unreachable!(),

            ExecError::TopPropagation(err) => match err {
                ExecTopPropagation::SuccessfulExit => {
                    panic!("Program exited manually ; expected a value instead")
                }

                ExecTopPropagation::FailureExit { code } => {
                    panic!(
                        "Program exited manually (with error code {code}) ; expected a value instead"
                    )
                }
            },
        },
    }
}

pub fn run_expect_value(source: &str) -> LocatedValue {
    run_expect_success(source)
        .expect("Expected the program to return a value, but it returned nothing")
}

pub fn run_expect_value_of_type<T: TypedValueParser>(source: &str) -> T::Parsed {
    let value = run_expect_value(source);

    T::parse(value.value.clone()).unwrap_or_else(|err| {
        panic!(
            "Program did not return the expected value type: {err}\n\n=> expected : {}\n=> got      : {}\n=> value    : {}",
            T::value_type().display_inline(),
            value
                .value
                .compute_type()
                .display_inline(),
                value.value.display_inline()
        )
    })
}

pub fn run_expect_specific_value<T: TypedValueParser>(
    source: &str,
    expect: impl PartialEq<T::Parsed> + Debug,
) where
    T::Parsed: Debug,
{
    let got = run_expect_value_of_type::<T>(source);

    if expect != got {
        panic!(
            "Program returned incorrect value.\n\n=> expected : {expect:?}\n=> got      : {got:?}"
        );
    }
}

pub fn run_expect_throw(source: &str) {
    match run(source) {
        Ok(_) => panic!("Program terminated successfully, but expected it to throw."),

        Err((err, ctx)) => match err {
            ExecError::ActualError(err) => match &err.nature {
                ExecActualErrorNature::Thrown { at: _, message: _ } => {
                    // OK
                }

                _ => {
                    reshell_reports::print_error(&ReportableError::Runtime(err), ctx.files_map());

                    panic!("Program failed without throwing, but expected it to throw.");
                }
            },

            ExecError::InternalPropagation(_) => unreachable!(),

            ExecError::TopPropagation(_) => {
                panic!("Program exited successfully, but expected it to throw.");
            }
        },
    }
}

pub fn run_expect_non_throw_error(source: &str) {
    match run(source) {
        Ok(_) => panic!("Program terminated successfully, but expected it to fail."),

        Err((err, ctx)) => match err {
            ExecError::ActualError(err) => match &err.nature {
                ExecActualErrorNature::Thrown { at: _, message: _ } => {
                    reshell_reports::print_error(&ReportableError::Runtime(err), ctx.files_map());

                    panic!("Program thrown, but expected to fail.");
                }

                _ => {
                    // OK
                }
            },

            ExecError::InternalPropagation(_) => unreachable!(),

            ExecError::TopPropagation(_) => {
                panic!("Program exited successfully, but expected it to fail.");
            }
        },
    }
}

pub fn run_expect_exit(source: &str) {
    match run(source) {
        Ok(_) => panic!("Program terminated successfully, but expected it to exit manually."),

        Err((err, ctx)) => match err {
            ExecError::ActualError(err) => {
                reshell_reports::print_error(&ReportableError::Runtime(err), ctx.files_map());

                panic!("Program failed")
            }

            ExecError::InternalPropagation(_) => unreachable!(),

            ExecError::TopPropagation(err) => match err {
                ExecTopPropagation::SuccessfulExit => {
                    // OK!
                }

                ExecTopPropagation::FailureExit { code } => {
                    panic!("Program exited with non-zero code: {code}");
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
