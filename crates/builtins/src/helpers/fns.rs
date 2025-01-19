//! This module contains utilities to create builtin functions
//!
//! Mainly the [`define_internal_fn`] macro

use reshell_parser::ast::{FnArg, ValueType};
use reshell_runtime::values::InternalFnBody;

/// Description of an internal function
pub struct InternalFunction {
    /// Name of the function
    pub name: &'static str,

    /// If it is a method, type it applies on
    pub method_on_type: Option<ValueType>,

    /// List of arguments the function takes
    pub args: Vec<FnArg>,

    /// Callback of the function
    pub run: InternalFnBody,

    /// Return type of the function
    pub ret_type: Option<ValueType>,
}

/// Define an internal function
#[macro_export]
macro_rules! define_internal_fn {
    ($name: expr, ( $( $arg_name: ident : $arg_handler_type: ty = $arg_handler_gen: expr ),* ) -> $ret_type: expr) => {
        use std::collections::HashMap;

        use reshell_parser::ast::{FnArg, FnPositionalArg, RuntimeCodeRange, RuntimeSpan};

        use reshell_runtime::{
            context::Context,
            errors::ExecResult,
            values::{RuntimeValue, LocatedValue, InternalFnCallData},
            functions::ValidatedFnCallArg
        };

        #[allow(unused_imports)]
        use $crate::helpers::{args::*, types::*, fns::*};

        struct Args {
            $( $arg_name: <$arg_handler_type as ArgHandler>::Parsed ),*
        }

        struct ArgsAt {
            $(
                #[allow(dead_code)]
                $arg_name: <$arg_handler_type as ArgHandler>::FixedOptionality<RuntimeCodeRange>
            ),*
        }

        fn _parse_args(
            ctx: &Context,
            call_at: RuntimeCodeRange,

            #[allow(unused_variables)]
            args_at: RuntimeCodeRange,

            #[allow(unused_mut)]
            mut args: HashMap<String, ValidatedFnCallArg>
        )
            -> Result<(Args, ArgsAt), (RuntimeCodeRange, String)>
        {
            // Get the location of each argument
            let parsed_args_at = ArgsAt {
                $( $arg_name: {
                    // Retrieve the handler for this argument
                    let arg_handler: $arg_handler_type = $arg_handler_gen;

                    // Get the argument from the call map
                    let arg = args.get(&arg_handler.names().var_name());

                    // Get the argument's location
                    <$arg_handler_type as ArgHandler>::min_unwrap(arg.map(|arg| arg.arg_value_at))
                } ),*
            };

            // Get the value of each argument
            let parsed = Args {
                $( $arg_name: {
                    // Retrieve the handler for this argument
                    let arg_handler: $arg_handler_type = $arg_handler_gen;

                    // Get (and remove) the argument from the call map
                    let arg = args.remove(&arg_handler.names().var_name());

                    // Get the argument's location (for error reporting)
                    // If no value was provided, fallback to the whole arguments' location
                    let arg_at = arg.as_ref().map_or(args_at, |arg| arg.arg_value_at);

                    // Get the argument's value
                    let arg_value = <$arg_handler_type as ArgHandler>::min_unwrap(arg.map(|arg| arg.value));

                    // Try to parse the argument's value
                    arg_handler.parse(arg_value).map_err(|err| (arg_at, err))?
                } ),*
            };

            if args.is_empty() {
                Ok((parsed, parsed_args_at))
            } else {
                ctx.panic(call_at, format!("unknown arguments in function call: {}", args.into_keys().collect::<Vec<_>>().join(", ")))
            }
        }

        fn _run(call_data: InternalFnCallData) -> ExecResult<Option<LocatedValue>> {
            let InternalFnCallData { call_at, args_at, args, ctx } = call_data;

            let (args, args_at) = _parse_args(ctx, call_at, args_at, args)
                .map_err(|(at, err)| ctx.error(at, err))?;

            let Runner(runner) = run();

            runner(call_at, args, args_at, ctx)
                .map(|value| value.map(|value| LocatedValue::new(call_at, value)))
        }

        struct Runner(Box<dyn Fn(RuntimeCodeRange, Args, ArgsAt, &mut Context) -> ExecResult<Option<RuntimeValue>>>);

        impl Runner {
            fn new(inner: impl Fn(RuntimeCodeRange, Args, ArgsAt, &mut Context) -> ExecResult<Option<RuntimeValue>> + 'static) -> Self {
                Self(Box::new(inner))
            }
        }

        pub fn build_fn() -> InternalFunction {
            let args = vec![
                $({
                    let arg: $arg_handler_type = $arg_handler_gen;
                    generate_internal_arg_decl(arg)
                }),*
            ];

            let method_on_type = args.first().and_then(|first_arg| match first_arg {
                FnArg::Positional(FnPositionalArg {
                    name: RuntimeSpan {
                        at: RuntimeCodeRange::Internal(_),
                        data: name,
                    },
                    is_optional: false,
                    typ
                }) if name == "self" => {
                    Some(typ.clone().unwrap_or_else(|| {
                        panic!("invalid method applyable type in native library: {:?}", typ)
                    }))
                }

                _ => None
            });

            InternalFunction {
                name: $name,
                ret_type: $ret_type,
                args,
                method_on_type,
                run: _run,
            }
        }
    };
}
