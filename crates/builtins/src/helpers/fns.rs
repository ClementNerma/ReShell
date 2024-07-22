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

        use reshell_parser::ast::{FnArg, FnPositionalArg, RuntimeCodeRange, RuntimeEaten};

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

        fn _parse_args(ctx: &Context, call_at: RuntimeCodeRange, #[allow(unused_mut)] mut args: HashMap<String, ValidatedFnCallArg>)
            -> Result<(Args, ArgsAt), (RuntimeCodeRange, String)>
        {
            struct PlaceholderArgsAt {
                $( $arg_name: Option<RuntimeCodeRange> ),*
            }

            #[allow(unused_mut, unused_variables)]
            let mut placeholder_args_at = PlaceholderArgsAt {
                $( $arg_name: None ),*
            };

            let parsed = Args {
                $( $arg_name: {
                    let arg_handler: $arg_handler_type = $arg_handler_gen;

                    let arg = args.remove(&arg_handler.names().var_name());

                    if let Some(arg) = &arg {
                        placeholder_args_at.$arg_name = Some(arg.arg_value_at);
                    }

                    let arg_may_be_at = arg.as_ref().map(|arg| arg.arg_value_at).unwrap_or(call_at);

                    arg_handler.parse(arg.map(|arg| arg.value)).map_err(|err| (arg_may_be_at, err))?
                } ),*
            };

            let args_at = ArgsAt {
                $( $arg_name: <$arg_handler_type as ArgHandler>::min_unwrap(placeholder_args_at.$arg_name) ),*
            };

            if args.is_empty() {
                Ok((parsed, args_at))
            } else {
                ctx.panic(call_at, format!("unknown arguments in function call: {}", args.into_keys().collect::<Vec<_>>().join(", ")))
            }
        }

        fn _run(call_data: InternalFnCallData) -> ExecResult<Option<LocatedValue>> {
            let InternalFnCallData { call_at, args, ctx } = call_data;

            let (args, args_at) = _parse_args(ctx, call_at, args)
                .map_err(|(at, err)| ctx.error(at, err))?;

            run().0
                (call_at, args, args_at, ctx)
                .map(|value| value.map(|value| LocatedValue::new(value, RuntimeCodeRange::Internal("native library's argument parser"))))
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
                    name: RuntimeEaten::Internal(name, _),
                    is_optional: false,
                    typ
                }) if name == "self" => {
                    match typ {
                        Some(RuntimeEaten::Internal(typ, _)) => Some(typ.clone()),
                        _ => panic!("invalid method applyable type in native library: {:?}", typ)
                    }
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
