use reshell_parser::ast::{
    FnArg, FnFlagArgNames, MethodApplyableType, RuntimeEaten, SingleValueType, ValueType,
};

use reshell_runtime::values::{InternalFnBody, RuntimeValue};

use crate::type_handlers::BoolType;

// #[derive(Clone)]
pub enum ArgNames {
    Positional(&'static str),
    Flag(ArgFlagNames),
}

// #[derive(Clone)]
pub enum ArgFlagNames {
    // Short(char),
    Long(ArgLongFlagName),
    LongAndShort(ArgLongFlagName, char),
}

pub struct ArgLongFlagName {
    raw: &'static str,
    var_name: String,
}

impl ArgLongFlagName {
    fn new(raw: &'static str) -> Self {
        Self {
            raw,
            var_name: long_flag_var_name(raw),
        }
    }
}

impl ArgNames {
    pub fn var_name(&self) -> String {
        match self {
            ArgNames::Positional(name) => (*name).to_owned(),
            ArgNames::Flag(flag) => match flag {
                // ArgFlagNames::Short(short) => short.to_string(),
                ArgFlagNames::Long(ArgLongFlagName { raw: _, var_name }) => var_name.clone(),
                ArgFlagNames::LongAndShort(ArgLongFlagName { raw: _, var_name }, _) => {
                    var_name.clone()
                }
            },
        }
    }
}

pub trait SingleTyping {
    fn underlying_single_type(&self) -> SingleValueType;

    type Parsed;
    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String>;
}

pub trait SingleTypingDirectCreation: SingleTyping {
    fn new_single_direct() -> Self;
}

pub trait Typing {
    fn underlying_type(&self) -> ValueType;

    type Parsed;
    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String>;
}

impl<T: SingleTyping> Typing for T {
    fn underlying_type(&self) -> ValueType {
        ValueType::Single(RuntimeEaten::Internal(
            self.underlying_single_type(),
            "native library's type generator",
        ))
    }

    type Parsed = T::Parsed;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        SingleTyping::parse(self, value)
    }
}

pub trait TypingDirectCreation: Typing {
    fn new_direct() -> Self;

    fn direct_underlying_type() -> ValueType
    where
        Self: Sized,
    {
        Self::new_direct().underlying_type()
    }
}

impl<T: SingleTypingDirectCreation> TypingDirectCreation for T {
    fn new_direct() -> Self {
        <T as SingleTypingDirectCreation>::new_single_direct()
    }
}

pub trait ArgHandler {
    fn is_optional(&self) -> bool;
    fn is_rest(&self) -> bool;
    fn names(&self) -> &ArgNames;

    type FixedOptionality<Z>;
    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z>;

    type Parsed;
    fn parse(&self, value: Option<RuntimeValue>) -> Result<Self::Parsed, String>;

    type BaseTyping: Typing;
    fn base_typing(&self) -> &Self::BaseTyping;
}

pub struct Arg<const OPTIONAL: bool, T: Typing> {
    names: ArgNames,
    base_typing: T,
    is_rest: bool,
}

pub type RequiredArg<T> = Arg<false, T>;
pub type OptionalArg<T> = Arg<true, T>;
pub type PresenceFlag = RequiredArg<BoolType>;

impl<const OPTIONAL: bool, T: Typing> Arg<OPTIONAL, T> {
    pub fn new(names: ArgNames, base_typing: T) -> Self {
        Self {
            names,
            base_typing,
            is_rest: false,
        }
    }
}

impl<const OPTIONAL: bool, T: TypingDirectCreation> Arg<OPTIONAL, T> {
    pub fn direct(names: ArgNames) -> Self {
        Self::new(names, T::new_direct())
    }

    pub fn positional(name: &'static str) -> Self {
        Self::direct(ArgNames::Positional(name))
    }

    pub fn long_flag(long: &'static str) -> Self {
        Self::direct(ArgNames::Flag(ArgFlagNames::Long(ArgLongFlagName::new(
            long,
        ))))
    }

    pub fn long_and_short_flag(long: &'static str, short: char) -> Self {
        Self::direct(ArgNames::Flag(ArgFlagNames::LongAndShort(
            ArgLongFlagName::new(long),
            short,
        )))
    }

    pub fn rest(name: &'static str) -> Self {
        let mut arg = Self::positional(name);
        arg.is_rest = true;
        arg
    }

    pub fn method_self() -> Self {
        Self::positional("self")
    }
}

impl<T: Typing> ArgHandler for Arg<false, T> {
    fn names(&self) -> &ArgNames {
        &self.names
    }

    fn is_optional(&self) -> bool {
        false
    }

    fn is_rest(&self) -> bool {
        self.is_rest
    }

    type FixedOptionality<Z> = Z;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value.unwrap()
    }

    type Parsed = T::Parsed;

    fn parse(&self, value: Option<RuntimeValue>) -> Result<Self::Parsed, String> {
        match value {
            None => Err("argument is missing".to_owned()),
            Some(value) => self.base_typing.parse(value),
        }
    }

    type BaseTyping = T;

    fn base_typing(&self) -> &Self::BaseTyping {
        &self.base_typing
    }
}

impl<T: Typing> ArgHandler for Arg<true, T> {
    fn names(&self) -> &ArgNames {
        &self.names
    }

    fn is_optional(&self) -> bool {
        true
    }

    fn is_rest(&self) -> bool {
        self.is_rest
    }

    type FixedOptionality<Z> = Option<Z>;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value
    }

    type Parsed = Option<T::Parsed>;

    fn parse(&self, value: Option<RuntimeValue>) -> Result<Self::Parsed, String> {
        value.map(|value| self.base_typing.parse(value)).transpose()
    }

    type BaseTyping = T;

    fn base_typing(&self) -> &Self::BaseTyping {
        &self.base_typing
    }
}

pub(super) fn generate_internal_arg_decl<
    Parsed,
    BaseTyping: Typing,
    A: ArgHandler<Parsed = Parsed, BaseTyping = BaseTyping>,
>(
    arg: A,
) -> FnArg {
    match arg.names() {
        ArgNames::Positional(name) => if !arg.is_rest() {
            FnArg::Positional {
                name: RuntimeEaten::Internal(
                    (*name).to_owned(),
                    "native library's arguments declaration",
                ),
                is_optional: arg.is_optional(),
                typ: Some(RuntimeEaten::Internal(
                    arg.base_typing().underlying_type(),
                    "native library's arguments declaration",
                )),
            }
        } else {
            FnArg::Rest { name: RuntimeEaten::Internal((*name).to_owned(), "native library's arguments declaration") }
        },

        ArgNames::Flag(flag) => {
            let names = match flag {
                // ArgFlagNames::Short(short) => {
                //     FnFlagArgNames::ShortFlag(RuntimeEaten::Internal(short))
                // }
                //
                ArgFlagNames::Long(long) => FnFlagArgNames::LongFlag(RuntimeEaten::Internal(
                    long.raw.to_owned(),
                    "native library's arguments declaration",
                )),

                ArgFlagNames::LongAndShort(long, short) => FnFlagArgNames::LongAndShortFlag {
                    long: RuntimeEaten::Internal(
                        long.raw.to_owned(),
                        "native library's arguments declaration",
                    ),
                    short: RuntimeEaten::Internal(*short, "native library's arguments declaration"),
                },
            };

            match arg.base_typing().underlying_type() {
                ValueType::Single(eaten) if matches!(eaten.data(), SingleValueType::Bool) => {
                    FnArg::PresenceFlag { names }
                }

                typ => FnArg::NormalFlag {
                    names,
                    is_optional: arg.is_optional(),
                    typ: Some(RuntimeEaten::Internal(
                        typ,
                        "native library's arguments declaration",
                    )),
                },
            }
        }
    }
}

/// Compute the variable name for a long flag
/// Converst a raw flag name to a valid (variable) identifier
///
/// Example: `push-with-lease` -> `pushWithLease`
pub fn long_flag_var_name(name: &str) -> String {
    let mut var_name = String::with_capacity(name.len());

    let mut uppercase = false;

    for char in name.chars() {
        if char == '-' {
            uppercase = true;
        } else if uppercase {
            uppercase = false;

            for char in char.to_uppercase() {
                var_name.push(char);
            }
        } else {
            var_name.push(char);
        }
    }

    var_name
}

pub struct InternalFunction {
    pub name: &'static str,
    pub method_on_type: Option<MethodApplyableType>,
    pub args: Vec<FnArg>,
    pub run: InternalFnBody,
    pub ret_type: Option<ValueType>,
}

#[macro_export]
macro_rules! define_internal_fn {
    ($name: expr, ( $( $arg_name: ident : $arg_handler_type: ty = $arg_handler_gen: expr ),* ) -> $ret_type: expr) => {
        use std::collections::HashMap;

        use reshell_parser::ast::{FnArg, MethodApplyableType, RuntimeCodeRange, RuntimeEaten, ValueType};

        use reshell_runtime::{
            context::Context,
            errors::ExecResult,
            values::{RuntimeValue, LocatedValue, InternalFnCallData},
            functions::ValidatedFnCallArg
        };

        #[allow(unused_imports)]
        use $crate::{helper::*, type_handlers::*};

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
                FnArg::Positional {
                    name: RuntimeEaten::Internal(name, _),
                    is_optional: false,
                    typ
                } if name == "self" => {
                    match typ {
                        Some(RuntimeEaten::Internal(ValueType::Single(typ), _)) => {
                            Some(MethodApplyableType::from_single_value_type(typ.data().clone()).unwrap_or_else(|| {
                                panic!("invalid method applyable type in native library: {:?}", typ.data())
                            }))
                        },

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
