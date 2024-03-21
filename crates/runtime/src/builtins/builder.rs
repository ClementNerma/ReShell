use parsy::{CodeRange, MaybeEaten};
use reshell_parser::ast::{FnArg, FnArgNames, SingleValueType, ValueType};

use crate::{
    context::Context,
    errors::ExecResult,
    values::{InternalFnBody, RuntimeValue},
};

use super::utils::forge_internal_token;

#[derive(Clone, Copy)]
pub enum ArgNames {
    Positional(&'static str),
    ShortFlag(char),
    LongFlag(&'static str),
    ShortAndLongFlag(char, &'static str),
}

pub trait ArgSingleTyping {
    fn arg_single_type(&self) -> SingleValueType;

    type Parsed;
    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String>;
}

pub trait ArgDirectSingleTyping: ArgSingleTyping {
    fn new() -> Self;
}

pub struct IntType;

impl ArgSingleTyping for IntType {
    fn arg_single_type(&self) -> SingleValueType {
        SingleValueType::Int
    }

    type Parsed = i64;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Int(int) => Ok(int),
            _ => Err("expected an integer".to_owned()),
        }
    }
}

impl ArgDirectSingleTyping for IntType {
    fn new() -> Self {
        Self
    }
}

pub trait ArgTyping {
    fn arg_type(&self) -> ValueType;

    type Parsed;
    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String>;
}

impl<T: ArgSingleTyping> ArgTyping for T {
    fn arg_type(&self) -> ValueType {
        ValueType::Single(MaybeEaten::Raw(self.arg_single_type()))
    }

    type Parsed = T::Parsed;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        ArgSingleTyping::parse(self, value)
    }
}

pub trait ArgDirectTyping: ArgTyping {
    fn new() -> Self;
}

impl<T: ArgDirectSingleTyping> ArgDirectTyping for T {
    fn new() -> Self {
        <T as ArgDirectSingleTyping>::new()
    }
}

pub trait ArgHandler {
    fn is_optional(&self) -> bool;
    fn is_rest(&self) -> bool;
    fn names(&self) -> ArgNames;

    type Parsed;
    fn parse(&self, value: Option<RuntimeValue>) -> Result<Self::Parsed, String>;

    type BaseTyping: ArgTyping;
    fn base_typing(&self) -> &Self::BaseTyping;
}

pub struct Arg<const OPTIONAL: bool, T: ArgTyping> {
    names: ArgNames,
    base_typing: T,
    is_rest: bool,
}

pub type RequiredArg<T> = Arg<false, T>;
pub type OptionalArg<T> = Arg<true, T>;

impl<const OPTIONAL: bool, T: ArgTyping> Arg<OPTIONAL, T> {
    pub fn new(names: ArgNames, base_typing: T) -> Self {
        Self {
            names,
            base_typing,
            is_rest: false,
        }
    }

    pub fn rest(mut self) -> Self {
        self.is_rest = true;
        self
    }
}

impl<const OPTIONAL: bool, T: ArgDirectTyping> Arg<OPTIONAL, T> {
    pub fn direct(names: ArgNames) -> Self {
        Self::new(names, T::new())
    }

    pub fn positional(name: &'static str) -> Self {
        Self::direct(ArgNames::Positional(name))
    }
}

impl<T: ArgTyping> ArgHandler for Arg<false, T> {
    fn names(&self) -> ArgNames {
        self.names
    }

    fn is_optional(&self) -> bool {
        false
    }

    fn is_rest(&self) -> bool {
        self.is_rest
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

impl<T: ArgTyping> ArgHandler for Arg<true, T> {
    fn names(&self) -> ArgNames {
        self.names
    }

    fn is_optional(&self) -> bool {
        true
    }

    fn is_rest(&self) -> bool {
        self.is_rest
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
    BaseTyping: ArgTyping,
    A: ArgHandler<Parsed = Parsed, BaseTyping = BaseTyping>,
>(
    arg: A,
) -> FnArg {
    FnArg {
        names: match arg.names() {
            ArgNames::Positional(name) => {
                FnArgNames::Positional(forge_internal_token(name.to_owned()))
            }

            ArgNames::ShortFlag(flag) => FnArgNames::ShortFlag(forge_internal_token(flag)),

            ArgNames::LongFlag(flag) => FnArgNames::LongFlag(forge_internal_token(flag.to_owned())),

            ArgNames::ShortAndLongFlag(short, long) => FnArgNames::LongAndShortFlag {
                short: forge_internal_token(short),
                long: forge_internal_token(long.to_owned()),
            },
        },
        is_optional: arg.is_optional(),
        is_rest: arg.is_rest(),
        typ: Some(forge_internal_token(arg.base_typing().arg_type())),
    }
}

pub struct InternalFunction {
    pub args: Vec<FnArg>,
    pub run: InternalFnBody,
}

// type FnArgsParser<T> =
//     fn(CodeRange, HashMap<Eaten<String>, RuntimeValue>) -> Result<T, (CodeRange, String)>;

type InternalFunctionBody<T, L> = fn(
    call_at: CodeRange,
    args: T,
    args_at: L,
    ctx: &mut Context,
) -> ExecResult<Option<RuntimeValue>>;

#[macro_export]
macro_rules! define_internal_fn {
    ($args_struct_name: ident [$args_loc_struct_name: ident] ( $( $arg_name: ident: $arg_handler: ty => $gen: expr ),* ), $run: expr) => {{
        use std::collections::HashMap;

        use reshell_parser::ast::FnArg;
        use parsy::{CodeRange, Eaten};

        use $crate::{
            context::Context,
            builtins::builder::{ArgHandler, generate_internal_arg_decl, InternalFunction},
            errors::ExecResult,
            values::{RuntimeValue, LocatedValue, InternalFnCallData}
        };

        struct $args_struct_name {
            $( $arg_name: <$arg_handler as ArgHandler>::Parsed ),*
        }

        struct $args_loc_struct_name {
            $( $arg_name: Option<CodeRange> ),*
        }

        fn build_args_decl() -> Vec<FnArg> {
            vec![
                $({
                    let arg: $arg_handler = $gen;
                    generate_internal_arg_decl(arg)
                }),*
            ]
        }

        fn parse_args(call_at: CodeRange, args: HashMap<Eaten<String>, LocatedValue>)
            -> Result<($args_struct_name, $args_loc_struct_name), (CodeRange, String)>
        {
            let mut args = args
                .into_iter()
                .map(|(Eaten { at, data }, value)| (data, (at, value)))
                .collect::<HashMap<_, _>>();

            let args_at = $args_loc_struct_name {
                $( $arg_name: args.get(stringify!($arg_name)).map(|(at, _)| *at) ),*
            };

            let parsed = $args_struct_name {
                $( $arg_name: {
                    let arg = args.remove(stringify!($arg_name));

                    let arg_may_be_at = match arg {
                        Some((at, _)) => at,
                        None => call_at
                    };

                    let arg_handler: $arg_handler = $gen;

                    let parsed =
                        arg_handler.parse(arg.map(|arg| arg.1.value)).map_err(|err| (arg_may_be_at, err))?;

                    parsed
                } ),*
            };

            if args.is_empty() {
                Ok((parsed, args_at))
            } else {
                Err((call_at, format!("internal error: unknown arguments: {}", args.into_keys().collect::<Vec<_>>().join(", "))))
            }
        }

        fn run(call_data: InternalFnCallData) -> ExecResult<Option<LocatedValue>> {
            let InternalFnCallData { call_at, args, ctx } = call_data;

            let (args, args_at) = parse_args(call_at, args)
                .map_err(|(at, err)| ctx.error(at, err))?;

            fn wrapper() -> impl Fn(CodeRange, $args_struct_name, $args_loc_struct_name, &mut Context) -> ExecResult<Option<RuntimeValue>> {
                $run
            }

            #[allow(clippy::redundant_closure_call)]
            wrapper()(call_at, args, args_at, ctx)
                .map(|value| value.map(|value| LocatedValue::new(value, forge_internal_loc())))
        }

        // fn $builder_name(run: InternalFunctionBody<$args_struct_name>) -> InternalFunction<$args_struct_name> {
        InternalFunction {
            args: build_args_decl(),
            run
        }
        // }
    }};
}
