use parsy::MaybeEaten;
use reshell_parser::ast::{FnArg, FnArgNames, SingleValueType, ValueType};

use crate::values::{InternalFnBody, RuntimeValue};

use super::utils::forge_internal_token;

#[derive(Clone, Copy)]
pub enum ArgNames {
    Positional(&'static str),
    ShortFlag(char),
    LongFlag(&'static str),
    LongAndShortFlag(&'static str, char),
}

impl ArgNames {
    pub fn static_name(&self) -> String {
        match self {
            ArgNames::Positional(name) => (*name).to_owned(),
            ArgNames::ShortFlag(short) => short.to_string(),
            ArgNames::LongFlag(long) => (*long).to_owned(),
            ArgNames::LongAndShortFlag(long, _) => (*long).to_owned(),
        }
    }
}

pub trait ArgSingleTyping {
    fn arg_single_type(&self) -> SingleValueType;

    type Parsed;
    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String>;
}

pub trait ArgSingleTypingDirectCreation: ArgSingleTyping {
    fn new_single_direct() -> Self;
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

pub trait ArgTypingDirectCreation: ArgTyping {
    fn new_direct() -> Self;
}

impl<T: ArgSingleTypingDirectCreation> ArgTypingDirectCreation for T {
    fn new_direct() -> Self {
        <T as ArgSingleTypingDirectCreation>::new_single_direct()
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

impl<const OPTIONAL: bool, T: ArgTypingDirectCreation> Arg<OPTIONAL, T> {
    pub fn direct(names: ArgNames) -> Self {
        Self::new(names, T::new_direct())
    }

    pub fn positional(name: &'static str) -> Self {
        Self::direct(ArgNames::Positional(name))
    }

    pub fn short_flag(short: char) -> Self {
        Self::direct(ArgNames::ShortFlag(short))
    }

    pub fn long_flag(long: &'static str) -> Self {
        Self::direct(ArgNames::LongFlag(long))
    }

    pub fn long_and_short_flag(long: &'static str, short: char) -> Self {
        Self::direct(ArgNames::LongAndShortFlag(long, short))
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

            ArgNames::LongAndShortFlag(long, short) => FnArgNames::LongAndShortFlag {
                long: forge_internal_token(long.to_owned()),
                short: forge_internal_token(short),
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

#[macro_export]
macro_rules! define_internal_fn {
    ($args_struct_name: ident [$args_loc_struct_name: ident] ( $( $arg_name: ident: $arg_handler: ty => $gen: expr ),* ), $run: expr) => {{
        use std::collections::HashMap;

        use reshell_parser::ast::FnArg;
        use parsy::{CodeRange, Eaten};

        use $crate::{
            context::Context,
            builtins::helper::{ArgHandler, InternalFunction, generate_internal_arg_decl},
            errors::ExecResult,
            values::{RuntimeValue, LocatedValue, InternalFnCallData}
        };

        struct $args_struct_name {
            $( $arg_name: <$arg_handler as ArgHandler>::Parsed ),*
        }

        struct $args_loc_struct_name {
            $(
                #[allow(dead_code)]
                $arg_name: Option<CodeRange>
            ),*
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
                $( $arg_name: {
                    let arg_handler: $arg_handler = $gen;

                    args.get(&arg_handler.names().static_name()).map(|(at, _)| *at)
                } ),*
            };

            let parsed = $args_struct_name {
                $( $arg_name: {
                    let arg_handler: $arg_handler = $gen;

                    let arg = args.remove(&arg_handler.names().static_name());

                    let arg_may_be_at = match arg {
                        Some((at, _)) => at,
                        None => call_at
                    };

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

        InternalFunction { args: build_args_decl(), run }
    }};
}
