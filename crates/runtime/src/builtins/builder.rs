use std::collections::HashMap;

use parsy::{CodeRange, Eaten, MaybeEaten};
use reshell_parser::ast::{FnArg, FnArgNames, SingleValueType, ValueType};

use crate::values::RuntimeValue;

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

fn generate_internal_arg_decl<
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

pub struct ArgsDeclaration<T> {
    build_args_decl: fn() -> Vec<FnArg>,
    parse_args: FnArsgParser<T>,
}

type FnArsgParser<T> =
    fn(CodeRange, HashMap<Eaten<String>, RuntimeValue>) -> Result<T, (CodeRange, String)>;

#[macro_export]
macro_rules! define_internal_args {
    ($arg_struct_name: ident ( $( $arg_name: ident: $arg_handler: ty => $gen: expr ),* )) => {{
        struct $arg_struct_name {
            $( $arg_name: <$arg_handler as ArgHandler>::Parsed ),*
        }

        fn build_args_decl() -> Vec<FnArg> {
            vec![
                $({
                    let arg: $arg_handler = $gen;
                    generate_internal_arg_decl(arg)
                }),*
            ]
        }

        fn parse_args(call_at: CodeRange, args: HashMap<Eaten<String>, RuntimeValue>)
            -> Result<$arg_struct_name, (CodeRange, String)>
        {
            let mut args = args
                .into_iter()
                .map(|(Eaten { at, data }, value)| (data, (at, value)))
                .collect::<HashMap<_, _>>();

            let parsed = $arg_struct_name {
                $( $arg_name: {
                    let arg = args.remove(stringify!($arg_name));

                    let arg_may_be_at = match arg {
                        Some((at, _)) => at,
                        None => call_at
                    };

                    let arg_handler: $arg_handler = $gen;

                    let parsed =
                        arg_handler.parse(arg.map(|arg| arg.1)).map_err(|err| (arg_may_be_at, err))?;

                    parsed
                } ),*
            };

            if args.is_empty() {
                Ok(parsed)
            } else {
                Err((call_at, format!("internal error: unknown arguments: {}", args.into_keys().collect::<Vec<_>>().join(", "))))
            }
        }

        ArgsDeclaration {
            build_args_decl,
            parse_args
        }
    }};
}
