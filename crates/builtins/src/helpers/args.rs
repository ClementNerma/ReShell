//! This module contains utilities for arguments handling, typechecking and type-safe values extraction

use std::marker::PhantomData;

use reshell_parser::ast::{
    FnArg, FnFlagArgNames, FnNormalFlagArg, FnPositionalArg, FnPresenceFlagArg, FnRestArg,
    SingleValueType, ValueType,
};
use reshell_runtime::values::RuntimeValue;

use super::types::{BoolType, DetachedListType};
use crate::utils::internal_runtime_span;

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

pub trait TypedValueParser {
    fn value_type() -> ValueType;

    type Parsed;
    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String>;
}

pub trait TypedValueEncoder {
    type Encodable;
    fn encode(value: Self::Encodable) -> RuntimeValue;
}

pub trait ArgHandler {
    fn is_optional(&self) -> bool;
    fn is_rest(&self) -> bool;
    fn names(&self) -> &ArgNames;

    type FixedOptionality<Z>;
    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z>;

    type Parsed;
    fn parse(&self, value: Self::FixedOptionality<RuntimeValue>) -> Result<Self::Parsed, String>;

    type TypedValueParser: TypedValueParser;

    fn hide_type(&self) -> bool {
        false
    }
}

pub struct Arg<const OPTIONAL: bool, T: TypedValueParser> {
    names: ArgNames,
    _t: PhantomData<T>,
}

pub type RequiredArg<T> = Arg<false, T>;
pub type OptionalArg<T> = Arg<true, T>;
pub type PresenceFlag = RequiredArg<BoolType>;

impl<const OPTIONAL: bool, T: TypedValueParser> Arg<OPTIONAL, T> {
    pub fn new(names: ArgNames) -> Self {
        Self {
            names,
            _t: PhantomData,
        }
    }

    pub fn positional(name: &'static str) -> Self {
        Self::new(ArgNames::Positional(name))
    }

    pub fn long_flag(long: &'static str) -> Self {
        Self::new(ArgNames::Flag(ArgFlagNames::Long(ArgLongFlagName::new(
            long,
        ))))
    }

    pub fn long_and_short_flag(long: &'static str, short: char) -> Self {
        Self::new(ArgNames::Flag(ArgFlagNames::LongAndShort(
            ArgLongFlagName::new(long),
            short,
        )))
    }

    pub fn method_self() -> Self {
        Self::positional("self")
    }
}

impl<T: TypedValueParser> ArgHandler for Arg<false, T> {
    fn names(&self) -> &ArgNames {
        &self.names
    }

    fn is_optional(&self) -> bool {
        false
    }

    fn is_rest(&self) -> bool {
        false
    }

    type FixedOptionality<Z> = Z;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value.unwrap()
    }

    type Parsed = T::Parsed;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        T::parse(value)
    }

    type TypedValueParser = T;
}

impl<T: TypedValueParser> ArgHandler for Arg<true, T> {
    fn names(&self) -> &ArgNames {
        &self.names
    }

    fn is_optional(&self) -> bool {
        true
    }

    fn is_rest(&self) -> bool {
        false
    }

    type FixedOptionality<Z> = Option<Z>;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value
    }

    type Parsed = Option<T::Parsed>;

    fn parse(&self, value: Option<RuntimeValue>) -> Result<Self::Parsed, String> {
        value.map(T::parse).transpose()
    }

    type TypedValueParser = T;
}

pub struct RestArg<T: TypedValueParser> {
    names: ArgNames,
    _t: PhantomData<T>,
}

impl<T: TypedValueParser> RestArg<T> {
    pub fn new(name: &'static str) -> Self {
        Self {
            names: ArgNames::Positional(name),
            _t: PhantomData,
        }
    }
}

impl<T: TypedValueParser> ArgHandler for RestArg<T> {
    fn names(&self) -> &ArgNames {
        &self.names
    }

    fn is_optional(&self) -> bool {
        false
    }

    fn is_rest(&self) -> bool {
        true
    }

    type FixedOptionality<Z> = Z;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value.unwrap()
    }

    type Parsed = Vec<T::Parsed>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        DetachedListType::<T>::parse(value)
    }

    type TypedValueParser = DetachedListType<T>;
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

pub fn generate_internal_arg_decl<
    Parsed,
    Parser: TypedValueParser,
    A: ArgHandler<Parsed = Parsed, TypedValueParser = Parser>,
>(
    arg: A,
) -> FnArg {
    match arg.names() {
        ArgNames::Positional(name) => {
            let name = internal_runtime_span((*name).to_owned());

            let typ = if !arg.hide_type() {
                Some(Parser::value_type())
            } else {
                None
            };

            if arg.is_rest() {
                FnArg::Rest(FnRestArg {
                    name,
                    typ: typ.map(internal_runtime_span),
                })
            } else {
                FnArg::Positional(FnPositionalArg {
                    name,
                    is_optional: arg.is_optional(),
                    typ,
                })
            }
        }

        ArgNames::Flag(flag) => {
            let names = match flag {
                // ArgFlagNames::Short(short) => {
                //     FnFlagArgNames::ShortFlag(RuntimeSpan::Internal(short))
                // }
                //
                ArgFlagNames::Long(long) => {
                    FnFlagArgNames::LongFlag(internal_runtime_span(long.raw.to_owned()))
                }

                ArgFlagNames::LongAndShort(long, short) => FnFlagArgNames::LongAndShortFlag {
                    long: internal_runtime_span(long.raw.to_owned()),
                    short: internal_runtime_span(*short),
                },
            };

            match Parser::value_type() {
                ValueType::Single(SingleValueType::Bool) => {
                    FnArg::PresenceFlag(FnPresenceFlagArg { names })
                }

                typ => FnArg::NormalFlag(FnNormalFlagArg {
                    names,
                    is_optional: arg.is_optional(),
                    typ,
                }),
            }
        }
    }
}
