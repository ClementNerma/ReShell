// TODO: errors thrown by type handlers should be turned into panics! Or give them directly a way to panic and remove the "Result" wrapping?

use reshell_parser::ast::{
    FnArg, FnFlagArgNames, FnNormalFlagArg, FnPositionalArg, FnPresenceFlagArg, FnRestArg,
    SingleValueType, ValueType,
};

use reshell_runtime::values::{CmdArgValue, RuntimeValue};

use crate::builder::internal_runtime_eaten;

use super::types::{BoolType, DetachedListType, UntypedListType};

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
        ValueType::Single(internal_runtime_eaten(self.underlying_single_type()))
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
    fn parse(&self, value: Self::FixedOptionality<RuntimeValue>) -> Result<Self::Parsed, ArgError>;

    type BaseTyping: Typing;
    fn base_typing(&self) -> &Self::BaseTyping;

    fn hide_type(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub enum ArgError {
    TypeError(String),
    Panic(String),
}

pub struct Arg<const OPTIONAL: bool, T: Typing> {
    names: ArgNames,
    base_typing: T,
}

pub type RequiredArg<T> = Arg<false, T>;
pub type OptionalArg<T> = Arg<true, T>;
pub type PresenceFlag = RequiredArg<BoolType>;

impl<const OPTIONAL: bool, T: Typing> Arg<OPTIONAL, T> {
    pub fn new(names: ArgNames, base_typing: T) -> Self {
        Self { names, base_typing }
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
        false
    }

    type FixedOptionality<Z> = Z;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value.unwrap()
    }

    type Parsed = T::Parsed;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, ArgError> {
        self.base_typing.parse(value).map_err(ArgError::TypeError)
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
        false
    }

    type FixedOptionality<Z> = Option<Z>;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value
    }

    type Parsed = Option<T::Parsed>;

    fn parse(&self, value: Option<RuntimeValue>) -> Result<Self::Parsed, ArgError> {
        value
            .map(|value| self.base_typing.parse(value))
            .transpose()
            .map_err(ArgError::TypeError)
    }

    type BaseTyping = T;

    fn base_typing(&self) -> &Self::BaseTyping {
        &self.base_typing
    }
}

pub struct RestArg<T: Typing> {
    names: ArgNames,
    list_typing: DetachedListType<T>,
}

impl<T: Typing> RestArg<T> {
    pub fn new(name: &'static str, base_typing: T) -> Self {
        Self {
            names: ArgNames::Positional(name),
            list_typing: DetachedListType::new(base_typing),
        }
    }
}

impl<T: Typing> ArgHandler for RestArg<T> {
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

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, ArgError> {
        match value {
            RuntimeValue::List(values) => values
                .read_promise_no_write()
                .iter()
                .enumerate()
                .map(|(i, value)| match value {
                    RuntimeValue::CmdArg(arg) => match arg.as_ref() {
                        CmdArgValue::Basic(loc_val) => self
                            .base_typing()
                            .inner()
                            .parse(loc_val.value.clone())
                            .map_err(|err| {
                                ArgError::TypeError(format!("in list item {}: {err}", i + 1))
                            }),

                            CmdArgValue::Flag(_) => Err(ArgError::Panic("typed rest arguments should be provided as a list of basic values, found a flag".to_owned())),
                    },

                    _ => Err(ArgError::Panic(format!("rest arguments should be provided as a list of command arguments, got: {value:?}"))),
                })
                .collect::<Result<Vec<_>, _>>(),

            _ => Err(ArgError::Panic(format!(
                "rest arguments should be a list, got: {value:?}"
            ))),
        }
    }

    type BaseTyping = DetachedListType<T>;

    fn base_typing(&self) -> &Self::BaseTyping {
        &self.list_typing
    }
}

impl<T: TypingDirectCreation> RestArg<T> {
    pub fn rest(name: &'static str) -> Self {
        Self::new(name, T::new_direct())
    }
}

pub struct UntypedRestArg {
    names: ArgNames,
}

impl UntypedRestArg {
    pub fn rest(name: &'static str) -> Self {
        Self {
            names: ArgNames::Positional(name),
        }
    }
}

impl ArgHandler for UntypedRestArg {
    fn is_optional(&self) -> bool {
        false
    }

    fn is_rest(&self) -> bool {
        true
    }

    fn names(&self) -> &ArgNames {
        &self.names
    }

    type FixedOptionality<Z> = Z;

    fn min_unwrap<Z>(value: Option<Z>) -> Self::FixedOptionality<Z> {
        value.unwrap()
    }

    type Parsed = Vec<CmdArgValue>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, ArgError> {
        match value {
            RuntimeValue::List(values) => values
                .read_promise_no_write()
                .iter()
                .map(|value| match value {
                    RuntimeValue::CmdArg(arg) => Ok(CmdArgValue::clone(arg)),
                    _ => Err(ArgError::Panic(format!("rest arguments should be provided as a list of command arguments, got: {value:?}"))),
                })
                .collect::<Result<Vec<_>, _>>(),

            _ => Err(ArgError::Panic(format!(
                "rest arguments should be a list, got: {value:?}"
            ))),
        }
    }

    type BaseTyping = UntypedListType;

    fn base_typing(&self) -> &Self::BaseTyping {
        &UntypedListType
    }

    fn hide_type(&self) -> bool {
        true
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

pub fn generate_internal_arg_decl<
    Parsed,
    BaseTyping: Typing,
    A: ArgHandler<Parsed = Parsed, BaseTyping = BaseTyping>,
>(
    arg: A,
) -> FnArg {
    match arg.names() {
        ArgNames::Positional(name) => {
            let name = internal_runtime_eaten((*name).to_owned());

            let typ = if !arg.hide_type() {
                Some(internal_runtime_eaten(arg.base_typing().underlying_type()))
            } else {
                None
            };

            if arg.is_rest() {
                FnArg::Rest(FnRestArg { name, typ })
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
                //     FnFlagArgNames::ShortFlag(RuntimeEaten::Internal(short))
                // }
                //
                ArgFlagNames::Long(long) => {
                    FnFlagArgNames::LongFlag(internal_runtime_eaten(long.raw.to_owned()))
                }

                ArgFlagNames::LongAndShort(long, short) => FnFlagArgNames::LongAndShortFlag {
                    long: internal_runtime_eaten(long.raw.to_owned()),
                    short: internal_runtime_eaten(*short),
                },
            };

            match arg.base_typing().underlying_type() {
                ValueType::Single(eaten) if matches!(eaten.data(), SingleValueType::Bool) => {
                    FnArg::PresenceFlag(FnPresenceFlagArg { names })
                }

                typ => FnArg::NormalFlag(FnNormalFlagArg {
                    names,
                    is_optional: arg.is_optional(),
                    typ: internal_runtime_eaten(typ),
                }),
            }
        }
    }
}
