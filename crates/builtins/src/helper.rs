use reshell_parser::ast::{FnArg, FnArgNames, RuntimeEaten, SingleValueType, ValueType};

use reshell_runtime::values::{InternalFnBody, RuntimeValue};

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
        ValueType::Single(RuntimeEaten::Internal(self.underlying_single_type()))
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
    fn names(&self) -> ArgNames;

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

impl<const OPTIONAL: bool, T: Typing> Arg<OPTIONAL, T> {
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

impl<const OPTIONAL: bool, T: TypingDirectCreation> Arg<OPTIONAL, T> {
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

impl<T: Typing> ArgHandler for Arg<false, T> {
    fn names(&self) -> ArgNames {
        self.names
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
    fn names(&self) -> ArgNames {
        self.names
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
    FnArg {
        names: match arg.names() {
            ArgNames::Positional(name) => {
                FnArgNames::Positional(RuntimeEaten::Internal(name.to_owned()))
            }

            ArgNames::ShortFlag(flag) => FnArgNames::ShortFlag(RuntimeEaten::Internal(flag)),

            ArgNames::LongFlag(flag) => {
                FnArgNames::LongFlag(RuntimeEaten::Internal(flag.to_owned()))
            }

            ArgNames::LongAndShortFlag(long, short) => FnArgNames::LongAndShortFlag {
                long: RuntimeEaten::Internal(long.to_owned()),
                short: RuntimeEaten::Internal(short),
            },
        },
        is_optional: arg.is_optional(),
        is_rest: arg.is_rest(),
        typ: Some(RuntimeEaten::Internal(arg.base_typing().underlying_type())),
    }
}

pub struct InternalFunction {
    pub name: &'static str,
    pub args: Vec<FnArg>,
    pub run: InternalFnBody,
    pub ret_type: Option<ValueType>,
}

#[macro_export]
macro_rules! define_internal_fn {
    ($name: expr, $args_struct_name: ident [$args_loc_struct_name: ident, $args_types_struct_name: ident] ( $( $arg_name: ident: $arg_handler: ty => $gen: expr ),* ) -> $ret_type: expr, $run: expr) => {{
        use std::collections::HashMap;

        use reshell_parser::ast::RuntimeCodeRange;

        use reshell_runtime::{
            context::Context,
            errors::ExecResult,
            values::{RuntimeValue, LocatedValue, InternalFnCallData},
            functions::ParsedFnCallArg
        };

        use $crate::helper::{ArgHandler, InternalFunction, generate_internal_arg_decl};

        struct $args_struct_name {
            $( $arg_name: <$arg_handler as ArgHandler>::Parsed ),*
        }

        struct $args_loc_struct_name {
            $(
                #[allow(dead_code)]
                $arg_name: <$arg_handler as ArgHandler>::FixedOptionality<RuntimeCodeRange>
            ),*
        }

        struct $args_types_struct_name {
            $(
                #[allow(dead_code)]
                $arg_name: $arg_handler
            ),*
        }

        fn parse_args(call_at: RuntimeCodeRange, mut args: HashMap<String, ParsedFnCallArg>)
            -> Result<($args_struct_name, $args_loc_struct_name, $args_types_struct_name), (RuntimeCodeRange, String)>
        {
            let args_ty = $args_types_struct_name {
                $( $arg_name: $gen ),*
            };

            struct PlaceholderArgsAt {
                $( $arg_name: Option<RuntimeCodeRange> ),*
            }

            let mut placeholder_args_at = PlaceholderArgsAt {
                $( $arg_name: None ),*
            };

            let parsed = $args_struct_name {
                $( $arg_name: {
                    let arg = args.remove(&args_ty.$arg_name.names().static_name());

                    if let Some(arg) = &arg {
                        placeholder_args_at.$arg_name = Some(arg.arg_value_at);
                    }

                    let arg_may_be_at = arg.as_ref().map(|arg| arg.arg_value_at).unwrap_or(call_at);

                    args_ty.$arg_name.parse(arg.map(|arg| arg.value)).map_err(|err| (arg_may_be_at, err))?
                } ),*
            };

            let args_at = $args_loc_struct_name {
                $( $arg_name: <$arg_handler as ArgHandler>::min_unwrap(placeholder_args_at.$arg_name) ),*
            };

            if args.is_empty() {
                Ok((parsed, args_at, args_ty))
            } else {
                Err((call_at, format!("internal error: unknown arguments: {}", args.into_keys().collect::<Vec<_>>().join(", "))))
            }
        }

        fn run(call_data: InternalFnCallData) -> ExecResult<Option<LocatedValue>> {
            let InternalFnCallData { call_at, args, ctx } = call_data;

            let (args, args_at, args_ty) = parse_args(call_at, args)
                .map_err(|(at, err)| ctx.error(at, err))?;

            fn wrapper() -> impl Fn(RuntimeCodeRange, $args_struct_name, $args_loc_struct_name, $args_types_struct_name, &mut Context) -> ExecResult<Option<RuntimeValue>> {
                $run
            }

            #[allow(clippy::redundant_closure_call)]
            wrapper()(call_at, args, args_at, args_ty, ctx)
                .map(|value| value.map(|value| LocatedValue::new(value, RuntimeCodeRange::Internal)))
        }

        InternalFunction {
            args: vec![
                $({
                    let arg: $arg_handler = $gen;
                    generate_internal_arg_decl(arg)
                }),*
            ],

            run,

            ret_type: $ret_type,

            name: $name
        }
    }};
}
