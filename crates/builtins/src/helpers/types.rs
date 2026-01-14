//!
//! This module defines and exposes *type handlers*, which are simple types that
//! allow to convert and parse some of the scripting language's native types.
//!

use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::Display,
    marker::PhantomData,
    sync::Arc,
    time::{Duration, Instant},
};

use indexmap::IndexMap;
use jiff::Zoned;
use parsy::CodeRange;
use regex::Regex;
use reshell_parser::ast::{SingleValueType, ValueType};
use reshell_runtime::{
    gc::GcCell,
    values::{CmdArgValue, ErrorValueContent, RangeValue, RuntimeValue},
};

use super::args::{TypedValueEncoder, TypedValueParser};

/// This macro helps create a type handler for any variant of the [`SingleValueType`] enum,
/// associated to a variant of the [`RuntimeValue`]
macro_rules! declare_basic_type_handlers {
    ($(
        $name: ident ($variant: ident) = $type: ty {
            fn parse($value_ident: ident) $parser: block
            $(fn encode($encodable_ident: ident: $encodable_type: ty) $encoder: block)?
        }
    ),+) => {
        $(
            pub struct $name;

            impl TypedValueParser for $name {
                fn value_type() -> ValueType {
                    ValueType::Single(SingleValueType::$variant)
                }

                type Parsed = $type;

                fn parse($value_ident: RuntimeValue) -> Result<Self::Parsed, String> {
                    $parser
                }
            }

            $(impl TypedValueEncoder for $name {
                type Encodable = $encodable_type;

                fn encode($encodable_ident: $encodable_type) -> RuntimeValue {
                    $encoder
                }
            })?
        )+
    };
}

// Implement type handlers for the most basic types
declare_basic_type_handlers!(
    AnyType (Any) = RuntimeValue {
       fn parse(value) { Ok(value) }
       fn encode(value: RuntimeValue) { value }
    },

    NullType (Null) = () {
        fn parse(value) {
            match value {
                RuntimeValue::Null => Ok(()),
                _ => Err("expected the null value".to_owned())
            }
        }

        fn encode(__: ()) { RuntimeValue::Null }
    },

    BoolType (Bool) = bool {
        fn parse(value) {
            match value {
                RuntimeValue::Bool(inner) => Ok(inner),
                _ => Err("expected a boolean".to_owned())
            }
        }

        fn encode(value: bool) { RuntimeValue::Bool(value) }
    },

    IntType (Int) = i64 {
        fn parse(value) {
            match value {
                RuntimeValue::Int(inner) => Ok(inner),
                _ => Err("expected an integer".to_owned())
            }
        }

        fn encode(value: i64) { RuntimeValue::Int(value) }
    },

    FloatType (Float) = f64 {
        fn parse(value) {
            match value {
                RuntimeValue::Float(inner) => Ok(inner),
                _ => Err("expected a float".to_owned())
            }
        }

        fn encode(value: f64) { RuntimeValue::Float(value) }
    },

    StringType (String) = String {
        fn parse(value) {
            match value {
                RuntimeValue::String(inner) => Ok(inner),
                _ => Err("expected a string".to_owned())
            }
        }

        fn encode(value: String) { RuntimeValue::String(value) }
    },

    DateTimeType (DateTime) = Arc<Zoned> {
        fn parse(value) {
            match value {
                RuntimeValue::DateTime(inner) => Ok(inner),
                _ => Err("expected a datetime".to_owned())
            }
        }

        fn encode(value: Arc<Zoned>) { RuntimeValue::DateTime(value) }
    },

    DurationType (Duration) = Duration {
        fn parse(value) {
            match value {
                RuntimeValue::Duration(inner) => Ok(inner),
                _ => Err("expected a duration".to_owned())
            }
        }

        fn encode(value: Duration) { RuntimeValue::Duration(value) }
    },

    InstantType (Instant) = Instant {
        fn parse(value) {
            match value {
                RuntimeValue::Instant(inner) => Ok(inner),
                _ => Err("expected an anstant".to_owned())
            }
        }

        fn encode(value: Instant) { RuntimeValue::Instant(value) }
    },

    RegexType (Regex) = Arc<Regex> {
        fn parse(value) {
            match value {
                RuntimeValue::Regex(inner) => Ok(inner),
                _ => Err("expected a regex".to_owned())
            }
        }

        fn encode(value: Arc<Regex>) { RuntimeValue::Regex(value) }
    },

    RangeType (Range) = RangeValue {
        fn parse(value) {
            match value {
                RuntimeValue::Range(inner) => Ok(inner),
                _ => Err("expected a range".to_owned())
            }
        }

        fn encode(value: RangeValue) { RuntimeValue::Range(value) }
    },

    ErrorType (Error) = (CodeRange, RuntimeValue) {
        fn parse(value) {
            match value {
                RuntimeValue::Error(err) => {
                    let ErrorValueContent { at, data } = *err;
                    Ok((at, data.clone()))
                },

                _ => Err("expected an error".to_owned())
            }
        }
    },

    CmdCallType (CmdCall) = CodeRange {
        fn parse(value) {
            match value {
                RuntimeValue::CmdCall { content_at } => Ok(content_at),
                _ => Err("expected a command call".to_owned())
            }
        }
    },

    UntypedListType (UntypedList) = GcCell<Vec<RuntimeValue>> {
        fn parse(value) {
            match value {
                RuntimeValue::List(items) => Ok(items),
                _ => Err("expected a list".to_owned())
            }
        }

        fn encode(items: Vec<RuntimeValue>) { RuntimeValue::List(GcCell::new(items)) }
    },

    UntypedMapType (UntypedMap) = GcCell<IndexMap<String, RuntimeValue>> {
        fn parse(value) {
            match value {
                RuntimeValue::Map(items) => Ok(items),
                _ => Err("expected a map".to_owned())
            }
        }

        fn encode(items: IndexMap<String, RuntimeValue>) { RuntimeValue::Map(GcCell::new(items)) }
    },

    UntypedStructType (UntypedStruct) = GcCell<IndexMap<String, RuntimeValue>> {
        fn parse(value) {
            match value {
                RuntimeValue::Struct(members) => Ok(members),
                _ => Err("expected a struct".to_owned())
            }
        }

        fn encode(members: IndexMap<String, RuntimeValue>) { RuntimeValue::Struct(GcCell::new(members)) }
    },

    CmdArgType (CmdArg) = Box<CmdArgValue> {
        fn parse(value) {
            match value {
                RuntimeValue::CmdArg(arg) => Ok(arg),
                _ => Err("expected a command argument value".to_owned())
            }
        }
    }
);

/// Type handler for the 'void' type
///
/// Only used to *expose* that type
pub struct VoidType;

impl TypedValueParser for VoidType {
    fn value_type() -> ValueType {
        ValueType::Single(SingleValueType::Void)
    }

    type Parsed = ();

    fn parse(_: RuntimeValue) -> Result<Self::Parsed, String> {
        Err("Can't parse a void value".to_owned())
    }
}

/// Type handler for a specific integer type
pub struct ExactIntType<From: RustIntType> {
    _f: PhantomData<From>,
}

impl<From: RustIntType> TypedValueParser for ExactIntType<From> {
    fn value_type() -> ValueType {
        ValueType::Single(SingleValueType::Int)
    }

    type Parsed = From;

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Int(int) => From::try_from(int).map_err(|_| {
                if int < 0 && From::MIN.to_i128() == 0 {
                    "expected a positive integer".to_owned()
                } else if int > 0 && From::MAX.to_i128() == 0 {
                    "expected a negative integer".to_owned()
                } else {
                    format!(
                        "expected an integer between {} and {}",
                        From::MIN,
                        From::MAX
                    )
                }
            }),
            _ => Err("expected an integer".to_owned()),
        }
    }
}

impl<From: RustIntType> TypedValueEncoder for ExactIntType<From> {
    type Encodable = From;

    fn encode(value: Self::Encodable) -> RuntimeValue {
        RuntimeValue::Int(
            value
                .try_into()
                .unwrap_or_else(|_| panic!("value is out of bounds for the integer type: {value}")),
        )
    }
}

/// Trait representing a specific Rust integer type
pub trait RustIntType: TryFrom<i64> + TryInto<i64> + Display + Copy + std::fmt::Debug {
    const MIN: Self;
    const MAX: Self;

    fn to_i128(self) -> i128;
}

/// Macro to add support for conversion between the shell's integer type and Rust's ones
macro_rules! implement_specific_int_types {
    ($($int_type: ident),+) => {
        $(
            impl RustIntType for $int_type {
                const MIN: Self = Self::MIN;
                const MAX: Self = Self::MAX;

                fn to_i128(self) -> i128 {
                    i128::try_from(self).unwrap()
                }
            }
        )+
    };
}

implement_specific_int_types!(u8, u16, u32, u64, i8, i16, i32, i64, usize);

/// Type handler for the literal string type
#[macro_export]
macro_rules! declare_string_literal_type {
    ($pub: vis $name: ident => enum $result_enum: ident { $($variant_name: ident ($string: literal)),+ }) => {
        $pub struct $name;

        impl $crate::helpers::args::TypedValueParser for $name {
            fn value_type() -> ::reshell_parser::ast::ValueType {
                ::reshell_parser::ast::ValueType::Union(vec![$(::reshell_parser::ast::SingleValueType::StringLiteral($string.to_owned())),+])
            }

            type Parsed = $result_enum;

            fn parse(value: ::reshell_runtime::values::RuntimeValue) -> Result<Self::Parsed, String> {
                match value {
                    ::reshell_runtime::values::RuntimeValue::String(string) => {
                        let string = string.as_str();

                        $(
                            if string == $string {
                                return Ok($result_enum::$variant_name);
                            }
                        )+

                        Err("none of string literal choices matched".to_owned())
                    },

                    _ => Err("expected a string".to_owned())
                }
            }
        }

        $pub enum $result_enum {
            $($variant_name),+
        }
    };
}

/// Type handler that clones a list before accessing it
pub struct DetachedListType<Inner: TypedValueParser> {
    _i: PhantomData<Inner>,
}

impl<Inner: TypedValueParser> TypedValueParser for DetachedListType<Inner> {
    fn value_type() -> ValueType {
        ValueType::Single(SingleValueType::TypedList(Box::new(Inner::value_type())))
    }

    type Parsed = Vec<Inner::Parsed>;

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
        let list = match value {
            RuntimeValue::List(list) => list,
            _ => return Err("expected a list".to_owned()),
        };

        let values = list
            .read_promise_no_write()
            .iter()
            .cloned()
            .map(|item| Inner::parse(item))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(values)
    }
}

impl<Inner: TypedValueParser + TypedValueEncoder> TypedValueEncoder for DetachedListType<Inner> {
    type Encodable = Vec<Inner::Encodable>;

    fn encode(value: Self::Encodable) -> RuntimeValue {
        RuntimeValue::List(GcCell::new(value.into_iter().map(Inner::encode).collect()))
    }
}

/// Type handler that clones a map before accessing it
pub struct DetachedMapType<Inner: TypedValueParser> {
    _i: PhantomData<Inner>,
}

impl<Inner: TypedValueParser> TypedValueParser for DetachedMapType<Inner> {
    fn value_type() -> ValueType {
        ValueType::Single(SingleValueType::TypedMap(Box::new(Inner::value_type())))
    }

    type Parsed = HashMap<String, Inner::Parsed>;

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
        let map = match value {
            RuntimeValue::Map(map) => map,
            _ => return Err("expected a map".to_owned()),
        };

        let values = map
            .read_promise_no_write()
            .iter()
            .map(|(key, item)| Inner::parse(item.clone()).map(|parsed| (key.clone(), parsed)))
            .collect::<Result<HashMap<_, _>, _>>()?;

        Ok(values)
    }
}

impl<Inner: TypedValueParser + TypedValueEncoder> TypedValueEncoder for DetachedMapType<Inner> {
    type Encodable = HashMap<String, Inner::Encodable>;

    fn encode(value: Self::Encodable) -> RuntimeValue {
        RuntimeValue::Map(GcCell::new(
            value
                .into_iter()
                .map(|(key, value)| (key, Inner::encode(value)))
                .collect(),
        ))
    }
}

/// Type handler that accepts either the provided generic type or the `null` value
pub struct NullableType<Inner: TypedValueParser> {
    _i: PhantomData<Inner>,
}

impl<Inner: TypedValueParser> TypedValueParser for NullableType<Inner> {
    fn value_type() -> ValueType {
        let mut union = match Inner::value_type() {
            ValueType::Single(single_value_type) => vec![single_value_type],
            ValueType::Union(vec) => vec,
        };

        union.push(SingleValueType::Null);

        ValueType::Union(union)
    }

    type Parsed = Option<Inner::Parsed>;

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Null => Ok(None),
            _ => Ok(Some(Inner::parse(value)?)),
        }
    }
}

impl<Inner: TypedValueParser + TypedValueEncoder> TypedValueEncoder for NullableType<Inner> {
    type Encodable = Option<Inner::Encodable>;

    fn encode(value: Self::Encodable) -> RuntimeValue {
        match value {
            Some(value) => Inner::encode(value),
            None => RuntimeValue::Null,
        }
    }
}

/// Macro to implement a type handler for a union type
macro_rules! generic_type_union_handler {
    ($handler_struct: ident ($($generic: ident),+) => $result_struct: ident) => {
        #[allow(non_snake_case)]
        pub struct $handler_struct<$($generic: TypedValueParser),+> {
            $( #[allow(dead_code)] $generic: $generic ),+
        }

        impl<$($generic: TypedValueParser),+> TypedValueParser
            for $handler_struct<$($generic,)+>
        {
            fn value_type() -> ValueType {
                let mut types = vec![];

                $(
                    match $generic::value_type() {
                        ValueType::Single(single) => types.push(single),
                        ValueType::Union(sub_types) => {
                            types.extend(sub_types);
                        }
                    }
                )+

                ValueType::Union(types)
            }

            type Parsed = $result_struct<$($generic,)+>;

            fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
                $(
                    if let Ok(parsed) = $generic::parse(value.clone()) {
                        return Ok($result_struct::$generic(parsed))
                    }
                )+;

                Err("union error".to_owned())
            }
        }

        pub enum $result_struct<$($generic: TypedValueParser),+> {
            $( $generic($generic::Parsed), )+
        }

        impl<$($generic: TypedValueParser),+> $result_struct<$($generic),+> {
            #[allow(dead_code, clippy::wrong_self_convention)]
            pub fn from_value_type(&self) -> ValueType {
                match self {
                    $(Self::$generic(_) => $generic::value_type()),+
                }
            }
        }
    };
}

// Create union type handlers
generic_type_union_handler!(Union2Type (A, B) => Union2Result);
// generic_type_union_handler!(Union3Type (A, B, C) => Union3Result);
// generic_type_union_handler!(Union4Type (A, B, C, D) => Union4Result);

#[macro_export]
macro_rules! declare_typed_union_handler {
    ($pub: vis $name: ident => enum $result_enum: ident { $($variant_name: ident ($variant_type: ty)),+ }) => {
        $pub struct $name;

        impl $crate::helpers::args::TypedValueParser for $name {
            fn value_type() -> ::reshell_parser::ast::ValueType {
                let mut type_union = vec![];

                $(
                    match <$variant_type as $crate::helpers::args::TypedValueParser>::value_type() {
                        ::reshell_parser::ast::ValueType::Single(single) => type_union.push(single),
                        ::reshell_parser::ast::ValueType::Union(sub_types) => {
                            type_union.extend(sub_types);
                        }
                    }
                )+

                ::reshell_parser::ast::ValueType::Union(type_union)
            }

            type Parsed = $result_enum;

            fn parse(value: ::reshell_runtime::values::RuntimeValue) -> Result<Self::Parsed, String> {
                $(
                    if let Ok(parsed) = <$variant_type as $crate::helpers::args::TypedValueParser>::parse(value.clone()) {
                        return Ok($result_enum::$variant_name(parsed))
                    }
                )+

                Err("union error".to_owned())
            }
        }

        impl $result_enum {
            #[allow(dead_code)]
            pub fn original_value_type(&self) -> ::reshell_parser::ast::ValueType {
                match self {
                    $(Self::$variant_name(_) => <$variant_type as $crate::helpers::args::TypedValueParser>::value_type()),+
                }
            }
        }

        $pub enum $result_enum {
            $($variant_name(<$variant_type as $crate::helpers::args::TypedValueParser>::Parsed)),+
        }
    }
}

/// Macro to create a struct type handler (without encoding support)
#[macro_export]
macro_rules! declare_typed_struct_handler {
    ($(#[$proc_macro: meta])? $pub: vis $struct: ident {
        $( $(#[$member_meta: meta])* $field_pub: vis $name: ident: $parser: ty ),+
    }) => {
        $(#[$proc_macro])?
        $pub struct $struct {
            $(
                $(#[$member_meta])* $field_pub $name: <$parser as $crate::helpers::args::TypedValueParser>::Parsed
            ),+
        }

        impl $crate::helpers::args::TypedValueParser for $struct {
            fn value_type() -> ::reshell_parser::ast::ValueType {
                ::reshell_parser::ast::ValueType::Single(::reshell_parser::ast::SingleValueType::TypedStruct(vec![
                    $(
                        ::reshell_parser::ast::StructTypeMember {
                            name: ::reshell_parser::ast::RuntimeSpan::internal(
                                "native library's type generator",
                                $crate::helpers::types::camel_case(stringify!($name)).into_owned()
                            ),
                            typ: <$parser as $crate::helpers::args::TypedValueParser>::value_type(),
                            optional: false
                        }
                    ),+
                ]))
            }

            #[allow(unused_parens)]
            type Parsed = Self;

            fn parse(value: ::reshell_runtime::values::RuntimeValue) -> Result<Self::Parsed, String> {
                let members = match value {
                    ::reshell_runtime::values::RuntimeValue::Struct(members) => members,
                    _ => return Err("expected a struct".to_owned()),
                };

                let members = members.read_promise_no_write();

                Ok(Self {
                    $($name: {
                        let name = $crate::helpers::types::camel_case(stringify!($name));

                        let value = members
                            .get(name.as_ref())
                            .ok_or_else(|| format!("property '{name}' is missing"))?;

                        <$parser>::parse(value.clone())
                            .map_err(|err| format!("type mismatch in struct member name {err}"))?
                    }),+
                })
            }
        }

        impl $crate::helpers::args::TypedValueEncoder for $struct {
            type Encodable = Self;

            fn encode(value: Self) -> ::reshell_runtime::values::RuntimeValue {
                let mut members = ::indexmap::IndexMap::new();

                $(
                    members.insert($crate::helpers::types::camel_case(stringify!($name)).into_owned(), <$parser as $crate::helpers::args::TypedValueEncoder>::encode(value.$name));
                )+

                ::reshell_runtime::values::RuntimeValue::Struct(::reshell_runtime::gc::GcCell::new(members))
            }
        }
    }
}

/// Macro to create a function type handler
#[macro_export]
macro_rules! declare_typed_fn_handler {
    ($( $(#[$proc_macro: meta])? $pub: vis $typename: ident ($($argname:ident : $argtype:ty),*) ),+ -> $rettype:ty ) => {
        $(
            $(#[$proc_macro])?
            $pub struct $typename;

            impl $typename {
                pub fn signature() -> ::reshell_parser::ast::FnSignature {
                    $crate::utils::forge_basic_fn_signature(
                        vec![
                            $( (stringify!($argname), <$argtype>::value_type()) ),*
                        ],
                        Some(<$rettype>::value_type())
                    )
                }
            }

            impl $crate::helpers::args::TypedValueParser for $typename {
                fn value_type() -> ::reshell_parser::ast::ValueType {
                    ::reshell_parser::ast::ValueType::Single(::reshell_parser::ast::SingleValueType::Function(
                        ::reshell_parser::ast::RuntimeSpan::internal(
                            "native library's type generator",
                            Self::signature(),
                        ),
                    ))
                }

                type Parsed = ::std::sync::Arc<::reshell_runtime::values::RuntimeFnValue>;

                fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
                    // NOTE: we don't check the function as it was already checked by the runtime
                    // at call time
                    match value {
                        RuntimeValue::Function(func) => Ok(func),
                        _ => Err("expected a function".to_owned()),
                    }
                }
            }
        )+
    }
}

/// Utility type enabling a type to be used in a context where a [`TypedValueEncoder`] is required,
/// but where the type only supports encoding in *some* contexts
// e.g. `NullableType<NonEncodableType>` => can be encoded if `None`
///
/// * When used as a parser, passes through to the underlying type
/// * When used as an encoder, panics
///
/// Usage example: `NullableType<FakeEncodableWrapper<NonEncodableType>>`
pub struct NonEncodableWrapper<T: TypedValueParser> {
    _t: PhantomData<T>,
}

impl<T: TypedValueParser> TypedValueParser for NonEncodableWrapper<T> {
    fn value_type() -> ValueType {
        T::value_type()
    }

    type Parsed = T::Parsed;

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
        T::parse(value)
    }
}

impl<T: TypedValueParser> TypedValueEncoder for NonEncodableWrapper<T> {
    type Encodable = T::Parsed;

    fn encode(_: Self::Encodable) -> RuntimeValue {
        unreachable!("Cannot encode the currently wrapped type")
    }
}

pub fn camel_case<'a>(input: &'a str) -> Cow<'a, str> {
    if !input.contains('-') && !input.contains('_') {
        return Cow::Borrowed(input);
    }

    let mut out = String::with_capacity(input.len());

    let mut chars = input.chars();
    let mut next_chars = input.chars().skip(1);

    while let Some(c) = chars.next() {
        let next_c = next_chars.next();

        match c {
            '-' | '_' => match next_c {
                Some(next_c) => {
                    chars.next();
                    next_chars.next();

                    for char in next_c.to_uppercase() {
                        out.push(char);
                    }
                }

                None => break,
            },

            _ => out.push(c),
        }
    }

    Cow::Owned(out)
}

pub struct WithOriginalValue<T: TypedValueParser> {
    _t: PhantomData<T>,
}

impl<T: TypedValueParser> TypedValueParser for WithOriginalValue<T> {
    fn value_type() -> ValueType {
        T::value_type()
    }

    type Parsed = (T::Parsed, RuntimeValue);

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
        T::parse(value.clone()).map(|parsed| (parsed, value))
    }
}
