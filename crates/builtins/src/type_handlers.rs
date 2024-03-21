//!
//! This module defines and exposes *type handlers*, which are simple types that
//! allow to convert and parse some of the scripting language's native types.
//!

use std::{collections::HashMap, fmt::Display, marker::PhantomData};

use colored::Colorize;
use parsy::CodeRange;
use reshell_parser::ast::{
    FnSignature, RuntimeEaten, SingleValueType, StructTypeMember, ValueType,
};

use reshell_runtime::{
    gc::{GcCell, GcReadOnlyCell},
    values::{ErrorValueContent, RuntimeFnValue, RuntimeValue},
};

use crate::helper::{SingleTyping, SingleTypingDirectCreation, Typing, TypingDirectCreation};

macro_rules! declare_basic_type_handlers {
    ($($name: ident ($variant: ident) = $type: ty => $value_ident: ident: $parser: expr),+) => {
        $(
            pub struct $name;

            impl SingleTyping for $name {
                fn underlying_single_type(&self) -> SingleValueType {
                    SingleValueType::$variant
                }

                type Parsed = $type;

                fn parse(&self, $value_ident: RuntimeValue) -> Result<Self::Parsed, String> {
                    $parser
                }
            }

            impl SingleTypingDirectCreation for $name {
                fn new_single_direct() -> Self {
                    Self
                }
            }
        )+
    };
}

declare_basic_type_handlers!(
    AnyType (Any) = RuntimeValue => value: Ok(value),

    NullType (Null) = () => value: match value {
        RuntimeValue::Null => Ok(()),
        _ => Err("expected the null value".to_owned())
    },

    BoolType (Bool) = bool => value: match value {
        RuntimeValue::Bool(inner) => Ok(inner),
        _ => Err("expected a boolean".to_owned())
    },

    IntType (Int) = i64 => value: match value {
        RuntimeValue::Int(inner) => Ok(inner),
        _ => Err("expected an integer".to_owned())
    },

    FloatType (Float) = f64 => value: match value {
        RuntimeValue::Float(inner) => Ok(inner),
        _ => Err("expected a float".to_owned())
    },

    StringType (String) = String => value: match value {
        RuntimeValue::String(inner) => Ok(inner),
        _ => Err("expected a string".to_owned())
    },

    RangeType (Range) = (i64, i64) => value: match value {
        RuntimeValue::Range { from, to } => Ok((from, to)),
        _ => Err("expected a range".to_owned())
    },

    ErrorType (Error) = (CodeRange, String) => value: match value {
        RuntimeValue::Error(err) => {
            let ErrorValueContent { at, msg } = *err;
            Ok((at, msg.clone()))
        },

        _ => Err("expected an error".to_owned())
    },

    UntypedListType (List) = GcCell<Vec<RuntimeValue>> => value: match value {
        RuntimeValue::List(items) => Ok(items),
        _ => Err("expected a list".to_owned())
    },

    UntypedMapType (Map) = GcCell<HashMap<String, RuntimeValue>> => value: match value {
        RuntimeValue::Map(items) => Ok(items),
        _ => Err("expected a map".to_owned())
    },

    UntypedStructType (UntypedStruct) = GcCell<HashMap<String, RuntimeValue>> => value: match value {
        RuntimeValue::Struct(members) => Ok(members),
        _ => Err("expected a struct".to_owned())
    }
);

pub struct ExactIntType<From: SpecificIntType> {
    _f: PhantomData<From>,
}

impl<From: SpecificIntType> SingleTyping for ExactIntType<From> {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::Int
    }

    type Parsed = From;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Int(int) => From::try_from(int).map_err(|_| {
                format!(
                    "expected an integer between {} and {}",
                    From::MIN,
                    From::MAX
                )
            }),
            _ => Err("expected an integer".to_owned()),
        }
    }
}

impl<From: SpecificIntType> SingleTypingDirectCreation for ExactIntType<From> {
    fn new_single_direct() -> Self {
        Self { _f: PhantomData }
    }
}

pub trait SpecificIntType: TryFrom<i64> + Display {
    const MIN: Self;
    const MAX: Self;
}

macro_rules! implement_specific_int_types {
    ($($int_type: ident),+) => {
        $(
            impl SpecificIntType for $int_type {
                const MIN: Self = Self::MIN;
                const MAX: Self = Self::MAX;
            }
        )+
    };
}

implement_specific_int_types!(u8, u16, u32, u64, i8, i16, i32, i64, usize);

pub struct DetachedListType<Inner: Typing> {
    inner: Inner,
}

impl<Inner: Typing> DetachedListType<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self { inner }
    }
}

impl<Inner: Typing> SingleTyping for DetachedListType<Inner> {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::List
    }

    type Parsed = Vec<Inner::Parsed>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        let list = match value {
            RuntimeValue::List(list) => list,
            _ => return Err("expected a list".to_owned()),
        };

        let values = list.with_ref(|items| {
            items
                .clone()
                .into_iter()
                .map(|item| self.inner.parse(item))
                .collect::<Result<Vec<_>, _>>()
        })?;

        Ok(values)
    }
}

impl<Inner: TypingDirectCreation> SingleTypingDirectCreation for DetachedListType<Inner> {
    fn new_single_direct() -> Self {
        Self::new(Inner::new_direct())
    }
}

pub struct MapType;

impl SingleTyping for MapType {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::Map
    }

    type Parsed = GcCell<HashMap<String, RuntimeValue>>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Map(map) => Ok(map),
            _ => Err("expected a map".to_owned()),
        }
    }
}

impl SingleTypingDirectCreation for MapType {
    fn new_single_direct() -> Self {
        Self
    }
}

pub struct NullableType<Inner: SingleTyping> {
    inner: Inner,
}

impl<Inner: SingleTyping> NullableType<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self { inner }
    }
}

impl<Inner: SingleTyping> Typing for NullableType<Inner> {
    fn underlying_type(&self) -> ValueType {
        ValueType::Union(vec![
            RuntimeEaten::Internal(self.inner.underlying_single_type()),
            RuntimeEaten::Internal(NullType.underlying_single_type()),
        ])
    }

    type Parsed = Option<Inner::Parsed>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Null => Ok(None),
            _ => Ok(Some(self.inner.parse(value)?)),
        }
    }
}

impl<Inner: SingleTypingDirectCreation> TypingDirectCreation for NullableType<Inner> {
    fn new_direct() -> Self {
        Self::new(Inner::new_single_direct())
    }
}

pub struct TypedFunctionType {
    signature: FnSignature,
}

impl TypedFunctionType {
    pub fn new(signature: FnSignature) -> Self {
        Self { signature }
    }

    pub fn signature(&self) -> &FnSignature {
        &self.signature
    }
}

impl SingleTyping for TypedFunctionType {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::Function(RuntimeEaten::Internal(self.signature.clone()))
    }

    type Parsed = GcReadOnlyCell<RuntimeFnValue>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Function(func) => {
                // NOTE: we don't check the function as it was already checked by the runtime
                // at call time
                Ok(func)
            }
            _ => Err("expected a function".to_owned()),
        }
    }
}

pub struct Tuple2Type<A: Typing, B: Typing> {
    a: A,
    b: B,
}

// impl<A: Typing, B: Typing> Tuple2Type<A, B> {
//     pub fn new(a: A, b: B) -> Self {
//         Self { a, b }
//     }
// }

impl<A: Typing, B: Typing> SingleTyping for Tuple2Type<A, B> {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::List
    }

    type Parsed = (A::Parsed, B::Parsed);

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        let items = match value {
            RuntimeValue::List(items) => items,
            _ => return Err("expected a tuple (list)".to_owned()),
        };

        items.with_ref(|items| {
            if items.len() != 2 {
                return Err(format!(
                    "tuple is expected to contain exactly 2 elements, contains {}",
                    items.len()
                ));
            }

            let a = self
                .a
                .parse(items[0].clone())
                .map_err(|err| format!("error in tuple's first element: {err}"))?;

            let b = self
                .b
                .parse(items[1].clone())
                .map_err(|err| format!("error in tuple's second element: {err}"))?;

            Ok((a, b))
        })
    }
}

impl<A: TypingDirectCreation, B: TypingDirectCreation> SingleTypingDirectCreation
    for Tuple2Type<A, B>
{
    fn new_single_direct() -> Self {
        Self {
            a: A::new_direct(),
            b: B::new_direct(),
        }
    }
}

pub struct Union2Type<A: SingleTyping, B: SingleTyping> {
    a: A,
    b: B,
}

impl<A: SingleTyping, B: SingleTyping> Union2Type<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A: SingleTyping, B: SingleTyping> Typing for Union2Type<A, B> {
    fn underlying_type(&self) -> ValueType {
        ValueType::Union(vec![
            RuntimeEaten::Internal(self.a.underlying_single_type()),
            RuntimeEaten::Internal(self.b.underlying_single_type()),
        ])
    }

    type Parsed = Union2Result<A, B>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match self.a.parse(value.clone()) {
            Ok(value) => Ok(Union2Result::A(value)),
            Err(a_err) => match self.b.parse(value) {
                Ok(value) => Ok(Union2Result::B(value)),
                Err(b_err) => Err(format!(
                    "union error: {a_err} {} {b_err}",
                    "/".bright_yellow()
                )),
            },
        }
    }
}

impl<A: SingleTypingDirectCreation, B: SingleTypingDirectCreation> TypingDirectCreation
    for Union2Type<A, B>
{
    fn new_direct() -> Self {
        Self::new(A::new_single_direct(), B::new_single_direct())
    }
}

pub enum Union2Result<A: SingleTyping, B: SingleTyping> {
    A(A::Parsed),
    B(B::Parsed),
}

pub struct Union3Type<A: SingleTyping, B: SingleTyping, C: SingleTyping> {
    a: A,
    b: B,
    c: C,
}

impl<A: SingleTyping, B: SingleTyping, C: SingleTyping> Union3Type<A, B, C> {
    pub fn new(a: A, b: B, c: C) -> Self {
        Self { a, b, c }
    }
}

impl<A: SingleTyping, B: SingleTyping, C: SingleTyping> Typing for Union3Type<A, B, C> {
    fn underlying_type(&self) -> ValueType {
        ValueType::Union(vec![
            RuntimeEaten::Internal(self.a.underlying_single_type()),
            RuntimeEaten::Internal(self.b.underlying_single_type()),
            RuntimeEaten::Internal(self.c.underlying_single_type()),
        ])
    }

    type Parsed = Union3Result<A, B, C>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match self.a.parse(value.clone()) {
            Ok(value) => Ok(Union3Result::A(value)),
            Err(a_err) => match self.b.parse(value.clone()) {
                Ok(value) => Ok(Union3Result::B(value)),
                Err(b_err) => match self.c.parse(value) {
                    Ok(c) => Ok(Union3Result::C(c)),
                    Err(c_err) => Err(format!(
                        "union error: {a_err} {} {b_err} {} {c_err}",
                        "/".bright_yellow(),
                        "/".bright_yellow()
                    )),
                },
            },
        }
    }
}

impl<
        A: SingleTypingDirectCreation,
        B: SingleTypingDirectCreation,
        C: SingleTypingDirectCreation,
    > TypingDirectCreation for Union3Type<A, B, C>
{
    fn new_direct() -> Self {
        Self::new(
            A::new_single_direct(),
            B::new_single_direct(),
            C::new_single_direct(),
        )
    }
}

pub enum Union3Result<A: SingleTyping, B: SingleTyping, C: SingleTyping> {
    A(A::Parsed),
    B(B::Parsed),
    C(C::Parsed),
}

macro_rules! declared_typed_struct_type_handler {
    ($( $struct: ident { $( $member: ident : $generic: ident ),+ } ),+ ) => {
        $(
            pub struct $struct<$($generic: Typing),+> {
                $($member: (String, $generic)),+
            }

            impl<$($generic: Typing),+> $struct<$($generic),+> {
                pub fn new( $($member: (impl Into<String>, $generic)),+ ) -> Self {
                    Self { $( $member: ($member.0.into(), $member.1) ),+ }
                }
            }

            impl<$($generic: Typing),+> SingleTyping for $struct<$($generic),+> {
                fn underlying_single_type(&self) -> SingleValueType {
                    SingleValueType::TypedStruct(vec![
                        $(
                            RuntimeEaten::Internal(StructTypeMember {
                                name: RuntimeEaten::Internal(self.$member.0.clone()),
                                typ: RuntimeEaten::Internal(self.$member.1.underlying_type())
                            })
                        ),+
                    ])
                }

                #[allow(unused_parens)]
                type Parsed = ( $( $generic::Parsed ),+ );

                fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
                    let members = match value {
                        RuntimeValue::Struct(members) => members,
                        _ => return Err("expected a struct".to_owned()),
                    };

                    members.with_ref(|members| {
                        $(
                            let $member = members
                                .get(&self.$member.0)
                                .ok_or_else(|| format!("property '{}' is missing", self.$member.0))?;

                            let $member = self
                                .$member
                                .1
                                .parse($member.clone())
                                .map_err(|err| format!("type mismatch in struct member '{}': {err}", self.$member.0))?;
                        )+

                        Ok(( $( $member ),+ ))
                    })
                }
            }
        )+
    }
}

declared_typed_struct_type_handler!(
    TypedStruct1Type { a: A },
    TypedStruct3Type { a: A, b: B, c: C },
    TypedStruct4Type {
        a: A,
        b: B,
        c: C,
        d: D
    }
);
