use std::{fmt::Display, marker::PhantomData};

use indexmap::IndexMap;
use parsy::{CodeRange, MaybeEaten};
use reshell_parser::ast::{FnSignature, SingleValueType, ValueType};

use crate::{
    gc::{GcCell, GcReadOnlyCell},
    values::{RuntimeFnValue, RuntimeValue},
};

use super::{
    helper::{ArgSingleTyping, ArgSingleTypingDirectCreation, ArgTyping, ArgTypingDirectCreation},
    utils::forge_internal_token,
};

macro_rules! declare_basic_types {
    ($($name: ident ($variant: ident) = $type: ty => $value_ident: ident: $parser: expr),+) => {
        $(
            pub struct $name;

            impl ArgSingleTyping for $name {
                fn arg_single_type() -> SingleValueType {
                    SingleValueType::$variant
                }

                type Parsed = $type;

                fn parse(&self, $value_ident: RuntimeValue) -> Result<Self::Parsed, String> {
                    $parser
                }
            }

            impl ArgSingleTypingDirectCreation for $name {
                fn new_direct() -> Self {
                    Self
                }
            }
        )+
    };
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

declare_basic_types!(
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

    RangeType (Range) = (usize, usize) => value: match value {
        RuntimeValue::Range { from, to } => Ok((from, to)),
        _ => Err("expected a range".to_owned())
    },

    ErrorType (Error) = (CodeRange, String) => value: match value {
        RuntimeValue::Error { at, msg } => Ok((at, msg)),
        _ => Err("expected an error".to_owned())
    },

    UntypedListType (List) = GcCell<Vec<RuntimeValue>> => value: match value {
        RuntimeValue::List(items) => Ok(items),
        _ => Err("expected a list".to_owned())
    },

    UntypedMapType (Map) = GcCell<IndexMap<String, RuntimeValue>> => value: match value {
        RuntimeValue::Map(items) => Ok(items),
        _ => Err("expected a map".to_owned())
    },

    UntypedStructType (UntypedStruct) = GcCell<IndexMap<String, RuntimeValue>> => value: match value {
        RuntimeValue::Struct(members) => Ok(members),
        _ => Err("expected a struct".to_owned())
    }
);

pub struct ExactIntType<From: SpecificIntType> {
    _f: PhantomData<From>,
}

impl<From: SpecificIntType> ArgSingleTyping for ExactIntType<From> {
    fn arg_single_type() -> SingleValueType {
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

impl<From: SpecificIntType> ArgSingleTypingDirectCreation for ExactIntType<From> {
    fn new_direct() -> Self {
        Self { _f: PhantomData }
    }
}

pub trait SpecificIntType: TryFrom<i64> + Display {
    const MIN: Self;
    const MAX: Self;
}

implement_specific_int_types!(u8, u16, u32, u64, i8, i16, i32, i64, usize);

pub struct DetachedListType<Inner: ArgTyping> {
    inner: Inner,
}

impl<Inner: ArgTyping> DetachedListType<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self { inner }
    }
}

impl<Inner: ArgTyping> ArgTyping for DetachedListType<Inner> {
    fn arg_type() -> ValueType {
        ValueType::Single(MaybeEaten::Raw(SingleValueType::List))
    }

    type Parsed = Vec<Inner::Parsed>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        let list = match value {
            RuntimeValue::List(list) => list,
            _ => return Err("expected a list".to_owned()),
        };

        let values = list
            .read()
            .clone()
            .into_iter()
            .map(|item| self.inner.parse(item))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(values)
    }
}

impl<Inner: ArgTypingDirectCreation> ArgTypingDirectCreation for DetachedListType<Inner> {
    fn new_direct() -> Self {
        Self::new(Inner::new_direct())
    }
}

pub struct UntypedFunctionType;

impl ArgSingleTypingDirectCreation for UntypedFunctionType {
    fn new_direct() -> Self {
        Self
    }
}

impl ArgSingleTyping for UntypedFunctionType {
    fn arg_single_type() -> SingleValueType {
        SingleValueType::Function(
            // Universal signature
            FnSignature {
                args: forge_internal_token(vec![]),
                ret_type: None,
            },
        )
    }

    type Parsed = GcReadOnlyCell<RuntimeFnValue>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Function(func) => Ok(func),
            _ => Err("expected a function".to_owned()),
        }
    }
}

pub struct Union2Type<A: ArgSingleTyping, B: ArgSingleTyping> {
    a: A,
    b: B,
}

impl<A: ArgSingleTyping, B: ArgSingleTyping> Union2Type<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A: ArgSingleTyping, B: ArgSingleTyping> ArgTyping for Union2Type<A, B> {
    fn arg_type() -> ValueType {
        ValueType::Union(vec![
            MaybeEaten::Raw(A::arg_single_type()),
            MaybeEaten::Raw(B::arg_single_type()),
        ])
    }

    type Parsed = Union2Result<A, B>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match self.a.parse(value.clone()) {
            Ok(value) => Ok(Union2Result::A(value)),
            Err(a_err) => match self.b.parse(value) {
                Ok(value) => Ok(Union2Result::B(value)),
                Err(b_err) => Err(format!("union error: {a_err} | {b_err}")),
            },
        }
    }
}

impl<A: ArgSingleTypingDirectCreation, B: ArgSingleTypingDirectCreation> ArgTypingDirectCreation
    for Union2Type<A, B>
{
    fn new_direct() -> Self {
        Self::new(A::new_direct(), B::new_direct())
    }
}

pub enum Union2Result<A: ArgSingleTyping, B: ArgSingleTyping> {
    A(A::Parsed),
    B(B::Parsed),
}

pub struct Union3Type<A: ArgSingleTyping, B: ArgSingleTyping, C: ArgSingleTyping> {
    a: A,
    b: B,
    c: C,
}

impl<A: ArgSingleTyping, B: ArgSingleTyping, C: ArgSingleTyping> Union3Type<A, B, C> {
    pub fn new(a: A, b: B, c: C) -> Self {
        Self { a, b, c }
    }
}

impl<A: ArgSingleTyping, B: ArgSingleTyping, C: ArgSingleTyping> ArgTyping for Union3Type<A, B, C> {
    fn arg_type() -> ValueType {
        ValueType::Union(vec![
            MaybeEaten::Raw(A::arg_single_type()),
            MaybeEaten::Raw(B::arg_single_type()),
            MaybeEaten::Raw(C::arg_single_type()),
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
                    Err(c_err) => Err(format!("union error: {a_err} | {b_err} | {c_err}")),
                },
            },
        }
    }
}

impl<
        A: ArgSingleTypingDirectCreation,
        B: ArgSingleTypingDirectCreation,
        C: ArgSingleTypingDirectCreation,
    > ArgTypingDirectCreation for Union3Type<A, B, C>
{
    fn new_direct() -> Self {
        Self::new(A::new_direct(), B::new_direct(), C::new_direct())
    }
}

pub enum Union3Result<A: ArgSingleTyping, B: ArgSingleTyping, C: ArgSingleTyping> {
    A(A::Parsed),
    B(B::Parsed),
    C(C::Parsed),
}
