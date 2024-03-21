use std::{collections::HashMap, fmt::Display, marker::PhantomData};

use colored::Colorize;
use parsy::{CodeRange, MaybeEaten};
use reshell_parser::ast::{FnSignature, SingleValueType, StructTypeMember, ValueType};

use reshell_runtime::{
    gc::{GcCell, GcReadOnlyCell},
    values::{RuntimeFnValue, RuntimeValue},
};

use crate::helper::{
    ArgSingleTyping, ArgSingleTypingDirectCreation, ArgTyping, ArgTypingDirectCreation,
};

macro_rules! declare_basic_types {
    ($($name: ident ($variant: ident) = $type: ty => $value_ident: ident: $parser: expr),+) => {
        $(
            pub struct $name;

            impl ArgSingleTyping for $name {
                fn underlying_single_type(&self) -> SingleValueType {
                    SingleValueType::$variant
                }

                type Parsed = $type;

                fn parse(&self, $value_ident: RuntimeValue) -> Result<Self::Parsed, String> {
                    $parser
                }
            }

            impl ArgSingleTypingDirectCreation for $name {
                fn new_single_direct() -> Self {
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

impl<From: SpecificIntType> ArgSingleTyping for ExactIntType<From> {
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

impl<From: SpecificIntType> ArgSingleTypingDirectCreation for ExactIntType<From> {
    fn new_single_direct() -> Self {
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

impl<Inner: ArgTyping> ArgSingleTyping for DetachedListType<Inner> {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::List
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

impl<Inner: ArgTypingDirectCreation> ArgSingleTypingDirectCreation for DetachedListType<Inner> {
    fn new_single_direct() -> Self {
        Self::new(Inner::new_direct())
    }
}

// pub struct UntypedFunctionType;

// impl ArgSingleTypingDirectCreation for UntypedFunctionType {
//     fn new_single_direct() -> Self {
//         Self
//     }
// }

// impl ArgSingleTyping for UntypedFunctionType {
//     fn underlying_single_type(&self) -> SingleValueType {
//         SingleValueType::Function(MaybeEaten::Raw(
//             // Universal signature (TODO: doesn't work :p)
//             FnSignature {
//                 args: forge_internal_token(vec![]),
//                 ret_type: None,
//             },
//         ))
//     }

//     type Parsed = GcReadOnlyCell<RuntimeFnValue>;

//     fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
//         match value {
//             RuntimeValue::Function(func) => Ok(func),
//             _ => Err("expected a function".to_owned()),
//         }
//     }
// }

pub struct MapType;

impl ArgSingleTyping for MapType {
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

impl ArgSingleTypingDirectCreation for MapType {
    fn new_single_direct() -> Self {
        Self
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

impl ArgSingleTyping for TypedFunctionType {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::Function(MaybeEaten::Raw(self.signature.clone()))
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

pub struct Tuple2Type<A: ArgTyping, B: ArgTyping> {
    a: A,
    b: B,
}

// impl<A: ArgTyping, B: ArgTyping> Tuple2Type<A, B> {
//     pub fn new(a: A, b: B) -> Self {
//         Self { a, b }
//     }
// }

impl<A: ArgTyping, B: ArgTyping> ArgSingleTyping for Tuple2Type<A, B> {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::List
    }

    type Parsed = (A::Parsed, B::Parsed);

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        let items = match value {
            RuntimeValue::List(items) => items,
            _ => return Err("expected a tuple (list)".to_owned()),
        };

        let items = items.read();

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
    }
}

impl<A: ArgTypingDirectCreation, B: ArgTypingDirectCreation> ArgSingleTypingDirectCreation
    for Tuple2Type<A, B>
{
    fn new_single_direct() -> Self {
        Self {
            a: A::new_direct(),
            b: B::new_direct(),
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
    fn underlying_type(&self) -> ValueType {
        ValueType::Union(vec![
            MaybeEaten::Raw(self.a.underlying_single_type()),
            MaybeEaten::Raw(self.b.underlying_single_type()),
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

impl<A: ArgSingleTypingDirectCreation, B: ArgSingleTypingDirectCreation> ArgTypingDirectCreation
    for Union2Type<A, B>
{
    fn new_direct() -> Self {
        Self::new(A::new_single_direct(), B::new_single_direct())
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
    fn underlying_type(&self) -> ValueType {
        ValueType::Union(vec![
            MaybeEaten::Raw(self.a.underlying_single_type()),
            MaybeEaten::Raw(self.b.underlying_single_type()),
            MaybeEaten::Raw(self.c.underlying_single_type()),
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
        A: ArgSingleTypingDirectCreation,
        B: ArgSingleTypingDirectCreation,
        C: ArgSingleTypingDirectCreation,
    > ArgTypingDirectCreation for Union3Type<A, B, C>
{
    fn new_direct() -> Self {
        Self::new(
            A::new_single_direct(),
            B::new_single_direct(),
            C::new_single_direct(),
        )
    }
}

pub enum Union3Result<A: ArgSingleTyping, B: ArgSingleTyping, C: ArgSingleTyping> {
    A(A::Parsed),
    B(B::Parsed),
    C(C::Parsed),
}

macro_rules! declare_typed_struct_type {
    ($( $struct: ident { $( $member: ident : $generic: ident ),+ } ),+ ) => {
        $(
            pub struct $struct<$($generic: ArgTyping),+> {
                $($member: (String, $generic)),+
            }

            impl<$($generic: ArgTyping),+> $struct<$($generic),+> {
                pub fn new( $($member: (impl Into<String>, $generic)),+ ) -> Self {
                    Self { $( $member: ($member.0.into(), $member.1) ),+ }
                }
            }

            impl<$($generic: ArgTyping),+> ArgSingleTyping for $struct<$($generic),+> {
                fn underlying_single_type(&self) -> SingleValueType {
                    SingleValueType::TypedStruct(vec![
                        $(
                            MaybeEaten::Raw(StructTypeMember {
                                name: MaybeEaten::Raw(self.$member.0.clone()),
                                typ: MaybeEaten::Raw(self.$member.1.underlying_type())
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

                    let members = members.read();

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
                }
            }
        )+
    }
}

declare_typed_struct_type!(
    TypedStruct1Type { a: A },
    TypedStruct3Type { a: A, b: B, c: C },
    TypedStruct4Type {
        a: A,
        b: B,
        c: C,
        d: D
    }
);
