//!
//! This module defines and exposes *type handlers*, which are simple types that
//! allow to convert and parse some of the scripting language's native types.
//!

use std::{any::Any, collections::HashMap, fmt::Display, marker::PhantomData};

use parsy::CodeRange;
use reshell_parser::ast::{
    FnSignature, RuntimeEaten, SingleValueType, StructTypeMember, ValueType,
};

use reshell_runtime::{
    cmd::CmdSingleArgResult,
    gc::{GcCell, GcReadOnlyCell},
    values::{CustomValueType, ErrorValueContent, RuntimeFnValue, RuntimeValue},
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

    ErrorType (Error) = (CodeRange, RuntimeValue) => value: match value {
        RuntimeValue::Error(err) => {
            let ErrorValueContent { at, data } = *err;
            Ok((at, data.clone()))
        },

        _ => Err("expected an error".to_owned())
    },

    CmdCallType (CmdCall) = CodeRange => value: match value {
        RuntimeValue::CmdCall { content_at } => Ok(content_at),

        _ => Err("expected a command call".to_owned())
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
    },

    SpreadType (ArgSpread) = GcReadOnlyCell<Vec<CmdSingleArgResult>> => value: match value {
        RuntimeValue::ArgSpread(spread) => Ok(spread),
        _ => Err("expected an arg spread".to_owned())
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

        let values = list
            .read_promise_no_write()
            .iter()
            .cloned()
            .map(|item| self.inner.parse(item))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(values)
    }
}

impl<Inner: TypingDirectCreation> SingleTypingDirectCreation for DetachedListType<Inner> {
    fn new_single_direct() -> Self {
        Self::new(Inner::new_direct())
    }
}

pub struct DetachedMapType<Inner: Typing> {
    inner: Inner,
}

impl<Inner: Typing> DetachedMapType<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self { inner }
    }
}

impl<Inner: Typing> SingleTyping for DetachedMapType<Inner> {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::Map
    }

    type Parsed = HashMap<String, Inner::Parsed>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        let map = match value {
            RuntimeValue::Map(map) => map,
            _ => return Err("expected a map".to_owned()),
        };

        let values = map
            .read_promise_no_write()
            .iter()
            .map(|(key, item)| {
                self.inner
                    .parse(item.clone())
                    .map(|parsed| (key.clone(), parsed))
            })
            .collect::<Result<HashMap<_, _>, _>>()?;

        Ok(values)
    }
}

impl<Inner: TypingDirectCreation> SingleTypingDirectCreation for DetachedMapType<Inner> {
    fn new_single_direct() -> Self {
        Self::new(Inner::new_direct())
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

pub struct CustomType<C: CustomValueType> {
    _c: PhantomData<C>,
}

impl<C: CustomValueType> SingleTyping for CustomType<C> {
    fn underlying_single_type(&self) -> SingleValueType {
        SingleValueType::Custom(C::typename_static())
    }

    type Parsed = Box<C>;

    fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Custom(value) => {
                Box::<dyn Any>::downcast::<C>(dyn_clone::clone_box::<dyn CustomValueType>(&**value))
                    .map_err(|_| {
                        format!(
                            "Failed to download value of type '{}'",
                            C::typename_static()
                        )
                    })
            }
            _ => Err(format!("expected a {}", C::typename_static())),
        }
    }
}

impl<C: CustomValueType> SingleTypingDirectCreation for CustomType<C> {
    fn new_single_direct() -> Self {
        Self { _c: PhantomData }
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

        let items = items.read_promise_no_write();

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

macro_rules! declare_typed_union_hanlder {
    ($handler_struct: ident ($($generic: ident),+) => $result_struct: ident) => {
        pub struct $handler_struct<$($generic: SingleTyping),+> {
            $(
                #[allow(non_snake_case)]
                $generic: $generic
            ),+
        }

        impl<$($generic: SingleTyping),+> $handler_struct<$($generic,)+> {
            pub fn new($( #[allow(non_snake_case)] $generic: $generic ),+) -> Self {
                Self { $($generic),+ }
            }
        }

        impl<$($generic: SingleTyping),+> Typing
            for $handler_struct<$($generic,)+>
        {
            fn underlying_type(&self) -> ValueType {
                ValueType::Union(vec![
                    $( RuntimeEaten::Internal(self.$generic.underlying_single_type()) ),+
                ])
            }

            type Parsed = $result_struct<$($generic,)+>;

            fn parse(&self, value: RuntimeValue) -> Result<Self::Parsed, String> {
                $(
                    match self.$generic.parse(value.clone()) {
                        Ok(parsed) => return Ok($result_struct::$generic(parsed)),
                        Err(err) => {
                            // TODO
                        }
                    }
                )+;

                Err(format!("union error")) // TODO: more detailed message
            }
        }

        impl<$($generic: SingleTypingDirectCreation),+> TypingDirectCreation for $handler_struct<$($generic,)+>
        {
            fn new_direct() -> Self {
                Self::new(
                    $($generic::new_direct()),+
                )
            }
        }

        pub enum $result_struct<$($generic: SingleTyping),+> {
            $( $generic($generic::Parsed), )+
        }
    };
}

declare_typed_union_hanlder!(Union2Type (A, B) => Union2Result);
declare_typed_union_hanlder!(Union3Type (A, B, C) => Union3Result);
declare_typed_union_hanlder!(Union4Type (A, B, C, D) => Union4Result);

macro_rules! declare_typed_struct_handler {
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

                    let members = members.read_promise_no_write();

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

declare_typed_struct_handler!(
    TypedStruct1Type { a: A },
    TypedStruct3Type { a: A, b: B, c: C },
    TypedStruct4Type {
        a: A,
        b: B,
        c: C,
        d: D
    }
);
