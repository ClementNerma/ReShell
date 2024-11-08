//!
//! This module defines and exposes *type handlers*, which are simple types that
//! allow to convert and parse some of the scripting language's native types.
//!

use std::{any::Any, collections::HashMap, fmt::Display, marker::PhantomData};

use parsy::CodeRange;
use reshell_parser::ast::{SingleValueType, ValueType};

use reshell_runtime::{
    gc::GcCell,
    values::{CmdArgValue, CustomValueType, ErrorValueContent, RuntimeValue},
};

use super::args::TypedValueParser;

/// This macro helps create a type handler for any variant of the [`SingleValueType`] enum,
/// associated to a variant of the [`RuntimeValue`]
macro_rules! declare_basic_type_handlers {
    ($($name: ident ($variant: ident) = $type: ty => $value_ident: ident: $parser: expr),+) => {
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
        )+
    };
}

// Implement type handlers for the most basic types
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

    UntypedListType (UntypedList) = GcCell<Vec<RuntimeValue>> => value: match value {
        RuntimeValue::List(items) => Ok(items),
        _ => Err("expected a list".to_owned())
    },

    UntypedMapType (UntypedMap) = GcCell<HashMap<String, RuntimeValue>> => value: match value {
        RuntimeValue::Map(items) => Ok(items),
        _ => Err("expected a map".to_owned())
    },

    UntypedStructType (UntypedStruct) = GcCell<HashMap<String, RuntimeValue>> => value: match value {
        RuntimeValue::Struct(members) => Ok(members),
        _ => Err("expected a struct".to_owned())
    },

    CmdArgType (CmdArg) = Box<CmdArgValue> => value: match value {
        RuntimeValue::CmdArg(arg) => Ok(arg),
        _ => Err("expected a command argument value".to_owned())
    }
);

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

/// Trait representing a specific Rust integer type
pub trait RustIntType: TryFrom<i64> + Display + std::fmt::Debug {
    const MIN: Self;
    const MAX: Self;
}

/// Macro to add support for conversion between the shell's integer type and Rust's ones
macro_rules! implement_specific_int_types {
    ($($int_type: ident),+) => {
        $(
            impl RustIntType for $int_type {
                const MIN: Self = Self::MIN;
                const MAX: Self = Self::MAX;
            }
        )+
    };
}

implement_specific_int_types!(u8, u16, u32, u64, i8, i16, i32, i64, usize);

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

/// Type handler for custom types
pub struct CustomType<C: CustomValueType> {
    _c: PhantomData<C>,
}

impl<C: CustomValueType> TypedValueParser for CustomType<C> {
    fn value_type() -> ValueType {
        ValueType::Single(SingleValueType::Custom(C::typename_static()))
    }

    type Parsed = Box<C>;

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
        match value {
            RuntimeValue::Custom(value) => {
                Box::<dyn Any>::downcast::<C>(dyn_clone::clone_box::<dyn CustomValueType>(&**value))
                    .map_err(|_| {
                        format!(
                            "Failed to downcast value of type '{}'",
                            C::typename_static()
                        )
                    })
            }
            _ => Err(format!("expected a {}", C::typename_static())),
        }
    }
}

/// Type handler for a list made of 2 elements
pub struct Tuple2Type<A: TypedValueParser, B: TypedValueParser> {
    _ab: PhantomData<(A, B)>,
}

impl<A: TypedValueParser, B: TypedValueParser> TypedValueParser for Tuple2Type<A, B> {
    fn value_type() -> ValueType {
        ValueType::Single(SingleValueType::UntypedList)
    }

    type Parsed = (A::Parsed, B::Parsed);

    fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
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

        let a = A::parse(items[0].clone())
            .map_err(|err| format!("error in tuple's first element: {err}"))?;

        let b = B::parse(items[1].clone())
            .map_err(|err| format!("error in tuple's second element: {err}"))?;

        Ok((a, b))
    }
}

/// Macro to implement a type handler for a union type
macro_rules! declare_typed_union_handler {
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
    };
}

// Create union type handlers
declare_typed_union_handler!(Union2Type (A, B) => Union2Result);
declare_typed_union_handler!(Union3Type (A, B, C) => Union3Result);
declare_typed_union_handler!(Union4Type (A, B, C, D) => Union4Result);

/// Macro to create a struct type handler
#[macro_export]
macro_rules! declare_typed_struct_handler {
    ($( $(#[$meta: meta])* $struct: ident { $( $(#[$member_meta: meta])* $name: ident: $parser: ty ),+ } ),+ ) => {
        $(
            $(#[$meta])*
            struct $struct {
                // TODO: auto snake case on idents
                $( $(#[$member_meta])* $name: <$parser as $crate::helpers::args::TypedValueParser>::Parsed ),+
            }

            impl $crate::helpers::args::TypedValueParser for $struct {
                fn value_type() -> ::reshell_parser::ast::ValueType {
                    ::reshell_parser::ast::ValueType::Single(::reshell_parser::ast::SingleValueType::TypedStruct(vec![
                        $(
                            ::reshell_parser::ast::StructTypeMember {
                                name: ::reshell_parser::ast::RuntimeEaten::internal("native library's type generator", stringify!($name).to_owned()),
                                typ: <$parser as $crate::helpers::args::TypedValueParser>::value_type()
                            }
                        ),+
                    ]))
                }

                #[allow(unused_parens)]
                type Parsed = Self;

                fn parse(value: RuntimeValue) -> Result<Self::Parsed, String> {
                    let members = match value {
                        RuntimeValue::Struct(members) => members,
                        _ => return Err("expected a struct".to_owned()),
                    };

                    let members = members.read_promise_no_write();

                    Ok(Self {
                        $($name: {
                            let value = members
                                .get(stringify!($name))
                                .ok_or_else(|| format!("property '{}' is missing", stringify!($name)))?;

                            <$parser>::parse(value.clone())
                                .map_err(|err| format!("type mismatch in struct member '{}': {err}", stringify!($name)))?
                        }),+
                    })
                }
            }
        )+
    }
}

/// Macro to create a function type handler
#[macro_export]
macro_rules! declare_typed_fn_handler {
    ($( $typename: ident => $signature: expr ),+ ) => {
        $(
            struct $typename;

            impl $typename {
                pub fn signature() -> ::reshell_parser::ast::FnSignature {
                    $signature
                }
            }

            impl $crate::helpers::args::TypedValueParser for $typename {
                fn value_type() -> ::reshell_parser::ast::ValueType {
                    ::reshell_parser::ast::ValueType::Single(::reshell_parser::ast::SingleValueType::Function(
                        ::reshell_parser::ast::RuntimeEaten::internal(
                            "native library's type generator",
                            Self::signature(),
                        ),
                    ))
                }

                type Parsed = ::reshell_runtime::gc::GcReadOnlyCell<::reshell_runtime::values::RuntimeFnValue>;

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
