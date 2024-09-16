use reshell_checker::typechecker::check_if_fn_signature_fits_another;
use reshell_parser::ast::{SingleValueType, StructTypeMember, ValueType};

use crate::{context::Context, values::RuntimeValue};

pub fn check_if_value_fits_type(value: &RuntimeValue, typ: &ValueType, ctx: &Context) -> bool {
    match typ {
        ValueType::Single(typ) => check_if_value_fits_single_type(value, typ, ctx),

        ValueType::Union(types) => types
            .iter()
            .any(|typ| check_if_value_fits_single_type(value, typ, ctx)),
    }
}

pub fn check_if_value_fits_single_type(
    value: &RuntimeValue,
    typ: &SingleValueType,
    ctx: &Context,
) -> bool {
    match typ {
        SingleValueType::Any => true,
        SingleValueType::Void => false,
        SingleValueType::Null => matches!(value, RuntimeValue::Null),
        SingleValueType::Bool => matches!(value, RuntimeValue::Bool(_)),
        SingleValueType::Int => matches!(value, RuntimeValue::Int(_)),
        SingleValueType::Float => matches!(value, RuntimeValue::Float(_)),
        SingleValueType::String => matches!(value, RuntimeValue::String(_)),
        SingleValueType::Error => matches!(value, RuntimeValue::Error(_)),
        SingleValueType::CmdCall => matches!(value, RuntimeValue::CmdCall { content_at: _ }),
        SingleValueType::CmdArg => matches!(value, RuntimeValue::CmdArg(_)),
        SingleValueType::UntypedList => matches!(value, RuntimeValue::List(_)),
        SingleValueType::UntypedMap => matches!(value, RuntimeValue::Map(_)),
        SingleValueType::UntypedStruct => matches!(value, RuntimeValue::Struct(_)),

        SingleValueType::TypedList(items_type) => match &value {
            RuntimeValue::List(items) => items
                .read_promise_no_write()
                .iter()
                .all(|item| check_if_value_fits_type(item, items_type, ctx)),

            _ => false,
        },

        SingleValueType::TypedMap(items_type) => match &value {
            RuntimeValue::Map(items) => items
                .read_promise_no_write()
                .iter()
                .all(|(_, value)| check_if_value_fits_type(value, items_type, ctx)),

            _ => false,
        },

        SingleValueType::TypedStruct(member_types) => match &value {
            RuntimeValue::Struct(members) => {
                let members = members.read_promise_no_write();

                member_types.iter().all(|member_type| {
                    let StructTypeMember { name, typ } = &member_type.data;

                    members
                        .get(&name.data)
                        .is_some_and(|value| check_if_value_fits_type(value, &typ.data, ctx))
                })
            }

            _ => false,
        },

        SingleValueType::Function(signature) => match value {
            RuntimeValue::Function(func) => check_if_fn_signature_fits_another(
                func.signature.inner(),
                &signature.data,
                ctx.type_alias_store(),
            ),

            _ => false,
        },

        SingleValueType::TypeAlias(type_alias) => {
            check_if_value_fits_type(value, &ctx.get_type_alias(type_alias).data, ctx)
        }

        SingleValueType::Custom(typename) => {
            matches!(value, RuntimeValue::Custom(value) if value.typename() == *typename)
        }
    }
}
