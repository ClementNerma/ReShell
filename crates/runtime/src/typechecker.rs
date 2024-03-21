use reshell_parser::ast::{FnArgNames, FnSignature, SingleValueType, StructTypeMember, ValueType};

use crate::{context::Context, display::dbg_loc};

pub fn check_if_type_fits_type(value_type: &ValueType, into: &ValueType, ctx: &Context) -> bool {
    match value_type {
        ValueType::Single(single_type) => {
            check_if_single_type_fits_type(single_type.data(), into, ctx)
        }

        ValueType::Union(types) => types
            .iter()
            .all(|typ| check_if_single_type_fits_type(typ.data(), into, ctx)),
    }
}

pub fn check_if_single_type_fits_type(
    value_type: &SingleValueType,
    into: &ValueType,
    ctx: &Context,
) -> bool {
    match into {
        ValueType::Single(single) => {
            check_if_single_type_fits_single(value_type, single.data(), ctx)
        }

        ValueType::Union(types) => types
            .iter()
            .all(|typ| check_if_single_type_fits_single(value_type, typ.data(), ctx)),
    }
}

pub fn check_if_type_fits_single(
    value_type: &ValueType,
    into: &SingleValueType,
    ctx: &Context,
) -> bool {
    match value_type {
        ValueType::Single(single) => check_if_single_type_fits_single(single.data(), into, ctx),

        ValueType::Union(types) => types
            .iter()
            .all(|typ| check_if_single_type_fits_single(typ.data(), into, ctx)),
    }
}

pub fn check_if_single_type_fits_single(
    value_type: &SingleValueType,
    into: &SingleValueType,
    ctx: &Context,
) -> bool {
    match (value_type, into) {
        (SingleValueType::Any, _) | (_, SingleValueType::Any) => true,

        (SingleValueType::TypeAlias(name), _) => {
            let typ = ctx
                .get_type_alias(name)
                .unwrap_or_else(|| panic!(
                    "type alias was not found (this is a bug in the checker)\nDetails:\n> {} (used at {})",
                    name.data,
                    dbg_loc(name.at, ctx.files_map()
                )));

            check_if_type_fits_single(&typ.data, into, ctx)
        }

        (_, SingleValueType::TypeAlias(name)) => {
            let typ = ctx
                .get_type_alias(name)
                .unwrap_or_else(|| panic!(
                    "type alias was not found (this is a bug in the checker)\nDetails:\n> {} (used at {})",
                    name.data,
                    dbg_loc(name.at, ctx.files_map()
                )));

            check_if_single_type_fits_type(value_type, &typ.data, ctx)
        }

        (SingleValueType::Null, SingleValueType::Null) => true,
        (SingleValueType::Null, _) | (_, SingleValueType::Null) => false,

        (SingleValueType::Bool, SingleValueType::Bool) => true,
        (SingleValueType::Bool, _) | (_, SingleValueType::Bool) => false,

        (SingleValueType::Int, SingleValueType::Int) => true,
        (SingleValueType::Int, _) | (_, SingleValueType::Int) => false,

        (SingleValueType::Float, SingleValueType::Float) => true,
        (SingleValueType::Float, _) | (_, SingleValueType::Float) => false,

        (SingleValueType::String, SingleValueType::String) => true,
        (SingleValueType::String, _) | (_, SingleValueType::String) => false,

        (SingleValueType::List, SingleValueType::List) => true,
        (SingleValueType::List, _) | (_, SingleValueType::List) => false,

        (SingleValueType::Range, SingleValueType::Range) => true,
        (SingleValueType::Range, _) | (_, SingleValueType::Range) => false,

        (SingleValueType::Map, SingleValueType::Map) => true,
        (SingleValueType::Map, _) | (_, SingleValueType::Map) => false,

        (SingleValueType::UntypedStruct, SingleValueType::UntypedStruct) => true,
        (SingleValueType::TypedStruct(_), SingleValueType::UntypedStruct) => true,

        (SingleValueType::TypedStruct(a), SingleValueType::TypedStruct(b)) => {
            b.iter().all(|b_member| {
                let StructTypeMember { name, typ } = b_member.data();

                a.iter()
                    .find(|member| member.data().name.data() == name.data())
                    .map_or(false, |a_member| {
                        check_if_type_fits_type(a_member.data().typ.data(), typ.data(), ctx)
                    })
            })
        }

        (SingleValueType::UntypedStruct, _) | (_, SingleValueType::UntypedStruct) => false,
        (SingleValueType::TypedStruct(_), _) | (_, SingleValueType::TypedStruct(_)) => false,

        (SingleValueType::Error, SingleValueType::Error) => true,
        (SingleValueType::Error, _) | (_, SingleValueType::Error) => false,

        (SingleValueType::Function(signature), SingleValueType::Function(into)) => {
            check_fn_signature_equality(signature.data(), into.data(), ctx)
        }
    }
}

pub fn check_fn_signature_equality(
    signature: &FnSignature,
    into: &FnSignature,
    ctx: &Context,
) -> bool {
    if signature.args.data().len() != into.args.data().len() {
        return false;
    }

    for (arg, cmp_arg) in signature.args.data().iter().zip(into.args.data().iter()) {
        if arg.is_optional != cmp_arg.is_optional {
            return false;
        }

        if arg.is_rest != cmp_arg.is_rest {
            return false;
        }

        match (&arg.names, &cmp_arg.names) {
            (FnArgNames::Positional(_), FnArgNames::Positional(_)) => {}
            (FnArgNames::ShortFlag(_), FnArgNames::ShortFlag(_)) => {}
            (FnArgNames::LongFlag(_), FnArgNames::LongFlag(_)) => {}
            (
                FnArgNames::LongAndShortFlag { long: _, short: _ },
                FnArgNames::LongAndShortFlag { long: _, short: _ },
            ) => {}

            (FnArgNames::Positional(_), _) | (_, FnArgNames::Positional(_)) => return false,
            (FnArgNames::ShortFlag(_), _) | (_, FnArgNames::ShortFlag(_)) => return false,
            (FnArgNames::LongFlag(_), _) | (_, FnArgNames::LongFlag(_)) => return false,
        }

        if let (Some(arg_type), Some(cmp_arg_type)) = (&arg.typ, &cmp_arg.typ) {
            if !check_if_type_fits_type(arg_type.data(), cmp_arg_type.data(), ctx) {
                return false;
            }
        }
    }

    if let (Some(ret_type), Some(cmp_ret_type)) = (&signature.ret_type, &into.ret_type) {
        if !check_if_type_fits_type(ret_type.data(), cmp_ret_type.data(), ctx) {
            return false;
        }
    }

    true
}
