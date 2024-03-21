use reshell_parser::ast::{FnArgNames, FnSignature, SingleValueType, StructTypeMember, ValueType};

use crate::{context::Context, errors::ExecResult};

pub fn check_if_type_fits(
    value_type: &ValueType,
    into: &ValueType,
    ctx: &Context,
) -> ExecResult<bool> {
    match value_type {
        ValueType::Single(single_type) => check_if_single_type_fits(single_type.data(), into, ctx),
        ValueType::Union(types) => {
            for typ in types {
                if check_if_single_type_fits(typ.data(), into, ctx)? {
                    return Ok(true);
                }
            }

            Ok(false)
        }
    }
}

pub fn check_if_single_type_fits(
    value_type: &SingleValueType,
    into: &ValueType,
    ctx: &Context,
) -> ExecResult<bool> {
    match into {
        ValueType::Single(single) => {
            check_if_single_type_fits_single(value_type, single.data(), ctx)
        }

        ValueType::Union(types) => {
            for typ in types {
                if check_if_single_type_fits_single(value_type, typ.data(), ctx)? {
                    return Ok(true);
                }
            }

            Ok(false)
        }
    }
}

pub fn check_if_single_type_fits_single(
    value_type: &SingleValueType,
    into: &SingleValueType,
    ctx: &Context,
) -> ExecResult<bool> {
    match (value_type, into) {
        (_, SingleValueType::Any) => Ok(true),
        (SingleValueType::Any, _) => Ok(true),

        (_, SingleValueType::TypeAlias(name)) => {
            let (_, typ) = ctx
                .visible_scopes()
                .flat_map(|scope| scope.type_aliases.iter())
                .find(|(typename, _)| typename == &&name.data)
                .ok_or_else(|| ctx.error(name.at, format!("type '{}' was not found", name.data)))?;

            check_if_single_type_fits(value_type, &typ.alias_content, ctx)
        }

        (SingleValueType::TypeAlias(_), _) => {
            // let (_, typ) = ctx
            //     .all_type_aliases()
            //     .find(|(c_name, typ)| &&name.data == c_name)
            //     .ok_or_else(|| {
            //         ctx.error(name.at, format!("type alias {} was not found", name.data))
            //     })?;

            // check_if_type_fits(typ, into, ctx)
            unreachable!()
        }

        (SingleValueType::Null, SingleValueType::Null) => Ok(true),
        (SingleValueType::Null, _) | (_, SingleValueType::Null) => Ok(false),

        (SingleValueType::Bool, SingleValueType::Bool) => Ok(true),
        (SingleValueType::Bool, _) | (_, SingleValueType::Bool) => Ok(false),

        (SingleValueType::Int, SingleValueType::Int) => Ok(true),
        (SingleValueType::Int, _) | (_, SingleValueType::Int) => Ok(false),

        (SingleValueType::Float, SingleValueType::Float) => Ok(true),
        (SingleValueType::Float, _) | (_, SingleValueType::Float) => Ok(false),

        (SingleValueType::String, SingleValueType::String) => Ok(true),
        (SingleValueType::String, _) | (_, SingleValueType::String) => Ok(false),

        (SingleValueType::List, SingleValueType::List) => Ok(true),
        (SingleValueType::List, _) | (_, SingleValueType::List) => Ok(false),

        (SingleValueType::Range, SingleValueType::Range) => Ok(true),
        (SingleValueType::Range, _) | (_, SingleValueType::Range) => Ok(false),

        (SingleValueType::Map, SingleValueType::Map) => Ok(true),
        (SingleValueType::Map, _) | (_, SingleValueType::Map) => Ok(false),

        (SingleValueType::UntypedStruct, SingleValueType::UntypedStruct) => Ok(true),
        (SingleValueType::TypedStruct(_), SingleValueType::UntypedStruct) => Ok(true),

        (SingleValueType::TypedStruct(a), SingleValueType::TypedStruct(b)) => {
            // TODO: make comparison stricter (no excess properties?)
            for member in b {
                let StructTypeMember { name, typ } = &member.data();

                match a
                    .iter()
                    .find(|member| member.data().name.data() == name.data())
                {
                    None => return Ok(false),
                    Some(member) => {
                        if !check_if_type_fits(member.data().typ.data(), typ.data(), ctx)? {
                            return Ok(false);
                        }
                    }
                }
            }

            Ok(true)
        }

        (SingleValueType::UntypedStruct, _) | (_, SingleValueType::UntypedStruct) => Ok(false),
        (SingleValueType::TypedStruct(_), _) | (_, SingleValueType::TypedStruct(_)) => Ok(false),

        (SingleValueType::Error, SingleValueType::Error) => Ok(true),
        (SingleValueType::Error, _) | (_, SingleValueType::Error) => Ok(false),

        (SingleValueType::Function(signature), SingleValueType::Function(into)) => {
            check_fn_equality(signature, into, ctx)
        }
    }
}

// TODO: with detailed error message (ExecResult<()>)?
pub fn check_fn_equality(
    signature: &FnSignature,
    into: &FnSignature,
    ctx: &Context,
) -> ExecResult<bool> {
    if signature.args.data.len() != into.args.data.len() {
        return Ok(false);
    }

    for (arg, cmp_arg) in signature.args.data.iter().zip(into.args.data.iter()) {
        if arg.is_optional != cmp_arg.is_optional {
            return Ok(false);
        }

        if arg.is_rest != cmp_arg.is_rest {
            return Ok(false);
        }

        match (&arg.names, &cmp_arg.names) {
            (FnArgNames::NotFlag(_), FnArgNames::NotFlag(_)) => {}
            (FnArgNames::ShortFlag(_), FnArgNames::ShortFlag(_)) => {}
            (FnArgNames::LongFlag(_), FnArgNames::LongFlag(_)) => {}
            (
                FnArgNames::LongAndShortFlag { long: _, short: _ },
                FnArgNames::LongAndShortFlag { long: _, short: _ },
            ) => {}

            (FnArgNames::NotFlag(_), _) | (_, FnArgNames::NotFlag(_)) => return Ok(false),
            (FnArgNames::ShortFlag(_), _) | (_, FnArgNames::ShortFlag(_)) => return Ok(false),
            (FnArgNames::LongFlag(_), _) | (_, FnArgNames::LongFlag(_)) => return Ok(false),
        }

        if let (Some(arg_type), Some(cmp_arg_type)) = (&arg.typ, &cmp_arg.typ) {
            if !check_if_type_fits(&arg_type.data, &cmp_arg_type.data, ctx)? {
                return Ok(false);
            }
        }
    }

    if let (Some(ret_type), Some(cmp_ret_type)) = (&signature.ret_type, &into.ret_type) {
        if !check_if_type_fits(ret_type.data.as_ref(), &cmp_ret_type.data, ctx)? {
            return Ok(false);
        }
    }

    Ok(true)
}
