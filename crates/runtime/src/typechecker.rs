use reshell_parser::ast::{
    FnArg, FnFlagArgNames, FnSignature, SingleValueType, StructTypeMember, ValueType,
};

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
            .any(|typ| check_if_single_type_fits_single(value_type, typ.data(), ctx)),
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

        (SingleValueType::ArgSpread, SingleValueType::ArgSpread) => true,
        (SingleValueType::ArgSpread, _) | (_, SingleValueType::ArgSpread) => false,

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
        match (arg, cmp_arg) {
            (
                FnArg::Positional {
                    name: _,
                    is_optional: a_is_optional,
                    typ: a_typ,
                },
                FnArg::Positional {
                    name: _,
                    is_optional: b_is_optional,
                    typ: b_typ,
                },
            ) => {
                // NOTE: We don't compare names as they only matter for the function body's scope

                if a_is_optional != b_is_optional {
                    return false;
                }

                if let (Some(a_type), Some(b_type)) = (a_typ, b_typ) {
                    if !check_if_type_fits_type(a_type.data(), b_type.data(), ctx) {
                        return false;
                    }
                }
            }

            (
                FnArg::Positional {
                    name: _,
                    is_optional: _,
                    typ: _,
                },
                _,
            )
            | (
                _,
                FnArg::Positional {
                    name: _,
                    is_optional: _,
                    typ: _,
                },
            ) => return false,

            (FnArg::PresenceFlag { names: a_names }, FnArg::PresenceFlag { names: b_names }) => {
                if !check_fn_flag_args_name_compat(a_names, b_names) {
                    return false;
                }
            }

            (FnArg::PresenceFlag { names: _ }, _) | (_, FnArg::PresenceFlag { names: _ }) => {
                return false
            }

            (
                FnArg::NormalFlag {
                    names: a_names,
                    is_optional: a_is_optional,
                    typ: a_typ,
                },
                FnArg::NormalFlag {
                    names: b_names,
                    is_optional: b_is_optional,
                    typ: b_typ,
                },
            ) => {
                if !check_fn_flag_args_name_compat(a_names, b_names) {
                    return false;
                }

                if a_is_optional != b_is_optional {
                    return false;
                }

                if let (Some(a_type), Some(b_type)) = (a_typ, b_typ) {
                    if !check_if_type_fits_type(a_type.data(), b_type.data(), ctx) {
                        return false;
                    }
                }
            }

            (
                FnArg::NormalFlag {
                    names: _,
                    is_optional: _,
                    typ: _,
                },
                _,
            )
            | (
                _,
                FnArg::NormalFlag {
                    names: _,
                    is_optional: _,
                    typ: _,
                },
            ) => return false,

            (FnArg::Rest { name: a_name }, FnArg::Rest { name: b_name }) => {
                if a_name.data() != b_name.data() {
                    return false;
                }
            }
            //
            // (FnArg::Rest { name: _ }, _) | (_, FnArg::Rest { name: _ }) => return false,
        }
    }

    if let (Some(ret_type), Some(cmp_ret_type)) = (&signature.ret_type, &into.ret_type) {
        if !check_if_type_fits_type(ret_type.data(), cmp_ret_type.data(), ctx) {
            return false;
        }
    }

    true
}

/// Check if a set of flag names is compatible with another
///
/// For instance, if the target (into) has '-c' and '--current',
/// the source must have both those parameters as it could otherwise produce
/// a case where a call to it would result in an error due to it missing
/// either the short and long flag
fn check_fn_flag_args_name_compat(curr: &FnFlagArgNames, into: &FnFlagArgNames) -> bool {
    match into {
        FnFlagArgNames::ShortFlag(b) => match curr {
            FnFlagArgNames::LongFlag(_) => false,
            FnFlagArgNames::ShortFlag(a)
            | FnFlagArgNames::LongAndShortFlag { long: _, short: a } => a.data() == b.data(),
        },

        FnFlagArgNames::LongFlag(b) => match curr {
            FnFlagArgNames::ShortFlag(_) => false,
            FnFlagArgNames::LongFlag(a)
            | FnFlagArgNames::LongAndShortFlag { long: a, short: _ } => a.data() == b.data(),
        },

        FnFlagArgNames::LongAndShortFlag { long, short } => match curr {
            FnFlagArgNames::ShortFlag(_) | FnFlagArgNames::LongFlag(_) => false,
            FnFlagArgNames::LongAndShortFlag {
                long: a_long,
                short: a_short,
            } => a_long.data() == long.data() && a_short.data() == short.data(),
        },
    }
}
