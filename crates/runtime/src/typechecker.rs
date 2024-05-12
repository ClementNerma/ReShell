use std::collections::HashSet;

use reshell_parser::ast::{
    FnArg, FnFlagArgNames, FnNormalFlagArg, FnPositionalArg, FnPresenceFlagArg, FnRestArg,
    FnSignature, SingleValueType, StructTypeMember, ValueType,
};

use crate::{context::Context, display::dbg_loc};

#[must_use]
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

#[must_use]
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

#[must_use]
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

#[must_use]
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

        (SingleValueType::CmdCall, SingleValueType::CmdCall) => true,
        (SingleValueType::CmdCall, _) | (_, SingleValueType::CmdCall) => false,

        (SingleValueType::List, SingleValueType::List) => true,
        (SingleValueType::List, _) | (_, SingleValueType::List) => false,

        (SingleValueType::Range, SingleValueType::Range) => true,
        (SingleValueType::Range, _) | (_, SingleValueType::Range) => false,

        (SingleValueType::Map, SingleValueType::Map) => true,
        (SingleValueType::Map, _) | (_, SingleValueType::Map) => false,

        (SingleValueType::CmdArg, SingleValueType::CmdArg) => true,
        (SingleValueType::CmdArg, _) | (_, SingleValueType::CmdArg) => false,

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

        (SingleValueType::Custom(a), SingleValueType::Custom(b)) => a == b,
        (SingleValueType::Custom(_), _) | (_, SingleValueType::Custom(_)) => false,

        (SingleValueType::Function(signature), SingleValueType::Function(into)) => {
            check_fn_signature_equality(signature.data(), into.data(), ctx)
        } // (SingleValueType::Function(_), _) | (_, SingleValueType::Function(_)) => false,
    }
}

#[must_use]
pub fn check_fn_signature_equality(
    signature: &FnSignature,
    into: &FnSignature,
    ctx: &Context,
) -> bool {
    if let (Some(ret_type), Some(cmp_ret_type)) = (&signature.ret_type, &into.ret_type) {
        if !check_if_type_fits_type(ret_type.data(), cmp_ret_type.data(), ctx) {
            return false;
        }
    }

    let FnCategorizedArgs {
        positionals,
        presence_flags,
        normal_flags,
        rest_arg,
    } = FnCategorizedArgs::categorize_from(signature);

    let FnCategorizedArgs {
        positionals: cmp_positionals,
        presence_flags: cmp_presence_flags,
        normal_flags: cmp_normal_flags,
        rest_arg: cmp_rest_arg,
    } = FnCategorizedArgs::categorize_from(into);

    if rest_arg.is_some() && cmp_rest_arg.is_none() {
        return false;
    }

    let mut cmp_positionals = cmp_positionals.iter();

    for positional in positionals {
        let compat = match cmp_positionals.next() {
            Some(cmp_positional) => {
                if positional.is_optional && !cmp_positional.is_optional && cmp_rest_arg.is_none() {
                    false
                } else if let (Some(typ), Some(cmp_typ)) = (&positional.typ, &cmp_positional.typ) {
                    check_if_type_fits_type(typ.data(), cmp_typ.data(), ctx)
                } else {
                    true
                }
            }

            None => positional.is_optional,
        };

        if !compat {
            return false;
        }
    }

    for cmp_positional in cmp_positionals {
        if !cmp_positional.is_optional {
            return false;
        }
    }

    // Need to be present in "cmp_presence_flag" UNLESS it has a rest argument
    // Same in the other way

    let mut used_flag_names = HashSet::new();
    let mut use_cmp_flag_names = HashSet::new();

    let mark_used_name = |flag_names: &FnFlagArgNames, used_names: &mut HashSet<String>| -> bool {
        match flag_names {
            FnFlagArgNames::ShortFlag(name) => used_names.insert(name.data().to_string()),
            FnFlagArgNames::LongFlag(name) => used_names.insert(name.data().clone()),
            FnFlagArgNames::LongAndShortFlag { long, short } => {
                used_names.insert(long.data().clone())
                    && used_names.insert(short.data().to_string())
            }
        }
    };

    for presence_flag in presence_flags {
        match cmp_presence_flags
            .iter()
            .find(|c| check_fn_flag_args_name_compat(&presence_flag.names, &c.names))
        {
            Some(cmp_presence_flag) => {
                if !mark_used_name(&presence_flag.names, &mut used_flag_names) {
                    return false;
                }

                if !mark_used_name(&cmp_presence_flag.names, &mut use_cmp_flag_names) {
                    return false;
                }
            }

            None => {
                assert!(mark_used_name(&presence_flag.names, &mut used_flag_names));

                if cmp_rest_arg.is_none() {
                    return false;
                }
            }
        }
    }

    for cmp_presence_flag in cmp_presence_flags {
        if mark_used_name(&cmp_presence_flag.names, &mut use_cmp_flag_names) {
            return false;
        }
    }

    for normal_flag in normal_flags {
        match cmp_normal_flags
            .iter()
            .find(|c| check_fn_flag_args_name_compat(&normal_flag.names, &c.names))
        {
            Some(cmp_normal_flag) => {
                if !mark_used_name(&normal_flag.names, &mut used_flag_names) {
                    return false;
                }

                if !mark_used_name(&cmp_normal_flag.names, &mut use_cmp_flag_names) {
                    return false;
                }

                if normal_flag.is_optional && !cmp_normal_flag.is_optional {
                    return false;
                }

                if let (Some(typ), Some(cmp_typ)) = (&normal_flag.typ, &cmp_normal_flag.typ) {
                    if !check_if_type_fits_type(typ.data(), cmp_typ.data(), ctx) {
                        return false;
                    }
                }
            }

            None => {
                assert!(mark_used_name(&normal_flag.names, &mut used_flag_names));

                if cmp_rest_arg.is_none() {
                    return false;
                }
            }
        }
    }

    for cmp_normal_flag in cmp_normal_flags {
        if mark_used_name(&cmp_normal_flag.names, &mut use_cmp_flag_names) {
            return false;
        }
    }

    todo!()
}

/// Check if a set of flag names is compatible with another
///
/// For instance, if the target (into) has '-c' and '--current',
/// the source must have both those parameters as it could otherwise produce
/// a case where a call to it would result in an error due to it missing
/// either the short and long flag
#[must_use]
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

/// Data structure for comparing functions' arguments
struct FnCategorizedArgs<'a> {
    positionals: Vec<&'a FnPositionalArg>,
    presence_flags: Vec<&'a FnPresenceFlagArg>,
    normal_flags: Vec<&'a FnNormalFlagArg>,
    rest_arg: Option<&'a FnRestArg>,
}

impl<'a> FnCategorizedArgs<'a> {
    #[must_use]
    pub fn categorize_from(fn_signature: &'a FnSignature) -> Self {
        let mut positionals = vec![];
        let mut presence_flags = vec![];
        let mut normal_flags = vec![];
        let mut rest_arg = None;

        for arg in fn_signature.args.data() {
            match arg {
                FnArg::Positional(arg) => positionals.push(arg),
                FnArg::PresenceFlag(arg) => presence_flags.push(arg),
                FnArg::NormalFlag(arg) => normal_flags.push(arg),
                FnArg::Rest(rest) => rest_arg = Some(rest),
            }
        }

        Self {
            positionals,
            presence_flags,
            normal_flags,
            rest_arg,
        }
    }
}
