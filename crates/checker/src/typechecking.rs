use std::collections::HashSet;

use reshell_parser::ast::{
    FnSignatureRestArg, FnSignature, FnSignatureArg, FnSignatureFlagArgNames, FnSignatureNormalFlagArg,
    FnSignaturePositionalArg, FnSignaturePresenceFlagArg, SingleValueType, StructTypeMember,
    ValueType,
};
use reshell_prettify::TypeAliasStore;

/// Check if a type can fit into another (which is if all values of this type would be compatible with the target type)
#[must_use]
pub fn check_if_type_fits_type(
    value_type: &ValueType,
    into: &ValueType,
    ctx: &TypeAliasStore,
) -> bool {
    match value_type {
        ValueType::Single(single) => check_if_single_type_fits_type(single, into, ctx),

        ValueType::Union(types) => types
            .iter()
            .all(|typ| check_if_single_type_fits_type(typ, into, ctx)),
    }
}

/// Check if a single (non-union) type fits into another type
#[must_use]
pub fn check_if_single_type_fits_type(
    value_type: &SingleValueType,
    into: &ValueType,
    ctx: &TypeAliasStore,
) -> bool {
    match into {
        ValueType::Single(single) => check_if_single_type_fits_single(value_type, single, ctx),

        ValueType::Union(types) => types
            .iter()
            .any(|typ| check_if_single_type_fits_single(value_type, typ, ctx)),
    }
}

/// Check if a type fits into a single (non-union) type
#[must_use]
pub fn check_if_type_fits_single(
    value_type: &ValueType,
    into: &SingleValueType,
    ctx: &TypeAliasStore,
) -> bool {
    match value_type {
        ValueType::Single(single) => check_if_single_type_fits_single(single, into, ctx),

        ValueType::Union(types) => types
            .iter()
            .all(|typ| check_if_single_type_fits_single(typ, into, ctx)),
    }
}

/// Check if a single (non-union) type fits into another single type
#[must_use]
pub fn check_if_single_type_fits_single(
    value_type: &SingleValueType,
    into: &SingleValueType,
    ctx: &TypeAliasStore,
) -> bool {
    match (value_type, into) {
        (SingleValueType::Void, SingleValueType::Void) => true,
        (SingleValueType::Void, _) | (_, SingleValueType::Void) => false,

        (SingleValueType::Any, _) | (_, SingleValueType::Any) => true,

        (SingleValueType::TypeAlias(name), _) => {
            check_if_type_fits_single(&ctx.get(name).unwrap().data, into, ctx)
        }

        (_, SingleValueType::TypeAlias(name)) => {
            check_if_single_type_fits_type(value_type, &ctx.get(name).unwrap().data, ctx)
        }

        (SingleValueType::Null, SingleValueType::Null) => true,
        (SingleValueType::Null, _) | (_, SingleValueType::Null) => false,

        (SingleValueType::Bool, SingleValueType::Bool) => true,
        (SingleValueType::Bool, _) | (_, SingleValueType::Bool) => false,

        (SingleValueType::Int, SingleValueType::Int) => true,
        (SingleValueType::Int, _) | (_, SingleValueType::Int) => false,

        (SingleValueType::Float, SingleValueType::Float) => true,
        (SingleValueType::Float, _) | (_, SingleValueType::Float) => false,

        (SingleValueType::String | SingleValueType::StringLiteral(_), SingleValueType::String) => {
            true
        }
        (SingleValueType::String, _) | (_, SingleValueType::String) => false,

        (SingleValueType::StringLiteral(left), SingleValueType::StringLiteral(right)) => {
            left == right
        }
        (SingleValueType::StringLiteral(_), _) | (_, SingleValueType::StringLiteral(_)) => false,

        (SingleValueType::CmdCall, SingleValueType::CmdCall) => true,
        (SingleValueType::CmdCall, _) | (_, SingleValueType::CmdCall) => false,

        (SingleValueType::CmdArg, SingleValueType::CmdArg) => true,
        (SingleValueType::CmdArg, _) | (_, SingleValueType::CmdArg) => false,

        (SingleValueType::Error, SingleValueType::Error) => true,
        (SingleValueType::Error, _) | (_, SingleValueType::Error) => false,

        (SingleValueType::Custom(a), SingleValueType::Custom(b)) => a == b,
        (SingleValueType::Custom(_), _) | (_, SingleValueType::Custom(_)) => false,

        (SingleValueType::Function(signature), SingleValueType::Function(into)) => {
            check_if_fn_signature_fits_another(&signature.data, &into.data, ctx)
        }

        (SingleValueType::Function(_), _) | (_, SingleValueType::Function(_)) => false,

        (SingleValueType::UntypedList, SingleValueType::UntypedList)
        | (SingleValueType::UntypedList, SingleValueType::TypedList(_))
        | (SingleValueType::TypedList(_), SingleValueType::UntypedList) => true,
        (SingleValueType::UntypedList, _) | (_, SingleValueType::UntypedList) => false,

        (SingleValueType::TypedList(a), SingleValueType::TypedList(b)) => {
            check_if_type_fits_type(a, b, ctx)
        }
        (SingleValueType::TypedList(_), _) | (_, SingleValueType::TypedList(_)) => false,

        (SingleValueType::UntypedMap, SingleValueType::UntypedMap)
        | (SingleValueType::UntypedMap, SingleValueType::TypedMap(_))
        | (SingleValueType::TypedMap(_), SingleValueType::UntypedMap) => true,
        (SingleValueType::UntypedMap, _) | (_, SingleValueType::UntypedMap) => false,

        (SingleValueType::TypedMap(a), SingleValueType::TypedMap(b)) => {
            check_if_type_fits_type(a, b, ctx)
        }
        (SingleValueType::TypedMap(_), _) | (_, SingleValueType::TypedMap(_)) => false,

        (SingleValueType::UntypedStruct, SingleValueType::UntypedStruct) => true,
        (SingleValueType::TypedStruct(_), SingleValueType::UntypedStruct) => true,
        (SingleValueType::UntypedStruct, SingleValueType::TypedStruct(_)) => true,

        (SingleValueType::TypedStruct(a), SingleValueType::TypedStruct(b)) => {
            b.iter().all(|b_member| {
                let StructTypeMember {
                    name,
                    optional: _,
                    typ,
                } = &b_member;

                match a.iter().find(|member| {
                    member.name.data == name.data && member.optional == b_member.optional
                }) {
                    Some(a_member) => check_if_type_fits_type(&a_member.typ, typ, ctx),
                    None => b_member.optional,
                }
            })
        } //
          // (SingleValueType::UntypedStruct, _) | (_, SingleValueType::UntypedStruct) => false,
          // (SingleValueType::TypedStruct(_), _) | (_, SingleValueType::TypedStruct(_)) => false,
    }
}

/// Check if a function signature can fit into another
#[must_use]
pub fn check_if_fn_signature_fits_another(
    signature: &FnSignature,
    into: &FnSignature,
    ctx: &TypeAliasStore,
) -> bool {
    // Compare return types
    if let (Some(ret_type), Some(cmp_ret_type)) = (&signature.ret_type, &into.ret_type) {
        if !check_if_type_fits_type(&ret_type.data, &cmp_ret_type.data, ctx) {
            return false;
        }
    }

    // Categorize arguments for easier comparison
    let FnCategorizedArgs {
        positionals: into_positionals,
        presence_flags: into_presence_flags,
        normal_flags: into_normal_flags,
        rest_arg: into_rest_arg,
    } = FnCategorizedArgs::categorize_from(into);

    let FnCategorizedArgs {
        positionals,
        presence_flags,
        normal_flags,
        rest_arg,
    } = FnCategorizedArgs::categorize_from(signature);

    // If the target type has a rest argument, the source one must too
    if into_rest_arg.is_some() && rest_arg.is_none() {
        return false;
    }

    // Compare rest arguments' type
    if let (Some(rest_arg), Some(into_rest_arg)) = (rest_arg, into_rest_arg) {
        if let (Some(rest_arg_type), Some(into_rest_arg_type)) = (&rest_arg.typ, &into_rest_arg.typ)
        {
            if !check_if_type_fits_type(&rest_arg_type.data, &into_rest_arg_type.data, ctx) {
                return false;
            }
        }
    }

    // Iterate over all positional arguments
    let mut positionals = positionals.iter();

    for into_positional in into_positionals {
        let compat = match positionals.next() {
            Some(positional) => {
                if into_positional.is_optional && !positional.is_optional && rest_arg.is_none() {
                    false
                } else if let (Some(positional_typ), Some(into_positional_typ)) =
                    (&positional.typ, &into_positional.typ)
                {
                    // Here we comparison is inversed as we need to check if every single value from the
                    // destination argument would be compatible with the provided signature
                    check_if_type_fits_type(into_positional_typ, positional_typ, ctx)
                } else {
                    true
                }
            }

            None => into_positional.is_optional,
        };

        if !compat {
            return false;
        }
    }

    // Ensure that no required positional argument is only present in the source type
    for positional in positionals {
        if !positional.is_optional {
            return false;
        }
    }

    // Mark used flag names
    let mut used_flag_names = HashSet::new();
    let mut used_into_flag_names = HashSet::new();

    let mark_used_name = |flag_names: &FnSignatureFlagArgNames,
                          used_names: &mut HashSet<String>|
     -> bool {
        match flag_names {
            FnSignatureFlagArgNames::ShortFlag(name) => used_names.insert(name.data.to_string()),
            FnSignatureFlagArgNames::LongFlag(name) => used_names.insert(name.data.clone()),
            FnSignatureFlagArgNames::LongAndShortFlag { long, short } => {
                used_names.insert(long.data.clone()) && used_names.insert(short.data.to_string())
            }
        }
    };

    for into_presence_flag in into_presence_flags {
        // Mark this flag's name as used (useful for next loop below)
        assert!(mark_used_name(
            &into_presence_flag.names,
            &mut used_flag_names
        ));

        // Find a presence flag that matches the target type's one
        match presence_flags
            .iter()
            .find(|c| check_fn_flag_args_name_compat(&into_presence_flag.names, &c.names))
        {
            // If there is a match...
            Some(presence_flag) => {
                // Mark the (source) flag's names as used
                // If the names are already used, it means we have already associated it beforehand
                // This can typically happen in the following scenario:
                //
                // fn(-r, --recursive) # two arguments '-r' and '--recursive'
                // fn(--recursive (r)) # one argument which can be either '-r' or '--recursive'
                // => these are NOT compatible
                if !mark_used_name(&presence_flag.names, &mut used_into_flag_names) {
                    return false;
                }
            }

            None => {
                // If there is no matching flag on the other side, compatibility can only be ensured if the source type
                // has a rest argument to collect it.
                if rest_arg.is_none() {
                    return false;
                }
            }
        }
    }

    // Ensure that no presence flag exists in the source type that refers to another argument in the target type
    for presence_flag in presence_flags {
        mark_used_name(&presence_flag.names, &mut used_into_flag_names);
    }

    for into_normal_flag in into_normal_flags {
        // Mark this flag's name as used (useful for next loop below)
        assert!(mark_used_name(
            &into_normal_flag.names,
            &mut used_flag_names
        ));

        // Find a normal flag that matches the target type's one
        match normal_flags
            .iter()
            .find(|c| check_fn_flag_args_name_compat(&into_normal_flag.names, &c.names))
        {
            // If there is a match...
            Some(normal_flag) => {
                // Mark the (source) flag's names as used
                // If the names are already used, it means we have already associated it beforehand
                // This can typically happen in the following scenario:
                //
                // fn(-n: string, --name: string) # two arguments '-n' and '--name'
                // fn(--name (-n): string)        # one argument which can be either '-n' or '--name'
                // => these are NOT compatible
                if !mark_used_name(&normal_flag.names, &mut used_into_flag_names) {
                    return false;
                }

                // If the target type marks it as optional, the source type must too
                if into_normal_flag.is_optional && !normal_flag.is_optional {
                    return false;
                }

                // Check if the type fits as well
                if !check_if_type_fits_type(&normal_flag.typ, &into_normal_flag.typ, ctx) {
                    return false;
                }
            }

            None => {
                // If there is no matching flag on the other side, compatibility can only be ensured if the source type
                // has a rest argument to collect it.
                if rest_arg.is_none() {
                    return false;
                }
            }
        }
    }

    // Ensure that no normal flag exists in the source type that refers to another argument in the target type
    // Also ensure that no required normal flag is absent from the target type
    for cmp_normal_flag in normal_flags {
        if mark_used_name(&cmp_normal_flag.names, &mut used_into_flag_names)
            && !cmp_normal_flag.is_optional
        {
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
#[must_use]
fn check_fn_flag_args_name_compat(
    curr: &FnSignatureFlagArgNames,
    into: &FnSignatureFlagArgNames,
) -> bool {
    match into {
        FnSignatureFlagArgNames::ShortFlag(b) => match curr {
            FnSignatureFlagArgNames::LongFlag(_) => false,
            FnSignatureFlagArgNames::ShortFlag(a)
            | FnSignatureFlagArgNames::LongAndShortFlag { long: _, short: a } => a.data == b.data,
        },

        FnSignatureFlagArgNames::LongFlag(b) => match curr {
            FnSignatureFlagArgNames::ShortFlag(_) => false,
            FnSignatureFlagArgNames::LongFlag(a)
            | FnSignatureFlagArgNames::LongAndShortFlag { long: a, short: _ } => a.data == b.data,
        },

        FnSignatureFlagArgNames::LongAndShortFlag { long, short } => match curr {
            FnSignatureFlagArgNames::ShortFlag(_) | FnSignatureFlagArgNames::LongFlag(_) => false,
            FnSignatureFlagArgNames::LongAndShortFlag {
                long: a_long,
                short: a_short,
            } => a_long.data == long.data && a_short.data == short.data,
        },
    }
}

/// Data structure for comparing functions' arguments
struct FnCategorizedArgs<'a> {
    positionals: Vec<&'a FnSignaturePositionalArg>,
    presence_flags: Vec<&'a FnSignaturePresenceFlagArg>,
    normal_flags: Vec<&'a FnSignatureNormalFlagArg>,
    rest_arg: Option<&'a FnSignatureRestArg>,
}

impl<'a> FnCategorizedArgs<'a> {
    #[must_use]
    pub fn categorize_from(fn_signature: &'a FnSignature) -> Self {
        let mut positionals = vec![];
        let mut presence_flags = vec![];
        let mut normal_flags = vec![];
        let mut rest_arg = None;

        for arg in &fn_signature.args.data {
            match arg {
                FnSignatureArg::Positional(arg) => positionals.push(arg),
                FnSignatureArg::PresenceFlag(arg) => presence_flags.push(arg),
                FnSignatureArg::NormalFlag(arg) => normal_flags.push(arg),
                FnSignatureArg::Rest(rest) => rest_arg = Some(rest),
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
