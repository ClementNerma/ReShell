use reshell_parser::ast::{FnArgNames, FnSignature, SingleValueType, ValueType};

pub fn check_if_type_fits(value_type: &ValueType, into: &ValueType) -> bool {
    match value_type {
        ValueType::Single(single_type) => check_if_single_type_fits(&single_type.data, into),
        ValueType::Union(types) => types
            .iter()
            .all(|single| check_if_single_type_fits(&single.data, into)),
    }
}

pub fn check_if_single_type_fits(value_type: &SingleValueType, into: &ValueType) -> bool {
    match into {
        ValueType::Single(single) => check_if_type_fits_single(value_type, &single.data),
        ValueType::Union(types) => types
            .iter()
            .any(|single| check_if_type_fits_single(value_type, &single.data)),
    }
}

pub fn check_if_type_fits_single(value_type: &SingleValueType, into: &SingleValueType) -> bool {
    match (value_type, into) {
        (_, SingleValueType::Any) => true,
        (SingleValueType::Any, _) => true,

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

        (SingleValueType::Struct, SingleValueType::Struct) => true,
        (SingleValueType::Struct, _) | (_, SingleValueType::Struct) => false,

        (SingleValueType::Error, SingleValueType::Error) => true,
        (SingleValueType::Error, _) | (_, SingleValueType::Error) => false,

        (SingleValueType::Function(signature), SingleValueType::Function(into)) => {
            check_fn_equality(signature, into)
        }
    }
}

// TODO: with detailed error message (ExecResult<()>)?
pub fn check_fn_equality(signature: &FnSignature, into: &FnSignature) -> bool {
    if signature.args.len() != into.args.len() {
        return false;
    }

    for (arg, cmp_arg) in signature.args.iter().zip(into.args.iter()) {
        if arg.is_optional != cmp_arg.is_optional {
            return false;
        }

        if arg.is_rest != cmp_arg.is_rest {
            return false;
        }

        match (&arg.names, &cmp_arg.names) {
            (FnArgNames::NotFlag(_), FnArgNames::NotFlag(_)) => {}
            (FnArgNames::ShortFlag(_), FnArgNames::ShortFlag(_)) => {}
            (FnArgNames::LongFlag(_), FnArgNames::LongFlag(_)) => {}
            (
                FnArgNames::LongAndShortFlag { long: _, short: _ },
                FnArgNames::LongAndShortFlag { long: _, short: _ },
            ) => {}

            (FnArgNames::NotFlag(_), _) | (_, FnArgNames::NotFlag(_)) => return false,
            (FnArgNames::ShortFlag(_), _) | (_, FnArgNames::ShortFlag(_)) => return false,
            (FnArgNames::LongFlag(_), _) | (_, FnArgNames::LongFlag(_)) => return false,
        }

        if let (Some(arg_type), Some(cmp_arg_type)) = (&arg.typ, &cmp_arg.typ) {
            if !check_if_type_fits(&arg_type.data, &cmp_arg_type.data) {
                return false;
            }
        }
    }

    if let (Some(ret_type), Some(cmp_ret_type)) = (&signature.ret_type, &into.ret_type) {
        if !check_if_type_fits(ret_type.data.as_ref(), &cmp_ret_type.data) {
            return false;
        }
    }

    true
}
