use std::borrow::Cow;

use parsy::{CodeRange, FileId};
use reshell_parser::ast::{FnArgNames, FnSignature, SingleValueType, ValueType};

use crate::{
    context::Context, errors::ExecResult, files_map::ScopableFilePath, values::RuntimeValue,
};

pub fn value_to_str(value: &RuntimeValue, at: CodeRange, ctx: &Context) -> ExecResult<String> {
    match value {
        RuntimeValue::Bool(bool) => Ok(bool.to_string()),
        RuntimeValue::Int(num) => Ok(num.to_string()),
        RuntimeValue::Float(num) => Ok(num.to_string()),
        RuntimeValue::String(str) => Ok(str.clone()),
        RuntimeValue::Null
        | RuntimeValue::List(_)
        | RuntimeValue::Range { from: _, to: _ }
        | RuntimeValue::Map(_)
        | RuntimeValue::Struct(_)
        | RuntimeValue::Function(_)
        | RuntimeValue::Error { at: _, msg: _ } => Err(ctx.error(
            at,
            format!(
                "cannot convert a {} to a string",
                readable_value_type(value, ctx)
            ),
        )),
    }
}

pub fn readable_value_type(value: &RuntimeValue, ctx: &Context) -> Cow<'static, str> {
    readable_single_type(&value.get_type(), ctx)
}

pub fn readable_type(value_type: &ValueType, ctx: &Context) -> Cow<'static, str> {
    match value_type {
        ValueType::Single(single) => readable_single_type(single.data(), ctx),
        ValueType::Union(types) => types
            .iter()
            .map(|typ| readable_single_type(typ.data(), ctx))
            .collect::<Vec<_>>()
            .join(" | ")
            .into(),
    }
}

pub fn readable_single_type(value_type: &SingleValueType, ctx: &Context) -> Cow<'static, str> {
    match value_type {
        SingleValueType::Any => "any".into(),
        SingleValueType::Null => "null value".into(),
        SingleValueType::Bool => "boolean".into(),
        SingleValueType::Int => "int".into(),
        SingleValueType::Float => "float".into(),
        SingleValueType::String => "string".into(),
        SingleValueType::List => "list".into(),
        SingleValueType::Range => "range".into(),
        SingleValueType::Map => "map".into(),
        SingleValueType::UntypedStruct => "struct".into(),
        SingleValueType::TypedStruct(_) =>
        // TODO: full type
        {
            "struct".into()
        }
        SingleValueType::Function(signature) => dbg_fn_signature(signature, ctx).into(),
        SingleValueType::Error => "error".into(),
        SingleValueType::TypeAlias(name) => format!(
            "{} {}",
            name.data,
            match ctx
                .all_type_aliases()
                .find_map(|(alias_name, typ)| if alias_name == &name.data {
                    Some(typ)
                } else {
                    None
                }) {
                Some(typ) => format!("({})", readable_type(typ, ctx)),
                None => "(unknown type alias)".to_string(),
            }
        )
        .into(),
    }
}

pub fn dbg_value(value: &RuntimeValue, ctx: &Context) -> String {
    match value {
        RuntimeValue::Null => "null".to_string(),
        RuntimeValue::Bool(bool) => bool.to_string(),
        RuntimeValue::Int(int) => int.to_string(),
        RuntimeValue::Float(float) => float.to_string(),
        RuntimeValue::String(string) => format!("\"{string}\""),
        RuntimeValue::List(items) => format!(
            "[{}]",
            items
                .iter()
                .map(|value| dbg_value(value, ctx))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        RuntimeValue::Range { from, to } => format!("range({from}, {to})"),
        RuntimeValue::Map(members) => format!(
            "map([{}])",
            members
                .iter()
                .map(|(key, value)| format!("{key} => {}", dbg_value(value, ctx)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        RuntimeValue::Struct(props) => format!(
            "{{ {} }}",
            props
                .iter()
                .map(|(key, value)| format!("{key}: {}", dbg_value(value, ctx)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        RuntimeValue::Function(func) => dbg_fn_signature(&func.signature, ctx),
        RuntimeValue::Error { at: _, msg } => format!("error(\"{msg}\")"),
    }
}

pub fn dbg_fn_signature(signature: &FnSignature, ctx: &Context) -> String {
    let FnSignature { args, ret_type } = signature;

    format!(
        "({}){}",
        args.iter()
            .map(|arg| {
                let names = match &arg.names {
                    FnArgNames::NotFlag(name) => name.data.clone(),
                    FnArgNames::ShortFlag(short) => format!("-{}", short.data),
                    FnArgNames::LongFlag(long) => format!("--{}", long.data),
                    FnArgNames::LongAndShortFlag { long, short } => {
                        format!("--{} (-{})", long.data, short.data)
                    }
                };

                format!(
                    "{}{}{}",
                    names,
                    if arg.is_optional { "?" } else { "" },
                    match &arg.typ {
                        Some(typ) => format!(": {}", readable_type(&typ.data, ctx)),
                        None => String::new(),
                    }
                )
            })
            .collect::<Vec<_>>()
            .join(", "),
        match ret_type {
            None => String::new(),
            Some(ret_type) => format!(" -> {}", readable_type(&ret_type.data, ctx)),
        }
    )
}

pub fn dbg_loc(at: CodeRange, ctx: &Context) -> String {
    match at.start.file_id {
        FileId::None => unreachable!(),
        FileId::Id(id) => {
            let Some(file) = ctx.files_map().get_file(id) else {
                return format!("<unknown file @ offset {}>", at.start.offset);
            };

            let bef = &file.content.as_str()[..at.start.offset];

            let line = bef.chars().filter(|c| *c == '\n').count() + 1;

            let after_last_nl = match bef.rfind('\n') {
                Some(index) => &bef[index + 1..],
                None => bef,
            };

            let col = after_last_nl.chars().count() + 1;

            format!(
                "{}:{}:{}",
                match &file.path {
                    ScopableFilePath::InMemory(name) => format!("<{name}>"),
                    ScopableFilePath::RealFile(path) => path.to_string_lossy().to_string(),
                },
                line,
                col
            )
        }
        FileId::Internal => "<internal>".into(),
        FileId::SourceLess { name } => match name {
            Some(name) => format!("<source-less: {name}>"),
            None => "<source-less>".into(),
        },
    }
}
