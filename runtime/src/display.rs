use std::borrow::Cow;

use colored::Color;
use parsy::{CodeRange, FileId};
use reshell_parser::ast::{FnArg, FnArgNames, FnSignature, SingleValueType, ValueType};

use crate::{
    context::Context,
    errors::ExecResult,
    files_map::{FilesMap, ScopableFilePath},
    pretty::{Colored, PrettyPrintOptions, PrettyPrintable, PrintablePiece},
    values::RuntimeValue,
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
                readable_value_type(value)
            ),
        )),
    }
}

pub fn readable_value_type(value: &RuntimeValue /*ctx: &Context*/) -> Cow<'static, str> {
    readable_single_type(&value.get_type())
}

pub fn readable_type(value_type: &ValueType /*, ctx: &Context*/) -> Cow<'static, str> {
    match value_type {
        ValueType::Single(single) => readable_single_type(single.data()),
        ValueType::Union(types) => types
            .iter()
            .map(|typ| readable_single_type(typ.data()))
            .collect::<Vec<_>>()
            .join(" | ")
            .into(),
    }
}

pub fn readable_single_type(
    value_type: &SingleValueType, /* , ctx: &Context*/
) -> Cow<'static, str> {
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
        SingleValueType::Function(signature) => {
            signature.render(PrettyPrintOptions::inline()).into()
        }
        SingleValueType::Error => "error".into(),
        SingleValueType::TypeAlias(name) => {
            //     format!(
            //     "{} {}",
            //     name.data,
            //     match ctx
            //         .all_type_aliases()
            //         .find_map(|(alias_name, typ)| if alias_name == &name.data {
            //             Some(typ)
            //         } else {
            //             None
            //         }) {
            //         Some(typ) => format!("({})", readable_type(typ, ctx)),
            //         None => "(unknown type alias)".to_string(),
            //     }
            // )
            // .into()
            name.data.clone().into()
        }
    }
}

pub fn dbg_loc(at: CodeRange, files_map: &FilesMap) -> String {
    match at.start.file_id {
        FileId::None => unreachable!(),
        FileId::Id(id) => {
            let Some(file) = files_map.get_file(id) else {
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

impl PrettyPrintable for RuntimeValue {
    fn generate_pretty_data(&self) -> PrintablePiece {
        match self {
            RuntimeValue::Null => {
                PrintablePiece::colored_atomic("null".to_string(), Color::BrightYellow)
            }
            RuntimeValue::Bool(bool) => {
                PrintablePiece::colored_atomic(bool.to_string(), Color::BrightYellow)
            }
            RuntimeValue::Int(int) => {
                PrintablePiece::colored_atomic(int.to_string(), Color::BrightYellow)
            }
            RuntimeValue::Float(float) => {
                PrintablePiece::colored_atomic(float.to_string(), Color::BrightYellow)
            }
            RuntimeValue::String(string) => PrintablePiece::colored_atomic(
                format!(
                    "\"{}\"",
                    string
                        .replace('\\', "\\\\")
                        .replace('\"', "\\\"")
                        .replace('\n', "\\n")
                ),
                Color::BrightGreen,
            ),
            RuntimeValue::List(list) => PrintablePiece::List {
                begin: Colored::with_color("[".to_string(), Color::Blue),
                items: list
                    .iter()
                    .map(|item| item.generate_pretty_data())
                    .collect(),
                sep: Colored::with_color(",".to_string(), Color::Blue),
                end: Colored::with_color("]".to_string(), Color::Blue),
                suffix: None,
            },
            RuntimeValue::Range { from, to } => PrintablePiece::List {
                begin: Colored::with_color("range(".to_string(), Color::Blue),
                items: vec![
                    PrintablePiece::Atomic(Colored::with_color(
                        from.to_string(),
                        Color::BrightYellow,
                    )),
                    PrintablePiece::Atomic(Colored::with_color(
                        to.to_string(),
                        Color::BrightYellow,
                    )),
                ],
                sep: Colored::with_color(",".to_string(), Color::Blue),
                end: Colored::with_color(")".to_string(), Color::Blue),
                suffix: None,
            },
            RuntimeValue::Map(map) => PrintablePiece::List {
                begin: Colored::with_color("map({".to_string(), Color::Blue),
                items: map
                    .iter()
                    .map(|(key, value)|
                        // Yes, that part is a hack :p
                        PrintablePiece::List {
                            begin: Colored::with_color(
                            format!(
                                "\"{}\": ",
                                key
                                    .replace('\\', "\\\\")
                                    .replace('\"', "\\\"")
                                    .replace('\n', "\\n")
                                ),
                                Color::Green
                            ),
                            items: vec![
                                value.generate_pretty_data()
                            ],
                            sep: Colored::empty(),
                            end: Colored::empty(),
                            suffix: None
                        })
                    .collect(),
                sep: Colored::with_color(",".to_string(), Color::Blue),
                end: Colored::with_color("})".to_string(), Color::Blue),
                suffix: None,
            },
            RuntimeValue::Struct(obj) => PrintablePiece::List {
                begin: Colored::with_color("{".to_string(), Color::Blue),
                items: obj
                    .iter()
                    .map(|(field, value)|
                        // Yes, that part is a hack :p
                        PrintablePiece::List {
                            begin: Colored::with_color(
                                format!("{field}: "),
                                Color::Red
                            ),
                            items: vec![
                                value.generate_pretty_data()
                            ],
                            sep: Colored::empty(),
                            end: Colored::empty(),
                            suffix: None
                        })
                    .collect(),
                sep: Colored::with_color(",".to_string(), Color::Blue),
                end: Colored::with_color("}".to_string(), Color::Blue),
                suffix: None,
            },
            RuntimeValue::Function(func) =>
            // TODO: show something like { ... } after the fn to show it's a value and not a type
            {
                func.signature.generate_pretty_data()
            }
            RuntimeValue::Error { at: _, msg } => PrintablePiece::List {
                begin: Colored::with_color("error(".to_string(), Color::Red),
                items: vec![
                    PrintablePiece::Atomic(Colored::with_color(
                        format!(
                            "\"{}\"",
                            msg.replace('\\', "\\\\")
                                .replace('\"', "\\\"")
                                .replace('\n', "\\n")
                        ),
                        Color::Green,
                    )),
                    // TODO: printing "at" requires access to context in order to find the filename
                ],
                sep: Colored::empty(),
                end: Colored::with_color(")".to_string(), Color::Red),
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for FnSignature {
    fn generate_pretty_data(&self) -> PrintablePiece {
        let Self { args, ret_type } = self;

        PrintablePiece::List {
            begin: Colored::with_color("(".to_string(), Color::Blue),
            items: args
                .iter()
                .map(|item| item.generate_pretty_data())
                .collect(),
            sep: Colored::with_color(",".to_string(), Color::Blue),
            end: Colored::with_color(")".to_string(), Color::Blue),
            suffix: ret_type.as_ref().map(|ret_type| {
                // TODO: colors for return type?
                Colored::with_color(
                    format!(" -> {}", readable_type(&ret_type.data)),
                    Color::BrightMagenta,
                )
            }),
        }
    }
}

impl PrettyPrintable for FnArg {
    fn generate_pretty_data(&self) -> PrintablePiece {
        let Self {
            names,
            is_optional,
            is_rest,
            typ,
        } = self;

        let mut out = vec![];

        if *is_rest {
            out.push(Colored::with_color("...".to_string(), Color::BrightYellow));
        }

        match names {
            FnArgNames::NotFlag(name) => {
                out.extend([Colored::with_color(name.data.clone(), Color::Red)]);
            }
            FnArgNames::ShortFlag(short) => {
                out.extend([
                    Colored::with_color("-".to_string(), Color::BrightYellow),
                    Colored::with_color(short.data.to_string(), Color::Red),
                ]);
            }
            FnArgNames::LongFlag(long) => {
                out.extend([
                    Colored::with_color("--".to_string(), Color::BrightYellow),
                    Colored::with_color(long.data.clone(), Color::Red),
                ]);
            }
            FnArgNames::LongAndShortFlag { long, short } => {
                out.extend([
                    Colored::with_color("--".to_string(), Color::BrightYellow),
                    Colored::with_color(long.data.clone(), Color::Red),
                    Colored::with_color(" (".to_string(), Color::Blue),
                    Colored::with_color("-".to_string(), Color::BrightYellow),
                    Colored::with_color(short.data.to_string(), Color::BrightYellow),
                    Colored::with_color(")".to_string(), Color::Blue),
                ]);
            }
        }

        if *is_optional {
            out.push(Colored::with_color("?".to_string(), Color::BrightYellow));
        }

        if let Some(typ) = typ {
            out.push(Colored::with_color(": ".to_string(), Color::BrightYellow));
            out.push(Colored::with_color(
                readable_type(&typ.data).into_owned(),
                Color::BrightMagenta,
            ));
        }

        PrintablePiece::Suite(out)
    }
}
