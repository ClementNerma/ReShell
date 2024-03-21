use colored::Color;
use parsy::{CodeRange, FileId};
use reshell_parser::ast::{
    CmdFlagNameArg, FnArg, FnArgNames, FnSignature, RuntimeCodeRange, SingleValueType,
    StructTypeMember, ValueType,
};

use crate::{
    cmd::{CmdArgResult, CmdSingleArgResult},
    context::Context,
    errors::ExecResult,
    files_map::{FilesMap, ScopableFilePath},
    pretty::{Colored, PrettyPrintOptions, PrettyPrintable, PrettyPrintablePiece},
    values::{RuntimeFnBody, RuntimeValue},
};

pub fn value_to_str(value: &RuntimeValue, at: CodeRange, ctx: &Context) -> ExecResult<String> {
    match value {
        RuntimeValue::Bool(bool) => Ok(bool.to_string()),
        RuntimeValue::Int(num) => Ok(num.to_string()),
        RuntimeValue::Float(num) => Ok(num.to_string()),
        RuntimeValue::String(str) => Ok(str.clone()),
        RuntimeValue::ArgSpread(_) => Err(ctx.error(at, "cannot convert a spread to a string".to_owned())),
        RuntimeValue::Null
        | RuntimeValue::List(_)
        | RuntimeValue::Range { from: _, to: _ }
        | RuntimeValue::Map(_)
        | RuntimeValue::Struct(_)
        | RuntimeValue::Function(_)
        | RuntimeValue::Error { at: _, msg: _ } => Err(ctx
            .error(
                at,
                format!(
                    "cannot convert {} to a string",
                    value
                        .get_type()
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            )
            .with_note(
                "this conversion happens because external commands can only take string-like arguments",
            )),
    }
}

impl PrettyPrintable for ValueType {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        match self {
            Self::Single(single) => single.data().generate_pretty_data(ctx),

            Self::Union(types) => PrettyPrintablePiece::List {
                begin: Colored::empty(),
                items: types
                    .iter()
                    .map(|typ| typ.data().generate_pretty_data(ctx))
                    .collect(),
                sep: Colored::with_color(" |", Color::Magenta),
                end: Colored::empty(),
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for SingleValueType {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        match self {
            Self::Any => PrettyPrintablePiece::colored_atomic("any", Color::Magenta),
            Self::Null => PrettyPrintablePiece::colored_atomic("null", Color::Magenta),
            Self::Bool => PrettyPrintablePiece::colored_atomic("boolean", Color::Magenta),
            Self::Int => PrettyPrintablePiece::colored_atomic("int", Color::Magenta),
            Self::Float => PrettyPrintablePiece::colored_atomic("float", Color::Magenta),
            Self::String => PrettyPrintablePiece::colored_atomic("string", Color::Magenta),
            Self::List => PrettyPrintablePiece::colored_atomic("list", Color::Magenta),
            Self::Range => PrettyPrintablePiece::colored_atomic("range", Color::Magenta),
            Self::Map => PrettyPrintablePiece::colored_atomic("map", Color::Magenta),
            Self::Error => PrettyPrintablePiece::colored_atomic("error", Color::Magenta),
            Self::ArgSpread => PrettyPrintablePiece::colored_atomic("spread", Color::Magenta),
            Self::UntypedStruct => PrettyPrintablePiece::colored_atomic("struct", Color::Magenta),
            Self::TypedStruct(members) => PrettyPrintablePiece::List {
                begin: Colored::with_color("struct { ", Color::Magenta),
                items: members
                    .iter()
                    .map(|member| {
                        let StructTypeMember { name, typ } = member.data();

                        PrettyPrintablePiece::Join(vec![
                            PrettyPrintablePiece::colored_atomic(name.data().clone(), Color::Red),
                            PrettyPrintablePiece::colored_atomic(": ", Color::BrightBlack),
                            typ.data().generate_pretty_data(ctx),
                        ])
                    })
                    .collect(),
                sep: Colored::with_color(",", Color::BrightBlack),
                end: Colored::with_color(" }", Color::Magenta),
                suffix: None,
            },
            Self::Function(signature) => signature.data().generate_pretty_data(ctx),
            Self::TypeAlias(name) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic(format!("{} (", name.data), Color::Magenta),
                ctx
                    .get_type_alias(name)
                    .unwrap_or_else(|| panic!("internal error: type alias not found while generating pretty data for value type\n> Debug location: {name:?}"))
                    .data
                    .generate_pretty_data(ctx),
                PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
            ]),
        }
    }
}

pub fn dbg_loc(at: impl Into<RuntimeCodeRange>, files_map: &FilesMap) -> String {
    match at.into() {
        RuntimeCodeRange::Parsed(at) => match at.start.file_id {
            FileId::None => unreachable!(),
            FileId::SourceFile(id) => {
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

                        ScopableFilePath::InMemoryWithCounter(name, counter) =>
                            format!("<{name}[{counter}]>"),

                        ScopableFilePath::RealFile(path) => path.to_string_lossy().to_string(),
                    },
                    line,
                    col
                )
            }
            FileId::Internal => "<internal>".into(),
            FileId::Custom(id) => format!("<custom: {id}>"),
        },

        RuntimeCodeRange::Internal => "internal location".to_string(),
    }
}

impl PrettyPrintable for RuntimeValue {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        match self {
            RuntimeValue::Null => PrettyPrintablePiece::colored_atomic("null", Color::BrightYellow),

            RuntimeValue::Bool(bool) => {
                PrettyPrintablePiece::colored_atomic(bool.to_string(), Color::BrightYellow)
            }

            RuntimeValue::Int(int) => {
                PrettyPrintablePiece::colored_atomic(int.to_string(), Color::BrightYellow)
            }

            RuntimeValue::Float(float) => {
                PrettyPrintablePiece::colored_atomic(float.to_string(), Color::BrightYellow)
            }

            RuntimeValue::String(string) => PrettyPrintablePiece::colored_atomic(
                format!(
                    "\"{}\"",
                    string
                        .replace('\\', "\\\\")
                        .replace('\"', "\\\"")
                        .replace('\n', "\\n")
                ),
                Color::BrightGreen,
            ),

            RuntimeValue::Range { from, to } => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("range(", Color::Blue),
                PrettyPrintablePiece::colored_atomic(from.to_string(), Color::BrightYellow),
                PrettyPrintablePiece::colored_atomic(",", Color::Blue),
                PrettyPrintablePiece::colored_atomic(to.to_string(), Color::BrightYellow),
                PrettyPrintablePiece::colored_atomic(")", Color::Blue),
            ]),

            RuntimeValue::Error { at, msg } => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("error(", Color::Red),
                PrettyPrintablePiece::colored_atomic(
                    format!(
                        "\"{}\"",
                        msg.replace('\\', "\\\\")
                            .replace('\"', "\\\"")
                            .replace('\n', "\\n")
                    ),
                    Color::Green,
                ),
                PrettyPrintablePiece::colored_atomic(" @ ", Color::BrightMagenta),
                PrettyPrintablePiece::colored_atomic(
                    dbg_loc(*at, ctx.files_map()),
                    Color::BrightMagenta,
                ),
                PrettyPrintablePiece::colored_atomic(")", Color::Red),
            ]),

            RuntimeValue::ArgSpread(items) => PrettyPrintablePiece::List {
                begin: Colored::with_color("spread(", Color::Magenta),
                items: items
                    .iter()
                    .map(|item| item.generate_pretty_data(ctx))
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color(")", Color::Blue),
                suffix: None,
            },

            RuntimeValue::List(list) => PrettyPrintablePiece::List {
                begin: Colored::with_color("[", Color::Blue),
                items: list.with_ref(|items| {
                    items
                        .iter()
                        .map(|item| item.generate_pretty_data(ctx))
                        .collect()
                }),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("]", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Map(map) => PrettyPrintablePiece::List {
                begin: Colored::with_color("map({", Color::Blue),
                items: map.with_ref(|items| {
                    items
                        .iter()
                        .map(|(key, value)|
                        // Yes, that part is a hack :p
                        PrettyPrintablePiece::List {
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
                                value.generate_pretty_data(ctx)
                            ],
                            sep: Colored::empty(),
                            end: Colored::empty(),
                            suffix: None
                        })
                        .collect()
                }),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("})", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Struct(obj) => PrettyPrintablePiece::List {
                begin: Colored::with_color("{", Color::Blue),
                items: obj.with_ref(|members| {
                    members
                        .iter()
                        .map(|(field, value)|
                        // Yes, that part is a hack :p
                        PrettyPrintablePiece::List {
                            begin: Colored::with_color(
                                format!("{field}: "),
                                Color::Red
                            ),
                            items: vec![
                                value.generate_pretty_data(ctx)
                            ],
                            sep: Colored::empty(),
                            end: Colored::empty(),
                            suffix: None
                        })
                        .collect()
                }),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("}", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Function(func) => PrettyPrintablePiece::Join(vec![
                func.signature.inner().generate_pretty_data(ctx),
                PrettyPrintablePiece::colored_atomic(
                    match func.body {
                        RuntimeFnBody::Block(_) => " { ... }",
                        RuntimeFnBody::Internal(_) => " { /* internal code */ }",
                    },
                    Color::BrightBlack,
                ),
            ]),
        }
    }
}

impl PrettyPrintable for FnSignature {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        let Self { args, ret_type } = self;

        PrettyPrintablePiece::List {
            begin: Colored::with_color("fn(", Color::Blue),
            items: args
                .data()
                .iter()
                .map(|item| item.generate_pretty_data(ctx))
                .collect(),
            sep: Colored::with_color(",", Color::Blue),
            end: Colored::with_color(")", Color::Blue),
            suffix: ret_type.as_ref().map(|ret_type| {
                Box::new(PrettyPrintablePiece::Join(vec![
                    PrettyPrintablePiece::colored_atomic(" -> ", Color::BrightMagenta),
                    ret_type.data().generate_pretty_data(ctx),
                ]))
            }),
        }
    }
}

impl PrettyPrintable for FnArg {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        let Self {
            names,
            is_optional,
            is_rest,
            typ,
        } = self;

        let mut out = vec![];

        if *is_rest {
            out.push(PrettyPrintablePiece::colored_atomic(
                "...",
                Color::BrightYellow,
            ));
        }

        match names {
            FnArgNames::Positional(name) => {
                out.extend([PrettyPrintablePiece::colored_atomic(
                    name.data().clone(),
                    Color::Red,
                )]);
            }
            FnArgNames::ShortFlag(short) => {
                out.extend([
                    PrettyPrintablePiece::colored_atomic("-", Color::BrightYellow),
                    PrettyPrintablePiece::colored_atomic(*short.data(), Color::Red),
                ]);
            }
            FnArgNames::LongFlag(long) => {
                out.extend([
                    PrettyPrintablePiece::colored_atomic("--", Color::BrightYellow),
                    PrettyPrintablePiece::colored_atomic(long.data().clone(), Color::Red),
                ]);
            }
            FnArgNames::LongAndShortFlag { long, short } => {
                out.extend([
                    PrettyPrintablePiece::colored_atomic("--", Color::BrightYellow),
                    PrettyPrintablePiece::colored_atomic(long.data().clone(), Color::Red),
                    PrettyPrintablePiece::colored_atomic(" (", Color::Blue),
                    PrettyPrintablePiece::colored_atomic("-", Color::BrightYellow),
                    PrettyPrintablePiece::colored_atomic(*short.data(), Color::BrightYellow),
                    PrettyPrintablePiece::colored_atomic(")", Color::Blue),
                ]);
            }
        }

        if *is_optional {
            out.push(PrettyPrintablePiece::colored_atomic(
                "?",
                Color::BrightYellow,
            ));
        }

        if let Some(typ) = typ {
            out.push(PrettyPrintablePiece::colored_atomic(
                ": ",
                Color::BrightYellow,
            ));
            out.push(typ.data().generate_pretty_data(ctx));
        }

        PrettyPrintablePiece::Join(out)
    }
}

impl PrettyPrintable for CmdArgResult {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        match self {
            CmdArgResult::Single(single) => single.generate_pretty_data(ctx),

            CmdArgResult::Spreaded(items) => PrettyPrintablePiece::List {
                begin: Colored::with_color("spread(", Color::Magenta),
                items: items
                    .iter()
                    .map(|item| item.generate_pretty_data(ctx))
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color(")", Color::Blue),
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for CmdSingleArgResult {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        match self {
            CmdSingleArgResult::Basic(loc_val) => loc_val.value.generate_pretty_data(ctx),

            CmdSingleArgResult::Flag {
                name,
                value,
                raw: _,
            } => {
                let mut join = vec![name.data.generate_pretty_data(ctx)];

                if let Some(loc_val) = value {
                    join.push(PrettyPrintablePiece::colored_atomic(" ", Color::Black));
                    join.push(loc_val.value.generate_pretty_data(ctx));
                }

                PrettyPrintablePiece::Join(join)
            }

            CmdSingleArgResult::RestSeparator => {
                PrettyPrintablePiece::colored_atomic("--", Color::Yellow)
            }
        }
    }
}

impl PrettyPrintable for CmdFlagNameArg {
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        let name = match self {
            CmdFlagNameArg::Short(name) => format!("-{name}"),
            CmdFlagNameArg::Long(name) => format!("--{name}"),
        };

        PrettyPrintablePiece::colored_atomic(name, Color::BrightYellow)
    }
}
