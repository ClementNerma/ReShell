use colored::Color;
use parsy::{CodeRange, FileId};
use reshell_parser::ast::{
    FnArg, FnArgNames, FnSignature, SingleValueType, StructTypeMember, ValueType,
};

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
                value
                    .get_type()
                    .render_colored(ctx, PrettyPrintOptions::inline())
            ),
        )),
    }
}

impl PrettyPrintable for ValueType {
    fn generate_pretty_data(&self, ctx: &Context) -> PrintablePiece {
        match self {
            Self::Single(single) => single.data().generate_pretty_data(ctx),

            Self::Union(types) => PrintablePiece::List {
                begin: Colored::empty(),
                items: types
                    .iter()
                    .map(|typ| typ.data().generate_pretty_data(ctx))
                    .collect(),
                sep: Colored::with_color(" | ", Color::Magenta),
                end: Colored::empty(),
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for SingleValueType {
    fn generate_pretty_data(&self, ctx: &Context) -> PrintablePiece {
        match self {
            Self::Any => PrintablePiece::colored_atomic("any", Color::Magenta),
            Self::Null => PrintablePiece::colored_atomic("null", Color::Magenta),
            Self::Bool => PrintablePiece::colored_atomic("boolean", Color::Magenta),
            Self::Int => PrintablePiece::colored_atomic("int", Color::Magenta),
            Self::Float => PrintablePiece::colored_atomic("float", Color::Magenta),
            Self::String => PrintablePiece::colored_atomic("string", Color::Magenta),
            Self::List => PrintablePiece::colored_atomic("list", Color::Magenta),
            Self::Range => PrintablePiece::colored_atomic("range", Color::Magenta),
            Self::Map => PrintablePiece::colored_atomic("map", Color::Magenta),
            Self::Error => PrintablePiece::colored_atomic("error", Color::Magenta),
            Self::UntypedStruct => PrintablePiece::colored_atomic("struct", Color::Magenta),
            Self::TypedStruct(members) => PrintablePiece::List {
                begin: Colored::with_color("struct {", Color::Magenta),
                items: members
                    .iter()
                    .map(|member| {
                        let StructTypeMember { name, typ } = member.data();

                        PrintablePiece::Join(vec![
                            PrintablePiece::colored_atomic(name.data().clone(), Color::Red),
                            PrintablePiece::colored_atomic(", ", Color::BrightBlack),
                            typ.data().generate_pretty_data(ctx),
                        ])
                    })
                    .collect(),
                sep: Colored::with_color(", ", Color::BrightBlack),
                end: Colored::with_color("}", Color::Magenta),
                suffix: None,
            },
            Self::Function(signature) => signature.generate_pretty_data(ctx),
            Self::TypeAlias(name) => PrintablePiece::Join(vec![
                PrintablePiece::colored_atomic(format!("{} (", name.data), Color::Magenta),
                match ctx.get_exact_type_alias(name) {
                    Some(typ) => typ.generate_pretty_data(ctx),
                    None => {
                        PrintablePiece::colored_atomic("<unknown type alias>", Color::BrightRed)
                    }
                },
                PrintablePiece::colored_atomic(")", Color::Magenta),
            ]),
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
    }
}

impl PrettyPrintable for RuntimeValue {
    fn generate_pretty_data(&self, ctx: &Context) -> PrintablePiece {
        match self {
            RuntimeValue::Null => PrintablePiece::colored_atomic("null", Color::BrightYellow),

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
                begin: Colored::with_color("[", Color::Blue),
                items: list
                    .iter()
                    .map(|item| item.generate_pretty_data(ctx))
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("]", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Range { from, to } => PrintablePiece::Join(vec![
                PrintablePiece::colored_atomic("range(", Color::Blue),
                PrintablePiece::colored_atomic(from.to_string(), Color::BrightYellow),
                PrintablePiece::colored_atomic(",", Color::Blue),
                PrintablePiece::colored_atomic(to.to_string(), Color::BrightYellow),
                PrintablePiece::colored_atomic(")", Color::Blue),
            ]),

            RuntimeValue::Map(map) => PrintablePiece::List {
                begin: Colored::with_color("map({", Color::Blue),
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
                                value.generate_pretty_data(ctx)
                            ],
                            sep: Colored::empty(),
                            end: Colored::empty(),
                            suffix: None
                        })
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("})", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Struct(obj) => PrintablePiece::List {
                begin: Colored::with_color("{", Color::Blue),
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
                                value.generate_pretty_data(ctx)
                            ],
                            sep: Colored::empty(),
                            end: Colored::empty(),
                            suffix: None
                        })
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("}", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Function(func) => PrintablePiece::Join(vec![
                func.signature.generate_pretty_data(ctx),
                PrintablePiece::colored_atomic(" { ... }", Color::BrightBlack),
            ]),

            RuntimeValue::Error { at: _, msg } => PrintablePiece::Join(vec![
                PrintablePiece::colored_atomic("error(", Color::Red),
                PrintablePiece::colored_atomic(
                    format!(
                        "\"{}\"",
                        msg.replace('\\', "\\\\")
                            .replace('\"', "\\\"")
                            .replace('\n', "\\n")
                    ),
                    Color::Green,
                ),
                // TODO: printing "at" requires access to context in order to find the filename
                PrintablePiece::colored_atomic(")", Color::Red),
            ]),
        }
    }
}

impl PrettyPrintable for FnSignature {
    fn generate_pretty_data(&self, ctx: &Context) -> PrintablePiece {
        let Self { args, ret_type } = self;

        PrintablePiece::List {
            begin: Colored::with_color("fn(", Color::Blue),
            items: args
                .iter()
                .map(|item| item.generate_pretty_data(ctx))
                .collect(),
            sep: Colored::with_color(",", Color::Blue),
            end: Colored::with_color(")", Color::Blue),
            suffix: ret_type.as_ref().map(|ret_type| {
                Box::new(PrintablePiece::Join(vec![
                    PrintablePiece::colored_atomic(" -> ", Color::BrightMagenta),
                    ret_type.data.generate_pretty_data(ctx),
                ]))
            }),
        }
    }
}

impl PrettyPrintable for FnArg {
    fn generate_pretty_data(&self, ctx: &Context) -> PrintablePiece {
        let Self {
            names,
            is_optional,
            is_rest,
            typ,
        } = self;

        let mut out = vec![];

        if *is_rest {
            out.push(PrintablePiece::colored_atomic("...", Color::BrightYellow));
        }

        match names {
            FnArgNames::NotFlag(name) => {
                out.extend([PrintablePiece::colored_atomic(
                    name.data.clone(),
                    Color::Red,
                )]);
            }
            FnArgNames::ShortFlag(short) => {
                out.extend([
                    PrintablePiece::colored_atomic("-", Color::BrightYellow),
                    PrintablePiece::colored_atomic(short.data, Color::Red),
                ]);
            }
            FnArgNames::LongFlag(long) => {
                out.extend([
                    PrintablePiece::colored_atomic("--", Color::BrightYellow),
                    PrintablePiece::colored_atomic(long.data.clone(), Color::Red),
                ]);
            }
            FnArgNames::LongAndShortFlag { long, short } => {
                out.extend([
                    PrintablePiece::colored_atomic("--", Color::BrightYellow),
                    PrintablePiece::colored_atomic(long.data.clone(), Color::Red),
                    PrintablePiece::colored_atomic(" (", Color::Blue),
                    PrintablePiece::colored_atomic("-", Color::BrightYellow),
                    PrintablePiece::colored_atomic(short.data, Color::BrightYellow),
                    PrintablePiece::colored_atomic(")", Color::Blue),
                ]);
            }
        }

        if *is_optional {
            out.push(PrintablePiece::colored_atomic("?", Color::BrightYellow));
        }

        if let Some(typ) = typ {
            out.push(PrintablePiece::colored_atomic(": ", Color::BrightYellow));
            out.push(typ.data.generate_pretty_data(ctx));
        }

        PrintablePiece::Join(out)
    }
}
