use colored::Color;
use parsy::FileId;
use reshell_parser::{
    ast::{
        CmdFlagNameArg, FlagValueSeparator, FnArg, FnFlagArgNames, FnSignature, RuntimeCodeRange,
        SingleValueType, StructTypeMember, ValueType,
    },
    files::{FilesMap, SourceFileLocation},
};

use crate::{
    cmd::{CmdArgResult, CmdSingleArgResult, FlagArgValueResult},
    context::Context,
    errors::{ExecErrorInfoType, ExecResult},
    pretty::{Colored, PrettyPrintOptions, PrettyPrintable, PrettyPrintablePiece},
    values::{ErrorValueContent, RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

pub fn value_to_str(
    value: &RuntimeValue,
    at: impl Into<RuntimeCodeRange>,
    ctx: &Context,
) -> ExecResult<String> {
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
        | RuntimeValue::Error(_)
        | RuntimeValue::CmdCall { content_at: _ }
        | RuntimeValue::ArgSpread(_)
        | RuntimeValue::Custom(_) // TODO?
        => Err(ctx
            .error(
                at,
                format!(
                    "cannot convert {} to a string",
                    value
                        .get_type()
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            )
            .with_info(
                ExecErrorInfoType::Tip,
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
            Self::CmdCall => PrettyPrintablePiece::colored_atomic("cmdcall", Color::Magenta),
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
            Self::Custom(value) => PrettyPrintablePiece::colored_atomic(*value, Color::Magenta)
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
                    match &file.location {
                        SourceFileLocation::CustomName(name) => format!("<{name}>"),
                        SourceFileLocation::RealFile(path) => path.to_string_lossy().to_string(),
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

            RuntimeValue::String(string) => pretty_print_string(string),

            RuntimeValue::Range { from, to } => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("range(", Color::Blue),
                PrettyPrintablePiece::colored_atomic(from.to_string(), Color::BrightYellow),
                PrettyPrintablePiece::colored_atomic(",", Color::Blue),
                PrettyPrintablePiece::colored_atomic(to.to_string(), Color::BrightYellow),
                PrettyPrintablePiece::colored_atomic(")", Color::Blue),
            ]),

            RuntimeValue::Error(err) => {
                let ErrorValueContent { at, msg } = &**err;

                PrettyPrintablePiece::Join(vec![
                    PrettyPrintablePiece::colored_atomic("error(", Color::Red),
                    pretty_print_string(msg),
                    PrettyPrintablePiece::colored_atomic(" @ ", Color::BrightMagenta),
                    PrettyPrintablePiece::colored_atomic(
                        dbg_loc(*at, ctx.files_map()),
                        Color::BrightMagenta,
                    ),
                    PrettyPrintablePiece::colored_atomic(")", Color::Red),
                ])
            }

            RuntimeValue::CmdCall { content_at } => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("@{", Color::Magenta),
                PrettyPrintablePiece::colored_atomic(
                    dbg_loc(*content_at, ctx.files_map()),
                    Color::Black,
                ),
                PrettyPrintablePiece::colored_atomic("}", Color::Magenta),
            ]),

            RuntimeValue::ArgSpread(items) => PrettyPrintablePiece::List {
                begin: Colored::with_color("spread(", Color::Magenta),
                items: items
                    .iter()
                    .map(|item| item.generate_pretty_data(ctx))
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color(")", Color::Magenta),
                suffix: None,
            },

            RuntimeValue::List(list) => PrettyPrintablePiece::List {
                begin: Colored::with_color("[", Color::Blue),
                items: list
                    .read_promise_no_write()
                    .iter()
                    .map(|item| item.generate_pretty_data(ctx))
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("]", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Map(map) => PrettyPrintablePiece::List {
                begin: Colored::with_color("map({", Color::Blue),
                items: map
                    .read_promise_no_write()
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
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("})", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Struct(obj) => PrettyPrintablePiece::List {
                begin: Colored::with_color("{", Color::Blue),
                items: obj
                    .read_promise_no_write()
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
                    .collect(),
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("}", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Function(func) => func.generate_pretty_data(ctx),

            RuntimeValue::Custom(value) => value.generate_pretty_data(ctx),
        }
    }
}

impl PrettyPrintable for RuntimeFnValue {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            self.signature.inner().generate_pretty_data(ctx),
            PrettyPrintablePiece::colored_atomic(
                match self.body {
                    RuntimeFnBody::Block(_) => " { ... }",
                    RuntimeFnBody::Internal(_) => " { /* internal code */ }",
                },
                Color::BrightBlack,
            ),
        ])
    }
}

impl PrettyPrintable for FnSignature {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        let Self { args, ret_type } = self;

        PrettyPrintablePiece::List {
            begin: Colored::with_color("fn(", Color::BrightMagenta),
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
        match self {
            FnArg::Positional {
                name,
                is_optional,
                typ,
            } => {
                let mut out = vec![PrettyPrintablePiece::colored_atomic(
                    name.data(),
                    Color::Red,
                )];

                if *is_optional {
                    out.push(PrettyPrintablePiece::colored_atomic("?", Color::White));
                }

                if let Some(typ) = typ {
                    out.push(PrettyPrintablePiece::colored_atomic(": ", Color::White));
                    out.push(typ.data().generate_pretty_data(ctx));
                }

                PrettyPrintablePiece::Join(out)
            }

            FnArg::PresenceFlag { names } => names.generate_pretty_data(ctx),

            FnArg::NormalFlag {
                names,
                is_optional,
                typ,
            } => {
                let mut out: Vec<PrettyPrintablePiece> = vec![names.generate_pretty_data(ctx)];

                if *is_optional {
                    out.push(PrettyPrintablePiece::colored_atomic("?", Color::White));
                }

                if let Some(typ) = typ {
                    out.push(PrettyPrintablePiece::colored_atomic(": ", Color::White));
                    out.push(typ.data().generate_pretty_data(ctx));
                }

                PrettyPrintablePiece::Join(out)
            }

            FnArg::Rest { name } => PrettyPrintablePiece::colored_atomic(
                format!("...{}", name.data()),
                Color::BrightYellow,
            ),
        }
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
                end: Colored::with_color(")", Color::Magenta),
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for CmdSingleArgResult {
    fn generate_pretty_data(&self, ctx: &Context) -> PrettyPrintablePiece {
        match self {
            CmdSingleArgResult::Basic(loc_val) => loc_val.value.generate_pretty_data(ctx),

            CmdSingleArgResult::Flag { name, value } => {
                let mut join = vec![name.data.generate_pretty_data(ctx)];

                if let Some(FlagArgValueResult { value, value_sep }) = value {
                    let value_sep = match value_sep {
                        FlagValueSeparator::Space => " ",
                        FlagValueSeparator::Equal => "=",
                    };

                    join.push(PrettyPrintablePiece::colored_atomic(
                        value_sep,
                        Color::BrightYellow,
                    ));

                    join.push(value.value.generate_pretty_data(ctx));
                }

                PrettyPrintablePiece::Join(join)
            }

            CmdSingleArgResult::RestSeparator(_) => {
                PrettyPrintablePiece::colored_atomic("--", Color::Yellow)
            }
        }
    }
}

impl PrettyPrintable for FnFlagArgNames {
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        let str = match self {
            FnFlagArgNames::ShortFlag(short) => format!("-{}", short.data()),
            FnFlagArgNames::LongFlag(long) => format!("--{}", long.data()),
            FnFlagArgNames::LongAndShortFlag { long, short } => {
                format!("--{} (-{})", long.data(), short.data())
            }
        };

        PrettyPrintablePiece::colored_atomic(str, Color::BrightYellow)
    }
}

impl PrettyPrintable for CmdFlagNameArg {
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        let name = match self {
            CmdFlagNameArg::Short(name) => format!("-{name}"),
            CmdFlagNameArg::Long(name) => format!("--{name}"),
            CmdFlagNameArg::LongNoConvert(name) => format!("--{name}"),
        };

        PrettyPrintablePiece::colored_atomic(name, Color::BrightYellow)
    }
}

pub fn pretty_print_string(string: &str) -> PrettyPrintablePiece {
    let mut pieces = vec![PrettyPrintablePiece::colored_atomic(
        '"'.to_owned(),
        Color::BrightGreen,
    )];

    let mut shift = 0;

    while let Some(mut pos) = string[shift..].find(['\\', '\r', '\n', '"']) {
        pos += shift;

        if pos > shift {
            pieces.push(PrettyPrintablePiece::colored_atomic(
                string[shift..pos].to_owned(),
                Color::BrightGreen,
            ));
        }

        let to_escape = match &string[pos..pos + 1] {
            "\r" => "r",
            "\n" => "n",
            str => str,
        };

        pieces.push(PrettyPrintablePiece::colored_atomic(
            format!("\\{to_escape}"),
            Color::Cyan,
        ));

        shift = pos + 1;
    }

    if shift < string.len() {
        pieces.push(PrettyPrintablePiece::colored_atomic(
            string[shift..].to_owned(),
            Color::BrightGreen,
        ));
    }

    pieces.push(PrettyPrintablePiece::colored_atomic(
        '"'.to_owned(),
        Color::BrightGreen,
    ));

    PrettyPrintablePiece::Join(pieces)
}
