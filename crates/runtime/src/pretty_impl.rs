//!
//! This module implements pretty-printing for several types.
//!

use std::time::Duration;

use colored::{Color, Colorize};
use jiff::Zoned;
use parsy::{FileId, InputRange};
use reshell_parser::{
    ast::{FlagValueSeparator, RuntimeCodeRange},
    files_map::{FilesMap, SourceFileLocation},
};
use reshell_prettify::{PrettyPrintable, PrettyPrintablePiece, Styled, pretty_printable_string};

use crate::{
    cmd::{CmdArgResult, FlagArgValueResult, SingleCmdArgResult},
    values::{
        CmdArgValue, CmdCallValue, CmdFlagValue, ErrorValue, RangeValue, RuntimeFnBody,
        RuntimeFnValue, RuntimeValue,
    },
};

impl PrettyPrintable for RuntimeValue {
    fn generate_pretty_data(&self) -> PrettyPrintablePiece {
        match self {
            RuntimeValue::Void => {
                PrettyPrintablePiece::Atomic(Styled::from("void".bright_black().italic()))
            }

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

            RuntimeValue::String(string) => pretty_printable_string(string),

            RuntimeValue::DateTime(datetime) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("datetime(", Color::Magenta),
                pretty_printable_date_time(datetime),
                PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
            ]),

            RuntimeValue::Instant(_) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("instant(", Color::Magenta),
                PrettyPrintablePiece::colored_atomic("<internal>", Color::BrightBlack),
                PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
            ]),

            RuntimeValue::Duration(duration) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("duration(", Color::Magenta),
                pretty_printable_duration(*duration),
                PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
            ]),

            RuntimeValue::Regex(regex) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("regex(", Color::Magenta),
                pretty_printable_string(regex.as_str()),
                PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
            ]),

            RuntimeValue::Range(RangeValue {
                from,
                to,
                include_last_value,
            }) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic(from.to_string(), Color::BrightYellow),
                PrettyPrintablePiece::colored_atomic(
                    if *include_last_value { "..=" } else { ".." },
                    Color::BrightYellow,
                ),
                PrettyPrintablePiece::colored_atomic(to.to_string(), Color::BrightYellow),
            ]),

            RuntimeValue::Error(err) => {
                let ErrorValue {
                    at: _,
                    data,
                    pretty_at,
                } = &**err;

                PrettyPrintablePiece::Join(vec![
                    PrettyPrintablePiece::colored_atomic("error(", Color::Red),
                    data.generate_pretty_data(),
                    PrettyPrintablePiece::colored_atomic(" @ ", Color::BrightMagenta),
                    // TODO: find a way to avoid cloning?
                    pretty_at.clone(),
                    PrettyPrintablePiece::colored_atomic(")", Color::Red),
                ])
            }

            RuntimeValue::CmdCall(call) => {
                let CmdCallValue {
                    content_at: _,
                    pretty_content_at,
                } = &**call;

                PrettyPrintablePiece::Join(vec![
                    PrettyPrintablePiece::colored_atomic("@(", Color::Magenta),
                    // TODO: find a way to avoid cloning?
                    pretty_content_at.clone(),
                    PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
                ])
            }

            RuntimeValue::CmdArg(cmd_arg) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("cmdarg(", Color::Magenta),
                cmd_arg.generate_pretty_data(),
                PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
            ]),

            RuntimeValue::List(list) => PrettyPrintablePiece::List {
                begin: vec![Styled::colored("[", Color::Blue)],
                items: list
                    .read_promise_no_write()
                    .iter()
                    .map(|item| item.generate_pretty_data())
                    .collect(),
                sep: vec![Styled::colored(",", Color::Blue)],
                end: vec![Styled::colored("]", Color::Blue)],
                suffix: None,
            },

            RuntimeValue::Map(map) => PrettyPrintablePiece::List {
                begin: vec![Styled::colored("map({", Color::Magenta)],
                items: {
                    map.read_promise_no_write()
                        .iter()
                        .map(|(key, value)| {
                            PrettyPrintablePiece::Join(vec![
                                pretty_printable_string(key),
                                PrettyPrintablePiece::colored_atomic(":", Color::Blue),
                                PrettyPrintablePiece::Atomic(Styled::colorless(" ")),
                                value.generate_pretty_data(),
                            ])
                        })
                        .collect()
                },
                sep: vec![Styled::colored(",", Color::Blue)],
                end: vec![Styled::colored("})", Color::Magenta)],
                suffix: None,
            },

            RuntimeValue::Struct(obj) => PrettyPrintablePiece::List {
                begin: vec![Styled::colored("{", Color::Blue)],
                items: {
                    obj.read_promise_no_write()
                        .iter()
                        .map(|(field, value)| {
                            PrettyPrintablePiece::Join(vec![
                                PrettyPrintablePiece::colored_atomic(field, Color::Red),
                                PrettyPrintablePiece::colored_atomic(":", Color::Blue),
                                PrettyPrintablePiece::Atomic(Styled::colorless(" ")),
                                value.generate_pretty_data(),
                            ])
                        })
                        .collect()
                },
                sep: vec![Styled::colored(",", Color::Blue)],
                end: vec![Styled::colored("}", Color::Blue)],
                suffix: None,
            },

            RuntimeValue::Function(func) => func.generate_pretty_data(),
        }
    }
}

impl PrettyPrintable for RuntimeFnValue {
    fn generate_pretty_data(&self) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            self.signature.inner().generate_pretty_data(),
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

impl PrettyPrintable for CmdArgValue {
    fn generate_pretty_data(&self) -> PrettyPrintablePiece {
        match self {
            Self::Basic(loc_val) => loc_val.value.generate_pretty_data(),
            Self::Flag(flag) => flag.generate_pretty_data(),
        }
    }
}

impl PrettyPrintable for CmdFlagValue {
    fn generate_pretty_data(&self) -> PrettyPrintablePiece {
        let Self { name, value } = self;

        let mut join = vec![name.data.generate_pretty_data()];

        if let Some(FlagArgValueResult { value, value_sep }) = value {
            let value_sep = match value_sep {
                FlagValueSeparator::Space => " ",
                FlagValueSeparator::Equal => "=",
            };

            join.push(PrettyPrintablePiece::colored_atomic(
                value_sep,
                Color::BrightYellow,
            ));

            join.push(value.value.generate_pretty_data());
        }

        PrettyPrintablePiece::Join(join)
    }
}

impl PrettyPrintable for CmdArgResult {
    fn generate_pretty_data(&self) -> PrettyPrintablePiece {
        match self {
            Self::Single(single) => single.generate_pretty_data(),

            Self::Spreaded(items) => PrettyPrintablePiece::List {
                begin: vec![Styled::colored("spread(", Color::Magenta)],
                items: items
                    .iter()
                    .map(|item| item.generate_pretty_data())
                    .collect(),
                sep: vec![Styled::colored(",", Color::Blue)],
                end: vec![Styled::colored(")", Color::Magenta)],
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for SingleCmdArgResult {
    fn generate_pretty_data(&self) -> PrettyPrintablePiece {
        match self {
            Self::Basic(loc_val) => loc_val.value.generate_pretty_data(),
            Self::Flag(flag) => flag.generate_pretty_data(),
        }
    }
}

pub fn pretty_printable_runtime_input_range(
    range: RuntimeCodeRange,
    files_map: &FilesMap,
) -> PrettyPrintablePiece {
    match range {
        RuntimeCodeRange::Parsed(at) => pretty_printable_input_range(at, files_map),
        RuntimeCodeRange::Internal(infos) => {
            // TODO: improve with coloration
            PrettyPrintablePiece::Atomic(Styled::colorless(format!("<internal location: {infos}>")))
        }
    }
}

pub fn pretty_printable_input_range(
    range: InputRange,
    files_map: &FilesMap,
) -> PrettyPrintablePiece {
    // TODO: improve with coloration
    let output = match range.start.file_id {
        FileId::None => unreachable!(),
        FileId::SourceFile(id) => {
            let Some(file) = files_map.get_file(id) else {
                return PrettyPrintablePiece::Atomic(Styled::colorless(format!(
                    "<unknown file @ offset {}>",
                    range.start.offset
                )));
            };

            let bef = &file.content.as_str()[..range.start.offset];

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
    };

    PrettyPrintablePiece::Atomic(Styled::colorless(output))
}

pub fn pretty_printable_date_time(zoned: &Zoned) -> PrettyPrintablePiece {
    pretty_printable_string(&jiff::fmt::rfc2822::to_string(zoned).unwrap())
}

pub fn pretty_printable_duration(dur: Duration) -> PrettyPrintablePiece {
    const SECS_PER_DAY: u64 = 86_400;
    const SECS_PER_HOUR: u64 = 3_600;
    const SECS_PER_MIN: u64 = 60;

    let total_secs = dur.as_secs();

    let days = total_secs / SECS_PER_DAY;
    let hours = (total_secs % SECS_PER_DAY) / SECS_PER_HOUR;
    let minutes = (total_secs % SECS_PER_HOUR) / SECS_PER_MIN;
    let seconds = total_secs % SECS_PER_MIN;

    let millis = dur.subsec_millis();

    let str = if days > 0 {
        format!("{days}d {hours}h {minutes}m {seconds}s {millis}ms")
    } else if hours > 0 {
        format!("{hours}h {minutes}m {seconds}s {millis}ms")
    } else if minutes > 0 {
        format!("{minutes}m {seconds}s {millis}ms")
    } else if seconds > 0 {
        format!("{seconds}s {millis}ms")
    } else {
        format!("{millis}ms")
    };

    PrettyPrintablePiece::colored_atomic(str, Color::Blue)
}
