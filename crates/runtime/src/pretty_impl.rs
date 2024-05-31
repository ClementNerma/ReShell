//!
//! This module implements pretty-printing for several types.
//!

use std::collections::BTreeSet;

use colored::Color;
use reshell_parser::ast::FlagValueSeparator;
use reshell_shared::pretty::{Colored, PrettyPrintable, PrettyPrintablePiece};

use crate::{
    cmd::{CmdArgResult, FlagArgValueResult, SingleCmdArgResult},
    context::Context,
    values::{ErrorValueContent, RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

impl PrettyPrintable for RuntimeValue {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
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
                let ErrorValueContent { at, data } = &**err;

                PrettyPrintablePiece::Join(vec![
                    PrettyPrintablePiece::colored_atomic("error(", Color::Red),
                    data.generate_pretty_data(ctx),
                    PrettyPrintablePiece::colored_atomic(" @ ", Color::BrightMagenta),
                    at.generate_pretty_data(ctx.files_map()),
                    PrettyPrintablePiece::colored_atomic(")", Color::Red),
                ])
            }

            RuntimeValue::CmdArg(arg) => arg.generate_pretty_data(ctx),

            RuntimeValue::CmdCall { content_at } => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("@{", Color::Magenta),
                content_at.generate_pretty_data(ctx.files_map()),
                PrettyPrintablePiece::colored_atomic("}", Color::Magenta),
            ]),

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
                begin: Colored::with_color("map({", Color::Magenta),
                items: {
                    let map = map.read_promise_no_write();

                    // Sort keys
                    let keys = map.keys().collect::<BTreeSet<_>>();

                    keys.iter()
                        .map(|key| {
                            // Yes, that part is a hack :p
                            PrettyPrintablePiece::List {
                                begin: Colored::with_color(
                                    format!(
                                        "\"{}\": ",
                                        key.replace('\\', "\\\\")
                                            .replace('\"', "\\\"")
                                            .replace('\n', "\\n")
                                    ),
                                    Color::Green,
                                ),
                                items: vec![map.get(*key).unwrap().generate_pretty_data(ctx)],
                                sep: Colored::empty(),
                                end: Colored::empty(),
                                suffix: None,
                            }
                        })
                        .collect()
                },
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("})", Color::Magenta),
                suffix: None,
            },

            RuntimeValue::Struct(obj) => PrettyPrintablePiece::List {
                begin: Colored::with_color("{", Color::Blue),
                items: {
                    let obj = obj.read_promise_no_write();

                    // Sort keys
                    let keys = obj.keys().collect::<BTreeSet<_>>();

                    keys.iter()
                        .map(|field|
                        // Yes, that part is a hack :p
                        PrettyPrintablePiece::List {
                            begin: Colored::with_color(
                                format!("{field}: "),
                                Color::Red
                            ),
                            items: vec![
                                obj.get(*field).unwrap().generate_pretty_data(ctx)
                            ],
                            sep: Colored::empty(),
                            end: Colored::empty(),
                            suffix: None
                        })
                        .collect()
                },
                sep: Colored::with_color(",", Color::Blue),
                end: Colored::with_color("}", Color::Blue),
                suffix: None,
            },

            RuntimeValue::Function(func) => func.generate_pretty_data(ctx),

            RuntimeValue::Custom(value) => value.generate_pretty_data(&()),
        }
    }
}

impl PrettyPrintable for RuntimeFnValue {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            self.signature
                .inner()
                .generate_pretty_data(ctx.type_alias_store()),
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

impl PrettyPrintable for CmdArgResult {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
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

impl PrettyPrintable for SingleCmdArgResult {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        match self {
            SingleCmdArgResult::Basic(loc_val) => loc_val.value.generate_pretty_data(ctx),

            SingleCmdArgResult::Flag { name, value } => {
                let mut join = vec![name.data().generate_pretty_data(&())];

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
        }
    }
}

pub fn pretty_print_string(string: &str) -> PrettyPrintablePiece {
    let mut pieces = vec![PrettyPrintablePiece::colored_atomic(
        '\''.to_owned(),
        Color::BrightGreen,
    )];

    let mut shift = 0;

    while let Some(mut pos) = string[shift..].find(['\\', '\r', '\n', '\'']) {
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
        '\''.to_owned(),
        Color::BrightGreen,
    ));

    PrettyPrintablePiece::Join(pieces)
}
