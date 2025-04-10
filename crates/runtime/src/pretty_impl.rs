//!
//! This module implements pretty-printing for several types.
//!

use colored::{Color, Colorize};
use reshell_parser::ast::FlagValueSeparator;
use reshell_prettify::{pretty_printable_string, PrettyPrintable, PrettyPrintablePiece, Styled};

use crate::{
    cmd::{CmdArgResult, FlagArgValueResult, SingleCmdArgResult},
    context::Context,
    values::{
        CmdArgValue, CmdFlagValue, ErrorValueContent, RuntimeFnBody, RuntimeFnValue, RuntimeValue,
    },
};

impl PrettyPrintable for RuntimeValue {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
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

            RuntimeValue::CmdCall { content_at } => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("@{", Color::Magenta),
                content_at.generate_pretty_data(ctx.files_map()),
                PrettyPrintablePiece::colored_atomic("}", Color::Magenta),
            ]),

            RuntimeValue::CmdArg(cmd_arg) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("cmdarg(", Color::Magenta),
                cmd_arg.generate_pretty_data(ctx),
                PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
            ]),

            RuntimeValue::List(list) => PrettyPrintablePiece::List {
                begin: vec![Styled::colored("[", Color::Blue)],
                items: list
                    .read_promise_no_write()
                    .iter()
                    .map(|item| item.generate_pretty_data(ctx))
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
                                value.generate_pretty_data(ctx),
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
                                value.generate_pretty_data(ctx),
                            ])
                        })
                        .collect()
                },
                sep: vec![Styled::colored(",", Color::Blue)],
                end: vec![Styled::colored("}", Color::Blue)],
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

impl PrettyPrintable for CmdArgValue {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        match self {
            Self::Basic(loc_val) => loc_val.value.generate_pretty_data(ctx),
            Self::Flag(flag) => flag.generate_pretty_data(ctx),
        }
    }
}

impl PrettyPrintable for CmdFlagValue {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        let Self { name, value } = self;

        let mut join = vec![name.data.generate_pretty_data(&())];

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

impl PrettyPrintable for CmdArgResult {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        match self {
            Self::Single(single) => single.generate_pretty_data(ctx),

            Self::Spreaded(items) => PrettyPrintablePiece::List {
                begin: vec![Styled::colored("spread(", Color::Magenta)],
                items: items
                    .iter()
                    .map(|item| item.generate_pretty_data(ctx))
                    .collect(),
                sep: vec![Styled::colored(",", Color::Blue)],
                end: vec![Styled::colored(")", Color::Magenta)],
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for SingleCmdArgResult {
    type Context = Context;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        match self {
            Self::Basic(loc_val) => loc_val.value.generate_pretty_data(ctx),
            Self::Flag(flag) => flag.generate_pretty_data(ctx),
        }
    }
}
