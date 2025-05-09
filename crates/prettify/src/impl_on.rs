//!
//! This module implements pretty-printing for several types.
//!

use std::{collections::HashMap, sync::Arc};

use colored::Color;
use parsy::{CodeRange, FileId, Span};
use reshell_parser::{
    ast::{
        CmdFlagArgName, FnSignature, FnSignatureArg, FnSignatureFlagArgNames,
        FnSignatureNormalFlagArg, FnSignaturePositionalArg, FnSignaturePresenceFlagArg,
        FnSignatureRestArg, RuntimeCodeRange, SingleValueType, StructTypeMember, ValueType,
    },
    files_map::{FilesMap, SourceFileLocation},
};

use crate::{PrettyPrintable, PrettyPrintablePiece, Styled};

pub type TypeAliasStore = HashMap<Span<String>, Arc<Span<ValueType>>>;

impl PrettyPrintable for ValueType {
    type Context = TypeAliasStore;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        match self {
            Self::Single(single) => single.generate_pretty_data(ctx),

            Self::Union(types) => PrettyPrintablePiece::List {
                begin: vec![Styled::empty()],
                items: types
                    .iter()
                    .map(|typ| typ.generate_pretty_data(ctx))
                    .collect(),
                sep: vec![Styled::colored(" |", Color::Magenta)],
                end: vec![],
                suffix: None,
            },
        }
    }
}

impl PrettyPrintable for SingleValueType {
    type Context = TypeAliasStore;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        match self {
            Self::Void => PrettyPrintablePiece::colored_atomic("void", Color::Magenta),
            Self::Any => PrettyPrintablePiece::colored_atomic("any", Color::Magenta),
            Self::Null => PrettyPrintablePiece::colored_atomic("null", Color::Magenta),
            Self::Bool => PrettyPrintablePiece::colored_atomic("boolean", Color::Magenta),
            Self::Int => PrettyPrintablePiece::colored_atomic("int", Color::Magenta),
            Self::Float => PrettyPrintablePiece::colored_atomic("float", Color::Magenta),
            Self::String => PrettyPrintablePiece::colored_atomic("string", Color::Magenta),
            Self::Error => PrettyPrintablePiece::colored_atomic("error", Color::Magenta),
            Self::CmdCall => PrettyPrintablePiece::colored_atomic("cmdcall", Color::Magenta),
            Self::CmdArg => PrettyPrintablePiece::colored_atomic("cmdarg", Color::Magenta),
            Self::UntypedList => PrettyPrintablePiece::colored_atomic("list", Color::Magenta),
            Self::StringLiteral(inner) => pretty_printable_string(inner),
            Self::TypedList(inner) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("list[", Color::Magenta),
                inner.generate_pretty_data(ctx),
                PrettyPrintablePiece::colored_atomic("]", Color::Magenta),
            ]),
            Self::UntypedMap => PrettyPrintablePiece::colored_atomic("map", Color::Magenta),
            Self::TypedMap(inner) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic("map[", Color::Magenta),
                inner.generate_pretty_data(ctx),
                PrettyPrintablePiece::colored_atomic("]", Color::Magenta),
            ]),
            Self::UntypedStruct => PrettyPrintablePiece::colored_atomic("struct", Color::Magenta),
            Self::TypedStruct(members) => PrettyPrintablePiece::List {
                begin: vec![Styled::colored("{ ", Color::Magenta)],
                items: members
                    .iter()
                    .map(|member| {
                        let StructTypeMember {
                            name,
                            optional,
                            typ,
                        } = member;

                        PrettyPrintablePiece::Join(vec![
                            PrettyPrintablePiece::colored_atomic(name.data.clone(), Color::Red),
                            if *optional {
                                PrettyPrintablePiece::colored_atomic("?", Color::BrightBlack)
                            } else {
                                PrettyPrintablePiece::Empty
                            },
                            PrettyPrintablePiece::colored_atomic(": ", Color::BrightBlack),
                            typ.generate_pretty_data(ctx),
                        ])
                    })
                    .collect(),
                sep: vec![Styled::colored(",", Color::BrightBlack)],
                end: vec![Styled::colored(" }", Color::Magenta)],
                suffix: None,
            },
            Self::Function(signature) => signature.data.generate_pretty_data(ctx),
            Self::TypeAlias(name) => PrettyPrintablePiece::Join(vec![
                PrettyPrintablePiece::colored_atomic(name.data.clone(), Color::Yellow),
                PrettyPrintablePiece::colored_atomic(" = ", Color::BrightMagenta),
                ctx.get(name).unwrap().data.generate_pretty_data(ctx),
            ]),
            Self::Custom(value) => PrettyPrintablePiece::colored_atomic(*value, Color::Magenta),
        }
    }
}

impl PrettyPrintable for RuntimeCodeRange {
    type Context = FilesMap;

    fn generate_pretty_data(&self, files_map: &Self::Context) -> PrettyPrintablePiece {
        match self {
            RuntimeCodeRange::Parsed(at) => at.generate_pretty_data(files_map),
            RuntimeCodeRange::Internal(infos) => {
                // TODO: improve with coloration
                PrettyPrintablePiece::Atomic(Styled::colorless(format!(
                    "<internal location: {infos}>"
                )))
            }
        }
    }
}

impl PrettyPrintable for CodeRange {
    type Context = FilesMap;

    fn generate_pretty_data(&self, files_map: &Self::Context) -> PrettyPrintablePiece {
        // TODO: improve with coloration
        let output = match self.start.file_id {
            FileId::None => unreachable!(),
            FileId::SourceFile(id) => {
                let Some(file) = files_map.get_file(id) else {
                    return PrettyPrintablePiece::Atomic(Styled::colorless(format!(
                        "<unknown file @ offset {}>",
                        self.start.offset
                    )));
                };

                let bef = &file.content.as_str()[..self.start.offset];

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
}

impl PrettyPrintable for FnSignature {
    type Context = TypeAliasStore;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        let Self { args, ret_type } = self;

        PrettyPrintablePiece::List {
            begin: vec![Styled::colored("fn(", Color::BrightMagenta)],
            items: args
                .data
                .iter()
                .map(|item| item.generate_pretty_data(ctx))
                .collect(),
            sep: vec![Styled::colored(",", Color::Blue)],
            end: vec![Styled::colored(")", Color::BrightMagenta)],
            suffix: ret_type.as_ref().map(|ret_type| {
                Box::new(PrettyPrintablePiece::Join(vec![
                    PrettyPrintablePiece::colored_atomic(" -> ", Color::BrightMagenta),
                    ret_type.data.generate_pretty_data(ctx),
                ]))
            }),
        }
    }
}

impl PrettyPrintable for FnSignatureArg {
    type Context = TypeAliasStore;

    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece {
        match self {
            FnSignatureArg::Positional(FnSignaturePositionalArg {
                name,
                is_optional,
                typ,
            }) => {
                let mut out = vec![PrettyPrintablePiece::colored_atomic(&name.data, Color::Red)];

                if *is_optional {
                    out.push(PrettyPrintablePiece::colored_atomic("?", Color::White));
                }

                if let Some(typ) = typ {
                    out.push(PrettyPrintablePiece::colored_atomic(": ", Color::White));
                    out.push(typ.generate_pretty_data(ctx));
                }

                PrettyPrintablePiece::Join(out)
            }

            FnSignatureArg::PresenceFlag(FnSignaturePresenceFlagArg { names }) => {
                PrettyPrintablePiece::Join(vec![
                    names.generate_pretty_data(&()),
                    PrettyPrintablePiece::colored_atomic("?", Color::White),
                ])
            }

            FnSignatureArg::NormalFlag(FnSignatureNormalFlagArg {
                names,
                is_optional,
                typ,
            }) => {
                let mut out = vec![names.generate_pretty_data(&())];

                if *is_optional {
                    out.push(PrettyPrintablePiece::colored_atomic("?", Color::White));
                }

                out.push(PrettyPrintablePiece::colored_atomic(": ", Color::White));
                out.push(typ.generate_pretty_data(ctx));

                PrettyPrintablePiece::Join(out)
            }

            FnSignatureArg::Rest(FnSignatureRestArg { name, typ }) => {
                let mut out = vec![PrettyPrintablePiece::colored_atomic(
                    format!("...{}", name.data),
                    Color::BrightYellow,
                )];

                if let Some(typ) = typ {
                    out.push(PrettyPrintablePiece::colored_atomic(": ", Color::White));
                    out.push(typ.data.generate_pretty_data(ctx));
                }

                PrettyPrintablePiece::Join(out)
            }
        }
    }
}

impl PrettyPrintable for FnSignatureFlagArgNames {
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        let str = match self {
            FnSignatureFlagArgNames::ShortFlag(short) => format!("-{}", short.data),
            FnSignatureFlagArgNames::LongFlag(long) => format!("--{}", long.data),
            FnSignatureFlagArgNames::LongAndShortFlag { long, short } => {
                format!("--{} (-{})", long.data, short.data)
            }
        };

        PrettyPrintablePiece::colored_atomic(str, Color::BrightYellow)
    }
}

impl PrettyPrintable for CmdFlagArgName {
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        let name = match self {
            CmdFlagArgName::Short(name) => format!("-{name}"),
            CmdFlagArgName::Long(name) => format!("--{name}"),
        };

        PrettyPrintablePiece::colored_atomic(name, Color::BrightYellow)
    }
}

pub fn pretty_printable_string(string: &str) -> PrettyPrintablePiece {
    let mut pieces = vec![Styled::colored("'", Color::BrightGreen)];

    let mut shift = 0;

    while let Some(mut pos) = string[shift..].find(['\\', '\r', '\n', '\'']) {
        pos += shift;

        if pos > shift {
            pieces.push(Styled::colored(&string[shift..pos], Color::BrightGreen));
        }

        let to_escape = match &string[pos..pos + 1] {
            "\r" => "r",
            "\n" => "n",
            str => str,
        };

        pieces.push(Styled::colored(format!("\\{to_escape}"), Color::Cyan));

        shift = pos + 1;
    }

    if shift < string.len() {
        pieces.push(Styled::colored(&string[shift..], Color::BrightGreen));
    }

    pieces.push(Styled::colored("'", Color::BrightGreen));

    PrettyPrintablePiece::Suite(pieces)
}
