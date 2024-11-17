use parsy::{CodeRange, Span};

use crate::ast::{
    CmdFlagNameArg, CmdRawString, CmdRawStringPiece, EscapableChar, FnFlagArgNames,
    RuntimeCodeRange, RuntimeSpan,
};

impl FnFlagArgNames {
    pub fn short_flag(&self) -> Option<RuntimeSpan<char>> {
        match self {
            FnFlagArgNames::ShortFlag(flag) => Some(*flag),
            FnFlagArgNames::LongFlag(_) => None,
            FnFlagArgNames::LongAndShortFlag { long: _, short } => Some(*short),
        }
    }

    pub fn long_flag(&self) -> Option<&RuntimeSpan<String>> {
        match self {
            FnFlagArgNames::ShortFlag(_) => None,
            FnFlagArgNames::LongFlag(flag) => Some(flag),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => Some(long),
        }
    }
}

impl CmdFlagNameArg {
    pub fn back_to_string(&self) -> String {
        match self {
            CmdFlagNameArg::Short(short) => format!("-{short}"),
            CmdFlagNameArg::Long(long) | CmdFlagNameArg::LongNoConvert(long) => format!("--{long}"),
        }
    }
}

impl<T> RuntimeSpan<T> {
    pub fn as_parsed(&self) -> Option<Span<&T>> {
        let Self { at, data } = &self;

        match at {
            RuntimeCodeRange::Parsed(at) => Some(Span::ate(*at, data)),
            RuntimeCodeRange::Internal(_) => None,
        }
    }

    pub fn internal(at: &'static str, data: T) -> Self {
        Self {
            at: RuntimeCodeRange::Internal(at),
            data,
        }
    }
}

impl<T> From<Span<T>> for RuntimeSpan<T> {
    fn from(value: Span<T>) -> Self {
        let Span { at, data } = value;

        Self {
            at: RuntimeCodeRange::Parsed(at),
            data,
        }
    }
}

impl RuntimeCodeRange {
    pub fn parsed_range(&self) -> Option<CodeRange> {
        match self {
            RuntimeCodeRange::Parsed(range) => Some(*range),
            RuntimeCodeRange::Internal(_) => None,
        }
    }
}

impl From<CodeRange> for RuntimeCodeRange {
    fn from(range: CodeRange) -> Self {
        Self::Parsed(range)
    }
}

impl EscapableChar {
    pub fn original_char(self) -> char {
        match self {
            EscapableChar::Newline => '\n',
            EscapableChar::CarriageReturn => '\r',
            EscapableChar::Tab => '\t',
            EscapableChar::DoubleQuote => '"',
            EscapableChar::BackQuote => '`',
            EscapableChar::SingleQuote => '\'',
            EscapableChar::Backslash => '\\',
            EscapableChar::DollarSign => '$',
            EscapableChar::Caret => '^',
        }
    }
}

impl CmdRawString {
    pub fn only_literal(&self) -> Option<&str> {
        if self.pieces.len() != 1 {
            return None;
        }

        let only_piece = self.pieces.first().unwrap();

        match &only_piece.data {
            CmdRawStringPiece::Literal(lit) => Some(lit),
            CmdRawStringPiece::Variable(_) => None,
        }
    }
}

impl CmdFlagNameArg {
    pub fn dynamic(name: String) -> Result<Self, &'static str> {
        match name.chars().count() {
            0 => Err("flag name cannot be empty!"),
            1 => Ok(CmdFlagNameArg::Short(name.chars().next().unwrap())),
            _ => Ok(CmdFlagNameArg::Long(name)),
        }
    }
}
