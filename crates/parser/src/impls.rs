use parsy::{InputRange, Span};

use crate::ast::{
    CmdFlagArgName, CmdRawString, CmdRawStringPiece, EscapableChar, FnFlagArgName,
    FnSignatureFlagArgNames, RuntimeCodeRange, RuntimeSpan,
};

impl FnSignatureFlagArgNames {
    pub fn short_flag(&self) -> Option<RuntimeSpan<char>> {
        match self {
            FnSignatureFlagArgNames::ShortFlag(flag) => Some(*flag),
            FnSignatureFlagArgNames::LongFlag(_) => None,
            FnSignatureFlagArgNames::LongAndShortFlag { long: _, short } => Some(*short),
        }
    }

    pub fn long_flag(&self) -> Option<&RuntimeSpan<String>> {
        match self {
            FnSignatureFlagArgNames::ShortFlag(_) => None,
            FnSignatureFlagArgNames::LongFlag(flag) => Some(flag),
            FnSignatureFlagArgNames::LongAndShortFlag { long, short: _ } => Some(long),
        }
    }
}

impl CmdFlagArgName {
    pub fn back_to_string(&self) -> String {
        match self {
            CmdFlagArgName::Short(short) => format!("-{short}"),
            CmdFlagArgName::Long(long) => format!("--{long}"),
        }
    }
}

impl From<FnFlagArgName> for CmdFlagArgName {
    fn from(value: FnFlagArgName) -> Self {
        match value {
            FnFlagArgName::Short(name) => Self::Short(name),
            FnFlagArgName::Long(name) => Self::Long(name),
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
    pub fn parsed_range(&self) -> Option<InputRange> {
        match self {
            RuntimeCodeRange::Parsed(range) => Some(*range),
            RuntimeCodeRange::Internal(_) => None,
        }
    }
}

impl From<InputRange> for RuntimeCodeRange {
    fn from(range: InputRange) -> Self {
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
            CmdRawStringPiece::Variable(_) | CmdRawStringPiece::CmdCapturedOutput(_) => None,
        }
    }
}

impl CmdFlagArgName {
    pub fn dynamic(name: String) -> Result<Self, &'static str> {
        match name.chars().count() {
            0 => Err("flag name cannot be empty!"),
            1 => Ok(CmdFlagArgName::Short(name.chars().next().unwrap())),
            _ => Ok(CmdFlagArgName::Long(name)),
        }
    }
}
