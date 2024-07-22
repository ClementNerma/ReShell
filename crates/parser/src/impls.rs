use parsy::{CodeRange, Eaten};

use crate::ast::{
    CmdFlagNameArg, CmdRawString, CmdRawStringPiece, EscapableChar, FnFlagArgNames,
    RuntimeCodeRange, RuntimeEaten,
};

impl FnFlagArgNames {
    pub fn short_flag(&self) -> Option<RuntimeEaten<char>> {
        match self {
            FnFlagArgNames::ShortFlag(flag) => Some(*flag),
            FnFlagArgNames::LongFlag(_) => None,
            FnFlagArgNames::LongAndShortFlag { long: _, short } => Some(*short),
        }
    }

    pub fn long_flag(&self) -> Option<&RuntimeEaten<String>> {
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

impl<T> RuntimeEaten<T> {
    pub fn at(&self) -> RuntimeCodeRange {
        match self {
            RuntimeEaten::Parsed(eaten) => RuntimeCodeRange::Parsed(eaten.at),
            RuntimeEaten::Internal(_, infos) => RuntimeCodeRange::Internal(infos),
        }
    }

    pub fn data(&self) -> &T {
        match &self {
            Self::Parsed(eaten) => &eaten.data,
            Self::Internal(raw, _) => raw,
        }
    }

    pub fn eaten(&self) -> Option<&Eaten<T>> {
        match self {
            RuntimeEaten::Parsed(eaten) => Some(eaten),
            RuntimeEaten::Internal(_, _) => None,
        }
    }

    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> RuntimeEaten<U> {
        match self {
            RuntimeEaten::Parsed(eaten) => RuntimeEaten::Parsed(eaten.map(func)),
            RuntimeEaten::Internal(data, infos) => RuntimeEaten::Internal(func(data), infos),
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
