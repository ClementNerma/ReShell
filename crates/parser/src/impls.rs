use parsy::{CodeRange, CodeRangeComparisonError, Eaten};

use crate::ast::{
    EscapableChar, FnArg, FnFlagArgNames, FnSignature, RuntimeCodeRange, RuntimeEaten,
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

impl<T> RuntimeEaten<T> {
    pub fn at(&self) -> RuntimeCodeRange {
        match self {
            RuntimeEaten::Parsed(eaten) => RuntimeCodeRange::Parsed(eaten.at),
            RuntimeEaten::Internal(_) => RuntimeCodeRange::Internal,
        }
    }

    pub fn data(&self) -> &T {
        match &self {
            Self::Parsed(eaten) => &eaten.data,
            Self::Internal(raw) => raw,
        }
    }

    pub fn eaten(&self) -> Option<&Eaten<T>> {
        match self {
            RuntimeEaten::Parsed(eaten) => Some(eaten),
            RuntimeEaten::Internal(_) => None,
        }
    }

    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> RuntimeEaten<U> {
        match self {
            RuntimeEaten::Parsed(eaten) => RuntimeEaten::Parsed(eaten.map(func)),
            RuntimeEaten::Internal(data) => RuntimeEaten::Internal(func(data)),
        }
    }
}

impl RuntimeCodeRange {
    pub fn parsed_range(&self) -> Option<CodeRange> {
        match self {
            RuntimeCodeRange::Parsed(range) => Some(*range),
            RuntimeCodeRange::Internal => None,
        }
    }

    pub fn contains(&self, range: CodeRange) -> Result<bool, CodeRangeComparisonError> {
        match self {
            RuntimeCodeRange::Parsed(c) => c.contains(range),
            RuntimeCodeRange::Internal => Ok(false),
        }
    }
}

impl From<CodeRange> for RuntimeCodeRange {
    fn from(range: CodeRange) -> Self {
        Self::Parsed(range)
    }
}

impl FnSignature {
    pub fn is_method(&self) -> bool {
        self.args.data().first().map_or(false, |arg| match arg {
            FnArg::Positional {
                name,
                is_optional,
                typ: _,
            } => name.data() == "self" && !*is_optional,

            FnArg::PresenceFlag { names: _ }
            | FnArg::NormalFlag {
                names: _,
                is_optional: _,
                typ: _,
            }
            | FnArg::Rest { name: _ } => false,
        })
    }
}

impl EscapableChar {
    pub fn original_char(self) -> char {
        match self {
            EscapableChar::Newline => '\n',
            EscapableChar::CarriageReturn => '\r',
            EscapableChar::DoubleQuote => '"',
            EscapableChar::BackQuote => '`',
            EscapableChar::SingleQuote => '\'',
            EscapableChar::Backslash => '\\',
            EscapableChar::DollarSign => '$',
        }
    }
}
