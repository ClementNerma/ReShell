use parsy::{CodeRange, Eaten};

use crate::{
    ast::{
        CmdComputedString, CmdComputedStringPiece, CmdFlagNameArg, EscapableChar, FnFlagArgNames,
        FunctionBody, MethodApplyableType, RuntimeCodeRange, RuntimeEaten, SingleValueType,
    },
    scope::AstScopeId,
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
            EscapableChar::DoubleQuote => '"',
            EscapableChar::BackQuote => '`',
            EscapableChar::SingleQuote => '\'',
            EscapableChar::Backslash => '\\',
            EscapableChar::DollarSign => '$',
        }
    }
}

impl CmdComputedString {
    pub fn only_literal(&self) -> Option<&str> {
        if self.pieces.len() != 1 {
            return None;
        }

        let only_piece = self.pieces.first().unwrap();

        match &only_piece.data {
            CmdComputedStringPiece::Literal(lit) => Some(lit),
            CmdComputedStringPiece::Escaped(_) | CmdComputedStringPiece::Variable(_) => None,
        }
    }
}

impl FunctionBody {
    pub fn ast_scope_id(&self) -> AstScopeId {
        match self {
            FunctionBody::Expr {
                content: _,
                scope_id,
            } => *scope_id,

            FunctionBody::Block(block) => block.data.scope_id,
        }
    }
}

impl MethodApplyableType {
    pub fn from_single_value_type(value: SingleValueType) -> Option<Self> {
        match value {
            SingleValueType::Bool => Some(Self::Bool),
            SingleValueType::Int => Some(Self::Int),
            SingleValueType::Float => Some(Self::Float),
            SingleValueType::String => Some(Self::String),
            SingleValueType::List => Some(Self::List),
            SingleValueType::Range => Some(Self::Range),
            SingleValueType::Map => Some(Self::Map),
            SingleValueType::Error => Some(Self::Error),
            SingleValueType::CmdCall => Some(Self::CmdCall),
            SingleValueType::ArgSpread => Some(Self::ArgSpread),
            SingleValueType::Custom(name) => Some(Self::Custom(name)),

            SingleValueType::Any
            | SingleValueType::Null
            | SingleValueType::UntypedStruct
            | SingleValueType::TypedStruct(_)
            | SingleValueType::Function(_)
            | SingleValueType::TypeAlias(_) => None,
        }
    }
}
