//!
//! This module defines the rule set and systems used for highlighting ReShell programs
//! using the [`super::utils::syntax`] module.
//!

use nu_ansi_term::{Color, Style};
use reedline::{Highlighter as RlHighlighter, StyledText};
use reshell_syntax_highlighter::{SyntaxItem, elements::*};

use crate::{repl::SHARED_CONTEXT, utils::cmd_checker::COMMANDS_CHECKER};

pub fn create_highlighter() -> Box<dyn RlHighlighter> {
    Box::new(Highlighter)
}

pub struct Highlighter;

impl RlHighlighter for Highlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        if line.is_empty() {
            COMMANDS_CHECKER
                .lock()
                .unwrap()
                .refresh(SHARED_CONTEXT.lock().unwrap().as_mut().unwrap());
        }

        highlight(line)
    }
}

fn highlight(input: &str) -> StyledText {
    let pieces = reshell_syntax_highlighter::syntax_highlight(
        input,
        Some(Box::new(|cmd_name, cmd_type| {
            COMMANDS_CHECKER.lock().unwrap().check(
                SHARED_CONTEXT.lock().unwrap().as_mut().unwrap(),
                cmd_name,
                cmd_type,
            )
        })),
    );

    let mut last = 0;
    let mut out = vec![];

    for piece in pieces {
        let SyntaxItem { start, len, item } = piece;

        if start > last {
            out.push((Style::default(), input[last..start].to_owned()));
        }

        last = start + len;
        out.push((color_match_item(item), input[start..start + len].to_owned()));
    }

    if last < input.len() {
        out.push((Style::default(), input[last..].to_owned()));
    }

    StyledText { buffer: out }
}

fn color_match_item(item: ItemType) -> Style {
    // Import all colors to make writing easier
    use Color::*;

    Style::new().fg(match item {
        ItemType::Identifier(identifier_type) => match identifier_type {
            IdentifierType::Variable => Red,
            IdentifierType::Constant => Red,
            IdentifierType::VariableOrConstant => Red,
            IdentifierType::Function => Blue,
            IdentifierType::FunctionOrMethod => Blue,
            IdentifierType::Method => Blue,
            IdentifierType::CmdNameOrPath => Blue,
            IdentifierType::StructMember => Red,
            IdentifierType::FnArgument => Red,
            IdentifierType::FlagName => Yellow,
            IdentifierType::Type => Magenta,
        },

        ItemType::Argument(argument_type) => match argument_type {
            ArgumentType::LongFlag => Yellow,
            ArgumentType::ShortFlag => Yellow,
            ArgumentType::LongOrShortFlag => Yellow,
        },

        ItemType::Value(value_type) => match value_type {
            ValueType::Null => Yellow,
            ValueType::Boolean => Yellow,
            ValueType::Number => Yellow,
            ValueType::RawCharacter => Green,
            ValueType::EscapedCharacter => Blue,
            ValueType::LiteralCharacter => Green,
            ValueType::NamedFunction => Magenta,
        },

        ItemType::Operator(operator_type) => match operator_type {
            OperatorType::Arithmetic => Yellow,
            OperatorType::Logic => Yellow,
            OperatorType::Comparison => Yellow,
            OperatorType::Assignment => DarkGray,
            OperatorType::Spread => Red,
        },

        ItemType::Symbol(symbol_type) => match symbol_type {
            SymbolType::MethodDotPrefix => DarkGray,
            SymbolType::CommentsMarker => DarkGray,
            SymbolType::FlagDashes => Yellow,
            SymbolType::CmdPipe => DarkGray,
            SymbolType::FnReturnTypePrefix => DarkGray,
            SymbolType::Parenthesis => DarkGray,
            SymbolType::Bracket => DarkGray,
            SymbolType::Brace => DarkGray,
            SymbolType::SpreadingBraceOrBracket => DarkGray,
            SymbolType::CmdSeparator => DarkGray,
            SymbolType::ArgSeparator => DarkGray,
            SymbolType::Colon => DarkGray,
            SymbolType::OptionalArgMarker => DarkGray,
            SymbolType::ExternalCmdMarker => Magenta,
            SymbolType::StructMemberDotPrefix => Red,
            SymbolType::FnArgumentTypeOrValueSpecifier => DarkGray,
            SymbolType::BindInSpreading => DarkGray,
        },

        ItemType::Wrapper(wrapper_type) => match wrapper_type {
            // TODO: highlight depending on the nesting level
            WrapperType::Block(_) => DarkGray,
            WrapperType::List(_) => DarkGray,
            WrapperType::VarSpreading(_) => DarkGray,
            WrapperType::ExpressionParen(_) => DarkGray,
            WrapperType::LiteralString(_) => Green,
            WrapperType::ComputedString(_) => Green,
            WrapperType::ExprInString(_) => DarkGray,
            WrapperType::CmdOutput(_) => DarkGray,
            WrapperType::CmdCall(_) => DarkGray,
            WrapperType::Lambda(_) => DarkGray,
            WrapperType::FnArgs(_) => DarkGray,
        },

        ItemType::Invalid(invalid_type) => match invalid_type {
            InvalidType::FunctionNotFound => Red,
            InvalidType::MethodNotFound => Red,
            InvalidType::CmdPathNotFound => Red,
        },

        ItemType::SyntaxError(syntax_error_type) => match syntax_error_type {
            SyntaxErrorType::ClosingWithoutOpening => return Style::new().fg(White).on(Red),
            SyntaxErrorType::UnclosedOpening => return Style::new().fg(White).on(Red),
        },

        ItemType::Keyword => Magenta,
        ItemType::Comment => DarkGray,
    })
}
