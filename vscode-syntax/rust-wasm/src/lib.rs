use reshell_syntax_highlighter::{
    HighlightedPiece,
    elements::{
        ArgumentType, IdentifierType, InvalidType, ItemType, OperatorType, SymbolType,
        SyntaxErrorType, ValueType, WrapperType,
    },
};
use wasm_bindgen::prelude::*;

macro_rules! define_token_types {
    ($enum:ident ($list_fn: ident) { $($variant:ident = $value:expr),+ }) => {
        #[wasm_bindgen]
        #[derive(Clone, Copy)]
        pub enum $enum {
            $($variant = $value),+
        }

        #[wasm_bindgen]
        pub fn $list_fn() -> Vec<String> {
            vec![ $($value.to_owned()),+ ]
        }
    }
}

define_token_types!(TokenType (list_token_types) {
    Struct = "struct",
    // Type = "type",
    Parameter = "parameter",
    Variable = "variable",
    Property = "property",
    Function = "function",
    Method = "method",
    Comment = "comment",
    String = "string",
    Keyword = "keyword",
    Number = "number",
    Operator = "operator",
    ConstantValue = "variable.readonly.defaultLibrary"
});

define_token_types!(TokenModifier (list_token_modifiers) {
    Declaration = "declaration"
});

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct Token {
    pub start: Position,
    pub end: Position,
    pub nature: TokenType,
    pub modifier: Option<TokenModifier>,
}

#[wasm_bindgen]
pub fn highlight(input: &str) -> Vec<Token> {
    let pieces = reshell_syntax_highlighter::syntax_highlight(input, |_, _| true);

    let mut line_offsets = vec![0];

    for line in input.split('\n') {
        line_offsets.push(line_offsets.last().unwrap() + line.len() + 1);
    }

    pieces
        .into_iter()
        .map(|HighlightedPiece { start, len, item }| {
            let line = line_offsets
                .iter()
                .rposition(|line_offset| start >= *line_offset)
                .unwrap();

            let col = start - line_offsets[line];

            let start = Position { line, col };

            let end = Position {
                line: start.line,
                col: start.col + len,
            };

            Token {
                start,
                end,
                nature: match item {
                    ItemType::Identifier(identifier_type) => match identifier_type {
                        IdentifierType::Variable => TokenType::Variable,
                        IdentifierType::Constant => TokenType::Variable,
                        IdentifierType::VariableOrConstant => TokenType::Variable,
                        IdentifierType::Function => TokenType::Function,
                        IdentifierType::FunctionOrMethod => TokenType::Function,
                        IdentifierType::Method => TokenType::Method,
                        IdentifierType::CmdNameOrPath => TokenType::Function,
                        IdentifierType::StructMember => TokenType::Property,
                        IdentifierType::FnArgument => TokenType::Parameter,
                        IdentifierType::FlagName => TokenType::Parameter,
                        IdentifierType::Type => TokenType::Keyword,
                    },

                    ItemType::Argument(argument_type) => match argument_type {
                        ArgumentType::LongFlag => TokenType::Parameter,
                        ArgumentType::ShortFlag => TokenType::Parameter,
                        ArgumentType::LongOrShortFlag => TokenType::Parameter,
                    },

                    ItemType::Value(value_type) => match value_type {
                        ValueType::Null => TokenType::ConstantValue,
                        ValueType::Boolean => TokenType::ConstantValue,
                        ValueType::Number => TokenType::Number,
                        ValueType::RawCharacter => TokenType::String,
                        ValueType::EscapedCharacter => TokenType::String,
                        ValueType::LiteralCharacter => TokenType::String,
                        ValueType::NamedFunction => TokenType::Function,
                    },

                    ItemType::Operator(operator_type) => match operator_type {
                        OperatorType::Arithmetic => TokenType::Operator,
                        OperatorType::Logic => TokenType::Operator,
                        OperatorType::Comparison => TokenType::Operator,
                        OperatorType::Assignment => TokenType::Operator,
                        OperatorType::Spread => TokenType::Operator,
                    },

                    ItemType::Symbol(symbol_type) => match symbol_type {
                        SymbolType::MethodDotPrefix => TokenType::Operator,
                        SymbolType::CommentsMarker => TokenType::Comment,
                        SymbolType::FlagDashes => TokenType::Parameter,
                        SymbolType::CmdPipe => TokenType::Operator,
                        SymbolType::FnReturnTypePrefix => TokenType::Operator,
                        SymbolType::Parenthesis => TokenType::Operator, // TODO
                        SymbolType::Bracket => TokenType::Operator,     // TODO
                        SymbolType::Brace => TokenType::Operator,       // TODO
                        SymbolType::SpreadingBraceOrBracket => TokenType::Operator, // TODO
                        SymbolType::CmdSeparator => TokenType::Operator,
                        SymbolType::ArgSeparator => TokenType::Operator,
                        SymbolType::Colon => TokenType::Operator,
                        SymbolType::OptionalArgMarker => TokenType::Operator,
                        SymbolType::ExternalCmdMarker => TokenType::Keyword,
                        SymbolType::StructMemberDotPrefix => TokenType::Operator,
                        SymbolType::FnArgumentTypeOrValueSpecifier => TokenType::Operator,
                        SymbolType::BindInSpreading => TokenType::Operator,
                    },

                    // TODO
                    ItemType::Wrapper(wrapper_type) => match wrapper_type {
                        WrapperType::Block(_) => TokenType::Operator,
                        WrapperType::List(_) => TokenType::Operator,
                        WrapperType::VarSpreading(_) => TokenType::Operator,
                        WrapperType::ExpressionParen(_) => TokenType::Operator,
                        WrapperType::LiteralString(_) => TokenType::String,
                        WrapperType::ComputedString(_) => TokenType::String,
                        WrapperType::ExprInString(_) => TokenType::Operator,
                        WrapperType::CmdOutput(_) => TokenType::Operator,
                        WrapperType::CmdCall(_) => TokenType::Operator,
                        WrapperType::Lambda(_) => TokenType::Operator,
                        WrapperType::LambdaArgs(_) => TokenType::Operator,
                    },

                    ItemType::Invalid(invalid_type) => match invalid_type {
                        InvalidType::FunctionNotFound => todo!(),
                        InvalidType::MethodNotFound => todo!(),
                        InvalidType::CmdPathNotFound => todo!(),
                    },

                    ItemType::SyntaxError(syntax_error_type) => match syntax_error_type {
                        SyntaxErrorType::ClosingWithoutOpening => todo!(),
                        SyntaxErrorType::UnclosedOpening => todo!(),
                    },

                    ItemType::Keyword => TokenType::Keyword,
                    ItemType::Comment => TokenType::Comment,
                },

                // TODO
                modifier: None,
            }
        })
        .collect()
}
