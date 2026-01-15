use parsy::{
    Parser, ParserConstUtils, ParserNonConstUtils, Span,
    parsers::helpers::{char, choice, digit, empty, end, filter, just, silent_choice},
};

use super::{
    COMPUTED_STRING, ESCAPABLE_CHAR, FN_CALL, INLINE_CMD_CALL, LAMBDA, LIST_VALUE, LITERAL_INT,
    LITERAL_STRING, LITERAL_VALUE, RANGE, SPREAD_VALUE, possible_ident_char, var_name,
};
use crate::{
    DELIMITER_CHARS,
    ast::{
        CmdCall, CmdCaptureType, CmdOutputCapture, ComputedString, ComputedStringPiece,
        EscapableChar, FnArg, FnCall, FnCallNature, FnFlagArgName, FnSignature, FnSignatureArg,
        FnSignaturePositionalArg, Function, ListItem, LiteralValue, MapItem, MapKey, Range,
        RangeBound, RuntimeSpan, SpreadValue, StructItem, Value,
    },
    parsers::{
        CMD_CALL, CMD_CAPTURE, EXPR, FN_ARG_LONG_FLAG_NAME_IDENTIFIER, FN_SIGNATURE_ARG, RAW_BLOCK,
        first_ident_char, ident, ms, msnl, s,
    },
};

pub fn escapable_char() -> impl Parser<EscapableChar> + Send + Sync {
    char('\\').ignore_then(
        choice((
            char('n').to(EscapableChar::Newline),
            char('r').to(EscapableChar::CarriageReturn),
            char('t').to(EscapableChar::Tab),
            char('"').to(EscapableChar::DoubleQuote),
            char('\'').to(EscapableChar::SingleQuote),
            char('`').to(EscapableChar::BackQuote),
            char('$').to(EscapableChar::DollarSign),
            char('\\').to(EscapableChar::Backslash),
            char('^').to(EscapableChar::Caret),
        ))
        .critical("this character is not escapable"),
    )
}

pub fn literal_string() -> impl Parser<String> + Send + Sync {
    char('\'')
        .ignore_then(
            choice((
                ESCAPABLE_CHAR
                    .static_ref()
                    .map(EscapableChar::original_char),
                filter(|c| c != '\''),
            ))
            .repeated_into_container::<String>(),
        )
        .then_ignore(char('\''))
}

pub fn literal_int() -> impl Parser<i64> + Send + Sync {
    char('-')
        .or_not()
        .then(digit(10).repeated().at_least(1))
        .not_followed_by(possible_ident_char)
        .map_str(|num| str::parse::<i64>(num).unwrap())
}

pub fn literal_value() -> impl Parser<LiteralValue> + Send + Sync {
    choice::<LiteralValue, _>((
        // Strings
        LITERAL_STRING
            .static_ref()
            .map(LiteralValue::String)
            .followed_by(
                silent_choice((
                    filter(|c| {
                        c != '\'' && (c.is_whitespace() || DELIMITER_CHARS.contains(&c) || c == ',')
                    }),
                    end(),
                ))
                .critical("literal string cannot be followed by another non-delimiting character"),
            ),
        // Booleans
        just("true").map(|_| LiteralValue::Boolean(true)),
        just("false").map(|_| LiteralValue::Boolean(false)),
        // Floats
        char('-')
            .or_not()
            .then(digit(10).repeated().at_least(1))
            .then(char('.'))
            .not_followed_by(char('.')) // don't mix up with ranges
            .then(
                digit(10)
                    .repeated()
                    .at_least(1)
                    .critical("expected digits after the dot separator"),
            )
            .not_followed_by(possible_ident_char)
            .map_str(|num| LiteralValue::Float(str::parse::<f64>(num).unwrap())),
        // Integers
        LITERAL_INT.static_ref().map(LiteralValue::Integer),
    ))
}

pub fn computed_string() -> impl Parser<ComputedString> + Send + Sync {
    char('"')
    .ignore_then(
        choice::<ComputedStringPiece, _>((
            // Escaped
            ESCAPABLE_CHAR.static_ref().map(ComputedStringPiece::Escaped),
            // Command calls
            CMD_CAPTURE.static_ref().map(Box::new).map(ComputedStringPiece::CmdOutput),
            // Variables
            char('$')
                .ignore_then(ident.critical("expected an identifier after '$' symbol (did you want to escape it with a backslash?)"))
                .spanned()
                .map(ComputedStringPiece::Variable),
            // Expressions
            char('`')
                .ignore_then(
                    EXPR.static_ref()
                        .spanned()
                        .padded_by(msnl)
                        .critical("expected an expression"),
                )
                .then_ignore(char('`').critical_auto_msg())
                .map(ComputedStringPiece::Expr),
            // Literal character suites
            filter(|c| c != '"' && c != '$' && c != '`' && c != '\\')
                .repeated()
                .at_least(1)
                .collect_string()
                .map(ComputedStringPiece::Literal),
        ))
        .repeated_into_vec(),
    )
    .then_ignore(char('"').critical_auto_msg())
    .map(|pieces| ComputedString { pieces })
}

pub fn range() -> impl Parser<Range> + Send + Sync {
    let range_bound = choice::<RangeBound, _>((
        LITERAL_INT.static_ref().map(RangeBound::Literal),
        var_name.spanned().map(RangeBound::Variable),
        char('(')
            .ignore_then(EXPR.static_ref().critical("expected an expression"))
            .then_ignore(char(')').critical_auto_msg())
            .spanned()
            .map(RangeBound::Expr),
    ));

    range_bound
        .then_ignore(just(".."))
        .then(char('=').or_not().map(|eq| eq.is_some()))
        .then(range_bound.critical("expected a closing range bound"))
        .map(|((from, include_last_value), to)| Range {
            from,
            to,
            include_last_value,
        })
}

pub fn lambda() -> impl Parser<Function> + Send + Sync {
    let normal_lambda = char('{')
        .ignore_then(msnl)
        .ignore_then(
            char('|')
                .ignore_then(msnl)
                .ignore_then(
                    FN_SIGNATURE_ARG
                        .static_ref()
                        .separated_by_into_vec(char(',').padded_by(msnl))
                        .spanned()
                        .map(RuntimeSpan::from)
                        .map(|args| FnSignature {
                            args,
                            ret_type: None,
                        }),
                )
                .then_ignore(msnl)
                .then_ignore(char('|').critical_auto_msg())
                .spanned()
                .then_ignore(msnl),
        )
        .then(
            RAW_BLOCK
                .static_ref()
                .erase_type()
                .critical("expected a body for the lambda")
                .spanned(),
        )
        .then_ignore(msnl)
        .then_ignore(char('}').critical_auto_msg())
        .map(|(signature, body)| Function { signature, body });

    let it_lambda = char(':')
        .spanned()
        .then_ignore(char('{'))
        .then_ignore(msnl)
        .then(
            RAW_BLOCK
                .static_ref()
                .erase_type()
                .critical("expected a body for the lambda")
                .spanned(),
        )
        .then_ignore(msnl)
        .then_ignore(char('}').critical_auto_msg())
        .map(|(it, body)| Function {
            signature: it.forge_here(FnSignature {
                args: RuntimeSpan::from(it.forge_here(vec![FnSignatureArg::Positional(
                    FnSignaturePositionalArg {
                        name: RuntimeSpan::from(it.forge_here("it".to_owned())),
                        is_optional: false,
                        typ: None,
                    },
                )])),
                ret_type: None,
            }),
            body,
        });

    normal_lambda.or(it_lambda)
}

pub fn cmd_capture() -> impl Parser<CmdOutputCapture> + Send + Sync {
    choice::<CmdCaptureType, _>((
        just("$(").to(CmdCaptureType::Stdout),
        just("$^(").to(CmdCaptureType::Stderr),
    ))
    .spanned()
    .then_ignore(msnl)
    .then(
        CMD_CALL
            .static_ref()
            .erase_type()
            .spanned()
            .critical("expected a command call to capture"),
    )
    .then_ignore(msnl)
    .then_ignore(char(')').critical_auto_msg())
    .map(|(capture, cmd_call)| CmdOutputCapture { capture, cmd_call })
}

pub fn inline_cmd_call() -> impl Parser<Span<CmdCall>> + Send + Sync {
    just("@(")
        .ignore_then(msnl)
        .ignore_then(
            CMD_CALL
                .static_ref()
                .erase_type()
                .spanned()
                .critical("expected a command call"),
        )
        .then_ignore(msnl)
        .then_ignore(char(')').critical_auto_msg())
}

pub fn spread_value() -> impl Parser<SpreadValue> + Send + Sync {
    just("...").ignore_then(
        choice::<SpreadValue, _>((
            char('$')
                .ignore_then(ident.critical("expected a variable name to spread"))
                .spanned()
                .map(SpreadValue::Variable),
            char('(')
                .ignore_then(msnl)
                .ignore_then(
                    EXPR.static_ref()
                        .critical("expected an expression to spread"),
                )
                .then_ignore(msnl)
                .then_ignore(char(')'))
                .map(SpreadValue::Expr),
        ))
        .critical("expected a value to spread"),
    )
}

pub fn fn_call() -> impl Parser<FnCall> + Send + Sync {
    let fn_arg = choice::<FnArg, _>((
        //
        // Flags
        //
        choice::<FnFlagArgName, _>((
            just("--")
                .ignore_then(
                    FN_ARG_LONG_FLAG_NAME_IDENTIFIER
                        .static_ref()
                        .critical("expected a flag name after '--'"),
                )
                .map(FnFlagArgName::Long),
            char('-')
                .ignore_then(first_ident_char)
                .map(FnFlagArgName::Short),
        ))
        .spanned()
        .then(
            silent_choice((s, ms.then(char('=')).then(ms)))
                .ignore_then(
                    EXPR.static_ref()
                        .critical("expected an expression")
                        .spanned(),
                )
                .or_not(),
        )
        .map(|(name, value)| FnArg::Flag { name, value }),
        //
        // Expression
        //
        EXPR.static_ref().spanned().map(FnArg::Expr),
    ));

    choice::<FnCallNature, _>((
        char('$').to(FnCallNature::Variable),
        char('.').to(FnCallNature::Method),
        empty().to(FnCallNature::NamedFunction),
    ))
    .then(ident.spanned())
    .then_ignore(char('('))
    .then(
        fn_arg
            .spanned()
            .padded_by(msnl)
            .separated_by_into_vec(char(','))
            .critical_if_fails_after_sep("expected a valid argument after separator")
            .spanned(),
    )
    .then_ignore(char(')').critical("unexpected symbol"))
    .map(|((nature, name), call_args)| FnCall {
        nature,
        name,
        call_args,
    })
}

pub fn list_value() -> impl Parser<Vec<ListItem>> + Send + Sync {
    char('[')
        .ignore_then(msnl)
        .ignore_then(
            choice::<ListItem, _>((
                EXPR.static_ref().map(ListItem::Single),
                SPREAD_VALUE.static_ref().spanned().map(ListItem::Spread),
            ))
            .separated_by_into_vec(char(',').padded_by(msnl)),
        )
        .then_ignore(msnl)
        .then_ignore(char(']').critical("expected a closing bracket ']' for the list"))
}

pub fn value() -> impl Parser<Value> + Send + Sync {
    let map_key = choice::<MapKey, _>((
        ident.map(MapKey::Raw),
        LITERAL_STRING.static_ref().map(MapKey::LiteralString),
        COMPUTED_STRING.static_ref().map(MapKey::ComputedString),
        char('[')
            .ignore_then(msnl)
            .ignore_then(EXPR.static_ref().critical("expected an expression"))
            .then_ignore(msnl)
            .then_ignore(char(']'))
            .map(MapKey::Expr),
    ));

    choice::<Value, _>((
        just("null").map(|_| Value::Null),
        // Range bounds
        RANGE.static_ref().map(Box::new).map(Value::Range),
        // Literals
        LITERAL_VALUE.static_ref().map(Value::Literal),
        // Computed strings
        COMPUTED_STRING.static_ref().map(Value::ComputedString),
        // Lists
        LIST_VALUE.static_ref().map(Value::List),
        // Maps
        just("map")
            .ignore_then(msnl)
            .ignore_then(char('{'))
            .ignore_then(msnl)
            .ignore_then(
                choice::<MapItem, _>((
                    //
                    // Single value
                    //
                    map_key
                        .spanned()
                        .then_ignore(ms)
                        .then_ignore(char(':').critical_auto_msg())
                        .then_ignore(msnl)
                        .then(EXPR.static_ref().critical("expected an expression"))
                        .map(|(key, value)| MapItem::Single { key, value }),
                    //
                    // Spread value
                    //
                    SPREAD_VALUE.static_ref().spanned().map(MapItem::Spread),
                ))
                .separated_by_into_vec(char(',').padded_by(msnl)),
            )
            .then_ignore(msnl)
            .then_ignore(char('}').critical_auto_msg())
            .map(Value::Map),
        // Lambdas
        LAMBDA.static_ref().map(Box::new).map(Value::Lambda),
        // Structures
        char('{')
            .ignore_then(msnl)
            .ignore_then(
                choice::<StructItem, _>((
                    //
                    // Single value
                    //
                    ident
                        .spanned()
                        .then_ignore(ms)
                        .then_ignore(char(':').critical_auto_msg())
                        .then_ignore(msnl)
                        .then(EXPR.static_ref().critical("expected an expression"))
                        .map(|(field, value)| StructItem::Single { field, value }),
                    //
                    // Spread value
                    //
                    SPREAD_VALUE.static_ref().spanned().map(StructItem::Spread),
                ))
                .separated_by_into_vec(char(',').padded_by(msnl)),
            )
            .then_ignore(msnl)
            .then_ignore(char('}').critical_auto_msg())
            .map(Value::Struct),
        // Function calls
        FN_CALL
            .static_ref()
            .spanned()
            .map(Box::new)
            .map(Value::FnCall),
        // Command output captures
        CMD_CAPTURE.static_ref().map(Box::new).map(Value::CmdOutput),
        // Variables
        var_name.spanned().map(Value::Variable),
        // Command calls
        INLINE_CMD_CALL.static_ref().map(Value::CmdCall),
        // Function as value
        char('@')
            .ignore_then(ident.critical("expected a function name"))
            .spanned()
            .map(Value::FnAsValue),
    ))
}
