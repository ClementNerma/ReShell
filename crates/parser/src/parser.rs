use std::{borrow::Cow, collections::HashSet, sync::LazyLock};

use parsy::{
    FileId, Parser, ParsingError,
    atoms::{alphanumeric, digits},
    char, choice, empty, end, filter, just, lookahead, newline, not, recursive, silent_choice,
    to_define, whitespaces,
};

use crate::{
    ast::{
        Block, CmdArg, CmdCall, CmdCallBase, CmdCaptureType, CmdEnvVar, CmdExternalPath,
        CmdFlagArg, CmdFlagNameArg, CmdFlagValueArg, CmdOutputCapture, CmdPath, CmdPipe,
        CmdPipeType, CmdRawString, CmdRawStringPiece, CmdRedirects, CmdValueMakingArg,
        ComputedString, ComputedStringPiece, DoubleOp, ElsIf, ElsIfExpr, EscapableChar, Expr,
        ExprInner, ExprInnerChaining, ExprInnerContent, ExprOp, FlagValueSeparator, FnSignatureArg, FnCall,
        FnArg, FnCallNature, FnSignatureFlagArgNames, FnSignatureNormalFlagArg, FnSignaturePositionalArg,
        FnSignaturePresenceFlagArg, FnSignatureRestArg, FnSignature, Function, Instruction, ListItem, LiteralValue,
        MapItem, MapKey, MatchCase, MatchExprCase, ObjPropSpreading, ObjPropSpreadingBinding,
        ObjPropSpreadingType, Program, PropAccess, PropAccessNature, RangeBound, RuntimeSpan,
        SingleCmdCall, SingleOp, SingleValueType, SingleVarDecl, SpreadValue, StructItem,
        StructTypeMember, TypeMatchCase, TypeMatchExprCase, Value, ValueType, VarSpreading,
    },
    files::SourceFile,
    scope::ScopeIdGenerator,
};

/// Parse a full program
///
/// Note that, due to the way scope IDs are generated, if scopes are reused
/// (e.g. REPL), then the **same** parser **MUST** be reused!
///
/// Otherwise the scope IDs counter will be reset and scope IDs will clash
pub fn program(
    load_file: impl Fn(String, FileId) -> Result<SourceFile, String> + 'static,
) -> impl Parser<Program> {
    let program = to_define::<Program>();
    let program_bis = program.clone();

    let comment = char('#').then(filter(|c| c != '\r' && c != '\n').repeated());

    let ms = silent_choice((
        comment,
        filter(|c| c.is_whitespace() && c != '\r' && c != '\n'),
    ))
    .repeated();

    let msnl = silent_choice((comment, filter(|c| c.is_whitespace()))).repeated();

    let scope_id_gen = ScopeIdGenerator::new();

    let raw_block = to_define::<Block>();

    let s = whitespaces().no_newline().at_least_one();

    let block = char('{')
        .critical_auto_msg()
        .ignore_then(msnl)
        .ignore_then(raw_block.clone())
        .then_ignore(msnl)
        .then_ignore(char('}').critical_auto_msg());

    let possible_ident_char = choice((char('_'), alphanumeric()));

    let first_ident_char = filter(|c| c == '_' || c.is_alphabetic());

    let ident = first_ident_char
        .then(filter(|c| c == '_' || c.is_alphanumeric()).repeated())
        .collect_string();

    let var_name = char('$').ignore_then(ident);

    let cmd_call = to_define::<CmdCall>();

    let expr = to_define::<Expr>();

    let cmd_flag_arg = to_define::<CmdFlagArg>();

    let fn_signature = to_define::<FnSignature>();

    let escapable_char = char('\\').ignore_then(
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
    );

    let literal_string = char('\'')
        .ignore_then(
            choice((
                escapable_char.map(EscapableChar::original_char),
                filter(|c| c != '\''),
            ))
            .repeated_into_container::<String>(),
        )
        .then_ignore(char('\''));

    let value_type = recursive::<ValueType, _>(|value_type| {
        let single_value_type = choice::<SingleValueType, _>((
            just("any")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Any),
            just("null")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Null),
            just("bool")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Bool),
            just("int")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Int),
            just("float")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Float),
            just("string")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::String),
            just("error")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Error),
            just("cmdcall")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::CmdCall),
            just("fn")
                .ignore_then(fn_signature.clone())
                .spanned()
                .map(RuntimeSpan::from)
                .map(SingleValueType::Function),
            just("list[")
                .ignore_then(value_type.clone().map(Box::new))
                .then_ignore(char(']').critical_auto_msg())
                .map(SingleValueType::TypedList),
            just("list")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::UntypedList),
            just("map[")
                .ignore_then(value_type.clone().map(Box::new))
                .then_ignore(char(']').critical_auto_msg())
                .map(SingleValueType::TypedMap),
            just("map")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::UntypedMap),
            char('{')
                .ignore_then(msnl)
                .ignore_then(
                    ident
                        .spanned()
                        .map(RuntimeSpan::from)
                        .then_ignore(ms)
                        .then(char('?').or_not())
                        .then_ignore(ms)
                        .then_ignore(char(':').critical_auto_msg())
                        .then_ignore(msnl)
                        .then(value_type.clone().critical("expected a value type"))
                        .map(|((name, optional), typ)| StructTypeMember {
                            name,
                            optional: optional.is_some(),
                            typ,
                        })
                        .separated_by_into_vec(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .map(SingleValueType::TypedStruct),
            just("struct")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::UntypedStruct),
            ident.spanned().map(SingleValueType::TypeAlias),
            literal_string.map(SingleValueType::StringLiteral),
        ));

        choice((
            // Union type
            char('(')
                .ignore_then(
                    single_value_type
                        .clone()
                        .separated_by_into_vec(char('|').padded_by(msnl))
                        .at_least(1)
                        .critical("expected a type union"),
                )
                .then_ignore(char(')').critical_auto_msg())
                .map(ValueType::Union),
            // Single type
            single_value_type.map(ValueType::Single),
        ))
    });

    let fn_arg_long_flag = just("--")
        .ignore_then(
            first_ident_char
                .then(filter(|c| c == '_' || c == '-' || c.is_alphanumeric()).repeated())
                .collect_string()
                .spanned()
                .critical("expected a flag name (identifier)"),
        )
        .followed_by(not(possible_ident_char).critical("unexpected symbol after long flag name"));

    let fn_arg_short_flag = char('-')
        .ignore_then(
            first_ident_char
                .spanned()
                .critical("expected a single-character identifier"),
        )
        .followed_by(not(possible_ident_char).critical("expected a single-character identifier"));

    let fn_flag_arg_signature_names = choice::<FnSignatureFlagArgNames, _>((
        // Long *and* short flags
        fn_arg_long_flag
            .map(RuntimeSpan::from)
            .then_ignore(ms)
            .then_ignore(char('('))
            .then(fn_arg_short_flag.map(RuntimeSpan::from))
            .then_ignore(char(')').critical_auto_msg())
            .map(|(long, short)| FnSignatureFlagArgNames::LongAndShortFlag { short, long }),
        // Long flag only
        fn_arg_long_flag
            .map(RuntimeSpan::from)
            .map(FnSignatureFlagArgNames::LongFlag),
        // Long flag only
        fn_arg_short_flag
            .map(RuntimeSpan::from)
            .map(FnSignatureFlagArgNames::ShortFlag),
    ));

    let fn_arg_signature = choice::<FnSignatureArg, _>((
        // Positional
        ident
            .spanned()
            .map(RuntimeSpan::from)
            .then_ignore(msnl)
            .then(char('?').or_not())
            .then(
                char(':')
                    .ignore_then(msnl)
                    .ignore_then(
                        value_type
                            .clone()
                            .critical("expected a type for the argument"),
                    )
                    .or_not(),
            )
            .map(|((name, is_optional), typ)| {
                FnSignatureArg::Positional(FnSignaturePositionalArg {
                    name,
                    is_optional: is_optional.is_some(),
                    typ,
                })
            }),
        // Normal flags
        fn_flag_arg_signature_names
            .then_ignore(msnl)
            .then(char('?').or_not())
            .then_ignore(char(':'))
            .then_ignore(msnl)
            .then(
                value_type
                    .clone()
                    .critical("expected a type for the flag argument"),
            )
            .map(|((names, is_optional), typ)| {
                FnSignatureArg::NormalFlag(FnSignatureNormalFlagArg {
                    names,
                    is_optional: is_optional.is_some(),
                    typ,
                })
            }),
        // Presence flags
        fn_flag_arg_signature_names
            .then_ignore(msnl)
            .then_ignore(
                char('?').critical("expected either a '?' or a ':' marker after flag argument"),
            )
            .map(|names| FnSignatureArg::PresenceFlag(FnSignaturePresenceFlagArg { names })),
        // Rest
        just("...")
            .ignore_then(ident)
            .spanned()
            .map(RuntimeSpan::from)
            .then(
                msnl.ignore_then(char(':'))
                    .ignore_then(msnl)
                    .ignore_then(
                        value_type
                            .clone()
                            .critical("expected a type for the rest parameter")
                            .spanned()
                            .map(RuntimeSpan::from),
                    )
                    .or_not(),
            )
            .map(|(name, typ)| FnSignatureArg::Rest(FnSignatureRestArg { name, typ })),
    ));

    fn_signature.define(
        char('(')
            .ignore_then(
                fn_arg_signature
                    .clone()
                    .separated_by_into_vec(char(',').padded_by(msnl))
                    .spanned()
                    .map(RuntimeSpan::from),
            )
            .then_ignore(msnl)
            .then_ignore(char(')').critical_auto_msg())
            .then(
                msnl.ignore_then(just("->"))
                    .ignore_then(ms)
                    .ignore_then(
                        value_type
                            .clone()
                            .map(Box::new)
                            .spanned()
                            .map(RuntimeSpan::from)
                            .critical("expected a type"),
                    )
                    .then_ignore(ms)
                    .or_not(),
            )
            .map(|(args, ret_type)| FnSignature { args, ret_type }),
    );

    let fn_arg = choice::<FnArg, _>((
        ident
            .spanned()
            .then_ignore(ms)
            .then_ignore(char(':'))
            .then_ignore(msnl)
            .then(
                expr.clone()
                    .spanned()
                    .critical("expected an expression for the flag"),
            )
            .map(|(name, value)| FnArg::Flag {
                name: name.map(|name| {
                    if name.chars().nth(1).is_some() {
                        CmdFlagNameArg::LongNoConvert(name)
                    } else {
                        CmdFlagNameArg::Short(name.chars().next().unwrap())
                    }
                }),
                value,
            }),
        expr.clone().spanned().map(FnArg::Expr),
    ));

    let fn_call = choice::<FnCallNature, _>((
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
            .spanned(),
    )
    .then_ignore(char(')').critical_auto_msg())
    .map(|((nature, name), call_args)| FnCall {
        nature,
        name,
        call_args,
    });

    let cmd_capture = choice::<CmdCaptureType, _>((
        just("$(").to(CmdCaptureType::Stdout),
        just("$^(").to(CmdCaptureType::Stderr),
    ))
    .spanned()
    .then_ignore(msnl)
    .then(
        cmd_call
            .clone()
            .spanned()
            .critical("expected a command call to capture"),
    )
    .then_ignore(msnl)
    .then_ignore(char(')').critical_auto_msg())
    .map(|(capture, cmd_call)| CmdOutputCapture { capture, cmd_call });

    let int_literal = char('-')
        .or_not()
        .then(digits(10))
        .not_followed_by(possible_ident_char)
        .map_str(|num| str::parse::<i64>(num).unwrap());

    let literal_value = choice::<LiteralValue, _>((
        // Strings
        literal_string.map(LiteralValue::String).followed_by(
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
            .then(digits(10))
            .then(char('.'))
            .then(digits(10).critical("expected digits after the dot separator"))
            .not_followed_by(possible_ident_char)
            .map_str(|num| LiteralValue::Float(str::parse::<f64>(num).unwrap())),
        // Integers
        int_literal.map(LiteralValue::Integer),
    ));

    let computed_string = char('"')
            .ignore_then(
                choice::<ComputedStringPiece, _>((
                    // Escaped
                    escapable_char.map(ComputedStringPiece::Escaped),
                    // Command calls
                    cmd_capture.clone().map(Box::new).map(ComputedStringPiece::CmdOutput),
                    // Variables
                    char('$')
                        .ignore_then(ident.critical("expected an identifier after '$' symbol (did you want to escape it with a backslash?)"))
                        .spanned()
                        .map(ComputedStringPiece::Variable),
                    // Expressions
                    char('`')
                        .ignore_then(
                            expr.clone()
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
            .map(|pieces| ComputedString { pieces });

    let lambda = char('{')
        .ignore_then(msnl)
        .ignore_then(
            char('|')
                .ignore_then(msnl)
                .ignore_then(
                    fn_arg_signature
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
            raw_block
                .clone()
                .critical("expected a body for the lambda")
                .spanned(),
        )
        .then_ignore(ms)
        .then_ignore(char('}').critical_auto_msg())
        .map(|(signature, body)| Function { signature, body });

    let inline_cmd_call = just("@(")
        .ignore_then(msnl)
        .ignore_then(
            cmd_call
                .clone()
                .spanned()
                .critical("expected a command call"),
        )
        .then_ignore(msnl)
        .then_ignore(char(')').critical_auto_msg());

    let spread_value = just("...").ignore_then(
        choice::<SpreadValue, _>((
            char('$')
                .ignore_then(ident.critical("expected a variable name to spread"))
                .spanned()
                .map(SpreadValue::Variable),
            char('(')
                .ignore_then(msnl)
                .ignore_then(expr.clone().critical("expected an expression to spread"))
                .then_ignore(msnl)
                .then_ignore(char(')'))
                .map(SpreadValue::Expr),
        ))
        .critical("expected a value to spread"),
    );

    let map_key = choice::<MapKey, _>((
        ident.map(MapKey::Raw),
        literal_string.map(MapKey::LiteralString),
        computed_string.clone().map(MapKey::ComputedString),
        char('[')
            .ignore_then(msnl)
            .ignore_then(expr.clone().critical("expected an expression"))
            .then_ignore(msnl)
            .then_ignore(char(']'))
            .map(MapKey::Expr),
    ));

    let value = choice::<Value, _>((
        just("null").map(|_| Value::Null),
        // Literals
        literal_value.map(Value::Literal),
        // Computed strings
        computed_string.clone().map(Value::ComputedString),
        // Lists
        char('[')
            .ignore_then(msnl)
            .ignore_then(
                choice::<ListItem, _>((
                    expr.clone().map(ListItem::Single),
                    spread_value.clone().spanned().map(ListItem::Spread),
                ))
                .separated_by_into_vec(char(',').padded_by(msnl)),
            )
            .then_ignore(msnl)
            .then_ignore(char(']').critical("expected a closing bracket ']' for the list"))
            .map(Value::List),
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
                        .then(expr.clone().critical("expected an expression"))
                        .map(|(key, value)| MapItem::Single { key, value }),
                    //
                    // Spread value
                    //
                    spread_value.clone().spanned().map(MapItem::Spread),
                ))
                .separated_by_into_vec(char(',').padded_by(msnl)),
            )
            .then_ignore(msnl)
            .then_ignore(char('}').critical_auto_msg())
            .map(Value::Map),
        // Lambdas
        lambda.clone().map(Box::new).map(Value::Lambda),
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
                        .then(expr.clone().critical("expected an expression"))
                        .map(|(field, value)| StructItem::Single { field, value }),
                    //
                    // Spread value
                    //
                    spread_value.clone().spanned().map(StructItem::Spread),
                ))
                .separated_by_into_vec(char(',').padded_by(msnl)),
            )
            .then_ignore(msnl)
            .then_ignore(char('}').critical_auto_msg())
            .map(Value::Struct),
        // Function calls
        fn_call.clone().spanned().map(Box::new).map(Value::FnCall),
        // Command output captures
        cmd_capture.clone().map(Box::new).map(Value::CmdOutput),
        // Variables
        var_name.spanned().map(Value::Variable),
        // Command calls
        inline_cmd_call.clone().map(Box::new).map(Value::CmdCall),
        // Function as value
        char('@')
            .ignore_then(ident.critical("expected a function name"))
            .spanned()
            .map(Value::FnAsValue),
    ));

    let single_op = choice::<SingleOp, _>((char('!').to(SingleOp::Neg),));

    let double_op = not(just("->")).ignore_then(choice::<DoubleOp, _>((
        char('+').to(DoubleOp::Add),
        char('-').to(DoubleOp::Sub),
        char('*').to(DoubleOp::Mul),
        char('/').to(DoubleOp::Div),
        char('%').to(DoubleOp::Mod),
        just("&&").to(DoubleOp::And),
        just("||").to(DoubleOp::Or),
        just("==").to(DoubleOp::Eq),
        just("!=").to(DoubleOp::Neq),
        just("<=").to(DoubleOp::Lte),
        just("<").to(DoubleOp::Lt),
        just(">=").to(DoubleOp::Gte),
        just(">").to(DoubleOp::Gt),
        just("??").to(DoubleOp::NullFallback),
    )));

    let braces_expr_body = char('{')
        .critical_auto_msg()
        .ignore_then(msnl)
        .ignore_then(expr.clone().map(Box::new))
        .then_ignore(msnl)
        .then_ignore(char('}').critical_auto_msg());

    let expr_inner_chaining = to_define::<ExprInnerChaining>();

    let scope_id_gen_bis = scope_id_gen.clone();

    let expr_inner_content = recursive(|expr_inner_content| {
        choice::<ExprInnerContent, _>((
            //
            // Single operator (e.g. '!') application
            //
            single_op
                .then_ignore(ms)
                .then(expr_inner_content.spanned().map(Box::new))
                .then(expr_inner_chaining.clone().spanned().repeated_into_vec())
                .map(
                    |((op, right), right_chainings)| ExprInnerContent::SingleOp {
                        op,
                        right,
                        right_chainings,
                    },
                ),
            //
            // Parenthesis-wrapped expression
            //
            char('(')
                .ignore_then(msnl)
                .ignore_then(
                    expr.clone()
                        .critical("expected an expression between the parenthesis")
                        .map(Box::new),
                )
                .then_ignore(msnl)
                .then_ignore(char(')'))
                .map(ExprInnerContent::ParenExpr),
            //
            // Ternaries
            //
            just("if")
                .ignore_then(s)
                .ignore_then(
                    expr.clone()
                        .map(Box::new)
                        .spanned()
                        .critical("expected a condition"),
                )
                .then_ignore(ms)
                .then(
                    braces_expr_body
                        .clone()
                        .critical("expected a body for the condition"),
                )
                .then(
                    msnl.ignore_then(just("else"))
                        .ignore_then(s)
                        .ignore_then(just("if"))
                        .ignore_then(s)
                        .ignore_then(
                            expr.clone()
                                .map(Box::new)
                                .spanned()
                                .critical("expected a condition for the 'else if' statement"),
                        )
                        .then_ignore(ms)
                        .then(braces_expr_body.clone())
                        .map(|(cond, body)| ElsIfExpr { cond, body })
                        .repeated_into_vec()
                        .then(
                            msnl.ignore_then(just("else").critical("expected an 'else' block"))
                                .ignore_then(ms)
                                .ignore_then(
                                    braces_expr_body
                                        .clone()
                                        .critical("expected a body for the 'else' block"),
                                ),
                        ),
                )
                .map(|((cond, body), (elsif, els))| ExprInnerContent::Ternary {
                    cond,
                    body,
                    elsif,
                    els,
                }),
            //
            // Value matching
            //
            just("match")
                .ignore_then(s)
                .ignore_then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected an expression to match on"),
                )
                .then_ignore(msnl)
                .then_ignore(char('{').critical_auto_msg())
                .then(
                    msnl.ignore_then(just("case"))
                        .ignore_then(ms)
                        .ignore_then(
                            expr.clone()
                                .spanned()
                                .critical("expected an expression to match"),
                        )
                        .then_ignore(msnl)
                        .then_ignore(char('{').critical_auto_msg())
                        .then_ignore(msnl)
                        .then(
                            expr.clone()
                                .critical("expected an expression to evaluate to"),
                        )
                        .then_ignore(msnl)
                        .then_ignore(char('}').critical_auto_msg())
                        .map(|(matches, then)| MatchExprCase { matches, then })
                        .repeated_into_vec(),
                )
                .then_ignore(msnl)
                .then_ignore(just("else").critical_auto_msg())
                .then_ignore(msnl)
                .then_ignore(char('{').critical_auto_msg())
                .then_ignore(msnl)
                .then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected an expression to evaluate to"),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .map(|((expr, cases), els)| ExprInnerContent::Match { expr, cases, els }),
            //
            // Type matching
            //
            just("typematch")
                .ignore_then(s)
                .ignore_then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected an expression to match on"),
                )
                .then_ignore(msnl)
                .then_ignore(char('{').critical_auto_msg())
                .then(
                    msnl.ignore_then(just("case"))
                        .ignore_then(ms)
                        .ignore_then(value_type.clone().critical("expected a type to match"))
                        .then_ignore(msnl)
                        .then_ignore(char('{').critical_auto_msg())
                        .then_ignore(msnl)
                        .then(
                            expr.clone()
                                .critical("expected an expression to evaluate to"),
                        )
                        .then_ignore(msnl)
                        .then_ignore(char('}').critical_auto_msg())
                        .map(|(matches, then)| TypeMatchExprCase { matches, then })
                        .repeated_into_vec(),
                )
                .then_ignore(msnl)
                .then_ignore(just("else").critical_auto_msg())
                .then_ignore(msnl)
                .then_ignore(char('{').critical_auto_msg())
                .then_ignore(msnl)
                .then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected an expression to evaluate to"),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .map(|((expr, cases), els)| ExprInnerContent::TypeMatch { expr, cases, els }),
            //
            // Try / catch
            //
            just("try")
                .ignore_then(msnl)
                .ignore_then(char('{'))
                .ignore_then(msnl)
                .ignore_then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected an expression"),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .then_ignore(msnl)
                .then_ignore(just("catch").critical_auto_msg())
                .then_ignore(s.critical_auto_msg())
                .then(ident.spanned().critical("expected a catch variable"))
                .then_ignore(msnl)
                .then_ignore(char('{').critical_auto_msg())
                .then_ignore(msnl)
                .then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected a catch expression"),
                )
                .then_ignore(msnl)
                .then_ignore(char('}'))
                .map(
                    move |((try_expr, catch_var), catch_expr)| ExprInnerContent::Try {
                        try_expr,
                        catch_var,
                        catch_expr,
                        catch_expr_scope_id: scope_id_gen_bis.next(),
                    },
                ),
            //
            // Throws
            //
            just("throw")
                .ignore_then(ms)
                .ignore_then(
                    expr.clone()
                        .map(Box::new)
                        .spanned()
                        .critical("expected an expression to throw"),
                )
                .map(ExprInnerContent::Throw),
            //
            // Loop continuation keyword
            //
            just("continue")
                .followed_by(silent_choice((
                    filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
                    end(),
                )))
                .map(|_| ExprInnerContent::LoopContinue),
            //
            // Loop breakage
            //
            just("break")
                .followed_by(silent_choice((
                    filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
                    end(),
                )))
                .map(|_| ExprInnerContent::LoopBreak),
            //
            // Simple values
            //
            value.clone().map(ExprInnerContent::Value),
        ))
    });

    let prop_access_nature = choice::<PropAccessNature, _>((
        char('.')
            .ignore_then(ident.spanned().critical("expected a property name"))
            .not_followed_by(char('('))
            .map(PropAccessNature::Prop),
        char('[')
            .not_followed_by(char(']'))
            .ignore_then(
                expr.clone()
                    .padded_by(msnl)
                    .spanned()
                    .critical("expected an expression"),
            )
            .map(Box::new)
            .then_ignore(char(']').critical_auto_msg())
            .map(PropAccessNature::Key),
    ));

    let prop_access = char('?')
        .or_not()
        .then(prop_access_nature.clone().spanned())
        .map(|(nullable, nature)| PropAccess {
            nullable: nullable.is_some(),
            nature,
        });

    expr_inner_chaining.define(choice::<ExprInnerChaining, _>((
        msnl.ignore_then(lookahead(char('.')))
            .ignore_then(fn_call.clone())
            .spanned()
            .map(ExprInnerChaining::MethodCall),
        lookahead(choice((char('.'), char('?'), char('[')))).ignore_then(
            prop_access
                .critical("expected either a method call or a property access")
                .map(ExprInnerChaining::PropAccess),
        ),
    )));

    let expr_inner = expr_inner_content
        .spanned()
        .then(expr_inner_chaining.repeated_into_vec())
        .map(|(content, chainings)| ExprInner { content, chainings });

    let expr_op = choice::<ExprOp, _>((
        //
        // Double operators
        //
        ms.ignore_then(double_op.spanned())
            .then_ignore(msnl)
            .then(
                expr_inner
                    .clone()
                    .spanned()
                    .critical("expected an expression after the operator")
                    .map(Box::new),
            )
            .map(|(op, right_op)| ExprOp::DoubleOp { op, right_op }),
        //
        // Typechecker operator
        //
        s.ignore_then(just("typeis"))
            .ignore_then(s)
            .ignore_then(value_type.clone().spanned())
            .map(|right_op| ExprOp::TypeIs { right_op }),
    ));

    expr.define(
        expr_inner
            .spanned()
            .then(expr_op.repeated_into_vec())
            .map(|(inner, right_ops)| Expr { inner, right_ops }),
    );

    let cmd_raw_string = not(just("->")).ignore_then(
        choice::<CmdRawStringPiece, _>((
            // Variables
            var_name.spanned().map(CmdRawStringPiece::Variable),
            // Literal character suites
            filter(|c| !c.is_whitespace() && !DELIMITER_CHARS.contains(&c))
                .repeated()
                .at_least(1)
                .collect_string()
                .map(CmdRawStringPiece::Literal),
        ))
        .spanned()
        .repeated_into_vec()
        .at_least(1)
        .map(|pieces| CmdRawString { pieces }),
    );

    let raw_cmd_path = filter(|c| !c.is_whitespace() && !DELIMITER_CHARS.contains(&c))
        .repeated()
        .at_least(1)
        .collect_string()
        .validate_or_critical(|cmd_name| {
            if 
            // Command name (no slashes)
            !cmd_name.contains(['/', '\\'])
                || (
                    // Absolute command path
                    cmd_name.starts_with(['/', '\\']) ||
                    // Home-dir relative command path
                    cmd_name.starts_with("~/") || cmd_name.starts_with("~\\") ||
                    // Relative command paths
                    cmd_name.starts_with("./") || cmd_name.starts_with("../") || cmd_name.starts_with(".\\") || cmd_name.starts_with("..\\")
                )
                {
                    Ok(())
                } else {
                    Err("Relative command paths must start with a dot and a slash (e.g. './path/to/cmd' or '../path/to/cmd')".into())
                }
        });

    let cmd_path = choice::<CmdPath, _>((
        //
        // External commands
        //
        char('^')
            .ignore_then(
                choice::<CmdExternalPath, _>((
                    // Raw path
                    cmd_raw_string.spanned().map(CmdExternalPath::RawString),
                    // Single-quoted string
                    literal_string.spanned().map(CmdExternalPath::LiteralString),
                    // Double-quoted string
                    computed_string
                        .clone()
                        .spanned()
                        .map(CmdExternalPath::ComputedString),
                ))
                .critical("expected a valid command name after the external marker '^'"),
            )
            .map(CmdPath::External),
        //
        // Methods
        //
        char('.')
            .ignore_then(ident.spanned())
            .map(CmdPath::Method)
            .not_followed_by(filter(|c| {
                !c.is_whitespace() && !DELIMITER_CHARS.contains(&c)
            })),
        //
        // Raw command paths
        //
        raw_cmd_path.spanned().map(CmdPath::Raw),
    ));

    let cmd_value_making_arg = choice::<CmdValueMakingArg, _>((
        // Literal values
        literal_value
            // Disambiguation: literal values should be followed by either
            // a space, a newline, a delimiter character or the end of the program
            // Otherwise this means we're in a raw argument with remaining symbols
            .followed_by(silent_choice((
                whitespaces().at_least_one(), /* includes newlines */
                filter(|c| DELIMITER_CHARS.contains(&c)),
                end(),
            )))
            .map(CmdValueMakingArg::LiteralValue),
        // Variable
        char('$')
            .ignore_then(ident)
            .spanned()
            .followed_by(silent_choice((
                filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
                end(),
            )))
            .map(CmdValueMakingArg::Variable),
        // Computed strings
        computed_string
            .clone()
            .map(CmdValueMakingArg::ComputedString),
        // Command output captures
        cmd_capture.map(CmdValueMakingArg::CmdCapturedOutput),
        // Parenthesis-wrapped expressions
        char('(')
            .ignore_then(
                expr.clone()
                    .spanned()
                    .padded_by(msnl)
                    .critical("expected an expression"),
            )
            .then_ignore(char(')').critical_auto_msg())
            .map(CmdValueMakingArg::ParenExpr),
        // Inline command call
        inline_cmd_call.map(CmdValueMakingArg::InlineCmdCall),
        // Lambdas
        lambda.clone().spanned().map(CmdValueMakingArg::Lambda),
        // Raw argument (but not flags, which aren't value making arguments)
        cmd_raw_string
            .spanned()
            // Avoid parsing e.g. 'err>' as 'err' and then making the parser fail because of the unexpected '>'
            .not_followed_by(silent_choice((char('<'), char('>'))))
            .map(CmdValueMakingArg::CmdRawString),
    ));

    let cmd_env_var = ident
        .spanned()
        .then_ignore(char('='))
        .then(
            cmd_value_making_arg
                .clone()
                .spanned()
                .critical("expected a value for the provided environment variable"),
        )
        .followed_by(
            s.critical("expected another assignment or the command call after the assignment"),
        )
        .map(|(name, value)| CmdEnvVar { name, value });

    let cmd_flag_name_arg = choice::<CmdFlagNameArg, _>((
        just("--")
            .ignore_then(
                first_ident_char
                    .then(filter(|c| c == '_' || c == '-' || c.is_alphanumeric()).repeated())
                    .collect_string(),
            )
            .map(CmdFlagNameArg::Long),
        char('-')
            .ignore_then(possible_ident_char)
            .map(CmdFlagNameArg::Short),
    ))
    .followed_by(silent_choice((
        filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
        char('='),
        end(),
    )));

    cmd_flag_arg.define(
        cmd_flag_name_arg
            .spanned()
            .then(
                choice((
                    s
                        // Flags must be excluded, otherwise '-a -b'
                        // would have '-b' considered as the value for '-a'
                        .not_followed_by(char('-'))
                        // Same thing goes for rest arguments
                        .not_followed_by(just("...").then(char('$').or(char('('))))
                        .to(FlagValueSeparator::Space),
                    char('=').to(FlagValueSeparator::Equal),
                ))
                .then(cmd_value_making_arg.clone().spanned())
                .map(|(value_sep, value)| CmdFlagValueArg { value, value_sep })
                .or_not(),
            )
            .map(|(name, value)| CmdFlagArg { name, value }),
    );

    let cmd_arg = choice::<CmdArg, _>((
        // Flag arguments
        cmd_flag_arg.map(CmdArg::Flag),
        // Spread
        spread_value.spanned().map(CmdArg::Spread),
        // Value-making
        cmd_value_making_arg.spanned().map(CmdArg::ValueMaking),
    ));

    let single_cmd_call = cmd_env_var
        .spanned()
        .then_ignore(s.critical("expected a space after the variable's value"))
        .repeated_into_vec()
        .spanned()
        .then(cmd_path.spanned().followed_by(silent_choice((
            end(),
            filter(|c| {
                c.is_whitespace() || c == ';' || c == '|' || c == '#' || c == ')' || c == '}'
            }),
        ))))
        .then(
            s.ignore_then(
                char('\\')
                    .then(ms)
                    .then(newline())
                    .then(
                        s.critical("expected at least one space for identation after the newline"),
                    )
                    .or_not(),
            )
            .ignore_then(cmd_arg.spanned())
            .repeated_into_vec()
            .spanned(),
        )
        .then(
            s.ignore_then(
                choice::<CmdRedirects, _>((
                    // Redirect both STDOUT and STDERR to a file
                    silent_choice((just("out+err>"), just("err+out>")))
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .map(CmdRedirects::StdoutAndStderrToFile),
                    // Redirect STDOUT to a file and STDERR to another
                    just(">")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .then_ignore(s)
                        .then_ignore(just("err>"))
                        .then_ignore(s)
                        .then(cmd_raw_string.spanned())
                        .map(|(path_for_stdout, path_for_stderr)| {
                            CmdRedirects::StdoutToFileAndStderrToFile {
                                path_for_stdout,
                                path_for_stderr,
                            }
                        }),
                    just("err>")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .then_ignore(s)
                        .then_ignore(just(">"))
                        .then_ignore(s)
                        .then(cmd_raw_string.spanned())
                        .map(|(path_for_stderr, path_for_stdout)| {
                            CmdRedirects::StdoutToFileAndStderrToFile {
                                path_for_stdout,
                                path_for_stderr,
                            }
                        }),
                    // Redirect STDERR to a file
                    just("err>")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .map(CmdRedirects::StdoutToFile),
                    // Redirect STDERR to STDOUT
                    just("err>").map(|_| CmdRedirects::StderrToStdout),
                    // Redirect STDOUT to SDTERR
                    just(">err").map(|_| CmdRedirects::StdoutToStderr),
                    // Redirect STDOUT to a file
                    just(">")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .map(CmdRedirects::StdoutToFile),
                ))
                .spanned(),
            )
            .or_not(),
        )
        .map(|(((env_vars, path), args), redirects)| SingleCmdCall {
            env_vars,
            path,
            args,
            redirects,
        });

    let cmd_call_base = choice::<CmdCallBase, _>((
        //
        // Expressions
        //
        expr.clone()
            // Disambiguation: expressions, after optional spaces, should be followed by either
            // newlines, delimiter characters or the end of the program
            // Otherwise this means we're in a command
            .followed_by(ms.then(silent_choice((
                end(),
                filter(|c| c == '\n' || c == '\r' || DELIMITER_CHARS.contains(&c)),
            ))))
            .map(Box::new)
            .spanned()
            .map(CmdCallBase::Expr),
        //
        // Normal command call
        //
        single_cmd_call
            .clone()
            .spanned()
            .map(CmdCallBase::SingleCmdCall),
    ));

    cmd_call.define(
        cmd_call_base
            .then(
                msnl.ignore_then(
                    choice::<CmdPipeType, _>((
                        just("^|").to(CmdPipeType::Stderr),
                        char('|').to(CmdPipeType::ValueOrStdout),
                    ))
                    .spanned(),
                )
                .then_ignore(msnl)
                .then(
                    single_cmd_call
                        .clone()
                        .spanned()
                        .critical("expected a command call after the pipe"),
                )
                .map(|(pipe_type, cmd)| CmdPipe { cmd, pipe_type })
                .repeated_into_vec(),
            )
            .map(|(base, pipes)| CmdCall { base, pipes }),
    );

    let scope_id_gen_bis = scope_id_gen.clone();

    let single_var_decl = just("mut")
        .to(())
        .not_followed_by(possible_ident_char)
        .then_ignore(s.critical_auto_msg())
        .spanned()
        .or_not()
        .then(ident.spanned())
        .then(
            ms.ignore_then(char(':'))
                .ignore_then(ms)
                .ignore_then(value_type.clone())
                .or_not(),
        )
        .map(|((is_mut, name), enforced_type)| SingleVarDecl {
            name,
            is_mut: is_mut.is_some(),
            enforced_type,
        });

    let var_decl_type = recursive(|var_decl_type| {
        let obj_destructuring_item_binding = choice::<ObjPropSpreadingBinding, _>((
            just("mut")
                .then(s)
                .or_not()
                .then(ident.spanned())
                .map(|(is_mut, alias)| ObjPropSpreadingBinding::BindTo {
                    is_mut: is_mut.is_some(),
                    alias,
                }),
            var_decl_type
                .clone()
                .spanned()
                .map(Box::new)
                .map(ObjPropSpreadingBinding::Deconstruct),
        ))
        .critical("expected a sub-declaration");

        let obj_destructuring_item_type = choice::<ObjPropSpreadingType, _>((
            // mut ident
            just("mut")
                .ignore_then(ms)
                .ignore_then(ident.spanned())
                .map(|name| ObjPropSpreadingType::RawKeyToMut { name }),
            // ident: <...>
            ident
                .spanned()
                .then(
                    ms.ignore_then(char(':'))
                        .ignore_then(msnl)
                        .ignore_then(
                            obj_destructuring_item_binding
                                .clone()
                                .critical("expected a binding after ':'"),
                        )
                        .or_not(),
                )
                .map(|(name, binding)| ObjPropSpreadingType::RawKeyToConst { name, binding }),
            // 'ident': <...>
            literal_string
                .spanned()
                .then_ignore(ms)
                .then_ignore(char(':'))
                .then_ignore(msnl)
                .then(obj_destructuring_item_binding)
                .map(
                    |(literal_name, binding)| ObjPropSpreadingType::LiteralKeyToConst {
                        literal_name,
                        binding,
                    },
                ),
        ));

        choice::<VarSpreading, _>((
            //
            // Lists
            //
            char('[')
                .ignore_then(msnl)
                .ignore_then(
                    var_decl_type
                        .clone()
                        .spanned()
                        .separated_by_into_vec(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char(']'))
                .map(VarSpreading::Tuple),
            //
            // Maps and structs
            //
            char('{')
                .ignore_then(msnl)
                .ignore_then(
                    obj_destructuring_item_type
                        .then(
                            msnl.ignore_then(char('='))
                                .ignore_then(msnl)
                                .ignore_then(expr.clone().critical("expected an expression"))
                                .or_not(),
                        )
                        .map(|(typ, default_value)| ObjPropSpreading { typ, default_value })
                        .separated_by_into_vec(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .map(VarSpreading::MapOrStruct),
            //
            // Single variables
            //
            single_var_decl.map(VarSpreading::Single),
        ))
    });

    let range_bound = choice::<RangeBound, _>((
        int_literal.map(RangeBound::Literal),
        var_name.spanned().map(RangeBound::Variable),
        char('(')
            .ignore_then(expr.clone().critical("expected an expression"))
            .then_ignore(char(')').critical_auto_msg())
            .spanned()
            .map(RangeBound::Expr),
    ));

    let instr = choice::<Instruction, _>((
        //
        // Variables declaration
        //
        just("let")
            .then_ignore(s)
            .ignore_then(
                var_decl_type
                    .spanned()
                    .critical("expected a valid variable declaration"),
            )
            .then(
                ms.ignore_then(char('=').critical_auto_msg())
                    .ignore_then(msnl)
                    .ignore_then(
                        expr.clone()
                            .spanned()
                            .critical("expected an expression to assign"),
                    ),
            )
            .map(|(names, init_expr)| Instruction::DeclareVar { names, init_expr }),
        //
        // Variables assignment
        //
        var_name
            .spanned()
            .then(prop_access_nature.clone().spanned().repeated_into_vec())
            .then(just("[]").to(()).spanned().or_not())
            .then_ignore(ms)
            .then_ignore(char('='))
            // Distinguish from '$someVariable == ...' expressions
            .not_followed_by(char('='))
            .then_ignore(msnl)
            .then(
                expr.clone()
                    .spanned()
                    .critical("expected an expression to assign"),
            )
            .map(
                |(((name, prop_acc), list_push), expr)| Instruction::AssignVar {
                    name,
                    prop_acc,
                    list_push,
                    expr,
                },
            ),
        //
        // Conditionals
        //
        just("if")
            .ignore_then(s)
            .ignore_then(expr.clone().spanned().critical("expected a condition"))
            .then_ignore(ms)
            .then(
                block
                    .clone()
                    .spanned()
                    .critical("expected a body for the condition"),
            )
            .then(
                msnl.ignore_then(just("else"))
                    .ignore_then(s)
                    .ignore_then(just("if"))
                    .ignore_then(s)
                    .ignore_then(
                        expr.clone()
                            .spanned()
                            .critical("expected a condition for the 'else if' statement"),
                    )
                    .then_ignore(ms)
                    .then(block.clone().spanned())
                    .map(|(cond, body)| ElsIf { cond, body })
                    .spanned()
                    .repeated_into_vec()
                    .then(
                        msnl.ignore_then(just("else"))
                            .ignore_then(ms)
                            .ignore_then(
                                block
                                    .clone()
                                    .spanned()
                                    .critical("expected a body for the 'else' block"),
                            )
                            .or_not(),
                    ),
            )
            .map(|((cond, body), (elsif, els))| Instruction::IfCond {
                cond,
                body,
                elsif,
                els,
            }),
        //
        // ranged 'for' loops
        //
        just("for")
            .ignore_then(s)
            .ignore_then(ident.spanned())
            .then_ignore(s)
            .then_ignore(just("in"))
            .then_ignore(s)
            .then(range_bound.clone().spanned())
            .then_ignore(just(".."))
            .then(char('=').or_not().map(|c| c.is_some()))
            .then(range_bound.spanned().critical("expected a range end value"))
            .then_ignore(ms)
            .then(
                block
                    .clone()
                    .spanned()
                    .critical("expected a body for the 'for' loop"),
            )
            .map(|((((iter_var, iter_from), inclusive), iter_to), body)| {
                Instruction::ForLoopRanged {
                    iter_var,
                    iter_from,
                    iter_to,
                    inclusive,
                    body,
                }
            }),
        //
        // 'for' loops
        //
        just("for")
            .ignore_then(s)
            .ignore_then(ident.spanned())
            .then_ignore(s)
            .then_ignore(just("in"))
            .then_ignore(s)
            .then(
                expr.clone()
                    .spanned()
                    .critical("expected an expression to iterate on"),
            )
            .then_ignore(ms)
            .then(
                block
                    .clone()
                    .spanned()
                    .critical("expected a body for the 'for' loop"),
            )
            .map(|((iter_var, iter_on), body)| Instruction::ForLoop {
                iter_var,
                iter_on,
                body,
            }),
        //
        // keyed 'for' loops
        //
        just("for")
            .ignore_then(s)
            .ignore_then(ident.spanned())
            .then_ignore(ms)
            .then_ignore(char(','))
            .then_ignore(ms)
            .then(ident.spanned())
            .then_ignore(s)
            .then_ignore(just("in"))
            .then_ignore(s)
            .then(
                expr.clone()
                    .spanned()
                    .critical("expected an expression to iterate on"),
            )
            .then_ignore(ms)
            .then(
                block
                    .clone()
                    .spanned()
                    .critical("expected a body for the 'for' loop"),
            )
            .map(
                |(((key_iter_var, value_iter_var), iter_on), body)| Instruction::ForLoopKeyed {
                    key_iter_var,
                    value_iter_var,
                    iter_on,
                    body,
                },
            ),
        //
        // 'while' loop
        //
        just("while")
            .ignore_then(s)
            .ignore_then(expr.clone().spanned())
            .then_ignore(ms)
            .then(
                block
                    .clone()
                    .spanned()
                    .critical("expected a body for the 'while' loop"),
            )
            .map(|(cond, body)| Instruction::WhileLoop { cond, body }),
        //
        // Loop continuation keyword
        //
        just("continue")
            .followed_by(silent_choice((
                filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
                end(),
            )))
            .map(|_| Instruction::LoopContinue),
        //
        // Loop breakage
        //
        just("break")
            .followed_by(silent_choice((
                filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
                end(),
            )))
            .map(|_| Instruction::LoopBreak),
        //
        // Matching
        //
        just("match")
            .ignore_then(s)
            .ignore_then(
                expr.clone()
                    .spanned()
                    .critical("expected an expression to match on"),
            )
            .then_ignore(msnl)
            .then_ignore(char('{').critical_auto_msg())
            .then(
                msnl.ignore_then(just("case"))
                    .ignore_then(ms)
                    .ignore_then(
                        expr.clone()
                            .spanned()
                            .critical("expected an expression to match"),
                    )
                    .then_ignore(ms)
                    .then(block.clone().spanned().critical("expected a block"))
                    .map(|(matches, body)| MatchCase { matches, body })
                    .repeated_into_vec(),
            )
            .then(
                msnl.ignore_then(just("else"))
                    .ignore_then(ms)
                    .ignore_then(block.clone().spanned().critical("expected a block"))
                    .or_not(),
            )
            .then_ignore(msnl)
            .then_ignore(char('}').critical_auto_msg())
            .map(|((expr, cases), els)| Instruction::Match { expr, cases, els }),
        //
        // Type matching
        //
        just("typematch")
            .ignore_then(s)
            .ignore_then(
                expr.clone()
                    .spanned()
                    .critical("expected an expression to match on"),
            )
            .then_ignore(msnl)
            .then_ignore(char('{').critical_auto_msg())
            .then(
                msnl.ignore_then(just("case"))
                    .ignore_then(ms)
                    .ignore_then(value_type.clone().critical("expected a type to match"))
                    .then_ignore(ms)
                    .then(block.clone().spanned().critical("expected a block"))
                    .map(|(matches, body)| TypeMatchCase { matches, body })
                    .repeated_into_vec(),
            )
            .then(
                msnl.ignore_then(just("else"))
                    .ignore_then(ms)
                    .ignore_then(block.clone().spanned().critical("expected a block"))
                    .or_not(),
            )
            .then_ignore(msnl)
            .then_ignore(char('}').critical_auto_msg())
            .map(|((expr, cases), els)| Instruction::TypeMatch { expr, cases, els }),
        //
        // Function declaration
        //
        just("fn")
            .ignore_then(s)
            .ignore_then(
                ident
                    .spanned()
                    .critical("expected identifier as the function's name"),
            )
            .then_ignore(ms)
            .then(
                fn_signature
                    .critical("expected a list of arguments opened by a '('")
                    .spanned(),
            )
            .then_ignore(ms)
            .then(
                block
                    .clone()
                    .spanned()
                    .critical("expected a body for the function"),
            )
            .and_then(|((name, signature), body)| {
                let on_type = signature
                    .data
                    .args
                    .data
                    .first()
                    .and_then(|first_arg| -> Option<Result<ValueType, ParsingError>> {
                        match first_arg {
                            FnSignatureArg::Positional(arg) => {
                                let FnSignaturePositionalArg {
                                    name,
                                    is_optional,
                                    typ,
                                } = arg;

                                let name_at = name.at.parsed_range().unwrap();

                                if name.data != "self" {
                                    None
                                } else if *is_optional {
                                    Some(Err(ParsingError::custom(name_at, "").criticalize(
                                        "'self' argument cannot be optional in methods".into(),
                                    )))
                                } else {
                                    Some(typ.clone().ok_or_else(|| {
                                        ParsingError::custom(name_at, "").criticalize(
                                            "'self' argument must have a specified type".into(),
                                        )
                                    }))
                                }
                            }

                            _ => None,
                        }
                    })
                    .transpose()?;

                Ok(match on_type {
                    Some(on_type) => Instruction::MethodDecl {
                        name,
                        on_type,
                        content: Function { signature, body },
                    },

                    None => Instruction::FnDecl {
                        name,
                        content: Function { signature, body },
                    },
                })
            }),
        //
        // Function return
        //
        just("return")
            .ignore_then(s.ignore_then(expr.clone().spanned()).or_not())
            .map(|expr| Instruction::FnReturn { expr }),
        //
        // Throws
        //
        just("throw")
            .ignore_then(s)
            .ignore_then(
                expr.clone()
                    .spanned()
                    .critical("expected an expression to throw"),
            )
            .map(Instruction::Throw),
        //
        // Try/Catch
        //
        just("try")
            .ignore_then(char('{').padded_by(msnl))
            .ignore_then(expr.clone().spanned().critical("expected an expression"))
            .then_ignore(char('}').padded_by(msnl))
            .then_ignore(just("catch").critical_auto_msg())
            .then_ignore(s.critical_auto_msg())
            .then(
                ident
                    .spanned()
                    .critical("expected a variable to catch the throw value in"),
            )
            .then_ignore(s.critical_auto_msg())
            .then(block.clone().spanned().critical("expected a block"))
            .map(
                move |((try_expr, catch_var), catch_body)| Instruction::Try {
                    try_expr,
                    catch_var,
                    catch_body,
                },
            ),
        //
        // Aliases declaration
        //
        just("alias")
            .ignore_then(s)
            .ignore_then(
                ident
                    .spanned()
                    .critical("expected an alias name (identifier)"),
            )
            .then_ignore(ms)
            .then_ignore(char('=').critical_auto_msg())
            .then_ignore(ms)
            .then(
                single_cmd_call
                    .clone()
                    .spanned()
                    .critical("expected a command call to alias"),
            )
            .map(move |(name, content)| Instruction::CmdAliasDecl {
                name,
                content_scope_id: scope_id_gen_bis.next(),
                content,
            }),
        //
        // Type aliases
        //
        just("type")
            .ignore_then(s)
            .ignore_then(
                ident
                    .spanned()
                    .critical("expected a type name (identifier)"),
            )
            .then_ignore(ms)
            .then_ignore(char('=').critical_auto_msg())
            .then_ignore(ms)
            .then(value_type.spanned().critical("expected a type to alias"))
            .map(|(name, content)| Instruction::TypeAliasDecl { name, content }),
        //
        // Base blocks
        //
        just("do")
            .ignore_then(s)
            .ignore_then(block.spanned().critical("expected a block"))
            .map(Instruction::DoBlock),
        //
        // Include statement
        //
        just("include")
            .ignore_then(s)
            .ignore_then(
                literal_string
                    .spanned()
                    .critical("expected a file path")
                    .and_then_or_critical(move |path| load_file(path.data, path.at.start.file_id).map_err(Cow::from))
                    .and_then(move |file| {
                        program_bis
                            .parse_str_with_file_id(&file.content, FileId::SourceFile(file.id))
                    })
                    .map(|program| program.data),
            )
            .map(Instruction::Include),
        //
        // Command calls
        //
        cmd_call.spanned().map(Instruction::CmdCall),
    ))
    .spanned()
    .then_ignore(ms)
    .then_ignore(
        silent_choice((
            lookahead(char('}')),
            lookahead(just("\r\n")),
            filter(|c| c == '\n' || c == ';'),
            end(),
        ))
        .critical("unexpected symbol"),
    );

    // Raw block
    raw_block.define(
        instr
            .padded_by(msnl)
            .repeated_into_vec()
            .map(move |instructions| Block {
                scope_id: scope_id_gen.next(),
                instructions,
            })
            .followed_by(
                msnl.then(silent_choice((end(), char('}'))))
                    .critical("expected an instruction"),
            ),
    );

    program.define(
        raw_block
            .spanned()
            .padded_by(msnl)
            .full()
            .critical("unexpected symbol")
            .map(|content| Program { content }),
    );

    program
}

pub static DELIMITER_CHARS: LazyLock<HashSet<char>> = LazyLock::new(|| {
    HashSet::from([
        '(', ')', '[', ']', '{', '}', '<', '>', ';', '|', '\'', '"', '`', '$', '#', '^',
    ])
});

// Usage: .debug(simple_debug) after any parser
#[allow(dead_code)]
fn simple_debug<T: std::fmt::Debug>(d: parsy::chainings::DebugType<'_, '_, T>) {
    println!("{d:#?}");
}
