use std::{
    collections::{HashMap, HashSet},
    sync::LazyLock,
};

use parsy::{
    atoms::{alphanumeric, digits},
    char, choice, empty, end, filter, just, late, lookahead, newline, not, recursive,
    silent_choice, whitespaces, FileId, Parser,
};

use crate::{
    ast::{
        Block, CmdArg, CmdCall, CmdCallBase, CmdEnvVar, CmdFlagArg, CmdFlagNameArg,
        CmdFlagValueArg, CmdPath, CmdPipe, CmdPipeType, CmdRawString, CmdRawStringPiece,
        CmdSpreadArg, CmdValueMakingArg, ComputedString, ComputedStringPiece, DoubleOp, ElsIf,
        ElsIfExpr, EscapableChar, Expr, ExprInner, ExprInnerChaining, ExprInnerContent,
        ExprInnerDirectChaining, ExprOp, FlagValueSeparator, FnArg, FnCall, FnCallArg,
        FnCallNature, FnFlagArgNames, FnNormalFlagArg, FnPositionalArg, FnPresenceFlagArg,
        FnRestArg, FnSignature, Function, Instruction, LiteralValue, MapDestructBinding, Program,
        PropAccess, PropAccessNature, RuntimeEaten, SingleCmdCall, SingleOp, SingleValueType,
        SingleVarDecl, StructTypeMember, SwitchCase, SwitchExprCase, Value, ValueType,
        VarDeconstruction,
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
    let program = late::<Program>();
    let program_bis = program.clone();

    let comment = char('#').then(filter(|c| c != '\r' && c != '\n').repeated());

    let ms = silent_choice((
        comment,
        filter(|c| c.is_whitespace() && c != '\r' && c != '\n'),
    ))
    .repeated();

    let msnl = silent_choice((comment, filter(|c| c.is_whitespace()))).repeated();

    let scope_id_gen = ScopeIdGenerator::new();

    let raw_block = recursive::<Block, _>(move |raw_block| {
        let s = whitespaces().no_newline().at_least_one();

        let block = char('{')
            .critical_with_no_message()
            .ignore_then(msnl)
            .ignore_then(raw_block.clone())
            .then_ignore(msnl)
            .then_ignore(char('}').critical_with_no_message());

        let possible_ident_char = choice((char('_'), alphanumeric()));

        let first_ident_char = filter(|c| c == '_' || c.is_alphabetic());

        let ident = first_ident_char
            .then(filter(|c| c == '_' || c.is_alphanumeric()).repeated())
            .collect_string();

        let var_name = char('$').ignore_then(ident);

        let cmd_call = late::<CmdCall>();
        let expr = late::<Expr>();

        let cmd_flag_arg = late::<CmdFlagArg>();

        let fn_signature = late::<FnSignature>();

        let value_type = late::<ValueType>();

        let single_value_type = choice::<_, SingleValueType>((
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
            just("list")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::List),
            just("map")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Map),
            just("error")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Error),
            just("cmdcall")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::CmdCall),
            just("fn")
                .ignore_then(fn_signature.clone())
                .spanned()
                .map(RuntimeEaten::Parsed)
                .map(SingleValueType::Function),
            just("struct")
                .ignore_then(ms)
                .ignore_then(char('{'))
                .ignore_then(msnl)
                .ignore_then(
                    ident
                        .spanned()
                        .map(RuntimeEaten::Parsed)
                        .then_ignore(ms)
                        .then_ignore(char(':').critical_with_no_message())
                        .then_ignore(msnl)
                        .then(
                            value_type
                                .clone()
                                .spanned()
                                .map(RuntimeEaten::Parsed)
                                .critical("expected a value type"),
                        )
                        .map(|(name, typ)| StructTypeMember { name, typ })
                        .spanned()
                        .map(RuntimeEaten::Parsed)
                        .separated_by(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_with_no_message())
                .map(SingleValueType::TypedStruct),
            just("struct")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::UntypedStruct),
            ident.spanned().map(SingleValueType::TypeAlias),
        ));

        value_type.finish(choice((
            // Single type
            single_value_type
                .clone()
                .spanned()
                .map(RuntimeEaten::Parsed)
                .map(ValueType::Single),
            // Type union
            char('(')
                .ignore_then(msnl)
                .ignore_then(
                    single_value_type
                        .clone()
                        .spanned()
                        .map(RuntimeEaten::Parsed)
                        .separated_by(char('|').padded_by(msnl))
                        .at_least(2)
                        .critical("expected at least 2 types in union"),
                )
                .then_ignore(msnl)
                .then_ignore(char(')').critical("expected a closing parenthesis for type union"))
                .map(ValueType::Union),
        )));

        let fn_arg_long_flag = just("--")
            .ignore_then(
                first_ident_char
                    .then(filter(|c| c == '_' || c == '-' || c.is_alphanumeric()).repeated())
                    .collect_string()
                    .spanned()
                    .critical("expected a flag name (identifier)"),
            )
            .followed_by(not(possible_ident_char).critical("unexpected symbol"));

        let fn_arg_short_flag = char('-')
            .ignore_then(
                first_ident_char
                    .spanned()
                    .critical("expected a single-character identifier"),
            )
            .followed_by(
                not(possible_ident_char).critical("expected a single-character identifier"),
            );

        let fn_flag_arg_names = choice::<_, FnFlagArgNames>((
            // Long *and* short flags
            fn_arg_long_flag
                .map(RuntimeEaten::Parsed)
                .then_ignore(ms)
                .then_ignore(char('('))
                .then(fn_arg_short_flag.map(RuntimeEaten::Parsed))
                .then_ignore(char(')').critical("unclosed short argument, expected ')'"))
                .map(|(long, short)| FnFlagArgNames::LongAndShortFlag { short, long }),
            // Long flag only
            fn_arg_long_flag
                .map(RuntimeEaten::Parsed)
                .map(FnFlagArgNames::LongFlag),
            // Long flag only
            fn_arg_short_flag
                .map(RuntimeEaten::Parsed)
                .map(FnFlagArgNames::ShortFlag),
        ));

        let fn_arg = choice::<_, FnArg>((
            // Positional
            ident
                .spanned()
                .map(RuntimeEaten::Parsed)
                .then_ignore(msnl)
                .then(char('?').or_not())
                .then(
                    char(':')
                        .ignore_then(msnl)
                        .ignore_then(
                            value_type
                                .clone()
                                .critical("expected a type for the argument")
                                .spanned()
                                .map(RuntimeEaten::Parsed),
                        )
                        .or_not(),
                )
                .map(|((name, is_optional), typ)| {
                    FnArg::Positional(FnPositionalArg {
                        name,
                        is_optional: is_optional.is_some(),
                        typ,
                    })
                }),
            // Normal flags
            fn_flag_arg_names
                .then_ignore(msnl)
                .then(char('?').or_not())
                .then_ignore(char(':'))
                .then_ignore(msnl)
                .then(
                    value_type
                        .clone()
                        .critical("expected a type for the flag argument")
                        .spanned()
                        .map(RuntimeEaten::Parsed),
                )
                .map(|((names, is_optional), typ)| {
                    FnArg::NormalFlag(FnNormalFlagArg {
                        names,
                        is_optional: is_optional.is_some(),
                        typ,
                    })
                }),
            // Presence flags
            fn_flag_arg_names
                .then_ignore(msnl)
                .then_ignore(
                    char('?').critical("expected either a '?' or a ':' marker after flag argument"),
                )
                .map(|names| FnArg::PresenceFlag(FnPresenceFlagArg { names })),
            // Rest
            just("...")
                .ignore_then(ident)
                .spanned()
                .map(RuntimeEaten::Parsed)
                .map(|name| FnArg::Rest(FnRestArg { name })),
        ));

        fn_signature.finish(
            char('(')
                .ignore_then(
                    fn_arg
                        .clone()
                        .separated_by(char(',').padded_by(msnl))
                        .spanned()
                        .map(RuntimeEaten::Parsed),
                )
                .then_ignore(msnl)
                .then_ignore(char(')').critical_with_no_message())
                .then(
                    msnl.ignore_then(just("->"))
                        .ignore_then(ms)
                        .ignore_then(
                            value_type
                                .clone()
                                .map(Box::new)
                                .spanned()
                                .map(RuntimeEaten::Parsed)
                                .critical("expected a type"),
                        )
                        .then_ignore(ms)
                        .or_not(),
                )
                .map(|(args, ret_type)| FnSignature { args, ret_type }),
        );

        let fn_call_arg = choice::<_, FnCallArg>((
            ident
                .spanned()
                .then_ignore(ms)
                .then_ignore(char(':'))
                .then_ignore(msnl)
                .then(expr.clone().spanned())
                .map(|(name, value)| FnCallArg::Flag {
                    name: name.map(|name| {
                        if name.chars().nth(1).is_some() {
                            CmdFlagNameArg::LongNoConvert(name)
                        } else {
                            CmdFlagNameArg::Short(name.chars().next().unwrap())
                        }
                    }),
                    value,
                }),
            expr.clone().spanned().map(FnCallArg::Expr),
        ));

        let fn_call = choice::<_, FnCallNature>((
            char('$').to(FnCallNature::Variable),
            char('.').to(FnCallNature::Method),
            empty().to(FnCallNature::NamedFunction),
        ))
        .then(ident.spanned())
        .then_ignore(char('('))
        .then(
            fn_call_arg
                .spanned()
                .padded_by(msnl)
                .separated_by(char(','))
                .spanned(),
        )
        .then_ignore(char(')').critical("expected an expression"))
        .map(|((nature, name), call_args)| FnCall {
            nature,
            name,
            call_args,
        });

        let escapable_char = char('\\').ignore_then(
            choice((
                char('n').to(EscapableChar::Newline),
                char('r').to(EscapableChar::CarriageReturn),
                char('"').to(EscapableChar::DoubleQuote),
                char('\'').to(EscapableChar::BackQuote),
                char('`').to(EscapableChar::BackQuote),
                char('$').to(EscapableChar::DollarSign),
                char('\\').to(EscapableChar::Backslash),
            ))
            .critical("this character is not escapable"),
        );

        let literal_string = char('\'')
            .ignore_then(
                choice((
                    escapable_char.map(EscapableChar::original_char),
                    filter(|c| c != '\''),
                ))
                .repeated_custom::<String>(),
            )
            .then_ignore(char('\''));

        let literal_value = choice::<_, LiteralValue>((
            // Strings
            literal_string.map(LiteralValue::String),
            // Booleans
            just("true")
                .not_followed_by(possible_ident_char)
                .map(|_| LiteralValue::Boolean(true)),
            just("false")
                .not_followed_by(possible_ident_char)
                .map(|_| LiteralValue::Boolean(false)),
            // Floats
            char('-')
                .or_not()
                .then(
                    digits(10)
                        .collect_string()
                        .validate(|str| !(str.starts_with('0') && str.len() > 1))
                        .with_custom_msg("Leading zeroes are not allowed")
                        .as_critical(),
                )
                .then(char('.'))
                .then(
                    digits(10)
                        .collect_string()
                        .validate(|str| !(str.ends_with('0') && str.len() > 1))
                        .with_custom_msg("Trailing zeroes are not allowed")
                        .as_critical(),
                )
                .not_followed_by(possible_ident_char)
                .collect_string()
                .map(|num| LiteralValue::Float(str::parse::<f64>(&num).unwrap())),
            // Integers
            char('-')
                .or_not()
                .then(
                    digits(10)
                        .collect_string()
                        .validate(|str| !(str.starts_with('0') && str.len() > 1))
                        .with_custom_msg("Leading zeroes are not allowed")
                        .as_critical(),
                )
                .not_followed_by(possible_ident_char)
                .collect_string()
                .map(|num| LiteralValue::Integer(str::parse::<i64>(&num).unwrap())),
        ))
        .followed_by(silent_choice((
            filter(|c| c.is_whitespace() || c == ',' || DELIMITER_CHARS.contains(&c)),
            end(),
        )));

        let computed_string = char('"')
            .ignore_then(
                choice::<_, ComputedStringPiece>((
                    // Escaped
                    escapable_char.map(ComputedStringPiece::Escaped),
                    // Command calls
                    just("$(")
                        .ignore_then(
                            cmd_call
                                .clone()
                                .spanned()
                                .padded_by(msnl)
                                .critical("expected a command call"),
                        )
                        .then_ignore(char(')').critical_with_no_message())
                        .map(ComputedStringPiece::CmdCall),
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
                        .then_ignore(char('`').critical_with_no_message())
                        .map(ComputedStringPiece::Expr),
                    // Literal character suites
                    filter(|c| c != '"' && c != '$' && c != '`' && c != '\\')
                        .repeated()
                        .at_least(1)
                        .collect_string()
                        .map(ComputedStringPiece::Literal),
                ))
                .spanned()
                .repeated_vec(),
            )
            .then_ignore(char('"').critical_with_no_message())
            .map(|pieces| ComputedString { pieces });

        let lambda = char('{')
            .ignore_then(ms)
            .ignore_then(
                (not(just("->")) // to avoid parsing short flag with '>' name
                    .ignore_then(fn_arg))
                .separated_by(char(',').padded_by(msnl))
                .spanned()
                .map(RuntimeEaten::Parsed)
                .map(|args| FnSignature {
                    args,
                    ret_type: None,
                })
                .spanned(),
            )
            .then_ignore(ms)
            .then_ignore(just("->").critical_with_no_message())
            .then_ignore(ms)
            .then(
                raw_block
                    .clone()
                    .critical("expected a body for the lambda")
                    .spanned(),
            )
            .then_ignore(ms)
            .then_ignore(char('}').critical_with_no_message())
            .map(|(signature, body)| Function { signature, body });

        let inline_cmd = just("@(")
            .ignore_then(msnl)
            .ignore_then(
                cmd_call
                    .clone()
                    .spanned()
                    .critical("expected a command call"),
            )
            .then_ignore(msnl)
            .then_ignore(char(')').critical_with_no_message());

        let value = choice::<_, Value>((
            just("null").map(|_| Value::Null),
            // Literals
            literal_value.spanned().map(Value::Literal),
            // Computed strings
            computed_string.clone().spanned().map(Value::ComputedString),
            // Lists
            char('[')
                .ignore_then(msnl)
                .ignore_then(
                    expr.clone()
                        .spanned()
                        .separated_by(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char(']').critical("expected a closing bracket ']' for the list"))
                .map(Value::List),
            just("struct {")
                .ignore_then(msnl)
                .ignore_then(
                    ident
                        .spanned()
                        .then_ignore(ms)
                        .then_ignore(char(':'))
                        .then_ignore(msnl)
                        .then(expr.clone().spanned().critical("expected an expression"))
                        .separated_by(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char('}'))
                .map(|members| {
                    Value::Struct(
                        members
                            .into_iter()
                            .map(|(name, expr)| (name.data, expr))
                            .collect::<HashMap<_, _>>(),
                    )
                }),
            // Function calls
            fn_call.clone().spanned().map(Value::FnCall),
            // Command calls
            just("$(")
                .ignore_then(cmd_call.clone().spanned())
                .then_ignore(char(')').critical("mising close parenthesis for command call"))
                .map(Value::CmdOutput),
            // Variables
            var_name.spanned().map(Value::Variable),
            // Commands
            inline_cmd.clone().map(Value::CmdCall),
            // Function as value
            char('@')
                .ignore_then(ident.critical("expected a function name"))
                .spanned()
                .map(Value::FnAsValue),
            // Lambdas
            lambda.clone().map(Value::Lambda),
        ));

        let single_op = choice::<_, SingleOp>((char('!').to(SingleOp::Neg),));

        let double_op = not(just("->")).ignore_then(choice::<_, DoubleOp>((
            char('+').to(DoubleOp::Add),
            char('-').to(DoubleOp::Sub),
            char('*').to(DoubleOp::Mul),
            char('/').to(DoubleOp::Div),
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
            .critical_with_no_message()
            .ignore_then(msnl)
            .ignore_then(expr.clone().map(Box::new).spanned())
            .then_ignore(msnl)
            .then_ignore(char('}').critical_with_no_message());

        let expr_inner_direct_chaining = late::<ExprInnerDirectChaining>();

        let scope_id_gen_bis = scope_id_gen.clone();

        let expr_inner_content = recursive(|expr_inner_content| {
            choice::<_, ExprInnerContent>((
                // Single operator (e.g. '!')
                single_op
                    .spanned()
                    .then_ignore(ms)
                    .then(expr_inner_content.map(Box::new).spanned())
                    .then(expr_inner_direct_chaining.clone().spanned().repeated_vec())
                    .map(
                        |((op, right), right_chainings)| ExprInnerContent::SingleOp {
                            op,
                            right,
                            right_chainings,
                        },
                    ),
                // Parenthesis-wrapped expression
                char('(')
                    .ignore_then(expr.clone().map(Box::new).spanned())
                    .then_ignore(char(')'))
                    .map(ExprInnerContent::ParenExpr),
                // Ternaries
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
                            .spanned()
                            .repeated_vec()
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
                // Switch
                just("switch")
                    .ignore_then(s)
                    .ignore_then(
                        expr.clone()
                            .map(Box::new)
                            .spanned()
                            .critical("expected an expression to switch on"),
                    )
                    .then_ignore(msnl)
                    .then_ignore(char('{').critical_with_no_message())
                    .then(
                        msnl.ignore_then(just("case"))
                            .ignore_then(ms)
                            .ignore_then(
                                expr.clone()
                                    .spanned()
                                    .critical("expected an expression to match"),
                            )
                            .then_ignore(msnl)
                            .then_ignore(just("->").critical("expected an arrow (->)"))
                            .then_ignore(msnl)
                            .then(
                                expr.clone()
                                    .spanned()
                                    .critical("expected an expression to evaluate to"),
                            )
                            .map(|(matches, then)| SwitchExprCase { matches, then })
                            .repeated_vec(),
                    )
                    .then_ignore(msnl)
                    .then_ignore(just("else").critical("expected an 'else' fallback"))
                    .then_ignore(msnl)
                    .then_ignore(just("->").critical("expected an arrow (->)"))
                    .then_ignore(msnl)
                    .then(
                        expr.clone()
                            .map(Box::new)
                            .spanned()
                            .critical("expected an expression to evaluate to"),
                    )
                    .then_ignore(msnl)
                    .then_ignore(char('}').critical_with_no_message())
                    .map(|((expr, cases), els)| ExprInnerContent::Switch { expr, cases, els }),
                // Try / catch
                just("try")
                    .ignore_then(s)
                    .ignore_then(
                        fn_call
                            .clone()
                            .spanned()
                            .critical("expected a function call"),
                    )
                    .then_ignore(s.critical("expected 'catch' keyword after function call"))
                    .then_ignore(
                        just("catch").critical("expected 'catch' keyword after function call"),
                    )
                    .then_ignore(s.critical("expected a space"))
                    .then(ident.spanned().critical("expected a catch variable"))
                    .then_ignore(s.critical("expected a space"))
                    .then(
                        expr.clone()
                            .map(Box::new)
                            .spanned()
                            .critical("expected a catch expression"),
                    )
                    .map(
                        move |((fn_call, catch_var), catch_expr)| ExprInnerContent::Try {
                            fn_call,
                            catch_var,
                            catch_expr,
                            catch_expr_scope_id: scope_id_gen_bis.gen(),
                        },
                    ),
                // Throws
                just("throw")
                    .ignore_then(ms)
                    .ignore_then(
                        expr.clone()
                            .map(Box::new)
                            .spanned()
                            .critical("expected an expression to throw"),
                    )
                    .map(ExprInnerContent::Throw),
                // Simple values
                value.clone().spanned().map(ExprInnerContent::Value),
            ))
        });

        let prop_access_nature = choice::<_, PropAccessNature>((
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
                .then_ignore(char(']').critical("unclosed property access, expected ']'"))
                .map(PropAccessNature::Key),
        ));

        let prop_access = char('?')
            .or_not()
            .then(prop_access_nature.clone().spanned())
            .map(|(nullable, nature)| PropAccess {
                nullable: nullable.is_some(),
                nature,
            });

        expr_inner_direct_chaining.finish(choice::<_, ExprInnerDirectChaining>((
            lookahead(char('.'))
                .ignore_then(fn_call.clone())
                .spanned()
                .map(ExprInnerDirectChaining::MethodCall),
            prop_access
                .spanned()
                .map(ExprInnerDirectChaining::PropAccess),
        )));

        let expr_inner_chaining = choice::<_, ExprInnerChaining>((
            expr_inner_direct_chaining.map(ExprInnerChaining::Direct),
            char('|')
                .padded_by(msnl)
                .ignore_then(fn_call.clone())
                .spanned()
                .map(ExprInnerChaining::FnCall),
        ));

        let expr_inner = expr_inner_content
            .spanned()
            .then(expr_inner_chaining.spanned().repeated_vec())
            .map(|(content, chainings)| ExprInner { content, chainings });

        let expr_op = double_op
            .spanned()
            .then_ignore(msnl)
            .then(
                expr_inner
                    .clone()
                    .spanned()
                    .critical("expected an expression after operator")
                    .map(Box::new),
            )
            .map(|(op, with)| ExprOp { op, with });

        expr.finish(
            expr_inner
                .spanned()
                .then(ms.ignore_then(expr_op).repeated_vec())
                .map(|(inner, right_ops)| Expr { inner, right_ops }),
        );

        let cmd_raw_string = not(just("->")).ignore_then(
            choice::<_, CmdRawStringPiece>((
                // Escaped character
                char('\\')
                    .ignore_then(
                        filter(|c| c != '\n' && c != '\r')
                            .critical("expected a character to escape"),
                    )
                    .map(CmdRawStringPiece::Escaped),
                // Variables
                var_name.spanned().map(CmdRawStringPiece::Variable),
                // Literal character suites
                filter(|c| !c.is_whitespace() && c != '\\' && !DELIMITER_CHARS.contains(&c))
                    .repeated()
                    .at_least(1)
                    .collect_string()
                    .map(CmdRawStringPiece::Literal),
            ))
            .spanned()
            .repeated_vec()
            .at_least(1)
            .map(|pieces| CmdRawString { pieces }),
        );

        let cmd_path = choice::<_, CmdPath>((
            // Direct
            just("direct")
                .ignore_then(ms)
                .ignore_then(cmd_raw_string.critical("expected a command name").spanned())
                .map(CmdPath::Direct),
            // Method
            char('.').ignore_then(ident.spanned()).map(CmdPath::Method),
            // Computed string
            computed_string
                .clone()
                .spanned()
                .map(CmdPath::ComputedString),
            // Command computable string
            cmd_raw_string.spanned().map(CmdPath::CmdRawString),
        ));

        let cmd_value_making_arg = choice::<_, CmdValueMakingArg>((
            // Literal values
            literal_value.spanned().map(CmdValueMakingArg::LiteralValue),
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
                .spanned()
                .map(CmdValueMakingArg::ComputedString),
            // Command call
            just("$(")
                .ignore_then(
                    cmd_call
                        .clone()
                        .spanned()
                        .padded_by(msnl)
                        .critical("expected a command call"),
                )
                .then_ignore(
                    char(')').critical("unclosed command call (expected a closing parenthesis)"),
                )
                .map(CmdValueMakingArg::CmdOutput),
            // Parenthesis-wrapped expressions
            char('(')
                .ignore_then(
                    expr.clone()
                        .spanned()
                        .padded_by(msnl)
                        .critical("expected an expression"),
                )
                .then_ignore(char(')').critical("unclosed expression"))
                .map(CmdValueMakingArg::ParenExpr),
            // Inline command call
            inline_cmd.map(CmdValueMakingArg::InlineCmdCall),
            // Single-parameter lambdas
            just("{{")
                .ignore_then(msnl)
                .ignore_then(
                    raw_block
                        .clone()
                        .critical("expected a body for the lambda")
                        .spanned(),
                )
                .then_ignore(msnl)
                .then_ignore(just("}}"))
                .map(CmdValueMakingArg::SingleParamLambda),
            // Lambdas
            lambda.clone().spanned().map(CmdValueMakingArg::Lambda),
            // Raw argument (but not flags, which aren't value making arguments)
            cmd_raw_string
                .spanned()
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

        let cmd_flag_name_arg = choice::<_, CmdFlagNameArg>((
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

        cmd_flag_arg.finish(
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

        let cmd_arg = choice::<_, CmdArg>((
            // Flag arguments
            cmd_flag_arg.map(CmdArg::Flag),
            // Spread
            just("...")
                .ignore_then(
                    choice::<_, CmdSpreadArg>((
                        char('$')
                            .ignore_then(ident)
                            .spanned()
                            .map(CmdSpreadArg::Variable),
                        char('(')
                            .ignore_then(expr.clone())
                            .then_ignore(char(')'))
                            .map(CmdSpreadArg::Expr),
                    ))
                    .critical("expected a value to spread"),
                )
                .spanned()
                .map(CmdArg::Spread),
            // Value-making
            cmd_value_making_arg.map(CmdArg::ValueMaking),
        ));

        let single_cmd_call = cmd_env_var
            .spanned()
            .separated_by(s)
            .spanned()
            .then(cmd_path.spanned())
            .then(
                s.ignore_then(
                    char('\\')
                        .then(ms)
                        .then(newline())
                        .then(s.critical(
                            "expected at least one space for identation after the newline",
                        ))
                        .or_not(),
                )
                .ignore_then(cmd_arg.spanned())
                .repeated_vec()
                .spanned(),
            )
            .map(|((env_vars, path), args)| SingleCmdCall {
                env_vars,
                path,
                args,
            });

        let cmd_call_base = choice::<_, CmdCallBase>((
            just("expr")
                .ignore_then(s)
                .ignore_then(expr.clone().map(Box::new).spanned())
                .map(CmdCallBase::Expr),
            single_cmd_call
                .clone()
                .spanned()
                .map(CmdCallBase::SingleCmdCall),
        ));

        cmd_call.finish(
            cmd_call_base
                .then(
                    msnl.ignore_then(
                        choice::<_, CmdPipeType>((
                            just("!|").to(CmdPipeType::Stderr),
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
                    .repeated_vec(),
                )
                .map(|(base, pipes)| CmdCall { base, pipes }),
        );

        let scope_id_gen_bis = scope_id_gen.clone();

        let single_var_decl = just("mut")
            .to(())
            .not_followed_by(possible_ident_char)
            .then_ignore(s.critical("expected a whitespace after 'mut' keyword"))
            .spanned()
            .or_not()
            .then(ident.spanned())
            .map(|(is_mut, name)| SingleVarDecl { name, is_mut });

        let var_decl_type = recursive(|var_decl_type| {
            choice::<_, VarDeconstruction>((
                char('[')
                    .ignore_then(msnl)
                    .ignore_then(
                        var_decl_type
                            .clone()
                            .spanned()
                            .separated_by(char(',').padded_by(msnl)),
                    )
                    .then_ignore(msnl)
                    .then_ignore(char(']'))
                    .map(VarDeconstruction::Tuple),
                char('{')
                    .ignore_then(msnl)
                    .ignore_then(
                        single_var_decl
                            .spanned()
                            .then(
                                char(':')
                                    .ignore_then(msnl)
                                    .ignore_then(
                                        choice::<_, MapDestructBinding>((
                                            ident.spanned().map(MapDestructBinding::BindTo),
                                            var_decl_type
                                                .clone()
                                                .spanned()
                                                .map(Box::new)
                                                .map(MapDestructBinding::Destruct),
                                        ))
                                        .critical("expected a sub-declaration"),
                                    )
                                    .or_not(),
                            )
                            .separated_by(char(',').padded_by(msnl)),
                    )
                    .then_ignore(msnl)
                    .then_ignore(char('}'))
                    .map(VarDeconstruction::MapOrStruct),
                single_var_decl.map(VarDeconstruction::Single),
            ))
        });

        let instr = choice::<_, Instruction>((
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
                    ms.ignore_then(char('=').critical_with_no_message())
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
                .then(prop_access_nature.clone().spanned().repeated_vec())
                .then(just("[]").to(()).spanned().or_not())
                .then_ignore(ms)
                .then_ignore(char('='))
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
                        .repeated_vec()
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
                .map(|(((key_iter_var, value_iter_var), iter_on), body)| {
                    Instruction::ForLoopKeyed {
                        key_iter_var,
                        value_iter_var,
                        iter_on,
                        body,
                    }
                }),
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
            just("continue").map(|_| Instruction::LoopContinue),
            //
            // Loop breakage
            //
            just("break").map(|_| Instruction::LoopBreak),
            //
            // Switch
            //
            just("switch")
                .ignore_then(s)
                .ignore_then(
                    expr.clone()
                        .spanned()
                        .critical("expected an expression to switch on"),
                )
                .then_ignore(msnl)
                .then_ignore(char('{').critical_with_no_message())
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
                        .map(|(matches, body)| SwitchCase { matches, body })
                        .repeated_vec(),
                )
                .then(
                    msnl.ignore_then(just("else"))
                        .ignore_then(ms)
                        .ignore_then(block.clone().spanned().critical("expected a block"))
                        .or_not(),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_with_no_message())
                .map(|((expr, cases), els)| Instruction::Switch { expr, cases, els }),
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
                .map(|((name, signature), body)| Instruction::FnDecl {
                    name,
                    content: Function { signature, body },
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
                .ignore_then(s)
                .ignore_then(
                    fn_call
                        .clone()
                        .spanned()
                        .critical("expected a function call"),
                )
                .then_ignore(s.critical("expected a space followed by 'catch'"))
                .then_ignore(just("catch").critical("expected the 'catch' keyword"))
                .then_ignore(s.critical("expected a space followed by the catch variable"))
                .then(
                    ident
                        .spanned()
                        .critical("expected a variable to catch the throw value in"),
                )
                .then_ignore(ms)
                .then(block.clone().spanned().critical("expected a block"))
                .map(move |((call, catch_var), catch_body)| Instruction::Try {
                    call,
                    catch_var,
                    catch_body,
                }),
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
                .then_ignore(char('=').critical("expected an assignment operator (=)"))
                .then_ignore(ms)
                .then(
                    single_cmd_call
                        .clone()
                        .spanned()
                        .critical("expected a command call to alias"),
                )
                .map(move |(name, content)| Instruction::CmdAliasDecl {
                    name,
                    content_scope_id: scope_id_gen_bis.gen(),
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
                .then_ignore(char('=').critical("expected an assignment operator (=)"))
                .then_ignore(ms)
                .then(value_type.spanned().critical("expected a type to alias"))
                .map(|(name, content)| Instruction::TypeAliasDecl { name, content }),
            //
            // Base blocks
            //
            lookahead(char('{'))
                .ignore_then(block.spanned())
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
                        .and_then_str(move |path| load_file(path.data, path.at.start.file_id))
                        .and_then(move |file| {
                            program_bis
                                .parse_str_as_file(&file.content, FileId::SourceFile(file.id))
                        }),
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
        let scope_id_gen_bis = scope_id_gen.clone();

        instr
            .padded_by(msnl)
            .repeated_vec()
            .map(move |instructions| Block {
                scope_id: scope_id_gen_bis.gen(),
                instructions,
            })
    });

    program.finish(
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
        '(', ')', '[', ']', '{', '}', '<', '>', ';', '|', '\'', '"', '`', '$', '#',
    ])
});

// Usage: .debug(simple_debug) after any parser
#[allow(dead_code)]
fn simple_debug<T: std::fmt::Debug>(d: parsy::chainings::DebugType<'_, '_, T>) {
    println!("{d:#?}");
}
