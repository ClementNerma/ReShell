use std::{
    collections::{HashMap, HashSet},
    sync::LazyLock,
};

use parsy::{
    atoms::{alphanumeric, digits},
    char, choice, end, filter, just, late, lookahead, newline, not, recursive, silent_choice,
    whitespaces, FileId, Parser,
};

use crate::{
    ast::{
        Block, CmdArg, CmdCall, CmdComputedString, CmdComputedStringPiece, CmdEnvVar, CmdFlagArg,
        CmdFlagNameArg, CmdFlagValueArg, CmdPath, CmdPipe, CmdPipeType, CmdValueMakingArg,
        ComputedString, ComputedStringPiece, DoubleOp, ElsIf, ElsIfExpr, EscapableChar, Expr,
        ExprInner, ExprInnerChaining, ExprInnerContent, ExprOp, FlagValueSeparator, FnArg, FnCall,
        FnCallArg, FnFlagArgNames, FnSignature, Function, FunctionBody, Instruction, LiteralValue,
        Program, PropAccess, PropAccessNature, RuntimeEaten, SingleCmdCall, SingleOp,
        SingleValueType, StructTypeMember, SwitchCase, Value, ValueType,
    },
    files::SourceFile,
};

pub fn program(
    load_file: impl Fn(String, FileId) -> Result<SourceFile, String> + 'static,
) -> impl Parser<Program> {
    let program = late::<Program>();
    let outer = program.clone();

    let comment = char('#').then(filter(|c| c != '\r' && c != '\n').repeated());

    let ms = silent_choice((
        comment,
        filter(|c| c.is_whitespace() && c != '\r' && c != '\n'),
    ))
    .repeated();

    let msnl = silent_choice((comment, filter(|c| c.is_whitespace()))).repeated();

    let raw_block = recursive::<Block, _>(move |raw_block| {
        let s = whitespaces().no_newline().at_least_one();

        let block = char('{')
            .critical_expectation()
            .ignore_then(msnl)
            .ignore_then(raw_block)
            .then_ignore(msnl)
            .then_ignore(char('}').critical_expectation());

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
                        .then_ignore(char(':').critical_expectation())
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
                .then_ignore(char('}').critical_expectation())
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

        enum _ParsedFnArgName {
            Positional(RuntimeEaten<String>),
            Flag(FnFlagArgNames),
        }

        let fn_arg = choice::<_, FnArg>((
            choice::<_, _ParsedFnArgName>((
                // Positional
                ident
                    .spanned()
                    .map(RuntimeEaten::Parsed)
                    .map(_ParsedFnArgName::Positional),
                // Flag
                fn_flag_arg_names.map(_ParsedFnArgName::Flag),
            ))
            .then(char('?').or_not().map(|is_optional| is_optional.is_some()))
            .then(
                ms.ignore_then(char(':'))
                    .ignore_then(msnl)
                    .ignore_then(value_type.clone().spanned().map(RuntimeEaten::Parsed))
                    .or_not(),
            )
            .map(|((name, is_optional), typ)| match name {
                _ParsedFnArgName::Positional(name) => FnArg::Positional {
                    name,
                    is_optional,
                    typ,
                },
                _ParsedFnArgName::Flag(names) => {
                    if !is_optional && typ.is_none() {
                        FnArg::PresenceFlag { names }
                    } else {
                        FnArg::NormalFlag {
                            names,
                            is_optional,
                            typ,
                        }
                    }
                }
            }),
            // Rest
            just("...")
                .ignore_then(ident)
                .spanned()
                .map(RuntimeEaten::Parsed)
                .map(|name| FnArg::Rest { name }),
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
                .then_ignore(char(')').critical_expectation())
                .then_ignore(msnl)
                .then(
                    just("->")
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

        let fn_call = char('$')
            .or_not()
            .then(ident)
            .spanned()
            .then_ignore(char('('))
            .then(
                fn_call_arg
                    .spanned()
                    .padded_by(msnl)
                    .separated_by(char(','))
                    .spanned(),
            )
            .then_ignore(char(')').critical("expected an expression"))
            .map(|(name, call_args)| FnCall {
                is_var_name: name.data.0.is_some(),
                name: name.map(|(_, name)| name),
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
                .repeated()
                .collect_string(),
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
                .then(digits(10))
                .then(char('.'))
                .then(digits(10))
                .not_followed_by(possible_ident_char)
                .collect_string()
                .map(|num| LiteralValue::Float(str::parse::<f64>(&num).unwrap())),
            // Integers
            char('-')
                .or_not()
                .then(digits(10))
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
                    // Expressions
                    var_name.spanned().map(ComputedStringPiece::Variable),
                    // Expressions
                    char('`')
                        .ignore_then(
                            expr.clone()
                                .spanned()
                                .padded_by(msnl)
                                .critical("expected an expression"),
                        )
                        .then_ignore(char('`').critical_expectation())
                        .map(ComputedStringPiece::Expr),
                    // Command calls
                    just("$(")
                        .ignore_then(
                            cmd_call
                                .clone()
                                .spanned()
                                .padded_by(msnl)
                                .critical("expected a command call"),
                        )
                        .then_ignore(char(')').critical_expectation())
                        .map(ComputedStringPiece::CmdCall),
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
            .then_ignore(char('"').critical_expectation())
            .map(|pieces| ComputedString { pieces });

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
            char('{')
                .ignore_then(msnl)
                .ignore_then(
                    ident
                        .spanned()
                        .then_ignore(ms)
                        .then_ignore(char(':').critical_expectation())
                        .then_ignore(msnl)
                        .then(expr.clone().spanned().critical("expected an expression"))
                        .separated_by(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical("expected closing brace '}' for opened object"))
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
            just("@(")
                .ignore_then(msnl)
                .ignore_then(
                    cmd_call
                        .clone()
                        .spanned()
                        .critical("expected a command call"),
                )
                .then_ignore(msnl)
                .then_ignore(char(')').critical_expectation())
                .map(Value::CmdCall),
            // Function as value
            char('@')
                .ignore_then(ident.critical("expected a function name"))
                .spanned()
                .map(Value::FnAsValue),
            // Closures
            char('|')
                .ignore_then(
                    fn_arg
                        .clone()
                        .separated_by(char(',').padded_by(msnl))
                        .spanned()
                        .map(RuntimeEaten::Parsed),
                )
                .then_ignore(char('|').critical("unclosed function arguments list"))
                .then_ignore(ms)
                .then(
                    just("->")
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
                .map(|(args, ret_type)| FnSignature { args, ret_type })
                .spanned()
                .then_ignore(ms)
                .then(
                    choice::<_, FunctionBody>((
                        lookahead(char('{'))
                            .ignore_then(block.clone().spanned())
                            .map(FunctionBody::Block),
                        expr.clone().map(Box::new).spanned().map(FunctionBody::Expr),
                    ))
                    .critical("expected a function body")
                    .spanned(),
                )
                .map(|(signature, body)| Value::Closure(Function { signature, body })),
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
            .critical_expectation()
            .ignore_then(msnl)
            .ignore_then(expr.clone().map(Box::new).spanned())
            .then_ignore(msnl)
            .then_ignore(char('}').critical_expectation());

        let expr_inner_content = recursive(|expr_inner_content| {
            choice::<_, ExprInnerContent>((
                // Single operator (e.g. '!')
                single_op
                    .spanned()
                    .then_ignore(ms)
                    .then(expr_inner_content.map(Box::new).spanned())
                    .map(|(op, right)| ExprInnerContent::SingleOp { op, right }),
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
                    .map(|((fn_call, catch_var), catch_expr)| ExprInnerContent::Try {
                        fn_call,
                        catch_var,
                        catch_expr,
                    }),
                // Simple values
                value.clone().spanned().map(ExprInnerContent::Value),
            ))
        });

        let prop_access_nature = choice::<_, PropAccessNature>((
            char('.')
                .ignore_then(ident.spanned().critical("expected a property name"))
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

        let expr_inner_chaining = choice::<_, ExprInnerChaining>((
            char('.')
                .ignore_then(fn_call.clone())
                .spanned()
                .map(ExprInnerChaining::MethodCall),
            just("->")
                .padded_by(msnl)
                .ignore_then(fn_call.clone())
                .spanned()
                .map(ExprInnerChaining::FnCall),
            prop_access.spanned().map(ExprInnerChaining::PropAccess),
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

        let cmd_computed_string = not(just("->")).ignore_then(
            choice::<_, CmdComputedStringPiece>((
                // Expressions
                var_name.spanned().map(CmdComputedStringPiece::Variable),
                // Literal character suites
                filter(|c| !c.is_whitespace() && !DELIMITER_CHARS.contains(&c))
                    .repeated()
                    .at_least(1)
                    .collect_string()
                    .map(CmdComputedStringPiece::Literal),
            ))
            .spanned()
            .repeated_vec()
            .at_least(1)
            .map(|pieces| CmdComputedString { pieces }),
        );

        let cmd_path = choice::<_, CmdPath>((
            // Direct
            just("@direct")
                .ignore_then(ms)
                .ignore_then(
                    cmd_computed_string
                        .critical("expected a command name")
                        .spanned(),
                )
                .map(CmdPath::Direct),
            // Expression that evealuates to a function
            char('(')
                .ignore_then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected an expression")
                        .spanned()
                        .padded_by(msnl),
                )
                .then_ignore(
                    char(')').critical("expected a closing parenthesis after the expression"),
                )
                .map(CmdPath::Expr),
            // Computed string
            computed_string
                .clone()
                .spanned()
                .map(CmdPath::ComputedString),
            // Command computable string
            cmd_computed_string
                .spanned()
                .map(CmdPath::CmdComputedString),
        ));

        let cmd_value_making_arg = choice::<_, CmdValueMakingArg>((
            // Literal values
            literal_value.spanned().map(CmdValueMakingArg::LiteralValue),
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
            // Raw argument (but not flags, which aren't value making arguments)
            cmd_computed_string
                .spanned()
                .map(CmdValueMakingArg::CmdComputedString),
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
                .ignore_then(first_ident_char)
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
                            // Flags must be exlucded, otherwise '-a -b'
                            // would have '-b' considered as the value for '-a'
                            .not_followed_by(char('-'))
                            // Same thing goes for rest arguments
                            .not_followed_by(just("...$"))
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
            just("...$")
                .ignore_then(ident.spanned().critical("expected a variable to spread"))
                .map(CmdArg::SpreadVar),
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

        cmd_call.finish(
            single_cmd_call
                .clone()
                .spanned()
                .then(
                    msnl.ignore_then(
                        choice::<_, CmdPipeType>((
                            just("->").to(CmdPipeType::Value),
                            just("!|").to(CmdPipeType::Stderr),
                            char('|').to(CmdPipeType::Stdout),
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

        let instr = choice::<_, Instruction>((
            //
            // Variables declaration
            //
            just("let")
                .then_ignore(s)
                .ignore_then(
                    just("mut")
                        .not_followed_by(possible_ident_char)
                        .then_ignore(s.critical("expected a whitespace after 'mut' keyword"))
                        .spanned()
                        .or_not(),
                )
                .then(ident.spanned().critical("expected a variable name"))
                .then(
                    ms.ignore_then(char('=').critical_expectation())
                        .ignore_then(msnl)
                        .ignore_then(
                            expr.clone()
                                .spanned()
                                .critical("expected an expression to assign"),
                        ),
                )
                .map(|((mutable, name), init_expr)| Instruction::DeclareVar {
                    mutable: mutable.map(|tok| tok.replace(())),
                    name,
                    init_expr,
                }),
            //
            // Variables assignment
            //
            var_name
                .spanned()
                .then(prop_access_nature.spanned().repeated_vec())
                .then(just("[]").to(()).spanned().or_not())
                .then_ignore(ms)
                .then_ignore(char('=').critical("expected the assignment operator '='"))
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
                .then_ignore(char('{').critical_expectation())
                .then_ignore(msnl)
                .then(
                    just("case")
                        .ignore_then(ms)
                        .ignore_then(
                            expr.clone()
                                .spanned()
                                .critical("expected an expression to match"),
                        )
                        .then_ignore(ms)
                        .then(block.clone().spanned().critical("expected a block"))
                        .map(|(cond, body)| SwitchCase { cond, body })
                        .repeated_vec(),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_expectation())
                .map(|(expr, cases)| Instruction::Switch { expr, cases }),
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
                        .critical("expected a body for the function")
                        .map(FunctionBody::Block)
                        .spanned(),
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
                .ignore_then(expr.spanned().critical("expected an expression to throw"))
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
                .map(|((call, catch_var), catch_body)| Instruction::Try {
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
                .map(|(name, content)| Instruction::CmdAliasDecl { name, content }),
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
                .ignore_then(literal_string.spanned().critical("expected a file path"))
                .try_map(move |path| load_file(path.data, path.at.start.file_id))
                .and_then(move |file| {
                    program.parse_str_as_file(&file.content, FileId::SourceFile(file.id))
                })
                .map(Instruction::Imported),
            //
            // Function call
            //
            fn_call.spanned().map(Instruction::FnCall),
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
        instr
            .padded_by(msnl)
            .repeated_vec()
            .map(|instructions| Block { instructions })
    });

    outer.finish(
        raw_block
            .spanned()
            .padded_by(msnl)
            .full()
            .critical("unexpected symbol")
            .map(|content| Program { content }),
    );

    outer
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
