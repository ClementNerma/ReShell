use std::collections::{HashMap, HashSet};

use parsy::{
    atoms::{alphanumeric, digits},
    char, choice, end, filter, just, late, lookahead, not, recursive, whitespaces, FileId, Parser,
};

use crate::{
    ast::{
        Block, CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdFlagArg, CmdFlagNameArg,
        CmdFlagValueArg, CmdPath, CmdPipe, CmdPipeType, CmdValueMakingArg, ComputedString,
        ComputedStringPiece, DoubleOp, ElsIf, ElsIfExpr, EscapableChar, Expr, ExprInner,
        ExprInnerChaining, ExprInnerContent, ExprOp, FlagValueSeparator, FnArg, FnCall, FnCallArg,
        FnFlagArgNames, FnSignature, Function, FunctionBody, Instruction, LiteralValue, Program,
        PropAccess, PropAccessNature, RuntimeEaten, SingleCmdCall, SingleOp, SingleValueType,
        StructTypeMember, SwitchCase, Value, ValueType,
    },
    files::SourceFile,
};

pub fn program(
    load_file: impl Fn(String, FileId) -> Result<SourceFile, String> + 'static,
) -> impl Parser<Program> {
    let program = late::<Program>();
    let outer = program.clone();

    let raw_block = recursive::<Block, _>(move |raw_block| {
        let ms = whitespaces().no_newline();
        let s = ms.at_least_one();
        let msnl = whitespaces();

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

        let var_name = char('$').ignore_then(ident.clone());

        let cmd_call = late::<CmdCall>();
        let expr = late::<Expr>();

        let cmd_flag_arg = late::<CmdFlagArg>();

        let fn_signature = late::<FnSignature>();

        let value_type = late::<ValueType>();

        let single_value_type = choice::<_, SingleValueType>((
            just("any")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::Any),
            just("null")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::Null),
            just("bool")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::Bool),
            just("int")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::Int),
            just("float")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::Float),
            just("string")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::String),
            just("list")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::List),
            just("map")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::Map),
            just("error")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::Error),
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
                        .clone()
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
                        .separated_by(char(',').padded()),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_expectation())
                .map(SingleValueType::TypedStruct),
            just("struct")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::UntypedStruct),
            ident.clone().spanned().map(SingleValueType::TypeAlias),
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
                        .separated_by(char('|').padded())
                        .at_least(2)
                        .critical("expected at least 2 types in union"),
                )
                .then_ignore(msnl)
                .then_ignore(char(')').critical("expected a closing parenthesis for type union"))
                .map(ValueType::Union),
        )));

        let fn_arg_long_flag = just("--")
            .ignore_then(
                ident
                    .clone()
                    .spanned()
                    .critical("expected a flag name (identifier)"),
            )
            .followed_by(not(possible_ident_char.clone()).critical("unexpected symbol"));

        let fn_arg_short_flag = char('-')
            .ignore_then(
                first_ident_char
                    .spanned()
                    .critical("expected a single-character identifier"),
            )
            .followed_by(
                not(possible_ident_char.clone()).critical("expected a single-character identifier"),
            );

        let fn_flag_arg_names = choice::<_, FnFlagArgNames>((
            // Long *and* short flags
            fn_arg_long_flag
                .clone()
                .map(RuntimeEaten::Parsed)
                .then_ignore(ms)
                .then_ignore(char('('))
                .then(fn_arg_short_flag.clone().map(RuntimeEaten::Parsed))
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
                    .clone()
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
                .ignore_then(ident.clone())
                .spanned()
                .map(RuntimeEaten::Parsed)
                .map(|name| FnArg::Rest { name }),
        ));

        fn_signature.finish(
            char('(')
                .ignore_then(
                    fn_arg
                        .clone()
                        .separated_by(char(',').padded())
                        .spanned()
                        .map(RuntimeEaten::Parsed),
                )
                .then_ignore(msnl)
                .then_ignore(char(')').critical("missing closing parenthesis for arguments list"))
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
                .map(|(args, ret_type)| FnSignature { args, ret_type }),
        );

        let fn_call_arg = choice::<_, FnCallArg>((
            ident
                .clone()
                .spanned()
                .then_ignore(ms)
                .then_ignore(char('='))
                .then_ignore(msnl)
                .then(expr.clone().spanned())
                .map(|(name, value)| FnCallArg::Flag {
                    name: name.map(|name| {
                        if name.chars().nth(1).is_some() {
                            CmdFlagNameArg::Long(name)
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
            .then(ident.clone())
            .spanned()
            .then_ignore(char('('))
            .then(
                fn_call_arg
                    .spanned()
                    .padded()
                    .separated_by(char(','))
                    .spanned(),
            )
            .then_ignore(char(')').critical("expected an expression"))
            .map(|(name, call_args)| FnCall {
                is_var_name: name.data.0.is_some(),
                name: name.map(|(_, name)| name),
                call_args,
            });

        let literal_value = choice::<_, LiteralValue>((
            // Booleans
            just("true")
                .not_followed_by(possible_ident_char.clone())
                .to(LiteralValue::Boolean(true)),
            just("false")
                .not_followed_by(possible_ident_char.clone())
                .to(LiteralValue::Boolean(false)),
            // Floats
            char('-')
                .or_not()
                .then(digits(10))
                .then(char('.'))
                .then(digits(10))
                .not_followed_by(possible_ident_char.clone())
                .collect_string()
                .map(|num| LiteralValue::Float(str::parse::<f64>(&num).unwrap())),
            // Integers
            char('-')
                .or_not()
                .then(digits(10))
                .not_followed_by(possible_ident_char.clone())
                .collect_string()
                .map(|num| LiteralValue::Integer(str::parse::<i64>(&num).unwrap())),
        ));

        let escapable_char = choice((
            char('n').to(EscapableChar::Newline),
            char('"').to(EscapableChar::DoubleQuote),
            char('\\').to(EscapableChar::Backslash),
            char('$').to(EscapableChar::DollarSign),
        ));

        let computed_string = char('"')
            .ignore_then(
                choice::<_, ComputedStringPiece>((
                    // Escaped
                    char('\\')
                        .ignore_then(escapable_char.critical("this character is not escapable"))
                        .map(ComputedStringPiece::Escaped),
                    // Expressions
                    var_name
                        .clone()
                        .spanned()
                        .map(ComputedStringPiece::Variable),
                    // Expressions
                    just("${")
                        .ignore_then(
                            expr.clone()
                                .spanned()
                                .padded()
                                .critical("expected an expression"),
                        )
                        .then_ignore(char('}').critical("missing closing brace for expression (})"))
                        .map(ComputedStringPiece::Expr),
                    // Command calls
                    just("$(")
                        .ignore_then(
                            cmd_call
                                .clone()
                                .spanned()
                                .padded()
                                .critical("expected a command call"),
                        )
                        .then_ignore(
                            char(')').critical("missing closing parenthesis for command call ')'"),
                        )
                        .map(ComputedStringPiece::CmdCall),
                    // Literal character suites
                    filter(|c| c != '"' && c != '\\' && c != '$')
                        .repeated()
                        .at_least(1)
                        .collect_string()
                        .map(ComputedStringPiece::Literal),
                ))
                .spanned()
                .repeated_vec(),
            )
            .then_ignore(char('"').critical("expected closing quote (\")"))
            .map(|pieces| ComputedString { pieces });

        let value = choice::<_, Value>((
            just("null").to(Value::Null),
            // Literals
            literal_value.clone().spanned().map(Value::Literal),
            // Computed strings
            computed_string.clone().spanned().map(Value::ComputedString),
            // Lists
            char('[')
                .ignore_then(msnl)
                .ignore_then(expr.clone().spanned().separated_by(char(',').padded()))
                .then_ignore(msnl)
                .then_ignore(char(']').critical("expected a closing bracket ']' for the list"))
                .map(Value::List),
            char('{')
                .ignore_then(msnl)
                .ignore_then(
                    ident
                        .clone()
                        .spanned()
                        .then_ignore(ms)
                        .then_ignore(char(':').critical_expectation())
                        .then_ignore(msnl)
                        .then(expr.clone().spanned().critical("expected an expression"))
                        .separated_by(char(',').padded()),
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
            var_name.clone().spanned().map(Value::Variable),
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
                .then_ignore(char(')').critical("unclosed command (expected '}')"))
                .map(Value::CmdSuccess),
            // Function as value
            char('@')
                .ignore_then(ident.clone().critical("expected a function name"))
                .spanned()
                .map(Value::FnAsValue),
            // Closures
            char('|')
                .ignore_then(
                    fn_arg
                        .clone()
                        .separated_by(char(',').padded())
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
                    .then(
                        ident
                            .clone()
                            .spanned()
                            .critical("expected a catch variable"),
                    )
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
                .ignore_then(ident.clone().spanned().critical("expected a property name"))
                .map(PropAccessNature::Prop),
            char('[')
                .not_followed_by(char(']'))
                .ignore_then(
                    expr.clone()
                        .padded()
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
            prop_access.spanned().map(ExprInnerChaining::PropAccess),
        ));

        let expr_inner = expr_inner_content
            .spanned()
            .then(expr_inner_chaining.spanned().repeated_vec())
            .then(
                msnl.ignore_then(just("->"))
                    .ignore_then(msnl)
                    .ignore_then(fn_call.clone().spanned())
                    .separated_by(msnl),
            )
            .map(|((content, chainings), pipes)| ExprInner {
                content,
                chainings,
                pipes,
            });

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
                .then_ignore(ms)
                .then(expr_op.separated_by(ms))
                .map(|(inner, right_ops)| Expr { inner, right_ops }),
        );

        let delimiter_chars = delimiter_chars();

        let cmd_raw = not(just("->")).ignore_then(
            filter(move |c| !c.is_whitespace() && !delimiter_chars.contains(&c))
                .repeated()
                .at_least(1)
                .collect_string(),
        );

        let cmd_path = choice::<_, CmdPath>((
            // Direct
            just("@direct")
                .ignore_then(ms)
                .ignore_then(
                    cmd_raw
                        .clone()
                        .critical("expected a command name")
                        .spanned(),
                )
                .map(CmdPath::Direct),
            // Call variable
            char('(')
                .ignore_then(
                    expr.clone()
                        .map(Box::new)
                        .critical("expected an expression")
                        .spanned()
                        .padded(),
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
            // Raw string
            cmd_raw.clone().spanned().map(CmdPath::RawString),
        ));

        let cmd_env_var_value = choice::<_, CmdEnvVarValue>((
            // Raw
            cmd_raw.clone().spanned().map(CmdEnvVarValue::Raw),
            // Computed string
            computed_string
                .clone()
                .spanned()
                .map(CmdEnvVarValue::ComputedString),
            // Expression
            char('(')
                .ignore_then(expr.clone().spanned().padded())
                .then_ignore(
                    char(')').critical("unclosed expression (expected a closing parenthesis)"),
                )
                .map(CmdEnvVarValue::Expr),
        ));

        let cmd_env_var = ident
            .clone()
            .spanned()
            .then_ignore(char('='))
            .then(
                cmd_env_var_value
                    .spanned()
                    .critical("expected a value for the provided environment variable"),
            )
            .followed_by(
                s.critical("expected another assignment or the command call after the assignment"),
            )
            .map(|(name, value)| CmdEnvVar { name, value });

        let cmd_flag_name_arg = choice::<_, CmdFlagNameArg>((
            just("--")
                .not_followed_by(char('-'))
                .ignore_then(cmd_raw.clone())
                .map(CmdFlagNameArg::Long),
            just("-")
                .ignore_then(first_ident_char)
                .map(CmdFlagNameArg::Short),
        ));

        let cmd_value_making_arg = choice::<_, CmdValueMakingArg>((
            // Literal values
            literal_value
                .clone()
                .spanned()
                .map(CmdValueMakingArg::LiteralValue),
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
                        .padded()
                        .critical("expected a command call"),
                )
                .then_ignore(
                    char(')').critical("unclosed command call (expected a closing parenthesis)"),
                )
                .map(CmdValueMakingArg::CmdCall),
            // Parenthesis-wrapped expressions
            char('(')
                .ignore_then(
                    expr.clone()
                        .spanned()
                        .padded()
                        .critical("expected an expression"),
                )
                .then_ignore(char(')').critical("unclosed expression"))
                .map(CmdValueMakingArg::ParenExpr),
            // Function as value
            char('@')
                .ignore_then(ident.clone().critical("expected a function name"))
                .spanned()
                .map(CmdValueMakingArg::FnAsValue),
            // Variables
            var_name.clone().spanned().map(CmdValueMakingArg::VarName),
            // Raw argument
            cmd_raw.clone().spanned().map(CmdValueMakingArg::Raw),
        ));

        cmd_flag_arg.finish(
            cmd_flag_name_arg
                .spanned()
                .then(
                    choice((
                        s.to(FlagValueSeparator::Space),
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
            // Rest separator
            just("--")
                .to(())
                .spanned()
                .not_followed_by(cmd_raw)
                .map(CmdArg::RestSeparator),
            // Spread
            just("$...")
                .ignore_then(
                    ident
                        .clone()
                        .spanned()
                        .critical("expected a variable to spread"),
                )
                .map(CmdArg::SpreadVar),
            // Value-making
            cmd_value_making_arg.map(CmdArg::ValueMaking),
        ));

        let single_cmd_call = cmd_env_var
            .spanned()
            .separated_by(s)
            .spanned()
            .then(cmd_path.clone().spanned())
            .then(s.ignore_then(cmd_arg.spanned()).repeated_vec().spanned())
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
                    ms.ignore_then(
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
            // Comments
            //
            char('#')
                .ignore_then(ms)
                .ignore_then(filter(|c| c != '\n').repeated().collect_string().spanned())
                .map(|content| Instruction::Comment { content }),
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
                .then(ident.clone().spanned())
                .then(
                    ms.ignore_then(char('='))
                        .ignore_then(msnl)
                        .ignore_then(
                            expr.clone()
                                .spanned()
                                .critical("expected an expression to assign"),
                        )
                        .or_not(),
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
                .ignore_then(ident.clone().spanned())
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
                .ignore_then(ident.clone().spanned())
                .then_ignore(ms)
                .then_ignore(char(','))
                .then_ignore(ms)
                .then(ident.clone().spanned())
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
            just("continue").to(Instruction::LoopContinue),
            //
            // Loop breakage
            //
            just("break").to(Instruction::LoopBreak),
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
                        .clone()
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
                .ignore_then(expr.spanned())
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
                        .clone()
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
                        .clone()
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
            just("do")
                .ignore_then(s)
                .ignore_then(block.spanned())
                .map(Instruction::DoBlock),
            //
            //
            //
            just("include")
                .ignore_then(s)
                .ignore_then(char('"').critical_expectation())
                .ignore_then(
                    filter(|c| c != '"' && c != '\r' && c != '\n')
                        .repeated()
                        .at_least(1)
                        .critical("expected a path")
                        .collect_string(),
                )
                .then_ignore(char('"').critical("expected a closing quote after the path (\")"))
                .spanned()
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
        .then_ignore(ms)
        .then_ignore(
            choice((
                lookahead(char('}')).map(|_| ()),
                lookahead(just("\r\n").map(|_| ())),
                filter(|c| c == '\n' || c == ';').map(|_| ()),
                end(),
            ))
            .critical("unexpected symbol"),
        );

        // Raw block
        instr
            .padded()
            .spanned()
            .repeated_vec()
            .map(|instructions| Block { instructions })
    });

    outer.finish(
        raw_block
            .spanned()
            .padded()
            .full()
            .critical("unexpected symbol")
            .map(|content| Program { content }),
    );

    outer
}

pub fn delimiter_chars() -> HashSet<char> {
    HashSet::from([
        '(', ')', '[', ']', '{', '}', '<', '>', '=', ';', '!', '?', '&', '|', '\'', '"', '$',
    ])
}

// Usage: .debug(simple_debug) after any parser
#[allow(dead_code)]
fn simple_debug<T: std::fmt::Debug>(d: parsy::chainings::DebugType<'_, '_, T>) {
    println!("{d:#?}");
}
