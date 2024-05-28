use std::collections::HashMap;

use parsy::{
    atoms::{alphanumeric, any_char, digits},
    char, choice, end, filter, just, late, lookahead, not, recursive, whitespaces, MaybeEaten,
    Parser,
};

use crate::ast::{
    CmdEnvVar, CmdEnvVarValue, CmdPath, CmdPipe, CmdPipeType, ExprInnerContent, FnArg, FnArgNames,
    FnCall, FnCallArg, FnSignature, PropAccess, PropAccessNature, SingleCmdCall, SingleValueType,
    StructTypeMember, ValueType,
};

use super::ast::{
    Block, CmdArg, CmdCall, ComputedString, ComputedStringPiece, DoubleOp, ElsIf, Expr, ExprInner,
    ExprOp, Instruction, LiteralValue, Program, SingleOp, Value,
};

pub fn program() -> impl Parser<Program> {
    let raw_block = recursive::<Block, _>(|raw_block| {
        let ms = whitespaces().no_newline();
        let s = ms.at_least_one();
        let msnl = whitespaces();
        // let snl = msnl.at_least_one();

        let block = char('{')
            .critical("expected an opening brace '{'")
            .ignore_then(msnl)
            .ignore_then(raw_block)
            .then_ignore(msnl)
            .then_ignore(char('}').critical("expected a closing brace '}'"));

        let possible_ident_char = choice((char('_'), alphanumeric()));

        let first_ident_char = filter(|c| c == '_' || c.is_alphabetic());

        let ident = first_ident_char
            .then(filter(|c| c == '_' || c.is_alphanumeric()).repeated())
            .collect_string();

        let var_name = char('$').ignore_then(ident.clone());

        let cmd_call = late::<CmdCall>();
        let expr = late::<Expr>();

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
                .map(SingleValueType::Function),
            just("struct")
                .ignore_then(ms)
                .ignore_then(char('{'))
                .ignore_then(msnl)
                .ignore_then(
                    ident
                        .clone()
                        .spanned()
                        .map(MaybeEaten::Eaten)
                        .then_ignore(ms)
                        .then_ignore(char(':').critical("expected a ':'"))
                        .then_ignore(msnl)
                        .then(
                            value_type
                                .clone()
                                .spanned()
                                .map(MaybeEaten::Eaten)
                                .critical("expected a value type"),
                        )
                        .map(|(name, typ)| StructTypeMember { name, typ })
                        .spanned()
                        .map(MaybeEaten::Eaten)
                        .separated_by(char(',').padded()),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical("expected a closing brace '}'"))
                .map(SingleValueType::TypedStruct),
            just("struct")
                .not_followed_by(possible_ident_char.clone())
                .to(SingleValueType::UntypedStruct),
            ident.clone().spanned().map(SingleValueType::TypeAlias),
        ));

        value_type.finish(
            single_value_type
                .clone()
                .spanned()
                .map(MaybeEaten::Eaten)
                .separated_by(char('|').padded())
                .at_least(1)
                .map(|mut types| {
                    if types.len() > 1 {
                        ValueType::Union(types)
                    } else {
                        ValueType::Single(types.remove(0))
                    }
                }),
        );

        let fn_arg_long_flag = just("--")
            .ignore_then(
                ident
                    .clone()
                    .spanned()
                    .critical("expected a flag name (identifier)"),
            )
            .followed_by(
                not(possible_ident_char.clone()).critical("expected a single-character identifier"),
            );

        let fn_arg_short_flag = char('-')
            .ignore_then(
                first_ident_char
                    .spanned()
                    .critical("expected a single-character identifier"),
            )
            .followed_by(
                not(possible_ident_char.clone()).critical("expected a single-character identifier"),
            );

        let fn_arg_names = choice::<_, FnArgNames>((
            // Long *and* short flags
            fn_arg_long_flag
                .clone()
                .then_ignore(ms)
                .then_ignore(char('('))
                .then(fn_arg_short_flag.clone())
                .then_ignore(char(')').critical("unclosed short argument, expected ')'"))
                .map(|(long, short)| FnArgNames::LongAndShortFlag { short, long }),
            // Long flag only
            fn_arg_long_flag.map(FnArgNames::LongFlag),
            // Long flag only
            fn_arg_short_flag.map(FnArgNames::ShortFlag),
            // No flag
            ident.clone().spanned().map(FnArgNames::NotFlag),
        ));

        let fn_arg = just("...")
            .or_not()
            .then(fn_arg_names)
            .then(ms.ignore_then(char('?')).to(()).or_not())
            .then(
                ms.ignore_then(char(':'))
                    .ignore_then(ms)
                    .ignore_then(value_type.clone().spanned())
                    .or_not(),
            )
            .map(|(((is_rest, names), is_optional), typ)| FnArg {
                names,
                is_rest: is_rest.is_some(),
                is_optional: is_optional.is_some(),
                typ,
            });

        fn_signature.finish(
            char('(')
                .ignore_then(fn_arg.clone().separated_by(char(',').padded()))
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
                                .critical("expected a type"),
                        )
                        .then_ignore(ms)
                        .or_not(),
                )
                .map(|(args, ret_type)| FnSignature { args, ret_type }),
        );

        let fn_call = char('$')
            .or_not()
            .then(ident.clone())
            .spanned()
            .then_ignore(char('('))
            .then(
                expr.clone()
                    .spanned()
                    .map(FnCallArg::Expr)
                    .spanned()
                    .separated_by(char(',').padded())
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
                .collect_string()
                .map(|num| LiteralValue::Float(str::parse::<f64>(&num).unwrap())),
            // Integers
            char('-')
                .or_not()
                .then(digits(10))
                .collect_string()
                .map(|num| LiteralValue::Integer(str::parse::<i64>(&num).unwrap())),
        ));

        let computed_string = char('"')
            .ignore_then(
                choice::<_, ComputedStringPiece>((
                    // Escaped
                    char('\\')
                        .ignore_then(any_char())
                        .spanned()
                        .map(ComputedStringPiece::Escaped),
                    // Expressions
                    var_name
                        .clone()
                        .spanned()
                        .map(ComputedStringPiece::Variable),
                    // Expressions
                    just("${")
                        .ignore_then(expr.clone().spanned().critical("expected an expression"))
                        .then_ignore(char('}').critical("missing closing brace for expression (})"))
                        .map(ComputedStringPiece::Expr),
                    // Command calls
                    just("$(")
                        .ignore_then(
                            cmd_call
                                .clone()
                                .spanned()
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
                        .spanned()
                        .map(ComputedStringPiece::Literal),
                    // '$' symbol
                    char('$')
                        .spanned()
                        .map(|lit| ComputedStringPiece::Literal(lit.map(|c| c.to_string()))),
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
                        .then_ignore(char(':').critical("expected a semicolon ':'"))
                        .then_ignore(msnl)
                        .then(expr.clone().spanned().critical("expected an expression"))
                        .separated_by(char(',').padded()),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical("expected closing brace '}' for opened object"))
                .map(|members| {
                    Value::Object(
                        members
                            .into_iter()
                            .map(|(name, expr)| (name.data, expr))
                            .collect::<HashMap<_, _>>(),
                    )
                }),
            // Function calls
            fn_call.spanned().map(Value::FnCall),
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
                .ignore_then(fn_arg.clone().separated_by(char(',').padded()))
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
                                .critical("expected a type"),
                        )
                        .then_ignore(ms)
                        .or_not(),
                )
                .then(block.clone().spanned())
                .map(|((args, ret_type), body)| Value::Closure {
                    signature: FnSignature { args, ret_type },
                    body,
                }),
        ));

        let single_op = choice::<_, SingleOp>((char('!').to(SingleOp::Neg),));

        let double_op = choice::<_, DoubleOp>((
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
        ));

        let expr_inner_content = recursive(|expr_inner_content| {
            choice::<_, ExprInnerContent>((
                // Single operator (e.g. '!')
                single_op
                    .spanned()
                    .then(expr_inner_content.map(Box::new).spanned())
                    .map(|(op, right)| ExprInnerContent::SingleOp { op, right }),
                // Parenthesis-wrapped expression
                char('(')
                    .ignore_then(expr.clone().map(Box::new).spanned())
                    .then_ignore(char(')'))
                    .map(ExprInnerContent::ParenExpr),
                // Simple values
                value.clone().spanned().map(ExprInnerContent::Value),
            ))
        });

        let prop_access_nature = choice::<_, PropAccessNature>((
            char('.')
                .ignore_then(ident.clone().spanned().critical("expected a property name"))
                .map(PropAccessNature::Prop),
            char('[')
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

        let expr_inner = expr_inner_content
            .spanned()
            .then(prop_access.spanned().repeated_vec())
            .map(|(content, prop_acc)| ExprInner { content, prop_acc });

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

        let cmd_raw = filter(|c|
            // The first part of the condition is to accelerate computation
            c.is_alphanumeric() || !(c.is_whitespace() || c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}')
        )
        .repeated()
        .at_least(1)
        .collect_string();

        let cmd_path = choice::<_, CmdPath>((
            // Raw
            cmd_raw.clone().spanned().map(CmdPath::Raw),
            // Computed string
            computed_string
                .clone()
                .spanned()
                .map(CmdPath::ComputedString),
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

        let cmd_arg = choice::<_, CmdArg>((
            // Literal values
            literal_value.clone().spanned().map(CmdArg::LiteralValue),
            // Computed strings
            computed_string
                .clone()
                .spanned()
                .map(CmdArg::ComputedString),
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
                .map(CmdArg::CmdCall),
            // Variables
            var_name.clone().spanned().map(CmdArg::VarName),
            // Spread
            just("$...")
                .ignore_then(
                    ident
                        .clone()
                        .spanned()
                        .critical("expected a variable to spread"),
                )
                .map(CmdArg::SpreadVar),
            // Parenthesis-wrapped expressions
            char('(')
                .ignore_then(
                    expr.clone()
                        .spanned()
                        .padded()
                        .critical("expected an expression"),
                )
                .then_ignore(char(')').critical("unclosed expression"))
                .map(CmdArg::ParenExpr),
            // Function as value
            char('@')
                .ignore_then(ident.clone().critical("expected a function name"))
                .spanned()
                .map(CmdArg::FnAsValue),
            // Raw argument
            cmd_raw.spanned().map(CmdArg::Raw),
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
                            just("err|").to(CmdPipeType::Stderr),
                            // just("all|").to(CmdPipeType::Both),
                            char('|').to(CmdPipeType::Stdout),
                        ))
                        .spanned(),
                    )
                    .then_ignore(msnl)
                    .then(
                        single_cmd_call
                            .clone()
                            .spanned()
                            .critical("expected a command call after the pipe '|' symbol"),
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
                .then_ignore(ms)
                .then_ignore(char('='))
                .then_ignore(msnl)
                .then(
                    expr.clone()
                        .spanned()
                        .critical("expected an expression to assign"),
                )
                .map(|((name, prop_acc), expr)| Instruction::AssignVar {
                    name,
                    prop_acc,
                    expr,
                }),
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
            just("break")
                // .ignore_then(s.ignore_then(expr.clone().spanned()).or_not())
                // .map(|ret| Instruction::LoopBreak { ret }),
                .to(Instruction::LoopBreak),
            // TODO: switch case
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
                .then(fn_signature.critical("expected a list of arguments opened by a '('"))
                .then_ignore(ms)
                .then(
                    block
                        .clone()
                        .spanned()
                        .critical("expected a body for the function"),
                )
                .map(|((name, signature), body)| Instruction::FnDecl {
                    name,
                    signature,
                    body,
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
                .ignore_then(ms)
                .ignore_then(block.spanned())
                .map(Instruction::BaseBlock),
            //
            // Command calls
            //
            cmd_call.spanned().map(Instruction::CmdCall),
        ))
        .then_ignore(ms)
        .then_ignore(
            choice((
                lookahead(char('}')).map(|_| ()),
                filter(|c| c == '\n' || c == ';').map(|_| ()),
                end(),
            ))
            .critical("unexpected symbol"),
        );

        // Raw block
        instr.padded().spanned().repeated_vec().map(|instr| Block {
            instructions: instr,
        })
    });

    raw_block
        .spanned()
        .padded()
        .full()
        .critical("unexpected symbol")
        .map(|content| Program { content })
}

// fn simple_debug<T: std::fmt::Debug>(d: parsy::chainings::DebugType<'_, '_, T>) {
//     println!("{d:#?}");
// }
