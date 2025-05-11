use std::borrow::Cow;

use parsy::{
    FileId, Parser, ParserInput, ParsingError, Span,
    helpers::{
        char, choice, custom, end, filter, get_context, just, lookahead, recursive_shared,
        silent_choice,
    },
    timed::LazilyDefined,
};

use crate::{
    DELIMITER_CHARS,
    ast::{
        ElsIf, FnSignatureArg, FnSignaturePositionalArg, Function, Instruction, MatchCase,
        ObjPropSpreading, ObjPropSpreadingBinding, ObjPropSpreadingType, RangeBound, SingleVarDecl,
        TypeMatchCase, ValueType, VarSpreading,
    },
    parser::{
        PROGRAM, ParserContext,
        blocks::{BLOCK, generate_scope_id},
        cmd_calls::{CMD_CALL, SINGLE_CMD_CALL},
        exprs::{EXPR, PROP_ACCESS_NATURE},
        functions::FN_SIGNATURE,
        types::VALUE_TYPE,
        values::{LITERAL_INT, LITERAL_STRING},
    },
    use_basic_parsers,
};

pub static INSTRUCTION: LazilyDefined<Span<Instruction>> = LazilyDefined::new(|| {
    use_basic_parsers!(possible_ident_char, ms, s, ident, msnl, var_name);

    let single_var_decl = just("mut")
        .to(())
        .not_followed_by(possible_ident_char)
        .then_ignore(s.critical_auto_msg())
        .spanned()
        .or_not()
        .then(
            ident
                .validate_or_critical(
                    |name| name != "it",
                    "Cannot declare a variable with reserved name 'it'",
                )
                .validate_or_critical(
                    |name| name != "self",
                    "Cannot declare a variable with reserved name 'self'",
                )
                .spanned(),
        )
        .then(
            ms.ignore_then(char(':'))
                .ignore_then(ms)
                .ignore_then(VALUE_TYPE.static_ref())
                .or_not(),
        )
        .map(|((is_mut, name), enforced_type)| SingleVarDecl {
            name,
            is_mut: is_mut.is_some(),
            enforced_type,
        });

    let var_decl_type = recursive_shared(|var_decl_type| {
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
            LITERAL_STRING
                .static_ref()
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
                                .ignore_then(EXPR.static_ref().critical("expected an expression"))
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
        LITERAL_INT.static_ref().map(RangeBound::Literal),
        var_name.spanned().map(RangeBound::Variable),
        char('(')
            .ignore_then(EXPR.static_ref().critical("expected an expression"))
            .then_ignore(char(')').critical_auto_msg())
            .spanned()
            .map(RangeBound::Expr),
    ));

    choice::<Instruction, _>((
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
                        EXPR.static_ref()
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
            .then(
                PROP_ACCESS_NATURE
                    .static_ref()
                    .spanned()
                    .repeated_into_vec(),
            )
            .then(just("[]").to(()).spanned().or_not())
            .then_ignore(ms)
            .then_ignore(char('='))
            // Distinguish from '$someVariable == ...' expressions
            .not_followed_by(char('='))
            .then_ignore(msnl)
            .then(
                EXPR.static_ref()
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
            .ignore_then(EXPR.static_ref().spanned().critical("expected a condition"))
            .then_ignore(ms)
            .then(
                BLOCK
                    .static_ref()
                    .critical("expected a body for the condition"),
            )
            .then(
                msnl.ignore_then(just("else"))
                    .ignore_then(s)
                    .ignore_then(just("if"))
                    .ignore_then(s)
                    .ignore_then(
                        EXPR.static_ref()
                            .spanned()
                            .critical("expected a condition for the 'else if' statement"),
                    )
                    .then_ignore(ms)
                    .then(BLOCK.static_ref())
                    .map(|(cond, body)| ElsIf { cond, body })
                    .repeated_into_vec()
                    .then(
                        msnl.ignore_then(just("else"))
                            .ignore_then(ms)
                            .ignore_then(
                                BLOCK
                                    .static_ref()
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
            .then(range_bound.spanned())
            .then_ignore(just(".."))
            .then(char('=').or_not().map(|c| c.is_some()))
            .then(range_bound.spanned().critical("expected a range end value"))
            .then_ignore(ms)
            .then(
                BLOCK
                    .static_ref()
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
                EXPR.static_ref()
                    .spanned()
                    .critical("expected an expression to iterate on"),
            )
            .then_ignore(ms)
            .then(
                BLOCK
                    .static_ref()
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
                EXPR.static_ref()
                    .spanned()
                    .critical("expected an expression to iterate on"),
            )
            .then_ignore(ms)
            .then(
                BLOCK
                    .static_ref()
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
            .ignore_then(EXPR.static_ref().spanned())
            .then_ignore(ms)
            .then(
                BLOCK
                    .static_ref()
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
                EXPR.static_ref()
                    .spanned()
                    .critical("expected an expression to match on"),
            )
            .then_ignore(msnl)
            .then_ignore(char('{').critical_auto_msg())
            .then(
                msnl.ignore_then(just("case"))
                    .ignore_then(ms)
                    .ignore_then(
                        EXPR.static_ref()
                            .spanned()
                            .critical("expected an expression to match"),
                    )
                    .then_ignore(ms)
                    .then(BLOCK.static_ref().critical("expected a block"))
                    .map(|(matches, body)| MatchCase { matches, body })
                    .repeated_into_vec(),
            )
            .then(
                msnl.ignore_then(just("else"))
                    .ignore_then(ms)
                    .ignore_then(BLOCK.static_ref().critical("expected a block"))
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
                EXPR.static_ref()
                    .spanned()
                    .critical("expected an expression to match on"),
            )
            .then_ignore(msnl)
            .then_ignore(char('{').critical_auto_msg())
            .then(
                msnl.ignore_then(just("case"))
                    .ignore_then(ms)
                    .ignore_then(VALUE_TYPE.static_ref().critical("expected a type to match"))
                    .then_ignore(ms)
                    .then(BLOCK.static_ref().critical("expected a block"))
                    .map(|(matches, body)| TypeMatchCase { matches, body })
                    .repeated_into_vec(),
            )
            .then(
                msnl.ignore_then(just("else"))
                    .ignore_then(ms)
                    .ignore_then(BLOCK.static_ref().critical("expected a block"))
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
                FN_SIGNATURE
                    .static_ref()
                    .critical("expected a list of arguments opened by a '('")
                    .spanned(),
            )
            .then_ignore(ms)
            .then(
                BLOCK
                    .static_ref()
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
                                        "'self' argument cannot be optional in methods",
                                    )))
                                } else {
                                    Some(typ.clone().ok_or_else(|| {
                                        ParsingError::custom(name_at, "").criticalize(
                                            "'self' argument must have a specified type",
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
            .ignore_then(s.ignore_then(EXPR.static_ref().spanned()).or_not())
            .map(|expr| Instruction::FnReturn { expr }),
        //
        // Throws
        //
        just("throw")
            .ignore_then(s)
            .ignore_then(
                EXPR.static_ref()
                    .spanned()
                    .critical("expected an expression to throw"),
            )
            .map(Instruction::Throw),
        //
        // Try/Catch
        //
        just("try")
            .ignore_then(char('{').padded_by(msnl))
            .ignore_then(
                EXPR.static_ref()
                    .spanned()
                    .critical("expected an expression"),
            )
            .then_ignore(char('}').padded_by(msnl))
            .then_ignore(just("catch").critical_auto_msg())
            .then_ignore(s.critical_auto_msg())
            .then(
                ident
                    .validate_or_critical(
                        |name| name != "it" && name != "self",
                        "Cannot declare a variable with reserved name 'it' or 'self'",
                    )
                    .spanned()
                    .critical("expected a variable to catch the throw value in"),
            )
            .then_ignore(s.critical_auto_msg())
            .then(BLOCK.static_ref().critical("expected a block"))
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
                SINGLE_CMD_CALL
                    .static_ref()
                    .spanned()
                    .critical("expected a command call to alias"),
            )
            .map(move |(name, content)| Instruction::CmdAliasDecl {
                name,
                content_scope_id: generate_scope_id(),
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
            .then(
                VALUE_TYPE
                    .static_ref()
                    .spanned()
                    .critical("expected a type to alias"),
            )
            .map(|(name, content)| Instruction::TypeAliasDecl { name, content }),
        //
        // Base blocks
        //
        just("do")
            .ignore_then(s)
            .ignore_then(BLOCK.static_ref().critical("expected a block"))
            .map(Instruction::DoBlock),
        //
        // Include statement
        //
        just("include")
            .ignore_then(s)
            .ignore_then(
                LITERAL_STRING
                    .static_ref()
                    .spanned()
                    .critical("expected a file path")
                    .then(get_context::<ParserContext>())
                    .and_then_or_critical(move |(path, ctx)| {
                        (ctx.load_file)(path.data, path.at.start.file_id).map_err(Cow::from)
                    })
                    .then(custom(|input| {
                        Ok(Span::ate(input.range(0), input.ctx().unwrap()))
                    }))
                    .and_then(move |(file, ctx_fn)| {
                        PROGRAM.static_ref().parse(&mut ParserInput::new_with_ctx(
                            &file.content,
                            FileId::SourceFile(file.id),
                            ctx_fn,
                        ))
                    })
                    .map(|program| program.data),
            )
            .map(Instruction::Include),
        //
        // Command calls
        //
        CMD_CALL.static_ref().spanned().map(Instruction::CmdCall),
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
    )
    .erase_type()
});
