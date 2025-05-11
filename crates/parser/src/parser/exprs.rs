use parsy::{
    Parser,
    helpers::{
        char, choice, end, filter, just, lookahead, not, recursive_shared, silent_choice,
        to_define_shared,
    },
    timed::LazilyDefined,
};

use super::types::VALUE_TYPE;
use crate::{
    DELIMITER_CHARS,
    ast::{
        DoubleOp, ElsIfExpr, Expr, ExprInner, ExprInnerChaining, ExprInnerContent, ExprOp,
        MatchExprCase, PropAccess, PropAccessNature, SingleOp, TypeMatchExprCase,
    },
    parser::{
        blocks::generate_scope_id,
        values::{FN_CALL, VALUE},
    },
    use_basic_parsers,
};

pub static PROP_ACCESS_NATURE: LazilyDefined<PropAccessNature> = LazilyDefined::new(|| {
    use_basic_parsers!(ident, msnl);

    choice::<PropAccessNature, _>((
        char('.')
            .ignore_then(ident.spanned().critical("expected a property name"))
            .not_followed_by(char('('))
            .map(PropAccessNature::Prop),
        char('[')
            .not_followed_by(char(']'))
            .ignore_then(
                EXPR.static_ref()
                    .padded_by(msnl)
                    .spanned()
                    .critical("expected an expression"),
            )
            .map(Box::new)
            .then_ignore(char(']').critical_auto_msg())
            .map(PropAccessNature::Key),
    ))
    .erase_type()
});

pub static EXPR: LazilyDefined<Expr> = LazilyDefined::new(|| {
    use_basic_parsers!(s, msnl, ms, ident);

    recursive_shared::<Expr, _>(|expr| {
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

        let expr_inner_chaining = to_define_shared::<ExprInnerChaining>();

        let expr_inner_content = recursive_shared(|expr_inner_content| {
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
                            .ignore_then(
                                VALUE_TYPE.static_ref().critical("expected a type to match"),
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
                    .then(
                        ident
                            .validate_or_critical(
                                |name| name != "it" && name != "self",
                                "Cannot declare a variable with reserved name 'it' or 'self'",
                            )
                            .spanned()
                            .critical("expected a catch variable"),
                    )
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
                            catch_expr_scope_id: generate_scope_id(),
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
                VALUE.static_ref().map(ExprInnerContent::Value),
            ))
        });

        let prop_access = char('?')
            .or_not()
            .then(PROP_ACCESS_NATURE.static_ref().spanned())
            .map(|(nullable, nature)| PropAccess {
                nullable: nullable.is_some(),
                nature,
            });

        expr_inner_chaining.define(choice::<ExprInnerChaining, _>((
            msnl.ignore_then(lookahead(char('.')))
                .ignore_then(FN_CALL.static_ref())
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
                .ignore_then(VALUE_TYPE.static_ref().spanned())
                .map(|right_op| ExprOp::TypeIs { right_op }),
        ));

        expr_inner
            .spanned()
            .then(expr_op.repeated_into_vec())
            .map(|(inner, right_ops)| Expr { inner, right_ops })
    })
    .erase_type()
});
