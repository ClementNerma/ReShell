use std::collections::HashMap;

use parsy::Eaten;
use reshell_parser::ast::{
    ComputedString, ComputedStringPiece, DoubleOp, ElsIfExpr, EscapableChar, Expr, ExprInner,
    ExprInnerContent, ExprOp, Function, LiteralValue, PropAccess, RuntimeCodeRange, SingleOp,
    Value,
};

use crate::{
    cmd::run_cmd,
    context::{Context, ScopeContent, ScopeVar},
    display::value_to_str,
    errors::{ExecErrorNature, ExecResult},
    functions::{eval_fn_call, eval_piped_fn_call},
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    props::{eval_props_access, PropAccessPolicy, PropAssignment},
    values::{
        are_values_equal, LocatedValue, NotComparableTypes, RuntimeFnBody, RuntimeFnSignature,
        RuntimeFnValue, RuntimeValue,
    },
};

pub fn eval_expr(expr: &Expr, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let Expr {
        inner,
        right_ops,
        method_calls,
    } = &expr;

    let expr_value = eval_expr_ref(inner, right_ops, ctx)?;

    let mut value = LocatedValue::new(
        expr_value,
        // TODO: location must cover inner + right_ops
        RuntimeCodeRange::Parsed(inner.at),
    );

    for call in method_calls {
        value = eval_piped_fn_call(call, Some(value), ctx)?
            .ok_or_else(|| ctx.error(call.at, "method call did not return a value"))?;
    }

    Ok(value.value)
}

fn eval_expr_ref(
    inner: &Eaten<ExprInner>,
    right_ops: &[ExprOp],
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    for precedence in (0..=4).rev() {
        let Some((pos, expr_op)) = right_ops
            .iter()
            .enumerate()
            .rfind(|(_, c)| operator_precedence(c.op.data) == precedence)
        else {
            continue;
        };

        if precedence == 1 {
            if let Some((p, _)) = right_ops
                .iter()
                .enumerate()
                .find(|(_, c)| operator_precedence(c.op.data) == 0)
            {
                if p != pos {
                    return Err(ctx.error(expr_op.op.at, "to avoid confusions, mixing operators '*' and '/' with '+' or '-' is not allowed (use parenthesis instead)"));
                }
            }
        }

        let left = eval_expr_ref(inner, &right_ops[..pos], ctx)?;
        let right = eval_expr_ref(&expr_op.with, &right_ops[pos + 1..], ctx)?;

        let result = apply_double_op(left, right, &expr_op.op, ctx)?;

        return Ok(result);
    }

    // We reach here only if there was no operator in the expression, so we just have to evaluate the inner
    assert!(right_ops.is_empty());
    eval_expr_inner(inner, ctx)
}

fn apply_double_op(
    left: RuntimeValue,
    right: RuntimeValue,
    op: &Eaten<DoubleOp>,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    let result = match op.data {
        DoubleOp::Add
        | DoubleOp::Sub
        | DoubleOp::Mul
        | DoubleOp::Div
        | DoubleOp::Lt
        | DoubleOp::Lte
        | DoubleOp::Gt
        | DoubleOp::Gte => match (&left, &right) {
            (RuntimeValue::Int(left), RuntimeValue::Int(right)) => match op.data {
                DoubleOp::Add => RuntimeValue::Int(left + right),
                DoubleOp::Sub => RuntimeValue::Int(left - right),
                DoubleOp::Mul => RuntimeValue::Int(left * right),
                DoubleOp::Div => RuntimeValue::Int(left / right),
                DoubleOp::Lt => RuntimeValue::Bool(left < right),
                DoubleOp::Lte => RuntimeValue::Bool(left <= right),
                DoubleOp::Gt => RuntimeValue::Bool(left > right),
                DoubleOp::Gte => RuntimeValue::Bool(left >= right),
                _ => unreachable!(),
            },

            (RuntimeValue::Float(left), RuntimeValue::Float(right)) => match op.data {
                DoubleOp::Add => RuntimeValue::Float(left + right),
                DoubleOp::Sub => RuntimeValue::Float(left - right),
                DoubleOp::Mul => RuntimeValue::Float(left * right),
                DoubleOp::Div => RuntimeValue::Float(left / right),
                DoubleOp::Lt => RuntimeValue::Bool(left < right),
                DoubleOp::Lte => RuntimeValue::Bool(left <= right),
                DoubleOp::Gt => RuntimeValue::Bool(left > right),
                DoubleOp::Gte => RuntimeValue::Bool(left >= right),
                _ => unreachable!(),
            },

            (RuntimeValue::Int(_), RuntimeValue::Float(_)) => {
                return Err(ctx.error(
                    op.at,
                    "left operand is an int but right operand is a float".to_string(),
                ))
            }

            (RuntimeValue::Float(_), RuntimeValue::Int(_)) => {
                return Err(ctx.error(
                    op.at,
                    "left operand is a float but right operand is an int".to_string(),
                ))
            }

            (_, _) => {
                return Err(ctx.error(
                    op.at,
                    format!(
                        "cannot apply this operator on a pair of {} and {}",
                        left.get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline()),
                        right
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ))
            }
        },

        DoubleOp::And | DoubleOp::Or => {
            let RuntimeValue::Bool(left) = left else {
                return Err(ctx.error(
                    op.at,
                    format!(
                        "left operand is not a boolean but a {}",
                        left.get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ));
            };

            let RuntimeValue::Bool(right) = right else {
                return Err(ctx.error(
                    op.at,
                    format!(
                        "right operand is not a boolean but a {}",
                        right
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ));
            };

            match op.data {
                DoubleOp::And => RuntimeValue::Bool(left && right),
                DoubleOp::Or => RuntimeValue::Bool(left || right),
                _ => unreachable!(),
            }
        }

        DoubleOp::Eq | DoubleOp::Neq => {
            let cmp =
                are_values_equal(&left, &right).map_err(|NotComparableTypes { reason }| {
                    ctx.error(
                        op.at,
                        format!(
                            "cannot compare {} and {}: {reason}",
                            left.get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline()),
                            right
                                .get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                    )
                })?;

            RuntimeValue::Bool(if op.data == DoubleOp::Eq { cmp } else { !cmp })
        }

        DoubleOp::NullFallback => {
            if matches!(left, RuntimeValue::Null) {
                right
            } else {
                left
            }
        }
    };

    Ok(result)
}

fn eval_expr_inner(inner: &Eaten<ExprInner>, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let ExprInner { content, prop_acc } = &inner.data;

    let mut left = eval_expr_inner_content(&content.data, ctx)?;

    for acc in prop_acc {
        let PropAccess { nature, nullable } = &acc.data;

        if *nullable && matches!(left, RuntimeValue::Null) {
            continue;
        }

        left = eval_props_access(
            &mut left,
            [nature].into_iter(),
            PropAccessPolicy::Read,
            ctx,
            |d, _| match d {
                PropAssignment::ReadExisting(d) => d.clone(),
                PropAssignment::WriteExisting(_) | PropAssignment::Create(_) => unreachable!(),
            },
        )?;
    }

    Ok(left)
}

fn eval_expr_inner_content(
    expr_inner_content: &ExprInnerContent,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    match &expr_inner_content {
        ExprInnerContent::SingleOp { op, right } => {
            let right_val = eval_expr_inner_content(&right.data, ctx)?;

            match op.data {
                SingleOp::Neg => match right_val {
                    RuntimeValue::Bool(bool) => Ok(RuntimeValue::Bool(!bool)),

                    _ => Err(ctx.error(
                        right.at,
                        format!(
                            "expected a boolean due to operator, found a: {}",
                            right_val
                                .get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                    )),
                },
            }
        }

        ExprInnerContent::ParenExpr(expr) => eval_expr(&expr.data, ctx),

        ExprInnerContent::Ternary {
            cond,
            body,
            elsif,
            els,
        } => {
            let cond_val =
                match eval_expr(&cond.data, ctx)? {
                    RuntimeValue::Bool(bool) => bool,
                    value => {
                        return Err(ctx.error(
                            cond.at,
                            format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value.get_type().render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                        ))
                    }
                };

            if cond_val {
                return eval_expr(&body.data, ctx);
            }

            for branch in elsif {
                let ElsIfExpr { cond, body } = &branch.data;

                let cond_val = eval_expr(&cond.data, ctx)?;
                let RuntimeValue::Bool(cond_val) = cond_val else {
                    return Err(ctx.error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            cond_val
                                .get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                    ));
                };

                if cond_val {
                    return eval_expr(&body.data, ctx);
                }
            }

            eval_expr(&els.data, ctx)
        }

        ExprInnerContent::Try {
            fn_call,
            catch_var,
            catch_expr,
        } => match eval_fn_call(fn_call, ctx) {
            Ok(returned) => returned
                .ok_or_else(|| ctx.error(fn_call.at, "function did not return a value"))
                .map(|loc_val| loc_val.value),

            Err(err) => match err.nature {
                ExecErrorNature::Thrown { value } => {
                    let mut scope = ScopeContent::new();

                    scope.vars.insert(
                        catch_var.data.clone(),
                        ScopeVar {
                            name_at: RuntimeCodeRange::Parsed(catch_var.at),
                            is_mut: false,
                            value: GcCell::new(Some(value)),
                        },
                    );

                    ctx.create_and_push_scope(RuntimeCodeRange::Parsed(catch_expr.at), scope);

                    let result = eval_expr(&catch_expr.data, ctx);

                    ctx.pop_scope();

                    result
                }

                _ => Err(err),
            },
        },

        ExprInnerContent::Value(value) => eval_value(value, ctx),
    }
}

fn eval_value(value: &Eaten<Value>, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    match &value.data {
        Value::Null => Ok(RuntimeValue::Null),

        Value::Literal(lit) => Ok(eval_literal_value(&lit.data)),

        Value::ComputedString(computed_str) => {
            eval_computed_string(computed_str, ctx).map(RuntimeValue::String)
        }

        Value::List(values) => Ok(RuntimeValue::List(GcCell::new(
            values
                .iter()
                .map(|expr| eval_expr(&expr.data, ctx))
                .collect::<Result<Vec<_>, _>>()?,
        ))),

        Value::Struct(obj) => {
            let members = obj
                .iter()
                .map(|(name, expr)| eval_expr(&expr.data, ctx).map(|value| (name.clone(), value)))
                .collect::<Result<HashMap<String, RuntimeValue>, _>>()?;

            Ok(RuntimeValue::Struct(GcCell::new(members)))
        }

        Value::Variable(name) => Ok(ctx
            .get_visible_var(name)
            .ok_or_else(|| ctx.error(name.at, "variable was not found"))?
            .value
            .read(name.at)
            .as_ref()
            .ok_or_else(|| {
                ctx.error(
                    name.at,
                    "trying to use variable before it is assigned a value",
                )
            })?
            .value
            .clone()),

        Value::FnAsValue(name) => Ok(RuntimeValue::Function(
            ctx.get_visible_fn_value(name)?.clone(),
        )),

        Value::FnCall(call) => eval_fn_call(call, ctx)?
            .map(|loc_val| loc_val.value)
            .ok_or_else(|| ctx.error(call.at, "function call did not return a value")),

        Value::CmdOutput(call) => Ok(RuntimeValue::String(run_cmd(call, ctx, true)?.0.unwrap())),

        Value::CmdSuccess(call) => match run_cmd(call, ctx, false) {
            Ok(_) => Ok(RuntimeValue::Bool(true)),
            Err(err) => match err.nature {
                ExecErrorNature::Custom(_)
                | ExecErrorNature::ParsingErr(_)
                | ExecErrorNature::Exit { code: _ }
                | ExecErrorNature::Thrown { value: _ } => Err(err),

                ExecErrorNature::CtrlC
                | ExecErrorNature::CommandFailedToStart { message: _ }
                | ExecErrorNature::CommandFailed {
                    message: _,
                    exit_status: _,
                } => Ok(RuntimeValue::Bool(false)),
            },
        },

        Value::Closure(Function { signature, body }) => Ok(RuntimeValue::Function(
            GcReadOnlyCell::new(RuntimeFnValue {
                signature: RuntimeFnSignature::Shared(
                    ctx.get_fn_signature(signature).unwrap_or_else(|| {
                        ctx.panic(signature.at, "unregistered function signature")
                    }),
                ),

                body: RuntimeFnBody::Block(
                    ctx.get_fn_body(body)
                        .unwrap_or_else(|| ctx.panic(body.at, "unregistered function body")),
                ),

                parent_scopes: ctx.generate_parent_scopes_list(),
                captured_deps: GcOnceCell::new_init(ctx.capture_deps(body.data.code_range)),
            }),
        )),
    }
}

pub fn eval_literal_value(value: &LiteralValue) -> RuntimeValue {
    match &value {
        LiteralValue::Boolean(bool) => RuntimeValue::Bool(*bool),
        LiteralValue::Integer(int) => RuntimeValue::Int(*int),
        LiteralValue::Float(float) => RuntimeValue::Float(*float),
    }
}

pub fn eval_computed_string(
    value: &Eaten<ComputedString>,
    ctx: &mut Context,
) -> ExecResult<String> {
    value
        .data
        .pieces
        .iter()
        .map(|piece| eval_computed_string_piece(piece, ctx))
        .collect::<Result<String, _>>()
}

fn eval_computed_string_piece(
    piece: &Eaten<ComputedStringPiece>,
    ctx: &mut Context,
) -> ExecResult<String> {
    match &piece.data {
        ComputedStringPiece::Literal(str) => Ok(str.clone()),
        ComputedStringPiece::Escaped(char) => Ok(match char {
            EscapableChar::Newline => '\n',
            EscapableChar::DoubleQuote => '"',
            EscapableChar::Backslash => '\\',
            EscapableChar::DollarSign => '$',
        }
        .to_string()),
        ComputedStringPiece::Variable(var_name) => Ok(value_to_str(
            &ctx.get_visible_var(var_name)
                .ok_or_else(|| ctx.error(var_name.at, "variable was not found"))?
                .value
                .read(var_name.at)
                .as_ref()
                .ok_or_else(|| {
                    ctx.error(
                        var_name.at,
                        "trying to use variable before it is assigned a value",
                    )
                })?
                .value,
            var_name.at,
            ctx,
        )?),
        ComputedStringPiece::Expr(expr) => {
            Ok(value_to_str(&eval_expr(&expr.data, ctx)?, expr.at, ctx)?)
        }
        ComputedStringPiece::CmdCall(call) => Ok(run_cmd(call, ctx, true)?.0.unwrap()),
    }
}

fn operator_precedence(op: DoubleOp) -> u8 {
    match op {
        DoubleOp::Add | DoubleOp::Sub => 0,
        DoubleOp::Mul | DoubleOp::Div => 1,
        DoubleOp::NullFallback => 2,
        DoubleOp::Eq
        | DoubleOp::Neq
        | DoubleOp::Lt
        | DoubleOp::Lte
        | DoubleOp::Gt
        | DoubleOp::Gte => 3,
        DoubleOp::And | DoubleOp::Or => 4,
    }
}
