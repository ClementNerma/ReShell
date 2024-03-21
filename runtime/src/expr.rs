use std::collections::HashMap;

use parsy::Eaten;
use reshell_parser::ast::{
    ComputedString, ComputedStringPiece, DoubleOp, Expr, ExprInner, ExprInnerContent, ExprOp,
    LiteralValue, PropAccess, SingleOp, Value,
};

use crate::{
    cmd::run_cmd,
    context::Context,
    display::{readable_value_type, value_to_str},
    errors::{ExecErrorContent, ExecResult},
    functions::call_fn,
    props::{eval_prop_access_suite, make_prop_access_suite, PropAccessPolicy},
    typechecker::check_if_single_type_fits_single,
    values::{RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

pub fn eval_expr(expr: &Expr, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let Expr { inner, right_ops } = expr;

    let mut left = eval_expr_inner(inner, ctx)?;

    // TODO: operators precedence
    let mut with_precedence = expr
        .right_ops
        .iter()
        .filter(|right_op| operator_precedence(right_op.op.data) > 0);

    let first = with_precedence.next();

    if let Some(first) = first {
        if expr
            .right_ops
            .iter()
            .any(|right_op| operator_precedence(right_op.op.data) == 0)
        {
            return Err(ctx.error(
                first.op.at,
                "cannot use operators with different precedences without parenthesis",
            ));
        }
    }

    if let Some(too_much) = with_precedence.next() {
        return Err(ctx.error(
            too_much.op.at,
            "cannot use two operators with precedence without parenthesis",
        ));
    }

    for expr_op in right_ops {
        left = eval_double_op(left, expr_op, ctx)?;
    }
    // ==========================

    Ok(left)
}

fn eval_double_op(
    left: RuntimeValue,
    ExprOp { op, with }: &ExprOp,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    let right = eval_expr_inner(with, ctx)?;

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
                        readable_value_type(&left, ctx),
                        readable_value_type(&right, ctx)
                    ),
                ))
            }
        },

        DoubleOp::And | DoubleOp::Or => {
            let RuntimeValue::Bool(left) = left else {
                return Err(ctx.error(op.at, format!("left operand is not a boolean but a {}", readable_value_type(&left, ctx))));
            };

            let RuntimeValue::Bool(right) = right else {
                return Err(ctx.error(op.at, format!("right operand is not a boolean but a {}", readable_value_type(&right, ctx))));
            };

            match op.data {
                DoubleOp::And => RuntimeValue::Bool(left && right),
                DoubleOp::Or => RuntimeValue::Bool(left || right),
                _ => unreachable!(),
            }
        }

        DoubleOp::Eq | DoubleOp::Neq => {
            let cmp = match (&left, &right) {
                (_, RuntimeValue::Null) => matches!(left, RuntimeValue::Null),
                (RuntimeValue::Null, _) => matches!(right, RuntimeValue::Null),
                (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => a == b,
                (RuntimeValue::Int(a), RuntimeValue::Int(b)) => a == b,
                (RuntimeValue::Float(a), RuntimeValue::Float(b)) => a == b, // TODO: comparing floats is a bad idea
                (RuntimeValue::String(a), RuntimeValue::String(b)) => a == b,
                _ => {
                    return Err(ctx.error(
                        op.at,
                        format!(
                            "cannot compare {} and {}",
                            readable_value_type(&left, ctx),
                            readable_value_type(&right, ctx)
                        ),
                    ))
                }
            };

            RuntimeValue::Bool(if op.data == DoubleOp::Eq { cmp } else { !cmp })
        }

        DoubleOp::NullFallback => match (left, right) {
            (RuntimeValue::Null, right) => right,
            (left, RuntimeValue::Null) => left,
            (left, right) => {
                if !check_if_single_type_fits_single(&left.get_type(), &right.get_type(), ctx)? {
                    todo!("Incompatible types");
                }

                left
            }
        },
    };

    Ok(result)
}

fn eval_expr_inner(inner: &Eaten<ExprInner>, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let ExprInner { content, prop_acc } = &inner.data;

    let mut left = eval_expr_inner_content(&content.data, ctx)?;
    let mut left = &mut left;

    for acc in prop_acc {
        let PropAccess { nature, nullable } = &acc.data;

        if *nullable && matches!(left, RuntimeValue::Null) {
            continue;
        }

        // TODO: this may be slow for such a widely-used function
        let suite = make_prop_access_suite([&nature.data].into_iter(), ctx)?;

        left = eval_prop_access_suite(
            left,
            [nature].into_iter(),
            suite,
            PropAccessPolicy::ExistingOnly,
        )
        .map_err(|err| err(ctx))?;
    }

    Ok(left.clone())
}

fn eval_expr_inner_content(
    expr_inner_content: &ExprInnerContent,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    match &expr_inner_content {
        ExprInnerContent::SingleOp { op, right } => {
            let right = eval_expr_inner_content(&right.data, ctx)?;

            match op.data {
                SingleOp::Neg => match right {
                    RuntimeValue::Bool(bool) => Ok(RuntimeValue::Bool(!bool)),
                    // TODO: improve error message
                    _ => Err(ctx.error(op.at, "right operand is not a boolean")),
                },
            }
        }

        ExprInnerContent::ParenExpr(expr) => eval_expr(&expr.data, ctx),

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
        Value::List(values) => Ok(RuntimeValue::List(
            values
                .iter()
                .map(|expr| eval_expr(&expr.data, ctx))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        Value::Object(obj) => {
            let props = obj
                .iter()
                .map(|(name, expr)| eval_expr(&expr.data, ctx).map(|value| (name.clone(), value)))
                .collect::<Result<HashMap<String, RuntimeValue>, _>>()?;

            Ok(RuntimeValue::Struct(props))
        }
        Value::Variable(name) => ctx.get_var_value(name).map(|value| value.clone()),
        Value::FnAsValue(name) => Ok(RuntimeValue::Function(ctx.get_fn_value(name)?.clone())),
        Value::FnCall(call) => Ok(call_fn(call, ctx)?
            .ok_or_else(|| ctx.error(value.at, "this function call didn't return any value"))?
            .value),
        Value::CmdOutput(call) => Ok(RuntimeValue::String(run_cmd(call, ctx, true)?.unwrap())),
        Value::CmdSuccess(call) => match run_cmd(call, ctx, false) {
            Ok(_) => Ok(RuntimeValue::Bool(true)),
            Err(err) => match err.content {
                ExecErrorContent::Str(_)
                | ExecErrorContent::String(_)
                | ExecErrorContent::ParsingErr(_) => Err(err),
                ExecErrorContent::CommandFailed {
                    message: _,
                    exit_status: _,
                } => Ok(RuntimeValue::Bool(false)),
            },
        },
        // TODO: performance
        Value::Closure { signature, body } => Ok(RuntimeValue::Function(RuntimeFnValue {
            signature: signature.clone(),
            body: RuntimeFnBody::Block(body.clone()),
        })),
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
        ComputedStringPiece::Literal(str) => Ok(str.data.to_owned()),
        ComputedStringPiece::Escaped(char) => Ok(char.data.to_string()),
        ComputedStringPiece::Variable(var_name) => Ok(value_to_str(
            ctx.get_var_value(var_name)?,
            var_name.at,
            ctx,
        )?),
        ComputedStringPiece::Expr(expr) => {
            Ok(value_to_str(&eval_expr(&expr.data, ctx)?, expr.at, ctx)?)
        }
        ComputedStringPiece::CmdCall(call) => Ok(run_cmd(call, ctx, true)?.unwrap()),
    }
}

fn operator_precedence(op: DoubleOp) -> u8 {
    match op {
        DoubleOp::Add => 0,
        DoubleOp::Sub => 0,
        DoubleOp::Mul => 1,
        DoubleOp::Div => 1,
        DoubleOp::And => 4,
        DoubleOp::Or => 4,
        DoubleOp::Eq => 3,
        DoubleOp::Neq => 3,
        DoubleOp::Lt => 3,
        DoubleOp::Lte => 3,
        DoubleOp::Gt => 3,
        DoubleOp::Gte => 3,
        DoubleOp::NullFallback => 2,
    }
}
