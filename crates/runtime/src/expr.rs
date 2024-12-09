use indexmap::IndexMap;
use parsy::Span;
use reshell_parser::ast::{
    ComputedString, ComputedStringPiece, DoubleOp, ElsIfExpr, Expr, ExprInner, ExprInnerChaining,
    ExprInnerContent, ExprOp, Function, LiteralValue, MapKey, MatchExprCase, PropAccess,
    RangeBound, RuntimeCodeRange, SingleOp, TypeMatchExprCase, Value, ValueType,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    cmd::{run_cmd, CmdExecParams, CmdPipeCapture},
    context::{Context, ScopeContent, ScopeVar},
    errors::{ExecError, ExecErrorNature, ExecResult},
    functions::eval_fn_call,
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    pretty_impl::pretty_printable_string,
    props::{eval_props_access, PropAccessMode, TailPropAccessPolicy},
    typechecker::check_if_value_fits_type,
    values::{
        are_values_equal, value_to_str, LocatedValue, NotComparableTypesErr, RuntimeFnBody,
        RuntimeFnSignature, RuntimeFnValue, RuntimeValue,
    },
};

pub fn eval_expr(expr: &Expr, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let Expr {
        inner,
        right_ops,
        check_if_type_is,
    } = &expr;

    eval_expr_ref(inner, right_ops, check_if_type_is.as_ref(), ctx)
}

fn eval_expr_ref(
    inner: &Span<ExprInner>,
    right_ops: &[ExprOp],
    check_if_type_is: Option<&ValueType>,
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

        let left = eval_expr_ref(inner, &right_ops[..pos], None, ctx)?;

        let right =
            |ctx: &'_ mut Context| eval_expr_ref(&expr_op.with, &right_ops[pos + 1..], None, ctx);

        let result = apply_double_op(left, right, &expr_op.op, ctx)?;

        return Ok(result);
    }

    // We reach here only if there was no operator in the expression, so we just have to evaluate the inner
    assert!(right_ops.is_empty());

    let inner = eval_expr_inner(inner, ctx)?;

    Ok(match check_if_type_is {
        Some(check_if_type_is) => {
            RuntimeValue::Bool(check_if_value_fits_type(&inner, check_if_type_is, ctx))
        }
        None => inner,
    })
}

/// Apply a double operator (which is an operator taking two operands)
///
/// The 'right' value is lazily evaluated, which allows short-circuiting in some scenarios
/// e.g. if the left operand of an AND operator is `false`, we can short-circuit the right value
fn apply_double_op(
    left: RuntimeValue,
    right: impl FnOnce(&mut Context) -> ExecResult<RuntimeValue>,
    op: &Span<DoubleOp>,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    let result = match op.data {
        DoubleOp::Add
        | DoubleOp::Sub
        | DoubleOp::Mul
        | DoubleOp::Div
        | DoubleOp::Mod
        | DoubleOp::Lt
        | DoubleOp::Lte
        | DoubleOp::Gt
        | DoubleOp::Gte => {
            let right = right(ctx)?;

            match (&left, &right) {
                (RuntimeValue::Int(left), RuntimeValue::Int(right)) => match op.data {
                    DoubleOp::Add => RuntimeValue::Int(left + right),
                    DoubleOp::Sub => RuntimeValue::Int(left - right),
                    DoubleOp::Mul => RuntimeValue::Int(left * right),
                    DoubleOp::Div => RuntimeValue::Int(left / right),
                    DoubleOp::Mod => RuntimeValue::Int(left % right),
                    DoubleOp::Lt => RuntimeValue::Bool(left < right),
                    DoubleOp::Lte => RuntimeValue::Bool(left <= right),
                    DoubleOp::Gt => RuntimeValue::Bool(left > right),
                    DoubleOp::Gte => RuntimeValue::Bool(left >= right),

                    DoubleOp::And
                    | DoubleOp::Or
                    | DoubleOp::Eq
                    | DoubleOp::Neq
                    | DoubleOp::NullFallback => unreachable!(),
                },

                (RuntimeValue::Float(left), RuntimeValue::Float(right)) => match op.data {
                    DoubleOp::Add => RuntimeValue::Float(left + right),
                    DoubleOp::Sub => RuntimeValue::Float(left - right),
                    DoubleOp::Mul => RuntimeValue::Float(left * right),
                    DoubleOp::Div => RuntimeValue::Float(left / right),
                    DoubleOp::Mod => RuntimeValue::Float(left % right),
                    DoubleOp::Lt => RuntimeValue::Bool(left < right),
                    DoubleOp::Lte => RuntimeValue::Bool(left <= right),
                    DoubleOp::Gt => RuntimeValue::Bool(left > right),
                    DoubleOp::Gte => RuntimeValue::Bool(left >= right),

                    DoubleOp::And
                    | DoubleOp::Or
                    | DoubleOp::Eq
                    | DoubleOp::Neq
                    | DoubleOp::NullFallback => unreachable!(),
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
                            left.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            ),
                            right.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    ))
                }
            }
        }

        DoubleOp::And => {
            let RuntimeValue::Bool(left) = left else {
                return Err(ctx.error(
                    op.at,
                    format!(
                        "left operand is not a boolean but a {}",
                        left.compute_type()
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ));
            };

            if !left {
                return Ok(RuntimeValue::Bool(false));
            }

            let right = right(ctx)?;

            let RuntimeValue::Bool(right) = right else {
                return Err(ctx.error(
                    op.at,
                    format!(
                        "right operand is not a boolean but a {}",
                        right
                            .compute_type()
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ));
            };

            RuntimeValue::Bool(right)
        }

        DoubleOp::Or => {
            let RuntimeValue::Bool(left) = left else {
                return Err(ctx.error(
                    op.at,
                    format!(
                        "left operand is not a boolean but a {}",
                        left.compute_type()
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ));
            };

            if left {
                return Ok(RuntimeValue::Bool(true));
            }

            let right = right(ctx)?;

            let RuntimeValue::Bool(right) = right else {
                return Err(ctx.error(
                    op.at,
                    format!(
                        "right operand is not a boolean but a {}",
                        right
                            .compute_type()
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ));
            };

            RuntimeValue::Bool(right)
        }

        DoubleOp::Eq | DoubleOp::Neq => {
            let right = right(ctx)?;

            let cmp =
                are_values_equal(&left, &right).map_err(|NotComparableTypesErr { reason }| {
                    ctx.error(
                        op.at,
                        format!(
                            "cannot compare {} and {}: {reason}",
                            left.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            ),
                            right.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    )
                })?;

            RuntimeValue::Bool(if op.data == DoubleOp::Eq { cmp } else { !cmp })
        }

        DoubleOp::NullFallback => {
            if matches!(left, RuntimeValue::Null) {
                right(ctx)?
            } else {
                left
            }
        }
    };

    Ok(result)
}

fn eval_expr_inner(inner: &Span<ExprInner>, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let ExprInner { content, chainings } = &inner.data;

    let mut left_val = eval_expr_inner_content(&content.data, ctx)?;
    let left_at = RuntimeCodeRange::Parsed(content.at);

    for chaining in chainings {
        left_val = eval_expr_inner_chaining(chaining, LocatedValue::new(left_at, left_val), ctx)?;

        // TODO: update location (left_at)
    }

    Ok(left_val)
}

fn eval_expr_inner_chaining(
    chaining: &ExprInnerChaining,
    mut left: LocatedValue,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    match &chaining {
        ExprInnerChaining::PropAccess(acc) => {
            let PropAccess { nature, nullable } = acc;

            if *nullable && matches!(left.value, RuntimeValue::Null) {
                return Ok(left.value);
            }

            let resolved = eval_props_access(
                &mut left.value,
                [nature].into_iter(),
                TailPropAccessPolicy::Read,
                ctx,
                |d, _| match d {
                    PropAccessMode::ReadExisting(d) => d.clone(),
                    PropAccessMode::WriteExisting(_) | PropAccessMode::Create(_) => {
                        unreachable!()
                    }
                },
            )?;

            Ok(resolved)
        }

        ExprInnerChaining::MethodCall(fn_call) => Ok(eval_fn_call(fn_call, Some(left), ctx)?
            .map_or(RuntimeValue::Void, |ret_val| ret_val.value)),
    }
}

fn eval_expr_inner_content(
    expr_inner_content: &ExprInnerContent,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    match &expr_inner_content {
        ExprInnerContent::SingleOp {
            op,
            right,
            right_chainings,
        } => {
            let mut right_val = eval_expr_inner_content(&right.data, ctx)?;

            let right_at = RuntimeCodeRange::Parsed(right.at);

            for chaining in right_chainings {
                right_val = eval_expr_inner_chaining(
                    &chaining.data,
                    LocatedValue::new(right_at, right_val),
                    ctx,
                )?;

                // TODO: update location (right_at)
            }

            match op {
                SingleOp::Neg => match right_val {
                    RuntimeValue::Bool(bool) => Ok(RuntimeValue::Bool(!bool)),

                    _ => Err(ctx.error(
                        right.at,
                        format!(
                            "expected a boolean due to operator, found a: {}",
                            right_val.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    )),
                },
            }
        }

        ExprInnerContent::ParenExpr(expr) => eval_expr(expr, ctx),

        ExprInnerContent::Ternary {
            cond,
            body,
            elsif,
            els,
        } => {
            let cond_val = match eval_expr(&cond.data, ctx)? {
                RuntimeValue::Bool(bool) => bool,
                value => {
                    return Err(ctx.error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    ))
                }
            };

            if cond_val {
                return eval_expr(body, ctx);
            }

            for branch in elsif {
                let ElsIfExpr { cond, body } = branch;

                let cond_val = eval_expr(&cond.data, ctx)?;

                let RuntimeValue::Bool(cond_val) = cond_val else {
                    return Err(ctx.error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            cond_val.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    ));
                };

                if cond_val {
                    return eval_expr(body, ctx);
                }
            }

            eval_expr(els, ctx)
        }

        ExprInnerContent::Match { expr, cases, els } => {
            let match_on = eval_expr(expr, ctx)?;

            for MatchExprCase { matches, then } in cases {
                let case_value = eval_expr(&matches.data, ctx)?;

                let cmp = are_values_equal(&match_on, &case_value).map_err(
                    |NotComparableTypesErr { reason }| {
                        ctx.error(
                            matches.at,
                            format!(
                                "cannot compare {} and {}: {reason}",
                                match_on.compute_type().render_colored(
                                    ctx.type_alias_store(),
                                    PrettyPrintOptions::inline()
                                ),
                                case_value.compute_type().render_colored(
                                    ctx.type_alias_store(),
                                    PrettyPrintOptions::inline()
                                )
                            ),
                        )
                    },
                )?;

                if cmp {
                    return eval_expr(then, ctx);
                }
            }

            eval_expr(els, ctx)
        }

        ExprInnerContent::TypeMatch { expr, cases, els } => {
            let match_on = eval_expr(expr, ctx)?;

            for TypeMatchExprCase { matches, then } in cases {
                if check_if_value_fits_type(&match_on, matches, ctx) {
                    return eval_expr(then, ctx);
                }
            }

            eval_expr(els, ctx)
        }

        ExprInnerContent::Try {
            try_expr,
            catch_var,
            catch_expr,
            catch_expr_scope_id,
        } => eval_expr(try_expr, ctx).or_else(|err| match err.nature {
            ExecErrorNature::Thrown { at, message } => {
                let mut scope = ScopeContent::new();

                scope.vars.insert(
                    catch_var.data.clone(),
                    ScopeVar {
                        name_at: RuntimeCodeRange::Parsed(catch_var.at),
                        decl_scope_id: *catch_expr_scope_id,
                        is_mut: false,
                        enforced_type: None,
                        value: GcCell::new(LocatedValue::new(at, RuntimeValue::String(message))),
                    },
                );

                ctx.create_and_push_scope(*catch_expr_scope_id, scope);

                let result = eval_expr(catch_expr, ctx);

                ctx.pop_scope();

                result
            }

            _ => Err(err),
        }),

        ExprInnerContent::FnAsValue(name) => ctx
            .get_visible_fn_value(name)
            .map(|func| RuntimeValue::Function(GcReadOnlyCell::clone(func))),

        ExprInnerContent::Throw(expr) => {
            let message = match eval_expr(&expr.data, ctx)? {
                RuntimeValue::String(string) => string,
                value => {
                    return Err(ctx.error(
                        expr.at,
                        format!(
                            "expected a string, found a {}",
                            value.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    ))
                }
            };

            Err(ctx.error(
                expr.at,
                ExecErrorNature::Thrown {
                    at: RuntimeCodeRange::Parsed(expr.at),
                    message,
                },
            ))
        }

        ExprInnerContent::Value(value) => eval_value(value, ctx),
    }
}

fn eval_value(value: &Value, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let value = match value {
        Value::Null => RuntimeValue::Null,

        Value::Literal(lit) => eval_literal_value(lit),

        Value::ComputedString(computed_str) => {
            eval_computed_string(computed_str, ctx).map(RuntimeValue::String)?
        }

        Value::List(values) => RuntimeValue::List(GcCell::new(
            values
                .iter()
                .map(|expr| eval_expr(&expr.data, ctx))
                .collect::<Result<Vec<_>, _>>()?,
        )),

        Value::Map(members) => {
            let mut map = IndexMap::with_capacity(members.len());

            for (key, value) in members {
                let key_at = key.at;

                let key = eval_map_key(key, ctx)?;

                if map.contains_key(&key) {
                    return Err(ctx.error(
                        key_at,
                        format!(
                            "key {} appears twice in this map",
                            pretty_printable_string(&key)
                                .render_colored(&(), PrettyPrintOptions::inline())
                        ),
                    ));
                }

                let value = eval_expr(value, ctx)?;

                map.insert(key, value);
            }

            RuntimeValue::Map(GcCell::new(map))
        }

        Value::Struct(obj) => {
            let members = obj
                .iter()
                .map(|(field, value)| {
                    let result = eval_expr(value, ctx)?;

                    Ok::<_, Box<ExecError>>((field.data.clone(), result))
                })
                .collect::<Result<IndexMap<_, _>, _>>()?;

            RuntimeValue::Struct(GcCell::new(members))
        }

        Value::Variable(name) => ctx.get_visible_var(name).value.read(name.at).value.clone(),
        Value::FnAsValue(name) => RuntimeValue::Function(ctx.get_visible_fn_value(name)?.clone()),

        Value::FnCall(call) => {
            return Ok(
                eval_fn_call(call, None, ctx)?.map_or(RuntimeValue::Void, |ret_val| ret_val.value)
            )
        }

        Value::CmdOutput(call) => RuntimeValue::String(
            run_cmd(
                call,
                ctx,
                CmdExecParams {
                    capture: Some(CmdPipeCapture::Stdout),
                    silent: false,
                },
            )?
            .as_captured()
            .unwrap(),
        ),

        Value::CmdCall(call) => RuntimeValue::CmdCall {
            content_at: call.at,
        },

        Value::Lambda(func) => lambda_to_value(func, ctx),
    };

    Ok(value)
}

pub fn eval_literal_value(value: &LiteralValue) -> RuntimeValue {
    match &value {
        LiteralValue::Boolean(bool) => RuntimeValue::Bool(*bool),
        LiteralValue::Integer(int) => RuntimeValue::Int(*int),
        LiteralValue::Float(float) => RuntimeValue::Float(*float),
        LiteralValue::String(string) => RuntimeValue::String(
            // TODO: performance?
            string.clone(),
        ),
    }
}

pub fn eval_computed_string(value: &ComputedString, ctx: &mut Context) -> ExecResult<String> {
    value
        .pieces
        .iter()
        .map(|piece| eval_computed_string_piece(piece, ctx))
        .collect::<Result<String, _>>()
}

fn eval_computed_string_piece(
    piece: &ComputedStringPiece,
    ctx: &mut Context,
) -> ExecResult<String> {
    match piece {
        ComputedStringPiece::Literal(str) => Ok(str.clone()),
        ComputedStringPiece::Escaped(char) => Ok(char.original_char().to_string()),
        ComputedStringPiece::Variable(var_name) => Ok(value_to_str(
            &ctx.get_visible_var(var_name).value.read(var_name.at).value,
            "only stringifyable variables can be used inside computable strings",
            var_name.at,
            ctx,
        )?),
        ComputedStringPiece::Expr(expr) => Ok(value_to_str(
            &eval_expr(&expr.data, ctx)?,
            "only stringifyable values can be used inside computable strings",
            expr.at,
            ctx,
        )?),
        ComputedStringPiece::CmdCall(call) => Ok(run_cmd(
            call,
            ctx,
            CmdExecParams {
                capture: Some(CmdPipeCapture::Stdout),
                silent: false,
            },
        )?
        .as_captured()
        .unwrap()),
    }
}

fn eval_map_key(key: &Span<MapKey>, ctx: &mut Context) -> ExecResult<String> {
    match &key.data {
        MapKey::Raw(str) => Ok(str.to_owned()),
        MapKey::LiteralString(str) => Ok(str.to_owned()),
        MapKey::ComputedString(computed_string) => eval_computed_string(computed_string, ctx),
        MapKey::Expr(expr) => match eval_expr(expr, ctx)? {
            RuntimeValue::String(str) => Ok(str),
            value => Err(ctx.error(
                key.at,
                format!(
                    "expected a string key for the map, got a: {}",
                    value
                        .compute_type()
                        .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            )),
        },
    }
}

pub fn lambda_to_value(lambda: &Function, ctx: &mut Context) -> RuntimeValue {
    let Function { signature, body } = &lambda;

    let signature = RuntimeFnSignature::Shared(ctx.get_fn_signature(signature));

    RuntimeValue::Function(GcReadOnlyCell::new(RuntimeFnValue {
        is_method: false,
        signature,
        body: RuntimeFnBody::Block(ctx.get_fn_body(body)),
        parent_scopes: ctx.generate_parent_scopes_list(),
        captured_deps: GcOnceCell::new_init(ctx.capture_deps(body.at, body.data.scope_id)),
    }))
}

pub fn eval_range_bound(range_bound: &Span<RangeBound>, ctx: &mut Context) -> ExecResult<i64> {
    let value = match &range_bound.data {
        RangeBound::Literal(literal) => RuntimeValue::Int(*literal),
        RangeBound::Variable(var) => ctx
            .get_visible_var(var)
            .value
            .read_promise_no_write()
            .value
            .clone(),
        RangeBound::Expr(expr) => eval_expr(&expr.data, ctx)?,
    };

    match value {
        RuntimeValue::Int(literal) => Ok(literal),
        _ => Err(ctx.error(
            range_bound.at,
            format!(
                "expected an integer, found a: {}",
                value.render_colored(ctx, PrettyPrintOptions::inline())
            ),
        )),
    }
}

fn operator_precedence(op: DoubleOp) -> u8 {
    match op {
        DoubleOp::Add | DoubleOp::Sub => 0,
        DoubleOp::Mul | DoubleOp::Div | DoubleOp::Mod => 1,
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
