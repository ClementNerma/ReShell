use std::cmp::Ordering;

use indexmap::IndexMap;
use parsy::Span;
use reshell_parser::ast::{
    ArithmeticDoubleOp, ComputedString, ComputedStringPiece, DoubleOp, ElsIfExpr,
    EqualityCmpDoubleOp, Expr, ExprInner, ExprInnerChaining, ExprInnerContent, ExprOp, Function,
    ListItem, LiteralValue, LogicDoubleOp, MapItem, MapKey, MatchExprCase, OrderingCmpDoubleOp,
    PropAccess, Range, RangeBound, RuntimeCodeRange, SingleOp, SpreadValue, StructItem,
    TypeMatchExprCase, Value,
};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable, pretty_printable_string};

use crate::{
    cmd::capture_cmd_output,
    context::{Context, ScopeContent, ScopeVar},
    errors::{ExecError, ExecErrorNature, ExecInternalPropagation, ExecResult},
    functions::eval_fn_call,
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    props::{PropAccessMode, TailPropAccessPolicy, eval_props_access},
    typechecking::check_if_value_fits_type,
    values::{
        LocatedValue, NotComparableTypesErr, RangeValue, RuntimeFnBody, RuntimeFnSignature,
        RuntimeFnValue, RuntimeValue, are_values_equal, value_to_str,
    },
};

pub fn eval_expr(expr: &Expr, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let Expr { inner, right_ops } = &expr;

    eval_expr_ref(inner, right_ops, ctx)
}

fn eval_expr_ref(
    inner: &Span<ExprInner>,
    right_ops: &[ExprOp],
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    for precedence in (0..=5).rev() {
        let Some((pos, expr_op)) = right_ops
            .iter()
            .enumerate()
            .rfind(|(_, c)| operator_precedence(c) == precedence)
        else {
            continue;
        };

        if precedence == 1
            && let Some((p, _)) = right_ops
                .iter()
                .enumerate()
                .find(|(_, c)| operator_precedence(c) == 0)
            && p != pos
        {
            let op_at = match expr_op {
                ExprOp::DoubleOp { op, right_op: _ } => op.at,

                // Precedence 0 and 1 is only for arithmetic (double) operators
                ExprOp::TypeIs { right_op: _ } => unreachable!(),
            };

            return Err(ctx.error(op_at, "to avoid confusions, mixing some operators '*' and '/' with '+' or '-' is not allowed (use parenthesis instead)"));
        }

        let left = eval_expr_ref(inner, &right_ops[..pos], ctx)?;

        let result = match expr_op {
            ExprOp::DoubleOp { op, right_op } => apply_double_op(
                left,
                |ctx| eval_expr_ref(right_op, &right_ops[pos + 1..], ctx),
                *op,
                ctx,
            )?,

            ExprOp::TypeIs { right_op } => {
                RuntimeValue::Bool(check_if_value_fits_type(&left, &right_op.data, ctx))
            }
        };

        return Ok(result);
    }

    // We reach here only if there was no operator in the expression, so we just have to evaluate the inner
    assert!(right_ops.is_empty());

    eval_expr_inner(inner, ctx)
}

/// Apply a double operator (which is an operator taking two operands)
///
/// The 'right' value is lazily evaluated, which allows short-circuiting in some scenarios
/// e.g. if the left operand of an AND operator is `false`, we can short-circuit the right value
fn apply_double_op(
    left_val: RuntimeValue,
    right_val: impl FnOnce(&mut Context) -> ExecResult<RuntimeValue>,
    op: Span<DoubleOp>,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    if let RuntimeValue::Custom(_) = &left_val {
        return apply_double_op_on_custom_value(left_val, right_val, op, ctx);
    }

    let result = match op.data {
        DoubleOp::Arithmetic(inner_op) => {
            let right = right_val(ctx)?;

            let got_overflow = || {
                ctx.error(
                    op.at,
                    format!(
                        "overflow during arithmetic operation with operands: {} and {}",
                        left_val.display(ctx, PrettyPrintOptions::inline()),
                        right.display(ctx, PrettyPrintOptions::inline())
                    ),
                )
            };

            match (&left_val, &right) {
                (RuntimeValue::Int(left), RuntimeValue::Int(right)) => match inner_op {
                    ArithmeticDoubleOp::Add => {
                        RuntimeValue::Int(left.checked_add(*right).ok_or_else(got_overflow)?)
                    }

                    ArithmeticDoubleOp::Sub => {
                        RuntimeValue::Int(left.checked_sub(*right).ok_or_else(got_overflow)?)
                    }

                    ArithmeticDoubleOp::Mul => {
                        RuntimeValue::Int(left.checked_mul(*right).ok_or_else(got_overflow)?)
                    }

                    ArithmeticDoubleOp::Div => {
                        if *right == 0 {
                            return Err(ctx.error(op.at, "cannot divide by zero"));
                        }

                        RuntimeValue::Int(left.checked_div(*right).ok_or_else(got_overflow)?)
                    }

                    ArithmeticDoubleOp::Mod => RuntimeValue::Int(left % right),
                },

                (RuntimeValue::Float(left), RuntimeValue::Float(right)) => {
                    let try_op = |result: f64| {
                        if result.is_finite() {
                            Ok(RuntimeValue::Float(result))
                        } else {
                            Err(got_overflow())
                        }
                    };

                    match inner_op {
                        ArithmeticDoubleOp::Add => try_op(left + right)?,
                        ArithmeticDoubleOp::Sub => try_op(left - right)?,
                        ArithmeticDoubleOp::Mul => try_op(left * right)?,
                        ArithmeticDoubleOp::Div => {
                            if *right == 0.0 {
                                return Err(ctx.error(op.at, "cannot divide by zero"));
                            }

                            try_op(left / right)?
                        }

                        ArithmeticDoubleOp::Mod => RuntimeValue::Float(left % right),
                    }
                }

                (RuntimeValue::Int(_), RuntimeValue::Float(_)) => {
                    return Err(ctx.error(
                        op.at,
                        "left operand is an int but right operand is a float".to_string(),
                    ));
                }

                (RuntimeValue::Float(_), RuntimeValue::Int(_)) => {
                    return Err(ctx.error(
                        op.at,
                        "left operand is a float but right operand is an int".to_string(),
                    ));
                }

                (_, _) => return Err(not_applicable_on_pair_err(&left_val, op, &right, ctx)),
            }
        }

        DoubleOp::EqualityCmp(inner_op) => {
            let right = right_val(ctx)?;

            let result = match (&left_val, &right) {
                (RuntimeValue::Null, RuntimeValue::Null) => match inner_op {
                    EqualityCmpDoubleOp::Eq => true,
                    EqualityCmpDoubleOp::Neq => false,
                },

                (RuntimeValue::Null, _) | (_, RuntimeValue::Null) => match inner_op {
                    EqualityCmpDoubleOp::Eq => false,
                    EqualityCmpDoubleOp::Neq => true,
                },

                (RuntimeValue::Bool(left), RuntimeValue::Bool(right)) => match inner_op {
                    EqualityCmpDoubleOp::Eq => left == right,
                    EqualityCmpDoubleOp::Neq => left != right,
                },

                (RuntimeValue::Int(left), RuntimeValue::Int(right)) => match inner_op {
                    EqualityCmpDoubleOp::Eq => left == right,
                    EqualityCmpDoubleOp::Neq => left != right,
                },

                (RuntimeValue::Float(left), RuntimeValue::Float(right)) => match inner_op {
                    EqualityCmpDoubleOp::Eq => left == right,
                    EqualityCmpDoubleOp::Neq => left != right,
                },

                (RuntimeValue::String(left), RuntimeValue::String(right)) => match inner_op {
                    EqualityCmpDoubleOp::Eq => left == right,
                    EqualityCmpDoubleOp::Neq => left != right,
                },

                (_, _) => return Err(not_applicable_on_pair_err(&left_val, op, &right, ctx)),
            };

            RuntimeValue::Bool(result)
        }

        DoubleOp::OrderingCmp(inner_op) => {
            let right = right_val(ctx)?;

            let result = match (&left_val, &right) {
                (RuntimeValue::Int(left), RuntimeValue::Int(right)) => match inner_op {
                    OrderingCmpDoubleOp::Lt => left < right,
                    OrderingCmpDoubleOp::Lte => left <= right,
                    OrderingCmpDoubleOp::Gt => left > right,
                    OrderingCmpDoubleOp::Gte => left >= right,
                },

                (RuntimeValue::Float(left), RuntimeValue::Float(right)) => match inner_op {
                    OrderingCmpDoubleOp::Lt => left < right,
                    OrderingCmpDoubleOp::Lte => left <= right,
                    OrderingCmpDoubleOp::Gt => left > right,
                    OrderingCmpDoubleOp::Gte => left >= right,
                },

                (_, _) => return Err(not_applicable_on_pair_err(&left_val, op, &right, ctx)),
            };

            RuntimeValue::Bool(result)
        }

        DoubleOp::Logic(inner_op) => {
            let RuntimeValue::Bool(left) = &left_val else {
                return Err(not_applicable_on_value_err(&left_val, op, ctx));
            };

            let right = || {
                let right_val = right_val(ctx)?;

                let RuntimeValue::Bool(right) = right_val else {
                    return Err(not_applicable_on_pair_err(&left_val, op, &right_val, ctx));
                };

                Ok(right)
            };

            let result = match inner_op {
                LogicDoubleOp::And => *left && right()?,
                LogicDoubleOp::Or => *left || right()?,
            };

            RuntimeValue::Bool(result)
        }

        DoubleOp::NullFallback => match left_val {
            RuntimeValue::Null => right_val(ctx)?,
            _ => left_val,
        },
    };

    Ok(result)
}

fn apply_double_op_on_custom_value(
    left_val: RuntimeValue,
    right_val: impl FnOnce(&mut Context) -> ExecResult<RuntimeValue>,
    op: Span<DoubleOp>,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    let RuntimeValue::Custom(left) = &left_val else {
        unreachable!()
    };

    if let DoubleOp::NullFallback = op.data {
        return Ok(left_val);
    }

    let right_val = right_val(ctx)?;

    let RuntimeValue::Custom(right) = &right_val else {
        return Err(not_applicable_on_pair_err(&left_val, op, &right_val, ctx));
    };

    if left.typename() != right.typename() {
        return Err(not_applicable_on_pair_err(&left_val, op, &right_val, ctx));
    }

    let result = match op.data {
        DoubleOp::Arithmetic(_) | DoubleOp::Logic(_) => {
            return Err(not_applicable_on_pair_err(&left_val, op, &right_val, ctx));
        }

        DoubleOp::EqualityCmp(inner_op) => {
            if !left.supports_eq() {
                return Err(not_applicable_on_value_err(&left_val, op, ctx));
            }

            match inner_op {
                EqualityCmpDoubleOp::Eq => left.eq(&***right),
                EqualityCmpDoubleOp::Neq => !left.eq(&***right),
            }
        }

        DoubleOp::OrderingCmp(inner_op) => {
            if !left.supports_eq() {
                return Err(not_applicable_on_value_err(&left_val, op, ctx));
            }

            // Just to be sure
            assert!(left.supports_eq());

            let ord = left.ord(&***right);

            match inner_op {
                OrderingCmpDoubleOp::Lt => matches!(ord, Ordering::Less),
                OrderingCmpDoubleOp::Lte => matches!(ord, Ordering::Less | Ordering::Equal),
                OrderingCmpDoubleOp::Gt => matches!(ord, Ordering::Greater),
                OrderingCmpDoubleOp::Gte => matches!(ord, Ordering::Greater | Ordering::Equal),
            }
        }

        DoubleOp::NullFallback => unreachable!(),
    };

    Ok(RuntimeValue::Bool(result))
}

fn not_applicable_on_value_err(
    left: &RuntimeValue,
    op: Span<DoubleOp>,
    ctx: &Context,
) -> Box<ExecError> {
    ctx.error(
        op.at,
        format!(
            "cannot apply this operator on a {}",
            left.compute_type()
                .display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
        ),
    )
}

fn not_applicable_on_pair_err(
    left: &RuntimeValue,
    op: Span<DoubleOp>,
    right: &RuntimeValue,
    ctx: &Context,
) -> Box<ExecError> {
    ctx.error(
        op.at,
        format!(
            "cannot apply this operator on a pair of {} and {}",
            left.compute_type()
                .display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
            right
                .compute_type()
                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
        ),
    )
}

fn eval_expr_inner(inner: &Span<ExprInner>, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let ExprInner { content, chainings } = &inner.data;

    let mut left_val = eval_expr_inner_content(content, ctx)?;
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
    content: &Span<ExprInnerContent>,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    match &content.data {
        ExprInnerContent::SingleOp {
            op,
            right,
            right_chainings,
        } => {
            let mut right_val = eval_expr_inner_content(right, ctx)?;

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
                            right_val
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
            let cond_val =
                match eval_expr(&cond.data, ctx)? {
                    RuntimeValue::Bool(bool) => bool,
                    value => return Err(ctx.error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    )),
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
                            cond_val
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
                                match_on
                                    .compute_type()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                                case_value
                                    .compute_type()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ));
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

        ExprInnerContent::LoopContinue => Err(ctx.error(
            content.at,
            ExecErrorNature::InternalPropagation(ExecInternalPropagation::LoopContinuation),
        )),

        ExprInnerContent::LoopBreak => Err(ctx.error(
            content.at,
            ExecErrorNature::InternalPropagation(ExecInternalPropagation::LoopBreakage),
        )),
    }
}

fn eval_value(value: &Value, ctx: &mut Context) -> ExecResult<RuntimeValue> {
    let value = match value {
        Value::Null => RuntimeValue::Null,

        Value::Literal(lit) => eval_literal_value(lit),

        Value::ComputedString(computed_str) => {
            eval_computed_string(computed_str, ctx).map(RuntimeValue::String)?
        }

        Value::Range(range) => {
            let Range {
                from,
                to,
                include_last_value,
            } = &**range;

            RuntimeValue::Range(RangeValue {
                from: eval_range_bound(from, ctx)?,
                to: eval_range_bound(to, ctx)?,
                include_last_value: *include_last_value,
            })
        }

        Value::List(values) => {
            let mut list = Vec::with_capacity(values.len());

            for item in values {
                match item {
                    ListItem::Single(expr) => {
                        let value = eval_expr(expr, ctx)?;
                        list.push(value);
                    }

                    ListItem::Spread(spread_value) => {
                        let values = eval_list_spread_value(spread_value, ctx)?;
                        list.extend(values.read_promise_no_write().iter().cloned());
                    }
                }
            }

            RuntimeValue::List(GcCell::new(list))
        }

        Value::Map(members) => {
            let mut map = IndexMap::with_capacity(members.len());

            for item in members {
                match item {
                    MapItem::Single { key, value } => {
                        let eval_key = eval_map_key(key, ctx)?;

                        if map.contains_key(&eval_key) {
                            return Err(ctx.error(
                                key.at,
                                format!(
                                    "key {} appears twice in this map",
                                    pretty_printable_string(&eval_key)
                                        .display(&(), PrettyPrintOptions::inline())
                                ),
                            ));
                        }

                        let value = eval_expr(value, ctx)?;

                        map.insert(eval_key, value);
                    }

                    MapItem::Spread(spread_value) => {
                        let spread_map = eval_map_or_struct_spread_value(spread_value, ctx)?;

                        map.extend(
                            spread_map
                                .read_promise_no_write()
                                .iter()
                                .map(|(key, value)| (key.clone(), value.clone())),
                        );
                    }
                }
            }

            RuntimeValue::Map(GcCell::new(map))
        }

        Value::Struct(members) => {
            let mut obj = IndexMap::with_capacity(members.len());

            for item in members {
                match item {
                    StructItem::Single { field, value } => {
                        obj.insert(field.data.clone(), eval_expr(value, ctx)?);
                    }

                    StructItem::Spread(spread_value) => {
                        let spread_struct = eval_map_or_struct_spread_value(spread_value, ctx)?;

                        obj.extend(
                            spread_struct
                                .read_promise_no_write()
                                .iter()
                                .map(|(key, value)| (key.clone(), value.clone())),
                        );
                    }
                }
            }

            RuntimeValue::Struct(GcCell::new(obj))
        }

        Value::Variable(name) => ctx.get_visible_var(name).value.read(name.at).value.clone(),
        Value::FnAsValue(name) => RuntimeValue::Function(ctx.get_visible_fn_value(name)?.clone()),

        Value::FnCall(call) => {
            return Ok(
                eval_fn_call(call, None, ctx)?.map_or(RuntimeValue::Void, |ret_val| ret_val.value)
            );
        }

        Value::CmdOutput(capture) => RuntimeValue::String(capture_cmd_output(capture, ctx)?),

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
        ComputedStringPiece::Variable(var_name) => value_to_str(
            &ctx.get_visible_var(var_name).value.read(var_name.at).value,
            var_name.at,
            "only stringifyable variables can be used inside computable strings",
            ctx,
        ),
        ComputedStringPiece::Expr(expr) => value_to_str(
            &eval_expr(&expr.data, ctx)?,
            expr.at,
            "only stringifyable values can be used inside computable strings",
            ctx,
        ),
        ComputedStringPiece::CmdOutput(capture) => capture_cmd_output(capture, ctx),
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
                        .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            )),
        },
    }
}

fn eval_spread_value(
    spread_value: &Span<SpreadValue>,
    ctx: &mut Context,
) -> ExecResult<RuntimeValue> {
    match &spread_value.data {
        SpreadValue::Variable(var_name) => {
            let var = ctx.get_visible_var(var_name);
            Ok(var.value.read(var_name.at).value.clone())
        }

        SpreadValue::Expr(expr) => eval_expr(expr, ctx),
    }
}

pub fn eval_list_spread_value(
    spread_value: &Span<SpreadValue>,
    ctx: &mut Context,
) -> ExecResult<GcCell<Vec<RuntimeValue>>> {
    match eval_spread_value(spread_value, ctx)? {
        RuntimeValue::List(items) => Ok(items),

        value => Err(ctx.error(
            spread_value.at,
            format!(
                "expected a list to spread, found a {}",
                value
                    .compute_type()
                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
            ),
        )),
    }
}

fn eval_map_or_struct_spread_value(
    spread_value: &Span<SpreadValue>,
    ctx: &mut Context,
) -> ExecResult<GcCell<IndexMap<String, RuntimeValue>>> {
    match eval_spread_value(spread_value, ctx)? {
        RuntimeValue::Map(obj) | RuntimeValue::Struct(obj) => Ok(obj),

        value => Err(ctx.error(
            spread_value.at,
            format!(
                "expected a map or struct to spread, found a {}",
                value
                    .compute_type()
                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
            ),
        )),
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

pub fn eval_range_bound(range_bound: &RangeBound, ctx: &mut Context) -> ExecResult<i64> {
    let (value_at, value) = match range_bound {
        RangeBound::Literal(literal) => return Ok(*literal),
        RangeBound::Variable(var) => (
            var.at,
            ctx.get_visible_var(var)
                .value
                .read_promise_no_write()
                .value
                .clone(),
        ),
        RangeBound::Expr(expr) => (expr.at, eval_expr(&expr.data, ctx)?),
    };

    match value {
        RuntimeValue::Int(literal) => Ok(literal),

        _ => Err(ctx.error(
            value_at,
            format!(
                "expected an integer, found a: {}",
                value.display(ctx, PrettyPrintOptions::inline())
            ),
        )),
    }
}

fn operator_precedence(op: &ExprOp) -> u8 {
    match op {
        ExprOp::DoubleOp { op, right_op: _ } => match op.data {
            DoubleOp::Arithmetic(op) => match op {
                ArithmeticDoubleOp::Add | ArithmeticDoubleOp::Sub => 0,
                ArithmeticDoubleOp::Mul | ArithmeticDoubleOp::Div | ArithmeticDoubleOp::Mod => 1,
            },
            DoubleOp::Logic(_) => 5,
            DoubleOp::EqualityCmp(_) | DoubleOp::OrderingCmp(_) => 4,
            DoubleOp::NullFallback => 2,
        },
        ExprOp::TypeIs { right_op: _ } => 3,
    }
}
