use parsy::Eaten;
use reshell_parser::ast::PropAccessNature;

use crate::{
    context::Context,
    errors::ExecResult,
    expr::eval_expr,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::RuntimeValue,
};

pub fn eval_props_access<'ast, T>(
    left: &mut RuntimeValue,
    accesses: impl ExactSizeIterator<Item = &'ast Eaten<PropAccessNature>>,
    policy: PropAccessPolicy,
    ctx: &mut Context,
    finalize: impl FnOnce(&mut RuntimeValue, &mut Context) -> T,
) -> ExecResult<T> {
    if accesses.len() == 0 {
        return Ok(finalize(left, ctx));
    }

    let mut left = left.clone();

    let mut accesses = accesses.into_iter().peekable();

    while let Some(acc) = accesses.next() {
        let next_acc = accesses.peek();

        match &acc.data {
            PropAccessNature::Key(key_expr) => match left {
                RuntimeValue::List(list) => {
                    let index = match eval_expr(&key_expr.data, ctx)? {
                        RuntimeValue::Int(index) => index as usize,

                        value => {
                            return Err(ctx.error(
                                key_expr.at,
                                format!(
                                    "expected an index (integer), found a {}",
                                    value
                                        .get_type()
                                        .render_colored(ctx, PrettyPrintOptions::inline())
                                ),
                            ));
                        }
                    };

                    let mut list = list.write();

                    match list.get_mut(index) {
                        Some(value) => match next_acc {
                            Some(next_acc) => match value.is_primitive() {
                                false => left = value.clone(),
                                true => {
                                    return Err(
                                        ctx.error(next_acc.at, "left operand is a primitive")
                                    )
                                }
                            },
                            None => return Ok(finalize(value, ctx)),
                        },

                        None => {
                            return Err(
                                ctx.error(key_expr.at, format!("index '{index}' is out-of-bounds"))
                            );
                        }
                    }
                }

                RuntimeValue::Map(map) => {
                    let key = match eval_expr(&key_expr.data, ctx)? {
                        RuntimeValue::String(key) => key,
                        value => {
                            return Err(ctx.error(
                                key_expr.at,
                                format!(
                                    "expected a key (string), found a {}",
                                    value
                                        .get_type()
                                        .render_colored(ctx, PrettyPrintOptions::inline())
                                ),
                            ));
                        }
                    };

                    // TODO: HACK: find a more proper way to to that
                    if policy == PropAccessPolicy::TrailingAccessMayNotExist
                        && !map.read().contains_key(&key)
                    {
                        map.write().insert(key.clone(), RuntimeValue::Null);
                    }

                    let mut map = map.write();

                    match map.get_mut(&key) {
                        Some(value) => match next_acc {
                            Some(next_acc) => match value.is_primitive() {
                                false => left = value.clone(),
                                true => {
                                    return Err(
                                        ctx.error(next_acc.at, "left operand is a primitive")
                                    )
                                }
                            },
                            None => return Ok(finalize(value, ctx)),
                        },

                        None => {
                            return Err(ctx.error(key_expr.at, format!("key '{key}' was not found")))
                        }
                    }
                }

                _ => {
                    return Err(ctx.error(
                        acc.at,
                        format!(
                            "left operand is not a map nor a list, but a {}",
                            left.get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                    ));
                }
            },

            PropAccessNature::Prop(prop) => match left {
                RuntimeValue::Struct(content) => {
                    let mut map = content.write();

                    match map.get_mut(&prop.data) {
                        Some(value) => match next_acc {
                            Some(next_acc) => match value.is_primitive() {
                                false => left = value.clone(),
                                true => {
                                    return Err(
                                        ctx.error(next_acc.at, "left operand is a primitive")
                                    )
                                }
                            },
                            None => return Ok(finalize(value, ctx)),
                        },

                        None => {
                            return Err(ctx.error(
                                prop.at,
                                format!(
                                    "property '{}' does not exist on the provided object",
                                    prop.data
                                ),
                            ));
                        }
                    }
                }

                _ => {
                    return Err(ctx.error(
                        acc.at,
                        format!(
                            "left operand is not a struct, but a {}",
                            left.get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                    ));
                }
            },
        }
    }

    unreachable!()
}

#[derive(PartialEq, Eq)]
pub enum PropAccessPolicy {
    ExistingOnly,
    TrailingAccessMayNotExist,
}
