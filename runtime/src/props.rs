use parsy::Eaten;
use reshell_parser::ast::PropAccessNature;

use crate::{
    context::Context,
    display::readable_value_type,
    errors::{ExecError, ExecResult},
    expr::eval_expr,
    values::RuntimeValue,
};

pub struct PropAccessSuite {
    inner: Vec<RuntimeValue>,
}

pub fn make_prop_access_suite<'a>(
    acc: impl Iterator<Item = &'a PropAccessNature>,
    ctx: &mut Context,
) -> ExecResult<PropAccessSuite> {
    let suite = acc
        .map(|acc| match acc {
            PropAccessNature::Key(expr) => eval_expr(&expr.data, ctx),
            PropAccessNature::Prop(str) => Ok(RuntimeValue::String(str.data.clone())),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(PropAccessSuite { inner: suite })
}

pub type WithCtxError = Box<dyn FnOnce(&Context) -> ExecError>;

pub fn eval_prop_access_suite<'a, 'b>(
    mut left: &'a mut RuntimeValue,
    acc: impl Iterator<Item = &'b Eaten<PropAccessNature>>,
    mut suite: PropAccessSuite,
    policy: PropAccessPolicy,
) -> Result<&'a mut RuntimeValue, WithCtxError> {
    for acc in acc {
        let computed = suite.inner.pop().unwrap();

        match &acc.data {
            PropAccessNature::Key(key_expr) => match left {
                RuntimeValue::List(list) => {
                    let index = match computed {
                        RuntimeValue::Int(index) => index as usize,

                        value => {
                            let at = key_expr.at;

                            return Err(Box::new(move |ctx| {
                                ctx.error(
                                    at,
                                    format!(
                                        "expected an index (integer), found a {}",
                                        readable_value_type(&value, ctx)
                                    ),
                                )
                            }));
                        }
                    };

                    match list.get_mut(index) {
                        Some(got) => left = got,
                        None => {
                            let at = key_expr.at;

                            return Err(Box::new(move |ctx| {
                                ctx.error(at, format!("index '{index}' is out-of-bounds"))
                            }));
                        }
                    }
                }

                RuntimeValue::Map(map) => {
                    let key = match computed {
                        RuntimeValue::String(key) => key,
                        value => {
                            let at = key_expr.at;

                            return Err(Box::new(move |ctx| {
                                ctx.error(
                                    at,
                                    format!(
                                        "expected a key (string), found a {}",
                                        readable_value_type(&value, ctx)
                                    ),
                                )
                            }));
                        }
                    };

                    // TODO: HACK: find a more proper way to to that
                    if policy == PropAccessPolicy::TrailingAccessMayNotExist
                        && !map.contains_key(&key)
                    {
                        map.insert(key.clone(), RuntimeValue::Null);
                    }

                    match map.get_mut(&key) {
                        Some(got) => left = got,
                        None => {
                            let at = key_expr.at;

                            return Err(Box::new(move |ctx| {
                                ctx.error(at, format!("key '{key}' was not found"))
                            }));
                        }
                    }
                }

                _ => {
                    // We can afford to .clone() here as errors are the end of the program anyway
                    let left = left.clone();
                    let at = acc.at;

                    return Err(Box::new(move |ctx| {
                        ctx.error(
                            at,
                            format!(
                                "left operand is not a map nor a list, but a {}",
                                readable_value_type(&left, ctx)
                            ),
                        )
                    }));
                }
            },

            PropAccessNature::Prop(prop) => match left {
                RuntimeValue::Struct(content) => match content.get_mut(&prop.data) {
                    Some(got) => left = got,
                    None => {
                        let prop = prop.clone();

                        return Err(Box::new(move |ctx| {
                            ctx.error(
                                prop.at,
                                format!(
                                    "property '{}' does not exist on the provided object",
                                    prop.data
                                ),
                            )
                        }));
                    }
                },

                _ => {
                    // We can afford to .clone() here as errors are the end of the program anyway
                    let left = left.clone();
                    let at = acc.at;

                    return Err(Box::new(move |ctx| {
                        ctx.error(
                            at,
                            format!(
                                "left operand is not a struct, but a {}",
                                readable_value_type(&left, ctx)
                            ),
                        )
                    }));
                }
            },
        }
    }

    assert!(suite.inner.is_empty());

    Ok(left)
}

#[derive(PartialEq, Eq)]
pub enum PropAccessPolicy {
    ExistingOnly,
    TrailingAccessMayNotExist,
}
