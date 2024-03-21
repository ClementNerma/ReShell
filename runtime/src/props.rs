use parsy::Eaten;
use reshell_parser::ast::PropAccessNature;

use crate::{
    context::Context, display::readable_value_type, errors::ExecResult, expr::eval_expr,
    values::RuntimeValue,
};

pub fn eval_prop_access_nature<'a>(
    left: &'a mut RuntimeValue,
    nature: &Eaten<PropAccessNature>,
    policy: PropAccessPolicy,
    ctx: &mut Context,
) -> ExecResult<&'a mut RuntimeValue> {
    match &nature.data {
        PropAccessNature::Key(key_expr) => match left {
            RuntimeValue::List(list) => {
                let index = match eval_expr(&key_expr.data, ctx)? {
                    RuntimeValue::Int(index) => index as usize,

                    value => {
                        return Err(ctx.error(
                            key_expr.at,
                            format!(
                                "expected an index (integer), found a {}",
                                readable_value_type(&value, ctx)
                            ),
                        ))
                    }
                };

                Ok(list.get_mut(index).ok_or_else(|| {
                    ctx.error(key_expr.at, format!("index '{index}' is out-of-bounds"))
                })?)
            }

            RuntimeValue::Map(map) => {
                let key = match eval_expr(&key_expr.data, ctx)? {
                    RuntimeValue::String(key) => key,
                    value => {
                        return Err(ctx.error(
                            key_expr.at,
                            format!(
                                "expected a key (string), found a {}",
                                readable_value_type(&value, ctx)
                            ),
                        ))
                    }
                };

                // TODO: HACK: find a more proper way to to that
                if policy == PropAccessPolicy::TrailingAccessMayNotExist && !map.contains_key(&key)
                {
                    map.insert(key.clone(), RuntimeValue::Null);
                }

                Ok(map
                    .get_mut(&key)
                    .ok_or_else(|| ctx.error(key_expr.at, format!("key '{key}' was not found")))?)
            }

            _ => Err(ctx.error(
                nature.at,
                format!(
                    "left operand is not a map nor a list, but a {}",
                    readable_value_type(left, ctx)
                ),
            )),
        },

        PropAccessNature::Prop(key) => match left {
            RuntimeValue::Struct(content) => Ok(content.get_mut(&key.data).ok_or_else(|| {
                ctx.error(
                    key.at,
                    format!(
                        "property '{}' does not exist on the provided object",
                        key.data
                    ),
                )
            })?),

            _ => Err(ctx.error(
                nature.at,
                format!(
                    "left operand is not a struct, but a {}",
                    readable_value_type(left, ctx)
                ),
            )),
        },
    }
}

#[derive(PartialEq, Eq)]
pub enum PropAccessPolicy {
    ExistingOnly,
    TrailingAccessMayNotExist,
}
