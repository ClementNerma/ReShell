//!
//! Properties handling module.
//!
//! Provides utilities to handle properties access from structs and maps

use std::collections::hash_map::{Entry, VacantEntry};

use parsy::Eaten;
use reshell_parser::ast::PropAccessNature;

use crate::{
    context::Context,
    errors::ExecResult,
    expr::eval_expr,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::RuntimeValue,
};

/// How properties assignment should behave on a given target
pub enum PropAssignmentMode<'c> {
    /// Only allow reading existing properties
    OnlyReadExisting(&'c RuntimeValue),

    /// Only allow writing existing properties
    OnlyWriteExisting(&'c mut RuntimeValue),

    /// Allow writing to existing properties or creating them if they don't exist
    WriteExistingOrCreate(VacantEntry<'c, String, RuntimeValue>),
}

/// Evaluate a chain of properties access
pub fn eval_props_access<'ast, 'c, T>(
    left: &'c mut RuntimeValue,
    accesses: impl ExactSizeIterator<Item = &'ast Eaten<PropAccessNature>>,
    policy: PropAccessPolicy,
    ctx: &'c mut Context,
    finalize: impl FnOnce(PropAssignmentMode, &mut Context) -> T,
) -> ExecResult<T> {
    if accesses.len() == 0 {
        return Ok(finalize(
            match policy {
                PropAccessPolicy::Read => PropAssignmentMode::OnlyReadExisting(left),
                PropAccessPolicy::Write(_) => PropAssignmentMode::OnlyWriteExisting(left),
            },
            ctx,
        ));
    }

    let mut left = left.clone();

    let mut accesses = accesses.into_iter().peekable();

    while let Some(acc) = accesses.next() {
        let next_acc = accesses.peek();

        match &acc.data {
            PropAccessNature::Key(key_expr) => match left {
                RuntimeValue::List(list) => {
                    let index = match eval_expr(&key_expr.data, ctx)? {
                        RuntimeValue::Int(index) => usize::try_from(index).map_err(|_| {
                            ctx.error(key_expr.at, format!("got a negative index: {index}"))
                        })?,

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

                    let items = list.read(key_expr.at);

                    match items.get(index) {
                        Some(value) => match next_acc {
                            Some(next_acc) => match value.is_container() {
                                false => left = value.clone(),
                                true => {
                                    return Err(
                                        ctx.error(next_acc.at, "left operand is a primitive")
                                    )
                                }
                            },

                            None => match policy {
                                PropAccessPolicy::Read => {
                                    return Ok(finalize(
                                        PropAssignmentMode::OnlyReadExisting(value),
                                        ctx,
                                    ))
                                }

                                PropAccessPolicy::Write(_) => {
                                    drop(items);

                                    return Ok(finalize(
                                        PropAssignmentMode::OnlyWriteExisting(
                                            list.write(key_expr.at, ctx)?.get_mut(index).unwrap(),
                                        ),
                                        ctx,
                                    ));
                                }
                            },
                        },

                        None => {
                            return Err(
                                ctx.error(key_expr.at, format!("index '{index}' is out-of-bounds (list only contains {} elements)", items.len()))
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

                    let map_read = map.read(key_expr.at);
                    let value = map_read.get(&key);

                    match (value, next_acc) {
                        (Some(value), Some(next_acc)) => {
                            if !value.is_container() {
                                return Err(
                                    ctx.error(next_acc.at, "left operand is not a value container")
                                );
                            } else {
                                left = value.clone();
                            }
                        }

                        (Some(value), None) => match policy {
                            PropAccessPolicy::Read => {
                                return Ok(finalize(
                                    PropAssignmentMode::OnlyReadExisting(value),
                                    ctx,
                                ))
                            }

                            PropAccessPolicy::Write(_) => {
                                drop(map_read);

                                let mut map = map.write(key_expr.at, ctx)?;

                                return Ok(finalize(
                                    PropAssignmentMode::OnlyWriteExisting(
                                        map.get_mut(&key).unwrap(),
                                    ),
                                    ctx,
                                ));
                            }
                        },

                        (None, Some(_)) => {
                            return Err(ctx.error(key_expr.at, format!("key '{key}' was not found")))
                        }

                        (None, None) => match policy {
                            PropAccessPolicy::Read
                            | PropAccessPolicy::Write(PropAccessTailPolicy::ExistingOnly) => {
                                return Err(
                                    ctx.error(key_expr.at, format!("key '{key}' was not found"))
                                )
                            }

                            PropAccessPolicy::Write(PropAccessTailPolicy::TailMayNotExist) => {
                                drop(map_read);

                                let mut map = map.write(key_expr.at, ctx)?;

                                let entry = match map.entry(key) {
                                    Entry::Occupied(_) => unreachable!(),
                                    Entry::Vacant(vacant) => vacant,
                                };

                                return Ok(finalize(
                                    PropAssignmentMode::WriteExistingOrCreate(entry),
                                    ctx,
                                ));
                            }
                        },
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
                RuntimeValue::Struct(obj) => {
                    let obj_read = obj.read(prop.at);

                    let value = obj_read.get(&prop.data).ok_or_else(|| {
                        ctx.error(
                            prop.at,
                            format!("member '{}' was not found in structure", prop.data),
                        )
                    })?;

                    match next_acc {
                        Some(next_acc) => {
                            if !value.is_container() {
                                return Err(
                                    ctx.error(next_acc.at, "left operand is not a value container")
                                );
                            } else {
                                left = value.clone();
                            }
                        }

                        None => match policy {
                            PropAccessPolicy::Read => {
                                return Ok(finalize(
                                    PropAssignmentMode::OnlyReadExisting(value),
                                    ctx,
                                ))
                            }

                            PropAccessPolicy::Write(_) => {
                                drop(obj_read);

                                let mut obj = obj.write(prop.at, ctx)?;

                                return Ok(finalize(
                                    PropAssignmentMode::OnlyWriteExisting(
                                        obj.get_mut(&prop.data).unwrap(),
                                    ),
                                    ctx,
                                ));
                            }
                        },
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
    Read,
    Write(PropAccessTailPolicy),
}

#[derive(PartialEq, Eq)]
pub enum PropAccessTailPolicy {
    ExistingOnly,
    TailMayNotExist,
}
