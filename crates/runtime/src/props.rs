//!
//! Properties handling module.
//!
//! Provides utilities to handle properties access from structs and maps

use std::collections::hash_map::{Entry, VacantEntry};

use parsy::Eaten;
use reshell_parser::ast::PropAccessNature;
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{context::Context, errors::ExecResult, expr::eval_expr, values::RuntimeValue};

/// Evaluate a chain of properties access
///
/// `left`: the value to operate on
/// `accesses`: iterator providing the property accesses to perform
/// `policy`: how the tail property should be accessed
/// `ctx`: runtime context
/// `finalize`: callback taking a value that allows reading,
///             writing or creating the tail property based on the provided policy
pub fn eval_props_access<'ast, 'c, T>(
    left: &'c mut RuntimeValue,
    accesses: impl ExactSizeIterator<Item = &'ast Eaten<PropAccessNature>>,
    policy: TailPropAccessPolicy,
    ctx: &'c mut Context,
    finalize: impl FnOnce(PropAccessMode, &mut Context) -> T,
) -> ExecResult<T> {
    // Special handling for empty accesses
    // (which are the most common ; most expressions don't have properties chain)
    if accesses.len() == 0 {
        return Ok(finalize(
            match policy {
                TailPropAccessPolicy::Read => PropAccessMode::ReadExisting(left),
                TailPropAccessPolicy::Write(_) => PropAccessMode::WriteExisting(left),
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
                                TailPropAccessPolicy::Read => {
                                    return Ok(finalize(PropAccessMode::ReadExisting(value), ctx))
                                }

                                TailPropAccessPolicy::Write(_) => {
                                    drop(items);

                                    return Ok(finalize(
                                        PropAccessMode::WriteExisting(
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
                            TailPropAccessPolicy::Read => {
                                return Ok(finalize(PropAccessMode::ReadExisting(value), ctx))
                            }

                            TailPropAccessPolicy::Write(_) => {
                                drop(map_read);

                                let mut map = map.write(key_expr.at, ctx)?;

                                return Ok(finalize(
                                    PropAccessMode::WriteExisting(map.get_mut(&key).unwrap()),
                                    ctx,
                                ));
                            }
                        },

                        (None, Some(_)) => {
                            return Err(ctx.error(key_expr.at, format!("key '{key}' was not found")))
                        }

                        (None, None) => match policy {
                            TailPropAccessPolicy::Read
                            | TailPropAccessPolicy::Write(TailPropWritingPolicy::ExistingOnly) => {
                                return Err(
                                    ctx.error(key_expr.at, format!("key '{key}' was not found"))
                                )
                            }

                            TailPropAccessPolicy::Write(TailPropWritingPolicy::TailMayNotExist) => {
                                drop(map_read);

                                let mut map = map.write(key_expr.at, ctx)?;

                                let entry = match map.entry(key) {
                                    Entry::Occupied(_) => unreachable!(),
                                    Entry::Vacant(vacant) => vacant,
                                };

                                return Ok(finalize(PropAccessMode::Create(entry), ctx));
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
                            TailPropAccessPolicy::Read => {
                                return Ok(finalize(PropAccessMode::ReadExisting(value), ctx))
                            }

                            TailPropAccessPolicy::Write(_) => {
                                drop(obj_read);

                                let mut obj = obj.write(prop.at, ctx)?;

                                return Ok(finalize(
                                    PropAccessMode::WriteExisting(obj.get_mut(&prop.data).unwrap()),
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

/// How properties should be accessed
#[derive(PartialEq, Eq)]
pub enum TailPropAccessPolicy {
    /// Read the final property
    Read,

    /// Write the final property
    Write(TailPropWritingPolicy),
}

/// How the last property should be accessed
#[derive(PartialEq, Eq)]
pub enum TailPropWritingPolicy {
    /// Only allow existing tail properties
    ExistingOnly,

    /// Allow non-existing tail properties
    TailMayNotExist,
}

/// How to perform final access on a property chain.
///
/// Will be generated depending on the provided [`PropAccessPolicy`].
///
/// It will contain a value allowing for reading / writing / creating the tail property.
pub enum PropAccessMode<'c> {
    /// Read the value of an existing property
    ReadExisting(&'c RuntimeValue),

    /// Only allow writing existing properties
    WriteExisting(&'c mut RuntimeValue),

    /// Allow writing to existing properties or creating them if they don't exist
    Create(VacantEntry<'c, String, RuntimeValue>),
}
