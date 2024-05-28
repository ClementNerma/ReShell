use std::collections::HashMap;

use parsy::Eaten;
use reshell_parser::ast::{
    CmdFlagArg, CmdFlagNameArg, CmdFlagValueArg, FlagValueSeparator, FnArg, FnArgNames, FnCall,
    FnCallArg, RuntimeCodeRange, RuntimeEaten, SingleValueType, ValueType,
};

use crate::{
    cmd::{
        eval_cmd_arg, eval_cmd_value_making_arg, CmdArgResult, CmdSingleArgResult,
        FlagArgValueResult,
    },
    context::{CallStackEntry, Context, ScopeContent, ScopeVar},
    errors::ExecResult,
    exec::{run_body_with_deps, InstrRet},
    expr::eval_expr,
    gc::{GcCell, GcReadOnlyCell},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    typechecker::check_if_single_type_fits_type,
    values::{InternalFnCallData, LocatedValue, RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

pub fn eval_fn_call(call: &Eaten<FnCall>, ctx: &mut Context) -> ExecResult<Option<LocatedValue>> {
    let func = if call.data.is_var_name {
        let var = ctx.get_visible_var(&call.data.name).ok_or_else(|| {
            ctx.error(
                call.data.name.at,
                "function variable was not found in scope",
            )
        })?;

        let var_value = var.value.read(call.data.name.at);

        let loc_val = var_value.as_ref().ok_or_else(|| {
            ctx.error(
                call.data.name.at,
                "calling variable before it is assigned a value",
            )
        })?;

        match &loc_val.value {
            RuntimeValue::Function(func) => func.clone(),
            value => {
                return Err(ctx.error(
                    call.data.name.at,
                    format!(
                        "expected a function, found a {} instead",
                        value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ))
            }
        }
    } else {
        ctx.get_visible_fn_value(&call.data.name)?.clone()
    };

    call_fn_value(
        RuntimeCodeRange::Parsed(call.at),
        &func,
        FnPossibleCallArgs::Parsed(&call.data.call_args),
        ctx,
    )
}

pub fn call_fn_value(
    call_at: RuntimeCodeRange,
    func: &RuntimeFnValue,
    call_args: FnPossibleCallArgs,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    let args = parse_fn_call_args(
        call_at,
        func,
        call_args,
        func.signature.inner().args.data(),
        ctx,
    )?;

    let returned = match &func.body {
        RuntimeFnBody::Block(body) => {
            let mut scope_content = ScopeContent::new();

            for (name, parsed) in args {
                let ValidatedFnCallArg {
                    decl_name_at,
                    arg_value_at,
                    value,
                } = parsed;

                scope_content.vars.insert(
                    name,
                    ScopeVar {
                        name_at: decl_name_at,
                        is_mut: false,
                        value: GcCell::new(Some(LocatedValue::new(value, arg_value_at))),
                    },
                );
            }

            let instr_ret = run_body_with_deps(
                &body.data,
                func.captured_deps.clone(),
                ctx,
                scope_content,
                func.parent_scopes.clone(),
                Some(CallStackEntry {
                    fn_called_at: call_at,
                }),
            )?;

            instr_ret.and_then(|instr_ret| match instr_ret {
                InstrRet::ContinueLoop | InstrRet::BreakLoop => unreachable!(),
                InstrRet::FnReturn(value) => value,
            })
        }

        RuntimeFnBody::Internal(run) => run(InternalFnCallData { call_at, args, ctx })?,
    };

    if let Some(ret_type) = &func.signature.inner().ret_type {
        let Some(ret_val) = &returned else {
            return Err(ctx.error(
                call_at,
                format!(
                    "function call did not return any value, was expected to return a {}",
                    ret_type
                        .data()
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            ));
        };

        if !check_if_single_type_fits_type(&ret_val.value.get_type(), ret_type.data(), ctx) {
            return Err(ctx.error(
                call_at,
                format!(
                    "function call returned a {}, was expected to return a {}",
                    ret_val
                        .value
                        .get_type()
                        .render_colored(ctx, PrettyPrintOptions::inline()),
                    ret_type
                        .data()
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            ));
        }
    }

    Ok(returned)
}

fn parse_fn_call_args(
    call_at: RuntimeCodeRange,
    func: &RuntimeFnValue,
    call_args: FnPossibleCallArgs,
    fn_args: &[FnArg],
    ctx: &mut Context,
) -> ExecResult<HashMap<String, ValidatedFnCallArg>> {
    let mut call_args = flatten_fn_call_args(call_args, ctx)?;

    let mut out = HashMap::<String, ValidatedFnCallArg>::new();
    let mut rest_args = None::<Vec<CmdSingleArgResult>>;

    let mut positional_fn_args = fn_args.iter().filter(|fn_arg| match fn_arg.names {
        FnArgNames::Positional(_) => true,

        FnArgNames::ShortFlag(_)
        | FnArgNames::LongFlag(_)
        | FnArgNames::LongAndShortFlag { long: _, short: _ } => false,
    });

    let mut value_on_hold = None::<(RuntimeCodeRange, CmdSingleArgResult)>;

    loop {
        let (arg_result_at, arg_result) = match value_on_hold.take() {
            Some(value) => value,
            None => {
                if call_args.is_empty() {
                    break;
                }

                call_args.pop().unwrap()
            }
        };

        if let Some(ref mut rest) = rest_args {
            // rest.push((arg_result_at, arg_result));
            rest.push(arg_result);
            continue;
        }

        match arg_result {
            CmdSingleArgResult::RestSeparator => {
                let has_rest_arg = fn_args.iter().any(|arg| arg.is_rest);

                if !has_rest_arg {
                    return Err(ctx.error(
                        arg_result_at,
                        "called function does not have a rest argument",
                    ));
                }

                rest_args = Some(vec![]);
            }

            CmdSingleArgResult::Flag { name, value } => {
                let matching_arg = fn_args
                    .iter()
                    .find(|arg| match &name.data {
                        CmdFlagNameArg::Short(name) => arg
                            .names
                            .short_flag()
                            .is_some_and(|flag| flag.data() == name),

                        CmdFlagNameArg::Long(name) => arg
                            .names
                            .long_flag()
                            .is_some_and(|flag| flag.data() == name),
                    })
                    .ok_or_else(|| ctx.error(name.at, "called function does not have this flag"))?;

                match value {
                    None => {
                        if !matching_arg.is_optional {
                            if matching_arg.typ.is_none() {
                                out.insert(
                                    fn_arg_var_name(matching_arg),
                                    ValidatedFnCallArg {
                                        decl_name_at: fn_arg_var_at(matching_arg),
                                        arg_value_at: RuntimeCodeRange::Parsed(name.at),
                                        value: RuntimeValue::Bool(true),
                                    },
                                );
                            } else {
                                return Err(ctx.error(name.at, "this flag requires a value"));
                            }
                        }
                    }

                    Some(arg_value_result) => {
                        let FlagArgValueResult {
                            value: loc_val,
                            value_sep,
                        } = arg_value_result;

                        let typ = match &matching_arg.typ {
                            None => match value_sep {
                                FlagValueSeparator::Space => {
                                    value_on_hold =
                                        Some((loc_val.from, CmdSingleArgResult::Basic(loc_val)));

                                    continue;
                                }

                                FlagValueSeparator::Equal => {
                                    &RuntimeEaten::Internal(ValueType::Single(
                                        RuntimeEaten::Internal(SingleValueType::Bool),
                                    ))
                                }
                            },

                            Some(typ) => typ,
                        };

                        if !check_if_single_type_fits_type(
                            &loc_val.value.get_type(),
                            typ.data(),
                            ctx,
                        ) {
                            return Err(ctx.error(
                                loc_val.from,
                                format!(
                                    "type mismatch: expected {}, found {}",
                                    loc_val
                                        .value
                                        .get_type()
                                        .render_colored(ctx, PrettyPrintOptions::inline()),
                                    typ.data().render_colored(ctx, PrettyPrintOptions::inline())
                                ),
                            ));
                        }

                        out.insert(
                            fn_arg_var_name(matching_arg),
                            ValidatedFnCallArg {
                                decl_name_at: fn_arg_var_at(matching_arg),
                                arg_value_at: loc_val.from,
                                value: loc_val.value,
                            },
                        );
                    }
                }
            }

            CmdSingleArgResult::Basic(loc_val) => {
                let matching_arg = positional_fn_args
                    .next()
                    .ok_or_else(|| ctx.error(arg_result_at, "too many arguments provided"))?;

                if let Some(typ) = &matching_arg.typ {
                    if !check_if_single_type_fits_type(&loc_val.value.get_type(), typ.data(), ctx) {
                        return Err(ctx.error(
                            loc_val.from,
                            format!(
                                "type mismatch: expected {}, found {}",
                                loc_val
                                    .value
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline()),
                                typ.data().render_colored(ctx, PrettyPrintOptions::inline())
                            ),
                        ));
                    }
                }

                out.insert(
                    fn_arg_var_name(matching_arg),
                    ValidatedFnCallArg {
                        decl_name_at: fn_arg_var_at(matching_arg),
                        arg_value_at: loc_val.from,
                        value: loc_val.value,
                    },
                );
            }
        }
    }

    let is_native_fn = match func.body {
        RuntimeFnBody::Block(_) => false,
        RuntimeFnBody::Internal(_) => true,
    };

    for arg in fn_args {
        let arg_name = fn_arg_var_name(arg);

        if out.contains_key(&arg_name) {
            continue;
        }

        if arg.is_optional {
            if !is_native_fn {
                out.insert(
                    arg_name,
                    ValidatedFnCallArg {
                        decl_name_at: fn_arg_var_at(arg),
                        arg_value_at: RuntimeCodeRange::Internal,
                        value: RuntimeValue::Null,
                    },
                );
            }

            continue;
        }

        if arg.is_rest {
            out.insert(
                arg_name,
                ValidatedFnCallArg {
                    decl_name_at: fn_arg_var_at(arg),
                    arg_value_at: call_at,
                    value: RuntimeValue::ArgSpread(GcReadOnlyCell::new(
                        rest_args.take().unwrap_or_default(),
                    )),
                },
            );

            continue;
        }

        if arg.names.is_flag() && arg.typ.is_none() {
            out.insert(
                arg_name,
                ValidatedFnCallArg {
                    decl_name_at: fn_arg_var_at(arg),
                    arg_value_at: RuntimeCodeRange::Internal,
                    value: RuntimeValue::Bool(false),
                },
            );

            continue;
        }

        return Err(ctx.error(
            call_at,
            format!(
                "argument '{}' is missing",
                match &arg.names {
                    FnArgNames::Positional(name) => name.data().clone(),
                    FnArgNames::ShortFlag(short) => format!("-{}", short.data()),
                    FnArgNames::LongFlag(long) => format!("--{}", long.data()),
                    FnArgNames::LongAndShortFlag { long, short } =>
                        format!("-{} / --{}", short.data(), long.data()),
                }
            ),
        ));
    }

    Ok(out)
}

fn flatten_fn_call_args(
    call_args: FnPossibleCallArgs,
    ctx: &mut Context,
) -> ExecResult<Vec<(RuntimeCodeRange, CmdSingleArgResult)>> {
    let mut out = vec![];

    match call_args {
        FnPossibleCallArgs::Parsed(parsed) => {
            for parsed in &parsed.data {
                match &parsed.data {
                    FnCallArg::Expr(expr) => {
                        let eval = eval_expr(&expr.data, ctx)?;

                        out.push((
                            RuntimeCodeRange::Parsed(parsed.at),
                            CmdSingleArgResult::Basic(LocatedValue::new(
                                eval,
                                RuntimeCodeRange::Parsed(expr.at),
                            )),
                        ))
                    }

                    FnCallArg::Flag(flag) => {
                        let CmdFlagArg { name, value } = &flag.data;

                        let value = value
                            .as_ref()
                            .map(|value| {
                                let CmdFlagValueArg { value, value_sep } = value;

                                eval_cmd_value_making_arg(&value.data, ctx).map(|value| {
                                    FlagArgValueResult {
                                        value,
                                        value_sep: *value_sep,
                                    }
                                })
                            })
                            .transpose()?;

                        out.push((
                            RuntimeCodeRange::Parsed(flag.at),
                            CmdSingleArgResult::Flag {
                                name: name.clone(),
                                value,
                            },
                        ))
                    }

                    FnCallArg::CmdArg(arg) => match eval_cmd_arg(&arg.data, ctx)? {
                        CmdArgResult::Single(single) => {
                            out.push((RuntimeCodeRange::Parsed(arg.at), single))
                        }

                        CmdArgResult::Spreaded(items) => {
                            out.extend(
                                items
                                    .into_iter()
                                    .map(|item| (RuntimeCodeRange::Parsed(arg.at), item)),
                            );
                        }
                    },
                }
            }
        }

        FnPossibleCallArgs::Internal(args) => {
            for (arg_at, arg_result) in args {
                match arg_result {
                    CmdArgResult::Single(single) => out.push((arg_at, single)),

                    CmdArgResult::Spreaded(items) => {
                        out.extend(items.into_iter().map(|item| (arg_at, item)));
                    }
                }
            }
        }
    }

    Ok(out)
}

fn fn_arg_var_name(arg: &FnArg) -> String {
    match &arg.names {
        FnArgNames::Positional(name) => name.data().clone(),
        FnArgNames::ShortFlag(short) => short.data().to_string(),
        FnArgNames::LongFlag(long) => long.data().clone(),
        FnArgNames::LongAndShortFlag { long, short: _ } => long.data().clone(),
    }
}

fn fn_arg_var_at(arg: &FnArg) -> RuntimeCodeRange {
    match &arg.names {
        FnArgNames::Positional(name) => name.at(),
        FnArgNames::ShortFlag(short) => short.at(),
        FnArgNames::LongFlag(long) => long.at(),
        FnArgNames::LongAndShortFlag { long, short: _ } => long.at(),
    }
}

pub enum FnPossibleCallArgs<'a> {
    Parsed(&'a Eaten<Vec<Eaten<FnCallArg>>>),
    Internal(Vec<(RuntimeCodeRange, CmdArgResult)>),
}

pub struct ValidatedFnCallArg {
    pub decl_name_at: RuntimeCodeRange,
    pub arg_value_at: RuntimeCodeRange,
    pub value: RuntimeValue,
}
