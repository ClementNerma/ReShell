use std::collections::{hash_map::Entry, HashMap};

use parsy::{CodeRange, Eaten, FileId, Location};
use reshell_parser::ast::{
    CmdArg, FnArg, FnArgNames, FnCall, FnCallArg, SingleValueType, ValueType,
};

use crate::{
    cmd::{eval_cmd_arg, CmdArgResult},
    context::{CallStackEntry, Context, ScopeContent, ScopeVar},
    errors::ExecResult,
    exec::{run_body_with_deps, InstrRet, InstrRetType},
    expr::eval_expr,
    gc::GcCell,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    typechecker::check_if_single_type_fits,
    values::{InternalFnCallData, LocatedValue, RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

pub fn call_fn(call: &Eaten<FnCall>, ctx: &mut Context) -> ExecResult<FnCallResult> {
    let func = if call.data.is_var_name {
        let var = ctx.get_visible_var(&call.data.name).ok_or_else(|| {
            ctx.error(
                call.data.name.at,
                "function variable was not found in scope",
            )
        })?;

        let var_value = var.value.read();

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
        call.at,
        &func,
        FnPossibleCallArgs::Parsed(&call.data.call_args),
        ctx,
    )
}

pub fn call_fn_value(
    call_at: CodeRange,
    func: &RuntimeFnValue,
    call_args: FnPossibleCallArgs,
    ctx: &mut Context,
) -> ExecResult<FnCallResult> {
    let args = parse_fn_call_args(call_at, call_args, &func.signature.args.data, ctx)?;

    let returned = match &func.body {
        RuntimeFnBody::Block(body) => {
            let mut scope_content = ScopeContent::new();

            for (name, loc_value) in args {
                scope_content.vars.insert(
                    name.data,
                    ScopeVar {
                        declared_at: name.at,
                        is_mut: false,
                        value: GcCell::new(Some(loc_value)),
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

            match instr_ret {
                Some(InstrRet { typ, from }) => match typ {
                    InstrRetType::ContinueLoop | InstrRetType::BreakLoop => {
                        return Err(ctx.error(from, "not in a loop"))
                    }
                    InstrRetType::FnReturn(value) => value,
                    InstrRetType::Thrown(value) => return Ok(FnCallResult::Thrown(value)),
                    InstrRetType::Exit => unreachable!(),
                },

                None => None,
            }
        }

        RuntimeFnBody::Internal(run) => run(InternalFnCallData { call_at, args, ctx })?,
    };

    if let Some(ret_type) = &func.signature.ret_type {
        let Some(ret_val) = &returned else {
            return Err(ctx.error(
                call_at,
                format!(
                    "function call did not return any value, was expected to return a {}",
                    ret_type
                        .data
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            ));
        };

        if !check_if_single_type_fits(&ret_val.value.get_type(), &ret_type.data, ctx)? {
            return Err(ctx.error(
                call_at,
                format!(
                    "function call returned a {}, was expected to return a {}",
                    ret_val
                        .value
                        .get_type()
                        .render_colored(ctx, PrettyPrintOptions::inline()),
                    ret_type
                        .data
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            ));
        }
    }

    Ok(FnCallResult::Success { returned })
}

fn parse_fn_call_args(
    call_at: CodeRange,
    mut call_args: FnPossibleCallArgs,
    fn_args: &[FnArg],
    ctx: &mut Context,
) -> ExecResult<HashMap<Eaten<String>, LocatedValue>> {
    let mut args = HashMap::new();

    let mut positional_args = fn_args.iter().filter(|arg| !arg.names.is_flag());

    let mut opened_flag = None;
    let mut opened_rest = None;

    let args_len = match call_args {
        FnPossibleCallArgs::Parsed(parsed) => parsed.data.len(),
        FnPossibleCallArgs::Direct { at: _, ref args } => args.len(),
    };

    for arg_index in 0..args_len {
        let (call_arg, call_arg_at) = match call_args {
            FnPossibleCallArgs::Parsed(parsed) => {
                let arg = &parsed.data.get(arg_index).unwrap().data;

                (
                    FnPossibleCallArg::Parsed(arg),
                    match arg {
                        FnCallArg::Expr(expr) => expr.at,
                        FnCallArg::CmdArg(cmd_arg) => cmd_arg.at,
                    },
                )
            }
            FnPossibleCallArgs::Direct {
                at: _,
                ref mut args,
            } => {
                let arg = args.remove(0);
                (FnPossibleCallArg::Direct(arg.value), arg.from)
            }
        };

        if let FnPossibleCallArg::Parsed(FnCallArg::CmdArg(cmd_arg)) = &call_arg {
            if let CmdArg::Raw(raw) = &cmd_arg.data {
                if !raw.data.chars().all(|c| c == '-') {
                    if let Some(without_first_dash) = raw.data.strip_prefix('-') {
                        // TODO: support for "--flag=<value>" syntax?

                        if let Some((_, at)) = opened_flag {
                            return Err(ctx.error(at, "value is missing for this flag".to_string()));
                        }

                        let flag = if let Some(long_flag) = raw.data.strip_prefix("--") {
                            fn_args.iter().find(|arg| {
                                arg.names
                                    .long_flag()
                                    .filter(|eaten| eaten.data == long_flag)
                                    .is_some()
                            })
                        } else {
                            let mut chars = without_first_dash.chars();

                            let short_flag = chars.next().unwrap();

                            if chars.next().is_some() {
                                return Err(ctx.error(
                                    call_arg_at,
                                    "expected a short flag (single character)",
                                ));
                            }

                            fn_args.iter().find(|arg| {
                                arg.names
                                    .short_flag()
                                    .filter(|eaten| eaten.data == short_flag)
                                    .is_some()
                            })
                        };

                        let flag = flag.ok_or_else(|| ctx.error(call_arg_at, "flag not found"))?;

                        match &flag.typ {
                            None => {
                                args.insert(
                                    fn_arg_var_name(flag),
                                    LocatedValue::new(RuntimeValue::Bool(true), call_arg_at),
                                );
                            }

                            Some(typ) => {
                                if is_type_bool(&typ.data) {
                                    args.insert(
                                        fn_arg_var_name(flag),
                                        LocatedValue::new(RuntimeValue::Bool(true), call_arg_at),
                                    );
                                } else {
                                    opened_flag = Some((flag, call_arg_at));
                                }
                            }
                        }

                        continue;
                    }
                }
            }
        }

        let fn_arg = if let Some((flag, _)) = opened_flag {
            opened_flag = None;
            Some(flag)
        } else if let Some(fn_arg) = positional_args.next() {
            if fn_arg.is_rest {
                opened_rest = Some(vec![]);
                None
            } else {
                Some(fn_arg)
            }
        } else {
            None
        };

        let Some(fn_arg) = fn_arg else {
            match opened_rest {
                None => return Err(ctx.error(call_arg_at, "too many arguments provided")),

                Some(ref mut opened_rest_values) => {
                    match call_arg {
                        FnPossibleCallArg::Parsed(parsed) => match parsed {
                            FnCallArg::Expr(expr) => {
                                opened_rest_values.push(eval_expr(&expr.data, ctx)?)
                            }
                            FnCallArg::CmdArg(cmd_arg) => match eval_cmd_arg(&cmd_arg.data, ctx)? {
                                CmdArgResult::Single(single) => opened_rest_values.push(single),
                                CmdArgResult::Spreaded(mut items) => {
                                    opened_rest_values.append(&mut items);
                                }
                            },
                        },
                        FnPossibleCallArg::Direct(loc_val) => opened_rest_values.push(loc_val),
                    }

                    continue;
                }
            }
        };

        let arg_value = match call_arg {
            FnPossibleCallArg::Parsed(parsed) => match parsed {
                FnCallArg::Expr(expr) => eval_expr(&expr.data, ctx)?,
                FnCallArg::CmdArg(cmd_arg) => match eval_cmd_arg(&cmd_arg.data, ctx)? {
                    CmdArgResult::Single(single) => single,
                    CmdArgResult::Spreaded(_) => {
                        return Err(ctx.error(
                            cmd_arg.at,
                            "spreads are not supported for functions outside of rest arguments",
                        ))
                    }
                },
            },
            FnPossibleCallArg::Direct(value) => value,
        };

        if let Some(expected_type) = &fn_arg.typ {
            if !check_if_single_type_fits(&arg_value.get_type(), &expected_type.data, ctx)? {
                return Err(ctx.error(
                    call_arg_at,
                    format!(
                        "type mismatch for argument '{}': expected a {}, found a {}",
                        fn_arg_var_name(fn_arg).data,
                        expected_type
                            .data
                            .render_colored(ctx, PrettyPrintOptions::inline()),
                        arg_value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ));
            }
        }

        args.insert(
            fn_arg_var_name(fn_arg),
            LocatedValue::new(arg_value, call_arg_at),
        );
    }

    if let Some((_, at)) = opened_flag {
        return Err(ctx.error(at, "value is missing for this flag".to_string()));
    }

    if let Some(arg) = positional_args.next() {
        if !arg.is_rest && !arg.is_optional {
            return Err(ctx.error(
                match call_args {
                    FnPossibleCallArgs::Parsed(parsed) => parsed.at,
                    FnPossibleCallArgs::Direct { at, args: _ } => at,
                },
                format!(
                    "not enough arguments, missing value for argument '{}'",
                    fn_arg_var_name(arg).data
                ),
            ));
        }
    }

    for flag in fn_args.iter().filter(|arg| arg.names.is_flag()) {
        let arg_name = fn_arg_var_name(flag);

        if let Entry::Vacant(e) = args.entry(arg_name) {
            let mut value = RuntimeValue::Null;

            if let Some(typ) = &flag.typ {
                if is_type_bool(&typ.data) {
                    value = RuntimeValue::Bool(false);
                } else if !flag.is_optional {
                    return Err(ctx.error(
                        call_at,
                        format!(
                            "value for flag '{}' was not provided",
                            fn_arg_var_name(flag).data
                        ),
                    ));
                }
            } else {
                value = RuntimeValue::Bool(false);
            }

            e.insert(LocatedValue::new(value, call_at));
        }
    }

    if let Some(rest_arg) = fn_args.iter().find(|arg| arg.is_rest) {
        let name = match &rest_arg.names {
            FnArgNames::Positional(name) => name.clone(),
            FnArgNames::ShortFlag(_)
            | FnArgNames::LongFlag(_)
            | FnArgNames::LongAndShortFlag { long: _, short: _ } => unreachable!(),
        };

        args.insert(
            name,
            LocatedValue {
                value: RuntimeValue::List(GcCell::new(match opened_rest {
                    Some(list) => list,
                    None => vec![],
                })),
                // TODO: improve (track positions in "opened_rest")
                from: call_at,
            },
        );
    }

    Ok(args)
}

fn fn_arg_var_name(arg: &FnArg) -> Eaten<String> {
    // TODO: performance
    match &arg.names {
        FnArgNames::Positional(name) => name.clone(),
        FnArgNames::ShortFlag(short) => short.map_ref(|c| c.to_string()),
        FnArgNames::LongFlag(long) => long.clone(),
        FnArgNames::LongAndShortFlag { long, short: _ } => long.clone(),
    }
}

fn is_type_bool(typ: &ValueType) -> bool {
    match typ {
        ValueType::Single(maybe_eaten) => matches!(maybe_eaten.data(), SingleValueType::Bool),
        ValueType::Union(_) => false,
    }
}

pub enum FnPossibleCallArgs<'a> {
    Parsed(&'a Eaten<Vec<Eaten<FnCallArg>>>),
    Direct {
        at: CodeRange,
        args: Vec<LocatedValue>,
    },
}

pub enum FnPossibleCallArg<'a> {
    Parsed(&'a FnCallArg),
    Direct(RuntimeValue),
}

pub enum FnCallResult {
    Success { returned: Option<LocatedValue> },
    Thrown(LocatedValue),
}
