use std::collections::{hash_map::Entry, HashMap};

use parsy::Eaten;
use reshell_parser::ast::{
    CmdArg, FnArg, FnArgNames, FnCall, FnCallArg, RuntimeCodeRange, RuntimeEaten, SingleValueType,
    ValueType,
};

use crate::{
    cmd::{eval_cmd_arg, CmdArgResult},
    context::{CallStackEntry, Context, ScopeContent, ScopeVar},
    errors::ExecResult,
    exec::{run_body_with_deps, InstrRet},
    expr::eval_expr,
    gc::GcCell,
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
        RuntimeCodeRange::CodeRange(call.at),
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
                let ParsedFnCallArg {
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
    mut call_args: FnPossibleCallArgs,
    fn_args: &[FnArg],
    ctx: &mut Context,
) -> ExecResult<HashMap<String, ParsedFnCallArg>> {
    let mut args: HashMap<String, ParsedFnCallArg> = HashMap::new();

    let mut positional_args = fn_args.iter().filter(|arg| !arg.names.is_flag());

    let mut opened_flag = None;
    let mut opened_rest = None;

    let args_len = match call_args {
        FnPossibleCallArgs::Parsed(parsed) => parsed.data.len(),
        FnPossibleCallArgs::Internal(ref args) => args.len(),
    };

    for arg_index in 0..args_len {
        let (call_arg, call_arg_at) = match call_args {
            FnPossibleCallArgs::Parsed(parsed) => {
                let arg = &parsed.data.get(arg_index).unwrap().data;

                (
                    FnPossibleCallArg::Parsed(arg),
                    RuntimeCodeRange::CodeRange(match arg {
                        FnCallArg::Expr(expr) => expr.at,
                        FnCallArg::CmdArg(cmd_arg) => cmd_arg.at,
                    }),
                )
            }

            FnPossibleCallArgs::Internal(ref mut args) => {
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
                                    .filter(|eaten| eaten.data() == long_flag)
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
                                    .filter(|eaten| *eaten.data() == short_flag)
                                    .is_some()
                            })
                        };

                        let flag = flag.ok_or_else(|| ctx.error(call_arg_at, "flag not found"))?;

                        match &flag.typ {
                            None => {
                                args.insert(
                                    fn_arg_var_name(flag),
                                    ParsedFnCallArg {
                                        decl_name_at: fn_arg_var_at(flag),
                                        arg_value_at: call_arg_at,
                                        value: RuntimeValue::Bool(true),
                                    },
                                );
                            }

                            Some(typ) => {
                                if is_type_bool(typ.data()) {
                                    args.insert(
                                        fn_arg_var_name(flag),
                                        ParsedFnCallArg {
                                            decl_name_at: fn_arg_var_at(flag),
                                            arg_value_at: call_arg_at,
                                            value: RuntimeValue::Bool(true),
                                        },
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
            if !check_if_single_type_fits_type(&arg_value.get_type(), expected_type.data(), ctx) {
                return Err(ctx.error(
                    call_arg_at,
                    format!(
                        "type mismatch for argument '{}': expected a {}, found a {}",
                        fn_arg_var_name(fn_arg),
                        expected_type
                            .data()
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
            ParsedFnCallArg {
                decl_name_at: fn_arg_var_at(fn_arg),
                arg_value_at: call_arg_at,
                value: arg_value,
            },
        );
    }

    if let Some((_, at)) = opened_flag {
        return Err(ctx.error(at, "value is missing for this flag".to_string()));
    }

    if let Some(arg) = positional_args.next() {
        if !arg.is_rest && !arg.is_optional {
            return Err(ctx.error(
                match call_args {
                    FnPossibleCallArgs::Parsed(parsed) => RuntimeCodeRange::CodeRange(parsed.at),
                    FnPossibleCallArgs::Internal(_) => RuntimeCodeRange::Internal,
                },
                format!(
                    "not enough arguments, missing value for argument '{}'",
                    fn_arg_var_name(arg)
                ),
            ));
        }
    }

    let is_native_fn = match func.body {
        RuntimeFnBody::Block(_) => false,
        RuntimeFnBody::Internal(_) => true,
    };

    for flag in fn_args.iter().filter(|arg| arg.names.is_flag()) {
        let arg_name = fn_arg_var_name(flag);

        if !is_native_fn {
            if let Entry::Vacant(entry) = args.entry(arg_name) {
                let value = if matches!(&flag.typ, Some(typ) if is_type_bool(typ.data())) {
                    RuntimeValue::Bool(false)
                } else if flag.is_optional {
                    RuntimeValue::Null
                } else {
                    return Err(ctx.error(
                        call_at,
                        format!(
                            "value for flag '{}' was not provided",
                            fn_arg_var_name(flag)
                        ),
                    ));
                };

                entry.insert(ParsedFnCallArg {
                    decl_name_at: fn_arg_var_at(flag),
                    arg_value_at: call_at,
                    value,
                });
            }
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
            match name {
                RuntimeEaten::Eaten(value) => value.data.clone(),
                RuntimeEaten::Internal(value) => value.clone(),
            },
            ParsedFnCallArg {
                decl_name_at: fn_arg_var_at(rest_arg),
                // TODO: improve (track positions in "opened_rest")
                arg_value_at: call_at,
                value: RuntimeValue::List(GcCell::new(match opened_rest {
                    Some(list) => list,
                    None => vec![],
                })),
            },
        );
    }

    Ok(args)
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

fn is_type_bool(typ: &ValueType) -> bool {
    match typ {
        ValueType::Single(maybe_eaten) => matches!(maybe_eaten.data(), SingleValueType::Bool),
        ValueType::Union(_) => false,
    }
}

pub enum FnPossibleCallArgs<'a> {
    Parsed(&'a Eaten<Vec<Eaten<FnCallArg>>>),
    Internal(Vec<LocatedValue>),
}

pub enum FnPossibleCallArg<'a> {
    Parsed(&'a FnCallArg),
    Direct(RuntimeValue),
}

pub struct ParsedFnCallArg {
    pub decl_name_at: RuntimeCodeRange,
    pub arg_value_at: RuntimeCodeRange,
    pub value: RuntimeValue,
}
