use std::collections::HashMap;

use parsy::{Eaten, CodeRange};
use reshell_parser::ast::{
    CmdFlagNameArg,  FnArg, FnCall, FnCallArg,
    FnFlagArgNames, RuntimeCodeRange, FlagValueSeparator,
};

use crate::{
    cmd::{
        eval_cmd_arg,  CmdArgResult, CmdSingleArgResult,
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
    eval_fn_call_type(call, None, ctx)
}

pub fn eval_fn_call_type(call: &Eaten<FnCall>, call_type: Option<FnCallType>, ctx: &mut Context) -> ExecResult<Option<LocatedValue>> {
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
        FnPossibleCallArgs::Parsed { call_type, args: &call.data.call_args },
        ctx,
    )
}

pub fn call_fn_value(
    call_at: RuntimeCodeRange,
    func: &RuntimeFnValue,
    call_args: FnPossibleCallArgs,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    ctx.ensure_no_ctrl_c_press(call_at)?;

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

            let Some(captured_deps) = func.captured_deps.get().as_ref().cloned() else {
                return Err(ctx.error(call_at, "function called before its declaration"));
            };

            let instr_ret = run_body_with_deps(
                body,
                captured_deps.clone(),
                ctx,
                Some(scope_content),
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
    let is_method = func.signature.inner().args.data().first().is_some_and(|arg| matches!(arg, FnArg::Positional { name, is_optional: _, typ: _ } if name.data() == "self"));

    let mut call_args = flatten_fn_call_args(call_at, is_method, call_args, ctx)?;

    // Reverse call args so we can .pop() them! without shifting elements
    call_args.reverse();

    let mut out = HashMap::<String, ValidatedFnCallArg>::new();
    let mut rest_args = Vec::<CmdSingleArgResult>::new();

    let mut positional_fn_args = fn_args.iter().filter_map(|fn_arg| match fn_arg {
        FnArg::Positional {
            name,
            is_optional: _,
            typ,
        } => Some((name, typ)),

        FnArg::PresenceFlag { names: _ }
        | FnArg::NormalFlag {
            names: _,
            is_optional: _,
            typ: _,
        }
        | FnArg::Rest { name: _ } => None,
    });

    let mut value_on_hold = None::<CmdSingleArgResult>;

    let has_rest_argument = fn_args.iter().any(|arg| matches!(arg, FnArg::Rest { name: _ }));

    'iter: while let Some(arg_result) = value_on_hold.take().or_else(|| call_args.pop()) {
        if !rest_args.is_empty() {
            rest_args.push(arg_result);
            continue;
        }

        match arg_result {
            CmdSingleArgResult::Flag { name, value } => {
                for arg in fn_args {
                    match arg {
                        FnArg::PresenceFlag { names } => {
                            let Some(var_name) = get_matching_var_name(&name.data, names, ctx) else {
                                continue;
                            };

                            if let Some(FlagArgValueResult { value, value_sep }) = value {
                                // Skip the flag if was associated the `false` value using the `=` sign
                                
                                let mut is_true_with_eq_sign = false;

                                if value_sep == FlagValueSeparator::Equal {
                                    if let RuntimeValue::Bool(bool) = &value.value {
                                        if *bool {
                                            is_true_with_eq_sign = true;
                                        } else {
                                            continue 'iter;
                                        }
                                    }
                                }

                                // Just set the flag normally if it's the `true` value with the `=` sign
                                // (which is equivalent to not providing a value at all)
                                if !is_true_with_eq_sign {
                                    value_on_hold = Some(CmdSingleArgResult::Basic(value));
                                }
                            }

                            out.insert(
                                var_name,
                                ValidatedFnCallArg {
                                    decl_name_at: fn_arg_var_at(arg),
                                    arg_value_at: RuntimeCodeRange::Parsed(name.at),
                                    value: RuntimeValue::Bool(true),
                                },
                            );

                            continue 'iter;
                        }

                        FnArg::NormalFlag {
                            names,
                            is_optional,
                            typ,
                        } => {
                            let Some(var_name) = get_matching_var_name(&name.data, names, ctx) else {
                                continue;
                            };

                            match value {
                                None => {
                                    if *is_optional {
                                        out.insert(
                                            var_name,
                                            ValidatedFnCallArg {
                                                decl_name_at: fn_arg_var_at(arg),
                                                arg_value_at: RuntimeCodeRange::Parsed(name.at),
                                                value: RuntimeValue::Null,
                                            },
                                        );
                                    } else {
                                        return Err(ctx.error(name.at, match typ {
                                            None => "a value is expected for this flag".to_owned(),
                                            Some(typ) => format!("a value of type '{}' is expected for this flag", typ.data().render_colored(ctx, PrettyPrintOptions::inline())),
                                        }));
                                    }

                                }

                                Some(FlagArgValueResult { value, value_sep: _ }) => {
                                    if let Some(typ) = typ {
                                        if !check_if_single_type_fits_type(&value.value.get_type(), typ.data(), ctx) {
                                            return Err(ctx.error(value.from, 
                                                format!("expected a value of type '{}' for flag '{}', found '{}'", typ.data().render_colored(ctx, PrettyPrintOptions::inline()),
                                            names.render_colored(ctx, PrettyPrintOptions::inline()), value.value.get_type().render_colored(ctx, PrettyPrintOptions::inline()))
                                            ));
                                        }
                                    }

                                    out.insert(
                                        var_name,
                                        ValidatedFnCallArg {
                                            decl_name_at: fn_arg_var_at(arg),
                                            arg_value_at: value.from,
                                            value: value.value,
                                        },
                                    );
                                }
                            }

                            continue 'iter;
                        }

                        FnArg::Positional {
                            name: _,
                            is_optional: _,
                            typ: _,
                        }
                        | FnArg::Rest { name: _ } => continue,
                    }
                }

                if has_rest_argument {
                    rest_args.push(CmdSingleArgResult::Flag { name, value });
                    continue;
                }

                return Err(ctx.error(
                    name.at,
                    "unknown flag provided",
                ));
            }

            CmdSingleArgResult::Basic(loc_val) => {
                let Some((name, typ)) = positional_fn_args.next() else {
                    if has_rest_argument {
                        rest_args.push(CmdSingleArgResult::Basic(loc_val));
                        continue;
                    }

                    return Err(ctx.error(loc_val.from, "too many arguments provided"));
                };

                if let Some(typ) = typ {
                    if !check_if_single_type_fits_type(&loc_val.value.get_type(), typ.data(), ctx) {
                        let is_method_self_arg = name.data() == "self";

                        return Err(ctx.error(
                        loc_val.from,
                            format!(
                                "type mismatch: {} '{}', found '{}'",
                                if is_method_self_arg {
                                    "method can only be applied on type".to_owned()
                                } else {
                                    format!("argument '{}' expected type", name.data())
                                },
                                typ.data().render_colored(ctx, PrettyPrintOptions::inline()),
                                loc_val
                                    .value
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline())
                            ),
                        ));
                    }
                }

                out.insert(
                    name.data().clone(),
                    ValidatedFnCallArg {
                        decl_name_at: name.at(),
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
        let arg_var_name = fn_arg_var_name(arg, ctx);

        if out.contains_key(&arg_var_name) {
            continue;
        }

        match arg {
            FnArg::Positional {
                name: _,
                is_optional,
                typ: _,
            }
            | FnArg::NormalFlag {
                names: _,
                is_optional,
                typ: _,
            } => {
                if *is_optional {
                    if !is_native_fn {
                        out.insert(
                            arg_var_name,
                            ValidatedFnCallArg {
                                decl_name_at: fn_arg_var_at(arg),
                                arg_value_at: RuntimeCodeRange::Internal,
                                value: RuntimeValue::Null,
                            },
                        );
                    }
                } else {
                    return Err(
                        ctx.error(call_at, format!("missing value for argument: {}", fn_arg_human_name(arg)))
                    );
                }
            }

            FnArg::PresenceFlag { names: _ } => {
                out.insert(
                    arg_var_name,
                    ValidatedFnCallArg {
                        decl_name_at: fn_arg_var_at(arg),
                        arg_value_at: RuntimeCodeRange::Internal,
                        value: RuntimeValue::Bool(false),
                    },
                );
            }

            FnArg::Rest { name: _ } => {
                out.insert(
                    arg_var_name,
                    ValidatedFnCallArg {
                        decl_name_at: fn_arg_var_at(arg),
                        arg_value_at: call_at,
                        value: RuntimeValue::ArgSpread(GcReadOnlyCell::new(
                            rest_args,
                        )),
                    },
                );

                // Rest argument is the last of a function's
                // This 'break' prevents having to deal with 'rest_args' being
                // potentially re-borrowed in the next loop's iteration
                break;
            }
        }
    }

    Ok(out)
}

fn flatten_fn_call_args(
    call_at: RuntimeCodeRange,
    is_method: bool,
    call_args: FnPossibleCallArgs,
    ctx: &mut Context,
) -> ExecResult<Vec<CmdSingleArgResult>> {
    let mut out = vec![];

    let mut treat_call_type = |call_type: Option<FnCallType>| -> ExecResult<()> {
        match call_type {
            None => {
                // if is_method {
                //     return Err(ctx.error(call_at, "cannot call a method like a normal function"));
                // }
            },

            // Methods can be called through the value piping operator
            Some(FnCallType::Piped(piped)) => {
                out.push(CmdSingleArgResult::Basic(piped))
            },

            Some(FnCallType::Method(left)) => {
                if !is_method {
                    return Err(ctx.error(call_at, "cannot call a normal function like a method"));
                }

                out.push(CmdSingleArgResult::Basic(left))
            },
        }

        Ok(())
    };

    match call_args {
        FnPossibleCallArgs::Parsed { call_type, args: parsed } => {
            treat_call_type(call_type)?;

            for parsed in &parsed.data {
                match &parsed.data {
                    FnCallArg::Expr(expr) => {
                        let eval = eval_expr(&expr.data, ctx)?;

                        out.push(CmdSingleArgResult::Basic(LocatedValue::new(
                            eval,
                            RuntimeCodeRange::Parsed(expr.at),
                        )))
                    }

                    FnCallArg::Flag { name, value } => {
                        out.push(CmdSingleArgResult::Flag {
                            name: name.clone(),
                            value: Some(FlagArgValueResult {
                                value: LocatedValue::new(eval_expr(&value.data, ctx)?, RuntimeCodeRange::Parsed(value.at)),
                                value_sep: FlagValueSeparator::Equal
                            }),
                        })
                    }

                    FnCallArg::CmdArg(arg) => match eval_cmd_arg(&arg.data, ctx)? {
                        CmdArgResult::Single(single) => out.push(single),

                        CmdArgResult::Spreaded(items) => {
                            out.extend(items);
                        }
                    },
                }
            }
        }

        FnPossibleCallArgs::ParsedCmdArgs { call_type, args } => {
            treat_call_type(call_type)?;
            
            for (arg_result, _) in args {
                match arg_result {
                    CmdArgResult::Single(single) => out.push(single),
                    CmdArgResult::Spreaded(items) => {
                        out.extend(items);
                    }
                }
            }
        }

        FnPossibleCallArgs::Internal(args) => {
            for arg_result in args {
                match arg_result {
                    CmdArgResult::Single(single) => out.push(single),
                    CmdArgResult::Spreaded(items) => {
                        out.extend(items);
                    }
                }
            }
        }
    }

    Ok(out)
}

fn fn_arg_human_name(arg: &FnArg) -> String {
    match &arg {
        FnArg::Positional {
            name,
            is_optional: _,
            typ: _,
        } => name.data().clone(),

        FnArg::PresenceFlag { names } | FnArg::NormalFlag { names, is_optional: _, typ: _ } => match names {
            FnFlagArgNames::ShortFlag(short) => format!("-{}", short.data()),
            FnFlagArgNames::LongFlag(long) => format!("--{}", long.data()),
            FnFlagArgNames::LongAndShortFlag { long, short } => format!("--{} (-{})", long.data(), short.data()),
        },

        FnArg::Rest { name } => name.data().clone(),
    }
}

fn fn_arg_var_name(arg: &FnArg, ctx: &mut Context) -> String {
    match &arg {
        FnArg::Positional {
            name,
            is_optional: _,
            typ: _,
        } => name.data().clone(),

        FnArg::PresenceFlag { names } => match names {
            FnFlagArgNames::ShortFlag(short) => short.data().to_string(),
            FnFlagArgNames::LongFlag(long) => ctx.get_long_flag_var_name(long),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => ctx.get_long_flag_var_name(long),
        },

        FnArg::NormalFlag {
            names,
            is_optional: _,
            typ: _,
        } => match names {
            FnFlagArgNames::ShortFlag(short) => short.data().to_string(),
            FnFlagArgNames::LongFlag(long) => ctx.get_long_flag_var_name(long),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => ctx.get_long_flag_var_name(long),
        },

        FnArg::Rest { name } => name.data().clone(),
    }
}

fn fn_arg_var_at(arg: &FnArg) -> RuntimeCodeRange {
    match &arg {
        FnArg::Positional {
            name,
            is_optional: _,
            typ: _,
        } => name.at(),

        FnArg::PresenceFlag { names } => match names {
            FnFlagArgNames::ShortFlag(short) => short.at(),
            FnFlagArgNames::LongFlag(long) => long.at(),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => long.at(),
        },

        FnArg::NormalFlag {
            names,
            is_optional: _,
            typ: _,
        } => match names {
            FnFlagArgNames::ShortFlag(short) => short.at(),
            FnFlagArgNames::LongFlag(long) => long.at(),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => long.at(),
        },

        FnArg::Rest { name } => name.at(),
    }
}

fn get_matching_var_name(name: &CmdFlagNameArg, into: &FnFlagArgNames, ctx: &mut Context) -> Option<String> {
    match name {
        CmdFlagNameArg::Short(name) => {
            match into {
                FnFlagArgNames::ShortFlag(short) => {
                    if name == short.data() {
                        Some(short.data().to_string())
                    } else {
                        None
                    }
                },

                FnFlagArgNames::LongFlag(_) => None,

                FnFlagArgNames::LongAndShortFlag { long, short } => {
                    if name == short.data() {
                        Some(ctx.get_long_flag_var_name(long))
                    } else {
                        None
                    }
                },
            }
        },

        CmdFlagNameArg::Long(name) => match into {
            FnFlagArgNames::ShortFlag(_) => None,

            FnFlagArgNames::LongFlag(long) | FnFlagArgNames::LongAndShortFlag { long, short: _ } => {
                if name == long.data() {
                    Some(ctx.get_long_flag_var_name(long))
                } else {
                    None
                }
            }
        },

        CmdFlagNameArg::LongNoConvert(name) => match into {
            FnFlagArgNames::ShortFlag(_) => None,

            FnFlagArgNames::LongFlag(long) | FnFlagArgNames::LongAndShortFlag { long, short: _ } => {
                let var_name = ctx.get_long_flag_var_name(long);

                if name == &var_name {
                    Some(var_name.clone())
                } else {
                    None
                }
            }
        },        
    }
}
pub enum FnPossibleCallArgs<'a> {
    Parsed { call_type: Option<FnCallType>, args: &'a Eaten<Vec<Eaten<FnCallArg>>> },
    ParsedCmdArgs { call_type: Option<FnCallType>, args: Vec<(CmdArgResult, CodeRange)> },
    Internal(Vec<CmdArgResult>),
}

pub enum FnCallType {
    Piped(LocatedValue),
    Method(LocatedValue)
}

pub struct ValidatedFnCallArg {
    pub decl_name_at: RuntimeCodeRange,
    pub arg_value_at: RuntimeCodeRange,
    pub value: RuntimeValue,
}
