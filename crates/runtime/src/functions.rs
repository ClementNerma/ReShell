use std::collections::HashMap;

use parsy::{CodeRange, Eaten};
use reshell_parser::ast::{
    CmdFlagNameArg, FlagValueSeparator, FnArg, FnCall, FnCallArg, FnCallNature, FnFlagArgNames,
    FnNormalFlagArg, FnPositionalArg, FnPresenceFlagArg, FnRestArg, RuntimeCodeRange, RuntimeEaten,
    ValueType,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    cmd::{eval_cmd_arg, CmdArgResult, CmdSingleArgResult, FlagArgValueResult},
    context::{CallStackEntry, Context, ScopeContent, ScopeMethod, ScopeVar},
    errors::{ExecInfoType, ExecResult},
    exec::{run_body_with_deps, InstrRet},
    expr::eval_expr,
    gc::{GcCell, GcReadOnlyCell},
    typechecker::check_if_value_fits_type,
    values::{InternalFnCallData, LocatedValue, RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

pub fn eval_fn_call(
    call: &Eaten<FnCall>,
    piped: Option<LocatedValue>,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    let func = match call.data.nature {
        FnCallNature::NamedFunction => {
            assert!(piped.is_none());

            ctx.get_visible_fn_value(&call.data.name)?.clone()
        }

        FnCallNature::Method => {
            let piped = piped.as_ref().unwrap();
            let method = find_applicable_method(&call.data.name, &piped.value, ctx)?;
            method.value.clone()
        }

        FnCallNature::Variable => {
            assert!(piped.is_none());

            let var = ctx.get_visible_var(&call.data.name);

            let var_value = var.value.read(call.data.name.at);

            match &var_value.value {
                RuntimeValue::Function(func) => func.clone(),

                value => {
                    return Err(ctx.error(
                        call.data.name.at,
                        format!(
                            "expected a function, found a {} instead",
                            value.compute_type().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    ))
                }
            }
        }
    };

    call_fn_value(
        RuntimeCodeRange::Parsed(call.at),
        &func,
        FnCallInfos {
            nature: call.data.nature,
            args: FnPossibleCallArgs::Parsed(&call.data.call_args),
            piped,
        },
        ctx,
    )
}

pub fn call_fn_value(
    call_at: RuntimeCodeRange,
    func: &GcReadOnlyCell<RuntimeFnValue>,
    infos: FnCallInfos,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    ctx.ensure_no_ctrl_c_press(call_at)?;

    let args = fill_fn_args(
        call_at,
        func,
        infos,
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
                        decl_scope_id: body.data.scope_id,
                        is_mut: false,
                        enforced_type: None,
                        value: GcCell::new(LocatedValue::new(value, arg_value_at)),
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
                InstrRet::WanderingValue(value) => Some(value),
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
                        .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            ));
        };

        if !check_if_value_fits_type(&ret_val.value, ret_type.data(), ctx) {
            return Err(ctx.error(
                call_at,
                format!(
                    "function call returned a {}, was expected to return a {}",
                    ret_val
                        .value
                        .compute_type()
                        .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                    ret_type
                        .data()
                        .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            ));
        }
    }

    ctx.ensure_no_ctrl_c_press(call_at)?;

    Ok(returned)
}

fn fill_fn_args(
    call_at: RuntimeCodeRange,
    func: &RuntimeFnValue,
    infos: FnCallInfos,
    fn_args: &[FnArg],
    ctx: &mut Context,
) -> ExecResult<HashMap<String, ValidatedFnCallArg>> {
    let ParsedFnCallArgs {
        mut parsed_args,
        rest_args,
    } = parse_fn_call_args(call_at, func, infos, fn_args, ctx)?;

    let is_native_fn = match func.body {
        RuntimeFnBody::Block(_) => false,
        RuntimeFnBody::Internal(_) => true,
    };

    for arg in fn_args {
        let arg_var_name = fn_arg_var_name(arg, ctx);

        if parsed_args.contains_key(&arg_var_name) {
            continue;
        }

        match arg {
            FnArg::Positional(FnPositionalArg {
                name: _,
                is_optional,
                typ: _,
            })
            | FnArg::NormalFlag(FnNormalFlagArg {
                names: _,
                is_optional,
                typ: _,
            }) => {
                if *is_optional {
                    if !is_native_fn {
                        parsed_args.insert(
                            arg_var_name,
                            ValidatedFnCallArg {
                                decl_name_at: fn_arg_var_at(arg),
                                arg_value_at: RuntimeCodeRange::Internal("unprovided argument"),
                                value: RuntimeValue::Null,
                            },
                        );
                    }
                } else {
                    return Err(ctx
                        .error(
                            call_at,
                            format!("missing value for argument: {}", fn_arg_human_name(arg)),
                        )
                        .with_info(
                            ExecInfoType::Tip,
                            format!(
                                "called function's signature is: {}",
                                func.signature.inner().render_colored(
                                    ctx.type_alias_store(),
                                    PrettyPrintOptions::inline()
                                )
                            ),
                        ));
                }
            }

            FnArg::PresenceFlag(_) => {
                parsed_args.insert(
                    arg_var_name,
                    ValidatedFnCallArg {
                        decl_name_at: fn_arg_var_at(arg),
                        arg_value_at: RuntimeCodeRange::Internal("unprovided argument"),
                        value: RuntimeValue::Bool(false),
                    },
                );
            }

            FnArg::Rest(_) => {
                parsed_args.insert(
                    arg_var_name,
                    ValidatedFnCallArg {
                        decl_name_at: fn_arg_var_at(arg),
                        arg_value_at: call_at,
                        value: RuntimeValue::List(GcCell::new(
                            rest_args
                                .into_iter()
                                .map(Box::new)
                                .map(RuntimeValue::CmdArg)
                                .collect(),
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

    Ok(parsed_args)
}

fn parse_fn_call_args(
    call_at: RuntimeCodeRange,
    func: &RuntimeFnValue,
    infos: FnCallInfos,
    fn_args: &[FnArg],
    ctx: &mut Context,
) -> ExecResult<ParsedFnCallArgs> {
    let mut call_args = flatten_fn_call_args(call_at, func.is_method, infos, ctx)?;

    // Reverse call args so we can .pop() them! without shifting elements
    call_args.reverse();

    let mut parsed_args = HashMap::<String, ValidatedFnCallArg>::new();
    let mut rest_args = Vec::<CmdSingleArgResult>::new();

    let mut positional_fn_args = fn_args.iter().filter_map(|fn_arg| match fn_arg {
        FnArg::Positional(FnPositionalArg {
            name,
            is_optional,
            typ,
        }) => Some((name, is_optional, typ)),

        FnArg::PresenceFlag(_) | FnArg::NormalFlag(_) | FnArg::Rest(_) => None,
    });

    let mut value_on_hold = None::<CmdSingleArgResult>;

    let has_rest_argument = fn_args.iter().any(|arg| matches!(arg, FnArg::Rest(_)));

    while let Some(arg_result) = value_on_hold.take().or_else(|| call_args.pop()) {
        if !rest_args.is_empty() {
            rest_args.push(arg_result);
            continue;
        }

        let parsed = parse_single_fn_call_arg(
            func,
            has_rest_argument,
            &mut positional_fn_args,
            arg_result,
            fn_args,
            ctx,
        )?;

        match parsed {
            ParsedSingleFnCallArg::Variable {
                name,
                value,
                value_on_hold: new_value_on_hold,
            } => {
                parsed_args.insert(name, value);
                value_on_hold = new_value_on_hold;
            }

            ParsedSingleFnCallArg::Rest(rest) => {
                rest_args.push(rest);
            }
        }
    }

    Ok(ParsedFnCallArgs {
        parsed_args,
        rest_args,
    })
}

struct ParsedFnCallArgs {
    parsed_args: HashMap<String, ValidatedFnCallArg>,
    rest_args: Vec<CmdSingleArgResult>,
}

fn parse_single_fn_call_arg<'a, 'b, 'c>(
    func: &RuntimeFnValue,
    has_rest_argument: bool,
    // TODO: use struct
    positional_fn_args: &mut impl Iterator<
        Item = (
            &'a RuntimeEaten<String>,
            &'b bool,
            &'c Option<RuntimeEaten<ValueType>>,
        ),
    >,
    arg_result: CmdSingleArgResult,
    fn_args: &[FnArg],
    ctx: &mut Context,
) -> ExecResult<ParsedSingleFnCallArg> {
    match arg_result {
        CmdSingleArgResult::Flag { name, value } => {
            for arg in fn_args {
                match arg {
                    FnArg::Positional(_) | FnArg::Rest(_) => continue,

                    FnArg::PresenceFlag(FnPresenceFlagArg { names }) => {
                        let Some(var_name) = get_matching_var_name(name.data(), names, ctx) else {
                            continue;
                        };

                        let make_ret_value =
                            |flag_value: bool, value_on_hold: Option<CmdSingleArgResult>| {
                                ParsedSingleFnCallArg::Variable {
                                    name: var_name,
                                    value: ValidatedFnCallArg {
                                        decl_name_at: fn_arg_var_at(arg),
                                        arg_value_at: name.at(),
                                        value: RuntimeValue::Bool(flag_value),
                                    },
                                    value_on_hold,
                                }
                            };

                        return match value {
                            // If no value is provided, the flag is set to `true`
                            None => Ok(make_ret_value(true, None)),

                            // Otherwise...
                            Some(FlagArgValueResult { value, value_sep }) => {
                                match &value.value {
                                    // If it's a boolean, set the flag to the provided value
                                    RuntimeValue::Bool(bool) => Ok(make_ret_value(*bool, None)),

                                    _ => {
                                        match value_sep {
                                            // Move the value as a separate argument
                                            FlagValueSeparator::Space => {
                                                Ok(make_ret_value(true, Some(CmdSingleArgResult::Basic(value))))
                                            }

                                            // Otherwise, fail
                                            FlagValueSeparator::Equal => {
                                                Err(ctx.error(
                                                    value.from,
                                                    "the provided flag doesn't accept a value (other than booleans)",
                                                ))
                                            }
                                        }
                                    }
                                }
                            }
                        };
                    }

                    FnArg::NormalFlag(FnNormalFlagArg {
                        names,
                        is_optional,
                        typ,
                    }) => {
                        let Some(var_name) = get_matching_var_name(name.data(), names, ctx) else {
                            continue;
                        };

                        return match value {
                            None => {
                                if *is_optional {
                                    Ok(ParsedSingleFnCallArg::Variable {
                                        name: var_name,
                                        value: ValidatedFnCallArg {
                                            decl_name_at: fn_arg_var_at(arg),
                                            arg_value_at: name.at(),
                                            value: RuntimeValue::Null,
                                        },
                                        value_on_hold: None,
                                    })
                                } else {
                                    Err(ctx
                                        .error(
                                            name.at(),
                                            format!(
                                                "a value of type '{}' is expected for this flag",
                                                typ.data().render_colored(
                                                    ctx.type_alias_store(),
                                                    PrettyPrintOptions::inline()
                                                )
                                            ),
                                        )
                                        .with_info(
                                            ExecInfoType::Tip,
                                            format!(
                                                "called function's signature is: {}",
                                                func.signature.inner().render_colored(
                                                    ctx.type_alias_store(),
                                                    PrettyPrintOptions::inline()
                                                )
                                            ),
                                        ))
                                }
                            }

                            Some(FlagArgValueResult {
                                value,
                                value_sep: _,
                            }) => {
                                let is_null_for_optional =
                                    *is_optional && matches!(value.value, RuntimeValue::Null);

                                return if !is_null_for_optional
                                    && !check_if_value_fits_type(&value.value, typ.data(), ctx)
                                {
                                    Err(
                                            ctx.error(
                                                value.from,
                                            format!(
                                                "expected a value of type '{}' for flag '{}', found '{}'",
                                                typ.data().render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                                                names.render_colored(&(), PrettyPrintOptions::inline()),
                                                value.value.compute_type().render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                                            )
                                        ).with_info(ExecInfoType::Tip, format!("called function's signature is: {}", func.signature.inner().render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline()))))
                                } else {
                                    Ok(ParsedSingleFnCallArg::Variable {
                                        name: var_name,
                                        value: ValidatedFnCallArg {
                                            decl_name_at: fn_arg_var_at(arg),
                                            arg_value_at: value.from,
                                            value: value.value,
                                        },
                                        value_on_hold: None,
                                    })
                                };
                            }
                        };
                    }
                }
            }

            if has_rest_argument {
                Ok(ParsedSingleFnCallArg::Rest(CmdSingleArgResult::Flag {
                    name,
                    value,
                }))
            } else {
                Err(ctx.error(name.at(), "unknown flag provided").with_info(
                    ExecInfoType::Tip,
                    format!(
                        "called function's signature is: {}",
                        func.signature
                            .inner()
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ))
            }
        }

        CmdSingleArgResult::Basic(loc_val) => {
            let Some((name, is_optional, typ)) = positional_fn_args.next() else {
                if has_rest_argument {
                    return Ok(ParsedSingleFnCallArg::Rest(CmdSingleArgResult::Basic(
                        loc_val,
                    )));
                }

                return Err(ctx
                    .error(loc_val.from, "too many arguments provided")
                    .with_info(
                        ExecInfoType::Tip,
                        format!(
                            "called function's signature is: {}",
                            func.signature.inner().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
                        ),
                    ));
            };

            let is_null_for_optional = *is_optional && matches!(loc_val.value, RuntimeValue::Null);

            if !is_null_for_optional {
                if let Some(typ) = typ {
                    if !check_if_value_fits_type(&loc_val.value, typ.data(), ctx) {
                        let is_method_self_arg = func.is_method && name.data() == "self";

                        return Err(ctx.error(
                            loc_val.from,
                            format!(
                                "type mismatch: {} '{}', found '{}'",
                                if is_method_self_arg {
                                    "method can only be applied on type".to_owned()
                                } else {
                                    format!("argument '{}' expected type", name.data())
                                },
                                typ.data().render_colored(
                                    ctx.type_alias_store(),
                                    PrettyPrintOptions::inline()
                                ),
                                loc_val.value.compute_type().render_colored(
                                    ctx.type_alias_store(),
                                    PrettyPrintOptions::inline()
                                )
                            ),
                        ));
                    }
                }
            }

            Ok(ParsedSingleFnCallArg::Variable {
                name: name.data().clone(),
                value: ValidatedFnCallArg {
                    decl_name_at: name.at(),
                    arg_value_at: loc_val.from,
                    value: loc_val.value,
                },
                value_on_hold: None,
            })
        }
    }
}

enum ParsedSingleFnCallArg {
    Variable {
        name: String,
        value: ValidatedFnCallArg,
        value_on_hold: Option<CmdSingleArgResult>,
    },
    Rest(CmdSingleArgResult),
}

fn flatten_fn_call_args(
    call_at: RuntimeCodeRange,
    is_method: bool,
    infos: FnCallInfos,
    ctx: &mut Context,
) -> ExecResult<Vec<CmdSingleArgResult>> {
    let FnCallInfos {
        nature,
        args,
        piped,
    } = infos;

    let mut out = vec![];

    match nature {
        FnCallNature::NamedFunction => {
            if let Some(piped) = piped {
                out.push(CmdSingleArgResult::Basic(piped));
            }
        }

        FnCallNature::Method => {
            if !is_method {
                return Err(ctx.error(call_at, "cannot call a normal function like a method"));
            }

            if let Some(piped) = piped {
                out.push(CmdSingleArgResult::Basic(piped))
            }
        }

        FnCallNature::Variable => {
            assert!(piped.is_none());
        }
    }

    match args {
        FnPossibleCallArgs::Parsed(args) => {
            for parsed in &args.data {
                match &parsed.data {
                    FnCallArg::Expr(expr) => {
                        out.push(CmdSingleArgResult::Basic(LocatedValue::new(
                            eval_expr(&expr.data, ctx)?,
                            RuntimeCodeRange::Parsed(expr.at),
                        )))
                    }

                    FnCallArg::Flag { name, value } => out.push(CmdSingleArgResult::Flag {
                        name: RuntimeEaten::Parsed(name.clone()),
                        value: Some(FlagArgValueResult {
                            value: LocatedValue::new(
                                eval_expr(&value.data, ctx)?,
                                RuntimeCodeRange::Parsed(value.at),
                            ),
                            value_sep: FlagValueSeparator::Equal,
                        }),
                    }),

                    FnCallArg::CmdArg(arg) => match eval_cmd_arg(&arg.data, ctx)? {
                        CmdArgResult::Single(single) => out.push(single),

                        CmdArgResult::Spreaded(items) => {
                            out.extend(items);
                        }
                    },
                }
            }
        }

        FnPossibleCallArgs::ParsedCmdArgs(args) => {
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
        FnArg::Positional(FnPositionalArg {
            name,
            is_optional: _,
            typ: _,
        }) => name.data().clone(),

        FnArg::PresenceFlag(FnPresenceFlagArg { names })
        | FnArg::NormalFlag(FnNormalFlagArg {
            names,
            is_optional: _,
            typ: _,
        }) => match names {
            FnFlagArgNames::ShortFlag(short) => format!("-{}", short.data()),
            FnFlagArgNames::LongFlag(long) => format!("--{}", long.data()),
            FnFlagArgNames::LongAndShortFlag { long, short } => {
                format!("--{} (-{})", long.data(), short.data())
            }
        },

        FnArg::Rest(FnRestArg { name }) => name.data().clone(),
    }
}

fn fn_arg_var_name(arg: &FnArg, ctx: &mut Context) -> String {
    match &arg {
        FnArg::Positional(FnPositionalArg {
            name,
            is_optional: _,
            typ: _,
        }) => name.data().clone(),

        FnArg::PresenceFlag(FnPresenceFlagArg { names }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.data().to_string(),
            FnFlagArgNames::LongFlag(long) => ctx.get_long_flag_var_name(long),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => ctx.get_long_flag_var_name(long),
        },

        FnArg::NormalFlag(FnNormalFlagArg {
            names,
            is_optional: _,
            typ: _,
        }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.data().to_string(),
            FnFlagArgNames::LongFlag(long) => ctx.get_long_flag_var_name(long),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => ctx.get_long_flag_var_name(long),
        },

        FnArg::Rest(FnRestArg { name }) => name.data().clone(),
    }
}

fn fn_arg_var_at(arg: &FnArg) -> RuntimeCodeRange {
    match &arg {
        FnArg::Positional(FnPositionalArg {
            name,
            is_optional: _,
            typ: _,
        }) => name.at(),

        FnArg::PresenceFlag(FnPresenceFlagArg { names }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.at(),
            FnFlagArgNames::LongFlag(long) => long.at(),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => long.at(),
        },

        FnArg::NormalFlag(FnNormalFlagArg {
            names,
            is_optional: _,
            typ: _,
        }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.at(),
            FnFlagArgNames::LongFlag(long) => long.at(),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => long.at(),
        },

        FnArg::Rest(FnRestArg { name }) => name.at(),
    }
}

fn get_matching_var_name(
    name: &CmdFlagNameArg,
    into: &FnFlagArgNames,
    ctx: &mut Context,
) -> Option<String> {
    match name {
        CmdFlagNameArg::Short(name) => match into {
            FnFlagArgNames::ShortFlag(short) => {
                if name == short.data() {
                    Some(short.data().to_string())
                } else {
                    None
                }
            }

            FnFlagArgNames::LongFlag(_) => None,

            FnFlagArgNames::LongAndShortFlag { long, short } => {
                if name == short.data() {
                    Some(ctx.get_long_flag_var_name(long))
                } else {
                    None
                }
            }
        },

        CmdFlagNameArg::Long(name) => match into {
            FnFlagArgNames::ShortFlag(_) => None,

            FnFlagArgNames::LongFlag(long)
            | FnFlagArgNames::LongAndShortFlag { long, short: _ } => {
                if name == long.data() {
                    Some(ctx.get_long_flag_var_name(long))
                } else {
                    None
                }
            }
        },

        CmdFlagNameArg::LongNoConvert(name) => match into {
            FnFlagArgNames::ShortFlag(_) => None,

            FnFlagArgNames::LongFlag(long)
            | FnFlagArgNames::LongAndShortFlag { long, short: _ } => {
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

pub fn find_applicable_method<'s>(
    name: &Eaten<String>,
    for_value: &RuntimeValue,
    ctx: &'s Context,
) -> ExecResult<&'s ScopeMethod> {
    ctx.find_applicable_method(name, for_value)
        .map_err(|not_matching| {
            let mut err = ctx.error(
                name.at,
                format!(
                    "no such method for type {}",
                    for_value
                        .compute_type()
                        .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            );

            for method in not_matching {
                err = err.with_info(
                    ExecInfoType::Note,
                    format!(
                        "found method for another type: {}",
                        method
                            .on_type
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                );
            }

            err
        })
}

pub struct FnCallInfos<'a> {
    pub nature: FnCallNature,
    pub args: FnPossibleCallArgs<'a>,
    pub piped: Option<LocatedValue>,
}

pub enum FnPossibleCallArgs<'a> {
    Parsed(&'a Eaten<Vec<Eaten<FnCallArg>>>),
    ParsedCmdArgs(Vec<(CmdArgResult, CodeRange)>),
    Internal(Vec<CmdArgResult>),
}

#[derive(Debug)]
pub struct ValidatedFnCallArg {
    pub decl_name_at: RuntimeCodeRange,
    pub arg_value_at: RuntimeCodeRange,
    pub value: RuntimeValue,
}
