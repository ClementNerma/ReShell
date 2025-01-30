use std::collections::HashMap;

use colored::Colorize;
use parsy::{CodeRange, Span};
use reshell_parser::ast::{
    CmdFlagNameArg, FlagValueSeparator, FnArg, FnCall, FnCallArg, FnCallNature, FnFlagArgNames,
    FnNormalFlagArg, FnPositionalArg, FnPresenceFlagArg, FnRestArg, RuntimeCodeRange, RuntimeSpan,
    SingleValueType, ValueType,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    cmd::{eval_cmd_arg, CmdArgResult, FlagArgValueResult, SingleCmdArgResult},
    context::{CallStackEntry, Context, ScopeContent, ScopeMethod, ScopeVar},
    errors::{ExecInfoType, ExecResult},
    exec::{run_body_with_deps, InstrRet},
    expr::eval_expr,
    gc::{GcCell, GcReadOnlyCell},
    typechecker::check_if_value_fits_type,
    values::{
        CmdArgValue, CmdFlagValue, InternalFnCallData, LocatedValue, RuntimeFnBody, RuntimeFnValue,
        RuntimeValue,
    },
};

pub fn eval_fn_call(
    call: &Span<FnCall>,
    piped: Option<LocatedValue>,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    let func = match call.data.nature {
        FnCallNature::NamedFunction => {
            assert!(piped.is_none());

            ctx.get_visible_fn_value(&call.data.name)?.clone()
        }

        FnCallNature::Method => {
            let Some(piped) = piped.as_ref() else {
                return Err(ctx.error(
                    call.data.call_args.at,
                    "please provide at least one argument to run the method on",
                ));
            };

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
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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

    let args_at = match &infos.args {
        FnPossibleCallArgs::Parsed(span) => RuntimeCodeRange::Parsed(span.at),
        FnPossibleCallArgs::ParsedCmdArgs { at, args: _ } => RuntimeCodeRange::Parsed(*at),
        FnPossibleCallArgs::Internal { at, args: _ } => RuntimeCodeRange::Internal(at),
    };

    let args = fill_fn_args(call_at, func, infos, &func.signature.inner().args.data, ctx)?;

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
                        value: GcCell::new(LocatedValue::new(arg_value_at, value)),
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

        RuntimeFnBody::Internal(run) => run(InternalFnCallData {
            call_at,
            args_at,
            args,
            ctx,
        })?,
    };

    // If the function's signature contains a return type...
    if let Some(ret_type) = &func.signature.inner().ret_type {
        // If the return type is 'void'...
        if matches!(*ret_type.data, ValueType::Single(SingleValueType::Void)) {
            // Ensure no value was returned from the function (as void = no return value)
            if returned.is_some() {
                return Err(ctx.error(
                    call_at,
                    format!(
                        "function call returned a value but has a return type of {}",
                        ret_type
                            .data
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ));
            }
        }
        // If the return type isn't 'void'...
        else {
            // Ensure the function returned a value
            let Some(ret_val) = &returned else {
                return Err(ctx.error(
                    call_at,
                    format!(
                        "function call did not return any value, was expected to return a {}",
                        ret_type
                            .data
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ));
            };

            // Check if the returned value's type matches the signature return type
            if !check_if_value_fits_type(&ret_val.value, &ret_type.data, ctx) {
                let nature = format!(
                    "{}function call returned a {}, was expected to return a {}",
                    match func.body {
                        RuntimeFnBody::Block(_) => "",
                        RuntimeFnBody::Internal(_) => "native ",
                    },
                    ret_val
                        .value
                        .compute_type()
                        .display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                    ret_type
                        .data
                        .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                );
                return Err(ctx.error(call_at, nature));
            }
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

    let mut rest_args = Some(rest_args);

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
                if !*is_optional {
                    return Err(ctx
                        .error(
                            call_at,
                            format!("missing value for argument: {}", fn_arg_human_name(arg)),
                        )
                        .with_info(
                            ExecInfoType::Tip,
                            format!(
                                "called function's signature is: {}",
                                func.signature
                                    .inner()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            ),
                        ));
                }

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

            FnArg::Rest(rest_arg) => {
                // This trick allows to avoid ownership problems
                // We will never have to fill the rest argument twice of course,
                // so we can .unwrap() here
                let rest_args = rest_args.take().unwrap();

                // By default, rest arguments are collected into a list of cmdarg values
                let rest_args = if rest_arg.typ.is_none() {
                    rest_args
                        .into_iter()
                        .map(|arg| match arg {
                            SingleCmdArgResult::Basic(data) => CmdArgValue::Basic(data),
                            SingleCmdArgResult::Flag(data) => CmdArgValue::Flag(data),
                        })
                        .map(Box::new)
                        .map(RuntimeValue::CmdArg)
                        .collect()
                }
                // But if an explicit type is provided (such as e.g. `list[string]`), that specific type
                // is used instead
                else {
                    rest_args
                        .into_iter()
                        .map(|arg| match arg {
                            SingleCmdArgResult::Basic(loc_val) => loc_val.value,
                            SingleCmdArgResult::Flag(_) => unreachable!(),
                        })
                        .collect()
                };

                parsed_args.insert(
                    arg_var_name,
                    ValidatedFnCallArg {
                        decl_name_at: fn_arg_var_at(arg),
                        arg_value_at: call_at,
                        value: RuntimeValue::List(GcCell::new(rest_args)),
                    },
                );
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
    let mut rest_args = Vec::<SingleCmdArgResult>::new();

    let mut positional_fn_args = fn_args.iter().filter_map(|fn_arg| match fn_arg {
        FnArg::Positional(FnPositionalArg {
            name,
            is_optional,
            typ,
        }) => Some(SimplifiedPositionalFnArg {
            name,
            is_optional: *is_optional,
            typ,
        }),

        FnArg::PresenceFlag(_) | FnArg::NormalFlag(_) | FnArg::Rest(_) => None,
    });

    let mut value_on_hold = None::<SingleCmdArgResult>;

    let rest_arg = fn_args.iter().find_map(|arg| match arg {
        FnArg::Rest(rest) => Some(rest),
        _ => None,
    });

    while let Some(arg_result) = value_on_hold.take().or_else(|| call_args.pop()) {
        let parsed = parse_single_fn_call_arg(
            func,
            rest_arg,
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

    // If we got rest arguments...
    if !rest_args.is_empty() {
        // And this function's rest argument has a declared type...
        if let Some(typ) = &rest_arg.unwrap().typ {
            // First ensure that all provided arguments are values and not flags
            let rest_args = rest_args
                .iter()
                .map(|rest_arg| match rest_arg {
                    SingleCmdArgResult::Basic(loc_val) => Ok(loc_val.clone()),
                    SingleCmdArgResult::Flag(CmdFlagValue { name, value: _ }) => Err(ctx.error(
                        name.at,
                        format!(
                            "provided a flag but this function's rest argument's type is: {}",
                            typ.data
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    )),
                })
                .collect::<Result<Vec<_>, _>>()?;

            // Type check the arguments against the expected rest type
            match &typ.data {
                ValueType::Union(_) => unreachable!(),
                ValueType::Single(typ) => {
                    match typ {
                        SingleValueType::UntypedList => {
                            // Always OK
                        }
                        SingleValueType::TypedList(expected_type) => {
                            // Check if any of the provided arguments have an incorrect type
                            let faulty_arg = rest_args.iter().find(|arg| {
                                !check_if_value_fits_type(&arg.value, expected_type, ctx)
                            });

                            // If so, return an error
                            if let Some(faulty_arg) = faulty_arg {
                                return Err(
                                    ctx.error(
                                        faulty_arg.from,
                                        format!(
                                            "incorrect value provided in rest arguments ; expected values of type {}, found {}",
                                            expected_type.display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                                            faulty_arg.value.compute_type().display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                                        )
                                    )
                                );
                            }
                        }
                        _ => unreachable!(),
                    }
                }
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
    rest_args: Vec<SingleCmdArgResult>,
}

struct SimplifiedPositionalFnArg<'a> {
    name: &'a RuntimeSpan<String>,
    is_optional: bool,
    typ: &'a Option<ValueType>,
}

/// Parse a single function call argument
///
/// Depending on the provided informations, it will determine the argument it belongs to along with its value
fn parse_single_fn_call_arg<'a>(
    func: &RuntimeFnValue,
    rest_arg: Option<&FnRestArg>,
    positional_fn_args: &mut impl Iterator<Item = SimplifiedPositionalFnArg<'a>>,
    arg_result: SingleCmdArgResult,
    fn_args: &[FnArg],
    ctx: &mut Context,
) -> ExecResult<ParsedSingleFnCallArg> {
    match arg_result {
        SingleCmdArgResult::Flag(CmdFlagValue { name, value }) => {
            for arg in fn_args {
                match arg {
                    FnArg::Positional(_) | FnArg::Rest(_) => continue,

                    FnArg::PresenceFlag(FnPresenceFlagArg { names }) => {
                        let Some(var_name) = get_matching_var_name(&name.data, names, ctx) else {
                            continue;
                        };

                        let make_ret_value =
                            |flag_value: bool, value_on_hold: Option<SingleCmdArgResult>| {
                                ParsedSingleFnCallArg::Variable {
                                    name: var_name,
                                    value: ValidatedFnCallArg {
                                        decl_name_at: fn_arg_var_at(arg),
                                        arg_value_at: name.at,
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
                                                Ok(make_ret_value(true, Some(SingleCmdArgResult::Basic(value))))
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
                        let Some(var_name) = get_matching_var_name(&name.data, names, ctx) else {
                            continue;
                        };

                        return match value {
                            None => {
                                if *is_optional {
                                    Ok(ParsedSingleFnCallArg::Variable {
                                        name: var_name,
                                        value: ValidatedFnCallArg {
                                            decl_name_at: fn_arg_var_at(arg),
                                            arg_value_at: name.at,
                                            value: RuntimeValue::Null,
                                        },
                                        value_on_hold: None,
                                    })
                                } else {
                                    Err(ctx
                                        .error(
                                            name.at,
                                            format!(
                                                "a value of type '{}' is expected for this flag",
                                                typ.display(
                                                    ctx.type_alias_store(),
                                                    PrettyPrintOptions::inline()
                                                )
                                            ),
                                        )
                                        .with_info(
                                            ExecInfoType::Tip,
                                            format!(
                                                "called function's signature is: {}",
                                                func.signature.inner().display(
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
                                    && !check_if_value_fits_type(&value.value, typ, ctx)
                                {
                                    Err(
                                        ctx.error(
                                            value.from,
                                        format!(
                                            "expected a value of type '{}' for flag '{}', found '{}'",
                                            typ.display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                                            names.display(&(), PrettyPrintOptions::inline()),
                                            value.value.compute_type().display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                                        )
                                    ).with_info(ExecInfoType::Tip, format!("called function's signature is: {}", func.signature.inner().display(ctx.type_alias_store(), PrettyPrintOptions::inline()))))
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

            // Here we have no match for the provided flag
            // If there is a rest argument *without* an explicit type, we can push it there
            if rest_arg.is_some_and(|rest_arg| rest_arg.typ.is_none()) {
                return Ok(ParsedSingleFnCallArg::Rest(SingleCmdArgResult::Flag(
                    CmdFlagValue { name, value },
                )));
            }

            // Otherwise, we have found an unknown flag
            Err(ctx.error(name.at, "unknown flag provided").with_info(
                ExecInfoType::Tip,
                format!(
                    "called function's signature is: {}",
                    func.signature
                        .inner()
                        .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            ))
        }

        SingleCmdArgResult::Basic(loc_val) => {
            let Some(positional_fn_arg) = positional_fn_args.next() else {
                // If we are out of positional arguments to fill, we check if there is a rest argument
                // If so, we can push it there
                return if rest_arg.is_some() {
                    Ok(ParsedSingleFnCallArg::Rest(SingleCmdArgResult::Basic(
                        loc_val,
                    )))
                }
                // Otherwise, we have found an argument that doesn't belong anywhere
                else {
                    Err(ctx
                        .error(loc_val.from, "too many arguments provided")
                        .with_info(
                            ExecInfoType::Tip,
                            format!(
                                "called function's signature is: {}",
                                func.signature
                                    .inner()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            ),
                        ))
                };
            };

            let SimplifiedPositionalFnArg {
                name,
                is_optional,
                typ,
            } = positional_fn_arg;

            if !is_optional || !matches!(loc_val.value, RuntimeValue::Null) {
                if let Some(typ) = typ {
                    if !check_if_value_fits_type(&loc_val.value, typ, ctx) {
                        let is_method_self_arg = func.is_method && name.data == "self";

                        return Err(ctx.error(
                            loc_val.from,
                            format!(
                                "type mismatch: {} {}, found {}",
                                if is_method_self_arg {
                                    "method can only be applied on type".to_owned()
                                } else {
                                    format!("argument {} expected type", name.data.bright_yellow())
                                },
                                typ.display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                                loc_val
                                    .value
                                    .compute_type()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            ),
                        ));
                    }
                }
            }

            Ok(ParsedSingleFnCallArg::Variable {
                name: name.data.clone(),
                value: ValidatedFnCallArg {
                    decl_name_at: name.at,
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
        value_on_hold: Option<SingleCmdArgResult>,
    },
    Rest(SingleCmdArgResult),
}

fn flatten_fn_call_args(
    call_at: RuntimeCodeRange,
    is_method: bool,
    infos: FnCallInfos,
    ctx: &mut Context,
) -> ExecResult<Vec<SingleCmdArgResult>> {
    let FnCallInfos {
        nature,
        args,
        piped,
    } = infos;

    let mut out = vec![];

    match nature {
        FnCallNature::NamedFunction => {
            if let Some(piped) = piped {
                out.push(SingleCmdArgResult::Basic(piped));
            }
        }

        FnCallNature::Method => {
            if !is_method {
                return Err(ctx.error(call_at, "cannot call a normal function like a method"));
            }

            if let Some(piped) = piped {
                out.push(SingleCmdArgResult::Basic(piped))
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
                        out.push(SingleCmdArgResult::Basic(LocatedValue::new(
                            RuntimeCodeRange::Parsed(expr.at),
                            eval_expr(&expr.data, ctx)?,
                        )))
                    }

                    FnCallArg::Flag { name, value } => {
                        out.push(SingleCmdArgResult::Flag(CmdFlagValue {
                            name: RuntimeSpan::from(name.clone()),
                            value: Some(FlagArgValueResult {
                                value: LocatedValue::new(
                                    RuntimeCodeRange::Parsed(value.at),
                                    eval_expr(&value.data, ctx)?,
                                ),
                                value_sep: FlagValueSeparator::Equal,
                            }),
                        }))
                    }

                    FnCallArg::CmdArg(arg) => match eval_cmd_arg(arg, ctx)? {
                        CmdArgResult::Single(single) => out.push(single),

                        CmdArgResult::Spreaded(items) => {
                            out.extend(items);
                        }
                    },
                }
            }
        }

        FnPossibleCallArgs::ParsedCmdArgs { at: _, args } => {
            for (arg_result, _) in args {
                match arg_result {
                    CmdArgResult::Single(single) => out.push(single),
                    CmdArgResult::Spreaded(items) => {
                        out.extend(items);
                    }
                }
            }
        }

        FnPossibleCallArgs::Internal { at: _, args } => {
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
        }) => name.data.clone(),

        FnArg::PresenceFlag(FnPresenceFlagArg { names })
        | FnArg::NormalFlag(FnNormalFlagArg {
            names,
            is_optional: _,
            typ: _,
        }) => match names {
            FnFlagArgNames::ShortFlag(short) => format!("-{}", short.data),
            FnFlagArgNames::LongFlag(long) => format!("--{}", long.data),
            FnFlagArgNames::LongAndShortFlag { long, short } => {
                format!("--{} (-{})", long.data, short.data)
            }
        },

        FnArg::Rest(FnRestArg { name, typ: _ }) => name.data.clone(),
    }
}

fn fn_arg_var_name(arg: &FnArg, ctx: &mut Context) -> String {
    match &arg {
        FnArg::Positional(FnPositionalArg {
            name,
            is_optional: _,
            typ: _,
        }) => name.data.clone(),

        FnArg::PresenceFlag(FnPresenceFlagArg { names }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.data.to_string(),
            FnFlagArgNames::LongFlag(long) => ctx.get_long_flag_var_name(long),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => ctx.get_long_flag_var_name(long),
        },

        FnArg::NormalFlag(FnNormalFlagArg {
            names,
            is_optional: _,
            typ: _,
        }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.data.to_string(),
            FnFlagArgNames::LongFlag(long) => ctx.get_long_flag_var_name(long),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => ctx.get_long_flag_var_name(long),
        },

        FnArg::Rest(FnRestArg { name, typ: _ }) => name.data.clone(),
    }
}

fn fn_arg_var_at(arg: &FnArg) -> RuntimeCodeRange {
    match &arg {
        FnArg::Positional(FnPositionalArg {
            name,
            is_optional: _,
            typ: _,
        }) => name.at,

        FnArg::PresenceFlag(FnPresenceFlagArg { names }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.at,
            FnFlagArgNames::LongFlag(long) => long.at,
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => long.at,
        },

        FnArg::NormalFlag(FnNormalFlagArg {
            names,
            is_optional: _,
            typ: _,
        }) => match names {
            FnFlagArgNames::ShortFlag(short) => short.at,
            FnFlagArgNames::LongFlag(long) => long.at,
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => long.at,
        },

        FnArg::Rest(FnRestArg { name, typ: _ }) => name.at,
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
                if *name == short.data {
                    Some(short.data.to_string())
                } else {
                    None
                }
            }

            FnFlagArgNames::LongFlag(_) => None,

            FnFlagArgNames::LongAndShortFlag { long, short } => {
                if *name == short.data {
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
                if *name == long.data {
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
    name: &Span<String>,
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
                        .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            );

            for method in not_matching {
                err = err.with_info(
                    ExecInfoType::Note,
                    format!(
                        "a method with the same name exists for type: {}",
                        method
                            .on_type
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
    Parsed(&'a Span<Vec<Span<FnCallArg>>>),
    ParsedCmdArgs {
        at: CodeRange,
        args: Vec<(CmdArgResult, CodeRange)>,
    },
    Internal {
        at: &'static str,
        args: Vec<CmdArgResult>,
    },
}

#[derive(Debug)]
pub struct ValidatedFnCallArg {
    pub decl_name_at: RuntimeCodeRange,
    pub arg_value_at: RuntimeCodeRange,
    pub value: RuntimeValue,
}
