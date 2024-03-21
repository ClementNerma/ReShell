use std::fmt::Debug;

use indexmap::IndexSet;
use parsy::{Eaten, FileId};
use reshell_checker::output::CheckerOutput;
use reshell_parser::ast::{
    Block, ElsIf, FunctionBody, Instruction, Program, RuntimeCodeRange, SwitchCase,
};

use crate::{
    cmd::{run_cmd, CmdExecParams},
    context::{
        CallStackEntry, Context, DepsScopeCreationData, ScopeCmdAlias, ScopeContent, ScopeFn,
        ScopeVar,
    },
    errors::{ExecErrorNature, ExecResult},
    expr::eval_expr,
    functions::eval_fn_call,
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    props::{eval_props_access, PropAccessPolicy, PropAccessTailPolicy, PropAssignment},
    values::{
        are_values_equal, CapturedDependencies, LocatedValue, NotComparableTypes, RuntimeCmdAlias,
        RuntimeFnBody, RuntimeFnSignature, RuntimeFnValue, RuntimeValue,
    },
};

pub fn run_program(
    program: &Eaten<Program>,
    checker_output: CheckerOutput,
    ctx: &mut Context,
) -> ExecResult<()> {
    // Reset Ctrl+C requests
    ctx.reset_ctrl_c_press_indicator();

    let Program { content } = &program.data;

    match content.at.start.file_id {
        FileId::None | FileId::Internal | FileId::Custom(_) => {
            return Err(ctx.error(content.at, "program must be backed by a source file"))
        }

        FileId::SourceFile(id) => assert!(ctx.files_map().get_file(id).is_some()),
    }

    ctx.prepare_for_new_program(program, checker_output);

    run_block_in_current_scope(&content.data, ctx).map(|result| match result {
        None => {
            ctx.reset_to_program_main_scope();
        }
        Some(_) => ctx.panic(
            content.at,
            "this instruction shouldn't have been allowed to run here",
        ),
    })
}

fn run_block(
    block: &Eaten<Block>,
    ctx: &mut Context,
    content: Option<ScopeContent>,
) -> ExecResult<Option<InstrRet>> {
    ctx.create_and_push_scope(
        RuntimeCodeRange::Parsed(block.at),
        content.unwrap_or_else(ScopeContent::new),
    );

    let result = run_block_in_current_scope(&block.data, ctx);

    ctx.pop_scope();

    result
}

fn run_block_in_current_scope(block: &Block, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    let Block { instructions } = block;

    // First pass: collect functions declaration
    for instr in instructions {
        if let Instruction::FnDecl { name, content } = &instr.data {
            let parent_scopes = ctx.generate_parent_scopes_list();

            let body = ctx
                .get_fn_body(&content.body)
                .unwrap_or_else(|| ctx.panic(content.body.at, "unregistered function body"));

            let signature = ctx.get_fn_signature(&content.signature).unwrap_or_else(|| {
                ctx.panic(content.signature.at, "unregistered function signature")
            });

            let fns = &mut ctx.current_scope_content_mut().fns;

            let dup = fns.insert(
                name.data.clone(),
                ScopeFn {
                    name_at: RuntimeCodeRange::Parsed(name.at),
                    value: GcReadOnlyCell::new(RuntimeFnValue {
                        body: RuntimeFnBody::Block(body),
                        signature: RuntimeFnSignature::Shared(signature),
                        parent_scopes,
                        captured_deps: GcOnceCell::new_uninit(),
                    }),
                },
            );

            // Ensure checker did its job correctly (no duplicate name)
            assert!(dup.is_none());
        }
    }

    // Second pass: run instructions
    for instr in instructions {
        if let Some(ret) = run_instr(instr, ctx)? {
            return Ok(Some(ret));
        }
    }

    Ok(None)
}

pub(crate) fn run_body_with_deps(
    body: &Eaten<FunctionBody>,
    captured_deps: CapturedDependencies,
    ctx: &mut Context,
    scope_content: Option<ScopeContent>,
    parent_scopes: IndexSet<u64>,
    call_stack_entry: Option<CallStackEntry>,
) -> ExecResult<Option<InstrRet>> {
    ctx.create_and_push_scope_with_deps(
        RuntimeCodeRange::Parsed(body.at),
        DepsScopeCreationData::CapturedDeps(captured_deps),
        scope_content,
        parent_scopes,
        call_stack_entry,
    );

    let result = match &body.data {
        FunctionBody::Block(block) => run_block_in_current_scope(&block.data, ctx),
        FunctionBody::Expr(expr) => eval_expr(&expr.data, ctx).map(|value| {
            Some(InstrRet::FnReturn(Some(LocatedValue::new(
                value,
                RuntimeCodeRange::Parsed(body.at),
            ))))
        }),
    };

    ctx.pop_scope();

    result
}

fn run_instr(instr: &Eaten<Instruction>, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    ctx.clear_wandering_value();

    match &instr.data {
        Instruction::DeclareVar {
            name,
            mutable,
            init_expr,
        } => {
            // assert!(
            //     !ctx.current_scope().content.vars.contains_key(&name.data),
            //     "duplicate variable declaration (this is a bug in the checker)"
            // );

            let init_value = eval_expr(&init_expr.data, ctx)
                .map(|value| LocatedValue::new(value, RuntimeCodeRange::Parsed(init_expr.at)))?;

            ctx.current_scope_content_mut().vars.insert(
                name.data.clone(),
                ScopeVar {
                    name_at: RuntimeCodeRange::Parsed(name.at),
                    is_mut: mutable.is_some(),
                    value: GcCell::new(init_value),
                },
            );
        }

        Instruction::AssignVar {
            name,
            prop_acc,
            list_push,
            expr,
        } => {
            let var = ctx.get_visible_var(name).cloned().unwrap_or_else(|| ctx.panic(name.at, format!("variable not alive (this is a bug either in the checker or in the garbage collector): {name:?}")));

            // Same goes here
            assert!(var.is_mut);

            let assign_value = eval_expr(&expr.data, ctx)?;

            eval_props_access(
                &mut var.value.write(name.at, ctx)?.value,
                prop_acc.iter(),
                PropAccessPolicy::Write(match list_push {
                    Some(_) => PropAccessTailPolicy::ExistingOnly,
                    None => PropAccessTailPolicy::TailMayNotExist,
                }),
                ctx,
                |left, ctx| match list_push {
                    None => {
                        match left {
                            PropAssignment::ReadExisting(_) => unreachable!(),
                            PropAssignment::WriteExisting(existing) => *existing = assign_value,
                            PropAssignment::Create(entry) => {
                                entry.insert(assign_value);
                            }
                        }

                        Ok(())
                    }

                    Some(list_push) => match left {
                        PropAssignment::ReadExisting(_) => unreachable!(),

                        PropAssignment::WriteExisting(existing) => match existing {
                            RuntimeValue::List(list) => {
                                list.write(list_push.at, ctx)?.push(assign_value);
                                Ok(())
                            }

                            _ => {
                                let left_type = existing
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline());

                                Err(ctx.error(
                                    list_push.at,
                                    format!(
                                        "cannot push a value as this is not a list but a {}",
                                        left_type
                                    ),
                                ))
                            }
                        },

                        PropAssignment::Create(_) => unreachable!(),
                    },
                },
            )??;
        }

        Instruction::IfCond {
            cond,
            body,
            elsif,
            els,
        } => {
            let cond_val =
                match eval_expr(&cond.data, ctx)? {
                    RuntimeValue::Bool(bool) => bool,
                    value => {
                        return Err(ctx.error(
                            cond.at,
                            format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value.get_type().render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                        ))
                    }
                };

            let mut ret = None;

            if cond_val {
                ret = Some(run_block(body, ctx, None)?);
            } else {
                for branch in elsif {
                    let ElsIf { cond, body } = &branch.data;

                    let cond_val = eval_expr(&cond.data, ctx)?;
                    let RuntimeValue::Bool(cond_val) = cond_val else {
                        return Err(ctx.error(cond.at, format!("expected the condition to resolve to a boolean, found a {} instead", cond_val.get_type().render_colored(ctx, PrettyPrintOptions::inline()))));
                    };

                    if cond_val {
                        ret = Some(run_block(body, ctx, None)?);
                        break;
                    }
                }

                if ret.is_none() {
                    if let Some(els) = els {
                        ret = Some(run_block(els, ctx, None)?);
                    }
                }
            }

            return Ok(ret.flatten());
        }

        Instruction::ForLoop {
            iter_var,
            iter_on,
            body,
        } => match eval_expr(&iter_on.data, ctx)? {
            RuntimeValue::List(list) => {
                for item in list.read(iter_on.at).iter() {
                    let mut loop_scope = ScopeContent::new();

                    loop_scope.vars.insert(
                        iter_var.data.clone(),
                        ScopeVar {
                            name_at: RuntimeCodeRange::Parsed(iter_var.at),
                            is_mut: false,
                            value: GcCell::new(LocatedValue::new(
                                item.clone(),
                                RuntimeCodeRange::Parsed(iter_var.at),
                            )),
                        },
                    );

                    if let Some(ret) = run_block(body, ctx, Some(loop_scope))? {
                        match ret {
                            InstrRet::ContinueLoop => {}
                            InstrRet::BreakLoop => break,
                            InstrRet::FnReturn(value) => {
                                return Ok(Some(InstrRet::FnReturn(value)))
                            }
                        }
                    }
                }
            }

            RuntimeValue::Range { from, to } => {
                for i in from..=to {
                    let mut loop_scope = ScopeContent::new();

                    loop_scope.vars.insert(
                        iter_var.data.clone(),
                        ScopeVar {
                            name_at: RuntimeCodeRange::Parsed(iter_var.at),
                            is_mut: false,
                            value: GcCell::new(LocatedValue::new(
                                RuntimeValue::Int(i),
                                RuntimeCodeRange::Parsed(iter_var.at),
                            )),
                        },
                    );

                    if let Some(ret) = run_block(body, ctx, Some(loop_scope))? {
                        match ret {
                            InstrRet::ContinueLoop => {}
                            InstrRet::BreakLoop => break,
                            InstrRet::FnReturn(value) => {
                                return Ok(Some(InstrRet::FnReturn(value)))
                            }
                        }
                    }
                }
            }

            value => {
                return Err(ctx.error(
                    iter_on.at,
                    format!(
                        "expected a list or range to iterate on, found a {} instead",
                        value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline())
                    ),
                ))
            }
        },

        Instruction::ForLoopKeyed {
            key_iter_var,
            value_iter_var,
            iter_on,
            body,
        } => {
            let map = match eval_expr(&iter_on.data, ctx)? {
                RuntimeValue::Map(map) => map,
                value => {
                    return Err(ctx.error(
                        iter_on.at,
                        format!(
                            "expected a map, got a {}",
                            value
                                .get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                    ))
                }
            };

            for (key, value) in map.read(iter_on.at).iter() {
                let mut loop_scope = ScopeContent::new();

                loop_scope.vars.insert(
                    key_iter_var.data.clone(),
                    ScopeVar {
                        name_at: RuntimeCodeRange::Parsed(key_iter_var.at),
                        is_mut: false,
                        value: GcCell::new(LocatedValue::new(
                            RuntimeValue::String(key.clone()),
                            RuntimeCodeRange::Parsed(key_iter_var.at),
                        )),
                    },
                );

                loop_scope.vars.insert(
                    value_iter_var.data.clone(),
                    ScopeVar {
                        name_at: RuntimeCodeRange::Parsed(value_iter_var.at),
                        is_mut: false,
                        value: GcCell::new(LocatedValue::new(
                            value.clone(),
                            RuntimeCodeRange::Parsed(value_iter_var.at),
                        )),
                    },
                );

                if let Some(ret) = run_block(body, ctx, Some(loop_scope))? {
                    match ret {
                        InstrRet::ContinueLoop => {}
                        InstrRet::BreakLoop => break,
                        InstrRet::FnReturn(value) => return Ok(Some(InstrRet::FnReturn(value))),
                    }
                }
            }
        }

        Instruction::WhileLoop { cond, body } => loop {
            let cond_val =
                match eval_expr(&cond.data, ctx)? {
                    RuntimeValue::Bool(bool) => bool,
                    value => {
                        return Err(ctx.error(
                            cond.at,
                            format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value.get_type().render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                        ))
                    }
                };

            if !cond_val {
                break;
            }

            if let Some(ret) = run_block(body, ctx, None)? {
                match ret {
                    InstrRet::ContinueLoop => {}
                    InstrRet::BreakLoop => break,
                    InstrRet::FnReturn(value) => return Ok(Some(InstrRet::FnReturn(value))),
                }
            }
        },

        Instruction::LoopContinue => return Ok(Some(InstrRet::ContinueLoop)),

        Instruction::LoopBreak => return Ok(Some(InstrRet::BreakLoop)),

        Instruction::Switch { expr, cases } => {
            let switch_on = eval_expr(&expr.data, ctx)?;

            for SwitchCase { cond, body } in cases {
                let case_value = eval_expr(&cond.data, ctx)?;

                let cmp = are_values_equal(&switch_on, &case_value).map_err(
                    |NotComparableTypes { reason }| {
                        ctx.error(
                            cond.at,
                            format!(
                                "cannot compare {} and {}: {reason}",
                                switch_on
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline()),
                                case_value
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline())
                            ),
                        )
                    },
                )?;

                if cmp {
                    run_block(body, ctx, None)?;
                    break;
                }
            }
        }

        Instruction::FnDecl { name, content } => {
            let captured_deps = ctx.capture_deps(content.body.at);

            ctx.current_scope_content_mut()
                .fns
                .get_mut(&name.data)
                .unwrap()
                .value
                .captured_deps
                .init(captured_deps)
                .unwrap();
        }

        Instruction::FnReturn { expr } => {
            return Ok(Some(InstrRet::FnReturn(
                expr.as_ref()
                    .map(|expr| {
                        eval_expr(&expr.data, ctx).map(|value| {
                            LocatedValue::new(value, RuntimeCodeRange::Parsed(expr.at))
                        })
                    })
                    .transpose()?,
            )))
        }

        Instruction::Throw(expr) => {
            let message = match eval_expr(&expr.data, ctx)? {
                RuntimeValue::String(string) => string,
                value => {
                    return Err(ctx.error(
                        expr.at,
                        format!(
                            "expected a string, found a {}",
                            value
                                .get_type()
                                .render_colored(ctx, PrettyPrintOptions::inline())
                        ),
                    ))
                }
            };

            return Err(ctx.error(
                instr.at,
                ExecErrorNature::Thrown {
                    at: RuntimeCodeRange::Parsed(instr.at),
                    message,
                },
            ));
        }

        Instruction::Try {
            call,
            catch_var,
            catch_body,
        } => {
            if let Err(err) = eval_fn_call(call, ctx) {
                match err.nature {
                    ExecErrorNature::Thrown { at, message } => {
                        let mut scope = ScopeContent::new();

                        scope.vars.insert(
                            catch_var.data.clone(),
                            ScopeVar {
                                name_at: RuntimeCodeRange::Parsed(catch_var.at),
                                is_mut: false,
                                value: GcCell::new(LocatedValue::new(
                                    RuntimeValue::String(message),
                                    at,
                                )),
                            },
                        );

                        run_block(catch_body, ctx, Some(scope))?;
                    }
                    _ => return Err(err),
                }
            }
        }

        Instruction::CmdAliasDecl { name, content } => {
            // We can do this thanks to the checker
            assert!(!ctx
                .current_scope_content_mut()
                .cmd_aliases
                .contains_key(&name.data));

            let parent_scopes = ctx.generate_parent_scopes_list();

            let captured_deps = ctx.capture_deps(content.at);

            let alias_content = ctx
                .get_cmd_alias_content(content)
                .unwrap_or_else(|| ctx.panic(content.at, "unregistered command alias content"));

            let cmd_aliases = &mut ctx.current_scope_content_mut().cmd_aliases;

            cmd_aliases.insert(
                name.data.clone(),
                ScopeCmdAlias {
                    name_at: name.at,
                    value: GcReadOnlyCell::new(RuntimeCmdAlias {
                        name_declared_at: name.at,
                        alias_content,
                        parent_scopes,
                        captured_deps,
                    }),
                },
            );
        }

        Instruction::TypeAliasDecl {
            name: _,
            content: _,
        } => {
            // Nothing to do here as this was already put inside context before
        }

        Instruction::CmdCall(call) => {
            let cmd_result = run_cmd(call, ctx, CmdExecParams { capture: None })?;

            if ctx.current_scope().id == ctx.program_main_scope().unwrap() {
                if let Some(Some(last_return_value)) = cmd_result.as_returned() {
                    ctx.set_wandering_value(last_return_value.value);
                }
            }
        }

        Instruction::FnCall(call) => {
            let returned = eval_fn_call(call, ctx)?;

            if ctx.current_scope().id == ctx.program_main_scope().unwrap() {
                if let Some(returned) = returned {
                    ctx.set_wandering_value(returned.value);
                }
            }
        }

        Instruction::DoBlock(block) => {
            run_block(block, ctx, None)?;
        }

        Instruction::Imported(program) => {
            ctx.inside_imported_program(program, |ctx| {
                let Program { content } = &program.data;

                run_block_in_current_scope(&content.data, ctx)
            })?;
        }
    }

    Ok(None)
}

#[derive(Debug, Clone)]
pub enum InstrRet {
    ContinueLoop,
    BreakLoop,
    FnReturn(Option<LocatedValue>),
}
