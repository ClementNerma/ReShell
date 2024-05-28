use std::fmt::Debug;

use indexmap::IndexSet;
use parsy::{CodeRange, Eaten, FileId};
use reshell_checker::CheckerOutput;
use reshell_parser::ast::{Block, ElsIf, Function, Instruction, Program, SwitchCase};

use crate::{
    cmd::run_cmd,
    context::{
        CallStackEntry, Context, DepsScopeCreationData, ScopeContent, ScopeFn, ScopeRange, ScopeVar,
    },
    errors::ExecResult,
    expr::eval_expr,
    functions::{call_fn, FnCallResult},
    gc::{GcCell, GcReadOnlyCell},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    props::{eval_props_access, PropAccessPolicy},
    values::{
        are_values_equal, CapturedDependencies, LocatedValue, NotComparableTypes, RuntimeCmdAlias,
        RuntimeFnBody, RuntimeFnValue, RuntimeValue,
    },
};

#[derive(Debug, Clone, Copy)]
#[must_use]
pub enum ProgramExitStatus {
    Normal,
    ExitRequested { code: u8 },
}

pub fn run_program(
    program: &Program,
    checker_output: CheckerOutput,
    ctx: &mut Context,
) -> ExecResult<ProgramExitStatus> {
    let Program { content } = program;

    match program.content.at.start.file_id {
        FileId::None | FileId::Internal | FileId::Custom(_) => {
            return Err(ctx.error(
                program.content.at,
                "program must be backed by a source file",
            ))
        }

        FileId::SourceFile(id) => assert!(ctx.files_map().get_file(id).is_some()),
    }

    ctx.append_checker_output(checker_output);
    ctx.set_curr_scope_range(program.content.at);

    match run_block_in_current_scope(&content.data, ctx) {
        Ok(instr_ret) => match instr_ret {
            None => Ok(ProgramExitStatus::Normal),
            Some(instr_ret) => {
                Err(ctx.error(instr_ret.from, "this instruction can't be used here"))
            }
        },

        Err(err) => {
            ctx.reset_to_first_scope();

            match err.has_exit_code {
                None => Err(err),
                Some(code) => Ok(ProgramExitStatus::ExitRequested { code }),
            }
        }
    }
}

fn run_block(
    block: &Block,
    ctx: &mut Context,
    content: ScopeContent,
) -> ExecResult<Option<InstrRet>> {
    let Block {
        instructions: _,
        code_range,
    } = block;

    ctx.create_and_push_scope(ScopeRange::CodeRange(*code_range), content);

    let ret = run_block_in_current_scope(block, ctx)?;

    ctx.pop_scope();

    Ok(ret)
}

fn run_block_in_current_scope(block: &Block, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    let Block {
        instructions,
        code_range: _,
    } = block;

    for instr in instructions {
        if let Some(ret) = run_instr(instr, ctx)? {
            return Ok(Some(ret));
        }
    }

    Ok(None)
}

pub(crate) fn run_body_with_deps(
    block: &Block,
    captured_deps: CapturedDependencies,
    ctx: &mut Context,
    content: ScopeContent,
    parent_scopes: IndexSet<u64>,
    call_stack_entry: Option<CallStackEntry>,
) -> ExecResult<Option<InstrRet>> {
    let Block {
        instructions: _,
        code_range,
    } = block;

    ctx.create_and_push_scope_with_deps(
        ScopeRange::CodeRange(*code_range),
        DepsScopeCreationData::CapturedDeps(captured_deps),
        content,
        parent_scopes,
        call_stack_entry,
    );

    let instr_ret = run_block_in_current_scope(block, ctx)?;

    ctx.pop_scope();

    Ok(instr_ret)
}

fn run_instr(instr: &Eaten<Instruction>, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    match &instr.data {
        Instruction::Comment { content: _ } => {}

        Instruction::DeclareVar {
            name,
            mutable,
            init_expr,
        } => {
            assert!(
                !ctx.current_scope().content.vars.contains_key(&name.data),
                "duplicate variable declaration (this is a bug in the checker)"
            );

            let init_value = init_expr
                .as_ref()
                .map(|expr| {
                    eval_expr(&expr.data, ctx).map(|value| LocatedValue::new(value, expr.at))
                })
                .transpose()?;

            ctx.current_scope_content_mut().vars.insert(
                name.data.clone(),
                ScopeVar {
                    declared_at: name.at,
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
            // We can expect() thanks to the checker
            let var = ctx.get_visible_var(name).cloned().unwrap_or_else(|| panic!("internal error: variable not alive (this is a bug either in the checker or in the garbage collector)\nDebug infos:\n> {name:?}"));

            // Same goes here
            assert!(var.is_mut);

            let assign_value = eval_expr(&expr.data, ctx)?;

            if var.value.read().is_none() {
                if let Some(first) = prop_acc.first() {
                    return Err(ctx.error(
                        first.at,
                        "cannot access this property as this variable wasn't assigned a value yet",
                    ));
                }

                *var.value.write() = Some(LocatedValue::new(assign_value, expr.at));
            } else {
                eval_props_access(
                    &mut var.value.write().as_mut().unwrap().value,
                    prop_acc.iter(),
                    PropAccessPolicy::TrailingAccessMayNotExist,
                    ctx,
                    |left, ctx| match list_push {
                        None => {
                            *left = assign_value;
                            Ok(())
                        }

                        Some(list_push) => match left {
                            RuntimeValue::List(list) => {
                                list.write().push(assign_value);
                                Ok(())
                            }

                            _ => {
                                let left_type = left
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
                    },
                )??;
            };
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
                ret = Some(run_block(&body.data, ctx, ScopeContent::new())?);
            } else {
                for branch in elsif {
                    let ElsIf { cond, body } = &branch.data;

                    let cond_val = eval_expr(&cond.data, ctx)?;
                    let RuntimeValue::Bool(cond_val) = cond_val else {
                        return Err(ctx.error(cond.at, format!("expected the condition to resolve to a boolean, found a {} instead", cond_val.get_type().render_colored(ctx, PrettyPrintOptions::inline()))));
                    };

                    if cond_val {
                        ret = Some(run_block(&body.data, ctx, ScopeContent::new())?);
                        break;
                    }
                }

                if ret.is_none() {
                    if let Some(els) = els {
                        ret = Some(run_block(&els.data, ctx, ScopeContent::new())?);
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
                // TODO: handle locks (e.g. program tries to modify list while iterating)
                for item in list.read().iter() {
                    let mut loop_scope = ScopeContent::new();

                    loop_scope.vars.insert(
                        iter_var.data.clone(),
                        ScopeVar {
                            declared_at: iter_var.at,
                            is_mut: false,
                            value: GcCell::new(Some(LocatedValue::new(item.clone(), iter_var.at))),
                        },
                    );

                    if let Some(ret) = run_block(&body.data, ctx, loop_scope)? {
                        let InstrRet { typ, from: _ } = ret;

                        if matches!(typ, InstrRetType::BreakLoop) {
                            break;
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
                            declared_at: iter_var.at,
                            is_mut: false,
                            value: GcCell::new(Some(LocatedValue::new(
                                RuntimeValue::Int(i as i64),
                                iter_var.at,
                            ))),
                        },
                    );

                    if let Some(ret) = run_block(&body.data, ctx, loop_scope)? {
                        let InstrRet { typ, from: _ } = ret;

                        if matches!(typ, InstrRetType::BreakLoop) {
                            break;
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

            if let Some(ret) = run_block(&body.data, ctx, ScopeContent::new())? {
                let InstrRet { typ, from: _ } = ret;

                if matches!(typ, InstrRetType::BreakLoop) {
                    break;
                }
            }
        },

        Instruction::LoopContinue => {
            return Ok(Some(InstrRet {
                from: instr.at,
                typ: InstrRetType::ContinueLoop,
            }))
        }

        Instruction::LoopBreak => {
            return Ok(Some(InstrRet {
                from: instr.at,
                typ: InstrRetType::BreakLoop,
            }))
        }

        Instruction::Switch { expr, cases } => {
            let switch_on = eval_expr(&expr.data, ctx)?;

            for SwitchCase { cond, body } in cases {
                let case_value = eval_expr(&cond.data, ctx)?;

                let cmp =
                    are_values_equal(&switch_on, &case_value).map_err(|NotComparableTypes| {
                        ctx.error(
                            cond.at,
                            format!(
                                "cannot compare {} and {}",
                                switch_on
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline()),
                                case_value
                                    .get_type()
                                    .render_colored(ctx, PrettyPrintOptions::inline())
                            ),
                        )
                    })?;

                if cmp {
                    run_block(&body.data, ctx, ScopeContent::new())?;
                    break;
                }
            }
        }

        Instruction::FnDecl { name, content } => {
            let parent_scopes = ctx.generate_parent_scopes();

            let captured_deps = ctx.capture_deps(content.body.data.code_range);

            let fns = &mut ctx.current_scope_content_mut().fns;

            // We can do this thanks to the checker
            assert!(!fns.contains_key(&name.data));

            let Function { signature, body } = content;

            fns.insert(
                name.data.clone(),
                ScopeFn {
                    declared_at: name.at,
                    value: GcReadOnlyCell::new(RuntimeFnValue {
                        body: RuntimeFnBody::Block(body.clone()),
                        signature: signature.clone(),
                        parent_scopes,
                        captured_deps,
                    }),
                },
            );
        }

        Instruction::FnReturn { expr } => {
            return Ok(Some(InstrRet {
                from: instr.at,
                typ: InstrRetType::FnReturn(
                    expr.as_ref()
                        .map(|expr| {
                            eval_expr(&expr.data, ctx)
                                .map(|value| LocatedValue::new(value, expr.at))
                        })
                        .transpose()?,
                ),
            }))
        }

        Instruction::Throw(expr) => {
            return Ok(Some(InstrRet {
                from: instr.at,
                typ: InstrRetType::Thrown(LocatedValue::new(eval_expr(&expr.data, ctx)?, expr.at)),
            }));
        }

        Instruction::Try {
            call,
            catch_var,
            catch_body,
        } => match call_fn(call, ctx)? {
            FnCallResult::Success { returned: _ } => {}
            FnCallResult::Thrown(LocatedValue { value, from }) => {
                let mut scope = ScopeContent::new();

                scope.vars.insert(
                    catch_var.data.clone(),
                    ScopeVar {
                        declared_at: catch_var.at,
                        is_mut: false,
                        value: GcCell::new(Some(LocatedValue { value, from })),
                    },
                );

                run_block(&catch_body.data, ctx, scope)?;
            }
        },

        Instruction::CmdAliasDecl { name, content } => {
            let parent_scopes = ctx.generate_parent_scopes();

            let captured_deps = ctx.capture_deps(content.at);

            let cmd_aliases = &mut ctx.current_scope_content_mut().cmd_aliases;

            // We can do this thanks to the checker
            assert!(!cmd_aliases.contains_key(&name.data));

            cmd_aliases.insert(
                name.data.clone(),
                RuntimeCmdAlias {
                    name_declared_at: name.at,
                    alias_content: GcReadOnlyCell::new(content.data.clone()),
                    parent_scopes,
                    captured_deps,
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
            run_cmd(call, ctx, false)?;
        }

        Instruction::BaseBlock(block) => {
            run_block(&block.data, ctx, ScopeContent::new())?;
        }
    }

    Ok(None)
}

#[derive(Debug)]
pub struct InstrRet {
    pub typ: InstrRetType,
    pub from: CodeRange,
}

#[derive(Debug, Clone)]
pub enum InstrRetType {
    ContinueLoop,
    BreakLoop,
    FnReturn(Option<LocatedValue>),
    Thrown(LocatedValue),
    Exit,
}
