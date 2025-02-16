use std::fmt::Debug;

use indexmap::IndexSet;
use parsy::{CodeRange, FileId, Span};
use reshell_parser::ast::{
    Block, ElsIf, Instruction, MatchCase, ObjDestructuringItem, ObjDestructuringItemBinding,
    ObjDestructuringItemType, Program, RuntimeCodeRange, SingleVarDecl, TypeMatchCase, ValueType,
    VarDeconstruction,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    cmd::{run_cmd, CmdExecParams},
    context::{
        CallStackEntry, Context, DepsScopeCreationData, ScopeCmdAlias, ScopeContent, ScopeFn,
        ScopeMethod, ScopeVar,
    },
    errors::{ExecError, ExecErrorNature, ExecInfoType, ExecResult},
    expr::{eval_expr, eval_range_bound},
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    props::{eval_props_access, PropAccessMode, TailPropAccessPolicy, TailPropWritingPolicy},
    typechecker::check_if_value_fits_type,
    values::{
        are_values_equal, CapturedDependencies, LocatedValue, NotComparableTypesErr,
        RuntimeCmdAlias, RuntimeFnBody, RuntimeFnSignature, RuntimeFnValue, RuntimeValue,
    },
};

pub fn run_program(program: &Span<Program>, ctx: &mut Context) -> ExecResult<Option<LocatedValue>> {
    // Reset Ctrl+C requests
    ctx.reset_ctrl_c_press_indicator();

    let Program { content } = &program.data;

    match content.at.start.file_id {
        FileId::None | FileId::Internal | FileId::Custom(_) => {
            return Err(ctx.error(content.at, "program must be backed by a source file"))
        }

        FileId::SourceFile(id) => assert!(ctx.files_map().get_file(id).is_some()),
    }

    // Check the program
    ctx.prepare_for_new_program(program).map_err(|err| {
        ctx.error(program.at, ExecErrorNature::CheckingErr(err))
            .with_info(
                ExecInfoType::Note,
                "Error was encountered before running the program".to_owned(),
            )
    })?;

    run_block_in_current_scope(&content.data, ctx).map(|result| match result {
        None => {
            ctx.reset_to_program_main_scope();
            None
        }

        Some(InstrRet::WanderingValue(value)) => {
            ctx.reset_to_program_main_scope();
            Some(value)
        }

        Some(_) => ctx.panic(
            content.at,
            "this instruction shouldn't have been allowed to run here",
        ),
    })
}

fn run_block(
    block: &Span<Block>,
    ctx: &mut Context,
    content: Option<ScopeContent>,
) -> ExecResult<Option<InstrRet>> {
    // Handle any Ctrl+C press
    ctx.ensure_no_ctrl_c_press(block.at)?;

    ctx.create_and_push_scope(
        block.data.scope_id,
        content.unwrap_or_else(ScopeContent::new),
    );

    let result = run_block_in_current_scope(&block.data, ctx);

    ctx.pop_scope();

    result
}

fn run_block_in_current_scope(block: &Block, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    let Block {
        scope_id: _,
        instructions,
    } = block;

    // First pass: collect functions declaration
    block_first_pass(instructions, block, ctx)?;

    // Second pass: run instructions
    run_instructions_in_current_scope(instructions, ctx)
}

fn run_instructions_in_current_scope(
    instructions: &[Span<Instruction>],
    ctx: &mut Context,
) -> ExecResult<Option<InstrRet>> {
    let mut wandering_value = None;

    for instr in instructions {
        wandering_value = None;

        match run_instr(instr, ctx)? {
            Some(InstrRet::WanderingValue(value)) => {
                wandering_value = Some(value);
            }

            Some(ret) => return Ok(Some(ret)),

            None => {}
        }
    }

    Ok(wandering_value.map(InstrRet::WanderingValue))
}

fn block_first_pass(
    instructions: &[Span<Instruction>],
    block: &Block,
    ctx: &mut Context,
) -> ExecResult<()> {
    for instr in instructions {
        // Handle any Ctrl+C press
        ctx.ensure_no_ctrl_c_press(instr.at)?;

        // Run the instruction
        match &instr.data {
            Instruction::FnDecl { name, content } => {
                let parent_scopes = ctx.generate_parent_scopes_list();

                let body = ctx.get_fn_body(&content.body);

                let signature = ctx.get_fn_signature(&content.signature);

                let fns = &mut ctx.current_scope_content_mut().fns;

                let dup = fns.insert(
                    name.data.clone(),
                    ScopeFn {
                        decl_scope_id: block.scope_id,
                        name_at: RuntimeCodeRange::Parsed(name.at),
                        value: GcReadOnlyCell::new(RuntimeFnValue {
                            body: RuntimeFnBody::Block(body),
                            signature: RuntimeFnSignature::Shared(signature),
                            is_method: false,
                            parent_scopes,
                            captured_deps: GcOnceCell::new_uninit(),
                        }),
                    },
                );

                // Ensure checker did its job correctly (no duplicate name)
                assert!(dup.is_none());
            }

            Instruction::MethodDecl {
                name,
                on_type,
                content,
            } => {
                let parent_scopes = ctx.generate_parent_scopes_list();

                let body = ctx.get_fn_body(&content.body);

                let signature = ctx.get_fn_signature(&content.signature);

                let methods = &mut ctx.current_scope_content_mut().methods;

                methods
                    .entry(name.data.clone())
                    .or_default()
                    .push(ScopeMethod {
                        name_at: RuntimeCodeRange::Parsed(name.at),
                        decl_scope_id: block.scope_id,
                        on_type: GcReadOnlyCell::new(on_type.clone()),
                        value: GcReadOnlyCell::new(RuntimeFnValue {
                            body: RuntimeFnBody::Block(body),
                            signature: RuntimeFnSignature::Shared(signature),
                            is_method: true,
                            parent_scopes,
                            captured_deps: GcOnceCell::new_uninit(),
                        }),
                    });
            }

            Instruction::Include(program) => {
                let Program { content } = program;

                let Block {
                    scope_id: _,
                    instructions,
                } = &content.data;

                block_first_pass(instructions, block, ctx)?;
            }

            _ => {}
        }
    }

    Ok(())
}

pub(crate) fn run_body_with_deps(
    body: &Span<Block>,
    captured_deps: CapturedDependencies,
    ctx: &mut Context,
    scope_content: Option<ScopeContent>,
    parent_scopes: IndexSet<u64>,
    call_stack_entry: Option<CallStackEntry>,
) -> ExecResult<Option<InstrRet>> {
    ctx.create_and_push_scope_with_deps(
        body.data.scope_id,
        DepsScopeCreationData::CapturedDeps(captured_deps),
        scope_content,
        parent_scopes,
        call_stack_entry,
    );

    let result = run_block_in_current_scope(&body.data, ctx);

    ctx.pop_scope();

    result
}

fn run_instr(instr: &Span<Instruction>, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    let instr_ret = match &instr.data {
        Instruction::DeclareVar { names, init_expr } => {
            let init_value = eval_expr(&init_expr.data, ctx)?;

            if matches!(init_value, RuntimeValue::Void) {
                return Err(ctx.error(init_expr.at, "cannot assign a void value"));
            }

            declare_vars(names, init_value, init_expr.at, ctx)?;

            None
        }

        Instruction::AssignVar {
            name,
            prop_acc,
            list_push,
            expr,
        } => {
            let var = ctx.get_visible_var(name).clone();

            // Same goes here
            assert!(var.is_mut);

            let assign_value = eval_expr(&expr.data, ctx)?;

            if matches!(assign_value, RuntimeValue::Void) {
                return Err(ctx.error(expr.at, "cannot assign a void value"));
            }

            if prop_acc.is_empty() && list_push.is_none() {
                if let Some(enforced_type) = &var.enforced_type {
                    if !check_if_value_fits_type(&assign_value, enforced_type, ctx) {
                        return Err(
                            ctx.error(
                                expr.at,
                                format!(
                                    "variable has enforced type {}, but tried to assign a value of type: {}",
                                    enforced_type.display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                                    assign_value.compute_type().display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                                )
                            )
                        );
                    }
                }
            }

            eval_props_access(
                &mut var.value.write(name.at, ctx)?.value,
                prop_acc.iter(),
                TailPropAccessPolicy::Write(match list_push {
                    Some(_) => TailPropWritingPolicy::ExistingOnly,
                    None => TailPropWritingPolicy::TailMayNotExist,
                }),
                ctx,
                |left, ctx| match list_push {
                    None => {
                        match left {
                            PropAccessMode::ReadExisting(_) => unreachable!(),
                            PropAccessMode::WriteExisting(existing) => *existing = assign_value,
                            PropAccessMode::Create(entry) => {
                                entry.insert(assign_value);
                            }
                        }

                        Ok(())
                    }

                    Some(list_push) => match left {
                        PropAccessMode::ReadExisting(_) => unreachable!(),

                        PropAccessMode::WriteExisting(existing) => match existing {
                            RuntimeValue::List(list) => {
                                list.write(list_push.at, ctx)?.push(assign_value);
                                Ok(())
                            }

                            _ => {
                                let existing_type = existing.compute_type();

                                let left_type = existing_type
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline());

                                Err(ctx.error(
                                    list_push.at,
                                    format!(
                                        "cannot push a value as this is not a list but a {}",
                                        left_type
                                    ),
                                ))
                            }
                        },

                        PropAccessMode::Create(_) => unreachable!(),
                    },
                },
            )??;

            var.value.write(name.at, ctx)?.from = RuntimeCodeRange::Parsed(expr.at);

            None
        }

        Instruction::IfCond {
            cond,
            body,
            elsif,
            els,
        } => {
            let cond_val = match eval_expr(&cond.data, ctx)? {
                RuntimeValue::Bool(bool) => bool,
                value => {
                    return Err(ctx.error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
                        return Err(ctx.error(cond.at, format!("expected the condition to resolve to a boolean, found a {} instead", cond_val.compute_type().display(ctx.type_alias_store(), PrettyPrintOptions::inline()))));
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

            ret.flatten()
        }

        Instruction::ForLoop {
            iter_var,
            iter_on,
            body,
        } => {
            match eval_expr(&iter_on.data, ctx)? {
                RuntimeValue::List(list) => {
                    for item in list.read(iter_on.at).iter() {
                        let mut loop_scope = ScopeContent::new();

                        loop_scope.vars.insert(
                            iter_var.data.clone(),
                            ScopeVar {
                                name_at: RuntimeCodeRange::Parsed(iter_var.at),
                                decl_scope_id: body.data.scope_id,
                                is_mut: false,
                                enforced_type: None,
                                value: GcCell::new(LocatedValue::new(
                                    RuntimeCodeRange::Parsed(iter_var.at),
                                    item.clone(),
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
                                InstrRet::WanderingValue(_) => {}
                            }
                        }
                    }
                }

                value => {
                    return Err(ctx.error(
                        iter_on.at,
                        format!(
                            "expected a list to iterate on, found a {} instead",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ))
                }
            }

            None
        }

        Instruction::ForLoopRanged {
            iter_var,
            iter_from,
            iter_to,
            inclusive,
            body,
        } => {
            let iter_from = eval_range_bound(iter_from, ctx)?;
            let iter_to = eval_range_bound(iter_to, ctx)?;

            let range = if *inclusive {
                iter_from..=iter_to
            } else if iter_to == i64::MIN {
                #[allow(clippy::reversed_empty_ranges)]
                {
                    0..=-1
                }
            } else {
                iter_from..=iter_to - 1
            };

            for num in range {
                let mut loop_scope = ScopeContent::new();

                loop_scope.vars.insert(
                    iter_var.data.clone(),
                    ScopeVar {
                        name_at: RuntimeCodeRange::Parsed(iter_var.at),
                        decl_scope_id: body.data.scope_id,
                        is_mut: false,
                        enforced_type: None,
                        value: GcCell::new(LocatedValue::new(
                            RuntimeCodeRange::Parsed(iter_var.at),
                            RuntimeValue::Int(num),
                        )),
                    },
                );

                if let Some(ret) = run_block(body, ctx, Some(loop_scope))? {
                    match ret {
                        InstrRet::ContinueLoop => {}
                        InstrRet::BreakLoop => break,
                        InstrRet::FnReturn(value) => return Ok(Some(InstrRet::FnReturn(value))),
                        InstrRet::WanderingValue(_) => {}
                    }
                }
            }

            None
        }

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
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
                        decl_scope_id: body.data.scope_id,
                        is_mut: false,
                        enforced_type: None,
                        value: GcCell::new(LocatedValue::new(
                            RuntimeCodeRange::Parsed(key_iter_var.at),
                            RuntimeValue::String(key.clone()),
                        )),
                    },
                );

                loop_scope.vars.insert(
                    value_iter_var.data.clone(),
                    ScopeVar {
                        name_at: RuntimeCodeRange::Parsed(value_iter_var.at),
                        decl_scope_id: body.data.scope_id,
                        is_mut: false,
                        enforced_type: None,
                        value: GcCell::new(LocatedValue::new(
                            RuntimeCodeRange::Parsed(value_iter_var.at),
                            value.clone(),
                        )),
                    },
                );

                if let Some(ret) = run_block(body, ctx, Some(loop_scope))? {
                    match ret {
                        InstrRet::ContinueLoop => {}
                        InstrRet::BreakLoop => break,
                        InstrRet::FnReturn(value) => return Ok(Some(InstrRet::FnReturn(value))),
                        InstrRet::WanderingValue(_) => {}
                    }
                }
            }

            None
        }

        Instruction::WhileLoop { cond, body } => {
            loop {
                let cond_val = match eval_expr(&cond.data, ctx)? {
                    RuntimeValue::Bool(bool) => bool,
                    value => {
                        return Err(ctx.error(
                            cond.at,
                            format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value.compute_type().display(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline()
                            )
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
                        InstrRet::WanderingValue(_) => {}
                    }
                }
            }

            None
        }

        Instruction::LoopContinue => return Ok(Some(InstrRet::ContinueLoop)),

        Instruction::LoopBreak => return Ok(Some(InstrRet::BreakLoop)),

        Instruction::Match { expr, cases, els } => {
            let match_on = eval_expr(&expr.data, ctx)?;

            for MatchCase { matches, body } in cases {
                let case_value = eval_expr(&matches.data, ctx)?;

                let cmp = are_values_equal(&match_on, &case_value).map_err(
                    |NotComparableTypesErr { reason }| {
                        ctx.error(
                            matches.at,
                            format!(
                                "cannot compare {} and {}: {reason}",
                                match_on
                                    .compute_type()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                                case_value
                                    .compute_type()
                                    .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            ),
                        )
                    },
                )?;

                if cmp {
                    return run_block(body, ctx, None);
                }
            }

            if let Some(els) = els {
                return run_block(els, ctx, None);
            }

            None
        }

        Instruction::TypeMatch { expr, cases, els } => {
            let match_on = eval_expr(&expr.data, ctx)?;

            for TypeMatchCase { matches, body } in cases {
                if check_if_value_fits_type(&match_on, matches, ctx) {
                    return run_block(body, ctx, None);
                }
            }

            if let Some(els) = els {
                return run_block(els, ctx, None);
            }

            None
        }

        Instruction::FnDecl { name, content } => {
            let captured_deps = ctx.capture_deps(content.body.at, content.body.data.scope_id);

            ctx.current_scope_content_mut()
                .fns
                .get_mut(&name.data)
                .unwrap()
                .value
                .captured_deps
                .init(captured_deps)
                .unwrap();

            None
        }

        Instruction::MethodDecl {
            name,
            on_type: _,
            content,
        } => {
            let captured_deps = ctx.capture_deps(content.body.at, content.body.data.scope_id);

            ctx.current_scope_content_mut()
                .methods
                .get_mut(&name.data)
                .unwrap()
                .iter_mut()
                .find(|method| match method.name_at {
                    RuntimeCodeRange::Parsed(decl_at) => decl_at == name.at,
                    RuntimeCodeRange::Internal(_) => false,
                })
                .unwrap()
                .value
                .captured_deps
                .init(captured_deps)
                .unwrap();

            None
        }

        Instruction::FnReturn { expr } => Some(InstrRet::FnReturn(
            expr.as_ref()
                .map(|expr| {
                    Ok::<_, Box<ExecError>>(LocatedValue::new(
                        RuntimeCodeRange::Parsed(expr.at),
                        eval_expr(&expr.data, ctx)?,
                    ))
                })
                .transpose()?,
        )),

        Instruction::Throw(expr) => {
            let message = match eval_expr(&expr.data, ctx)? {
                RuntimeValue::String(string) => string,

                value => {
                    return Err(ctx.error(
                        expr.at,
                        format!(
                            "expected a string, found a {}",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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
            try_expr,
            catch_var,
            catch_body,
        } => match eval_expr(&try_expr.data, ctx) {
            Ok(result) => Some(InstrRet::WanderingValue(LocatedValue::new(
                RuntimeCodeRange::Parsed(try_expr.at),
                result,
            ))),

            Err(err) => match err.nature {
                ExecErrorNature::Thrown { at, message } => {
                    let mut scope = ScopeContent::new();

                    scope.vars.insert(
                        catch_var.data.clone(),
                        ScopeVar {
                            name_at: RuntimeCodeRange::Parsed(catch_var.at),
                            decl_scope_id: catch_body.data.scope_id,
                            is_mut: false,
                            enforced_type: None,
                            value: GcCell::new(LocatedValue::new(
                                at,
                                RuntimeValue::String(message),
                            )),
                        },
                    );

                    run_block(catch_body, ctx, Some(scope))?
                }

                _ => return Err(err),
            },
        },

        Instruction::CmdAliasDecl {
            name,
            content,
            content_scope_id,
        } => {
            // We can do this thanks to the checker
            assert!(!ctx
                .current_scope_content_mut()
                .cmd_aliases
                .contains_key(&name.data));

            let parent_scopes = ctx.generate_parent_scopes_list();

            let captured_deps = ctx.capture_deps(content.at, *content_scope_id);

            let alias_content = ctx.get_cmd_alias_content(content);

            let decl_scope_id = ctx.current_scope().ast_scope_id;

            let cmd_aliases = &mut ctx.current_scope_content_mut().cmd_aliases;

            cmd_aliases.insert(
                name.data.clone(),
                ScopeCmdAlias {
                    decl_scope_id,
                    name_at: name.at,
                    value: GcReadOnlyCell::new(RuntimeCmdAlias {
                        name: name.data.clone(),
                        name_declared_at: name.at,
                        content: alias_content,
                        content_scope_id: *content_scope_id,
                        parent_scopes,
                        captured_deps,
                    }),
                },
            );

            None
        }

        Instruction::TypeAliasDecl {
            name: _,
            content: _,
        } => {
            // Nothing to do here as this was already put inside context before

            None
        }

        Instruction::DoBlock(block) => run_block(block, ctx, None)?,

        Instruction::CmdCall(call) => {
            let cmd_result = run_cmd(
                call,
                ctx,
                CmdExecParams {
                    capture: None,
                    silent: false,
                },
            )?;

            cmd_result.as_returned().map(InstrRet::WanderingValue)
        }

        Instruction::Include(program) => {
            let Program { content } = program;

            let Block {
                scope_id: _,
                instructions,
            } = &content.data;

            // NOTE: we don't use `run_block_in_current_scope` as it triggers a first-pass execution,
            //       which would re-declare functions, methods etc. that were already handled when
            //       exploring the parent block.
            return run_instructions_in_current_scope(instructions, ctx);
        }
    };

    Ok(instr_ret)
}

fn declare_vars(
    names: &Span<VarDeconstruction>,
    value: RuntimeValue,
    value_at: CodeRange,
    ctx: &mut Context,
) -> ExecResult<()> {
    match &names.data {
        VarDeconstruction::Single(single) => {
            let SingleVarDecl {
                name,
                is_mut,
                enforced_type,
            } = single;

            declare_var(
                name,
                DeclareVarData {
                    is_mut: *is_mut,
                    value,
                    value_at,
                    enforced_type: enforced_type.clone(),
                },
                ctx,
            )?;
        }

        VarDeconstruction::Tuple(members) => {
            let list = match value {
                RuntimeValue::List(list) => list,

                _ => {
                    return Err(ctx.error(
                        names.at,
                        format!(
                            "tried to spread a list but found a: {}",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ))
                }
            };

            let list = list.read_promise_no_write();

            if members.len() > list.len() {
                return Err(ctx.error(
                    names.at,
                    format!(
                        "tried to spread {} elements, but provided list contains {}",
                        members.len(),
                        list.len()
                    ),
                ));
            }

            for (names, value) in members.iter().zip(list.iter().cloned()) {
                declare_vars(names, value, value_at, ctx)?;
            }
        }

        VarDeconstruction::MapOrStruct(members) => {
            let map = match value {
                RuntimeValue::Map(map) => map,

                RuntimeValue::Struct(members) => members,

                _ => {
                    return Err(ctx.error(
                        names.at,
                        format!(
                            "tried to spread a map or struct but found a: {}",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ))
                }
            };

            let map = map.read_promise_no_write();

            for var in members {
                let ObjDestructuringItem { typ, default_value } = var;

                let (name, binding, is_mut) = match typ {
                    ObjDestructuringItemType::RawKeyToConst { name, binding } => {
                        (name, binding.as_ref(), false)
                    }

                    ObjDestructuringItemType::RawKeyToMut { name } => (name, None, true),

                    ObjDestructuringItemType::LiteralKeyToConst {
                        literal_name,
                        binding,
                    } => (literal_name, Some(binding), false),
                };

                let value = match map.get(&name.data) {
                    Some(value) => value.clone(),

                    None => match default_value {
                        Some(default_value) => eval_expr(default_value, ctx)?,

                        None => {
                            return Err(
                                ctx.error(name.at, "this property was not found in provided value")
                            );
                        }
                    },
                };

                match binding {
                    Some(ObjDestructuringItemBinding::BindTo { alias, is_mut }) => declare_var(
                        alias,
                        DeclareVarData {
                            is_mut: *is_mut,
                            value,
                            value_at,
                            enforced_type: None,
                        },
                        ctx,
                    )?,

                    Some(ObjDestructuringItemBinding::Deconstruct(deconstruct)) => {
                        declare_vars(deconstruct, value, value_at, ctx)?;
                    }

                    None => {
                        declare_var(
                            name,
                            DeclareVarData {
                                is_mut,
                                value,
                                value_at,
                                enforced_type: None,
                            },
                            ctx,
                        )?;
                    }
                }
            }
        }
    }

    Ok(())
}

struct DeclareVarData {
    is_mut: bool,
    value: RuntimeValue,
    value_at: CodeRange,
    enforced_type: Option<ValueType>,
}

fn declare_var(name: &Span<String>, data: DeclareVarData, ctx: &mut Context) -> ExecResult<()> {
    let DeclareVarData {
        is_mut,
        value,
        value_at,
        enforced_type,
    } = data;

    if let Some(enforced_type) = &enforced_type {
        if !check_if_value_fits_type(&value, enforced_type, ctx) {
            return Err(ctx.error(
                value_at,
                format!(
                    "variable has enforced type {}, but tried to assign a value of type: {}",
                    enforced_type.display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                    value
                        .compute_type()
                        .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                ),
            ));
        }
    }

    let decl_scope_id = ctx.current_scope().ast_scope_id;

    ctx.current_scope_content_mut().vars.insert(
        name.data.clone(),
        ScopeVar {
            name_at: RuntimeCodeRange::Parsed(name.at),
            decl_scope_id,
            enforced_type,
            is_mut,
            value: GcCell::new(LocatedValue::new(RuntimeCodeRange::Parsed(value_at), value)),
        },
    );

    Ok(())
}

#[derive(Debug, Clone)]
pub enum InstrRet {
    ContinueLoop,
    BreakLoop,
    FnReturn(Option<LocatedValue>),
    WanderingValue(LocatedValue),
}
