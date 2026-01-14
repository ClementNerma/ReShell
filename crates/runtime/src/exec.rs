use std::fmt::Debug;

use indexmap::IndexSet;
use parsy::{CodeRange, FileId, Span};
use reshell_parser::ast::{
    AstScopeId, Block, ElsIf, Instruction, MatchCase, ObjPropSpreading, ObjPropSpreadingBinding,
    ObjPropSpreadingType, Program, RuntimeCodeRange, SingleVarDecl, TypeMatchCase,
    ValueDestructuring, ValueType,
};
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    cmd::{CmdExecParams, run_cmd},
    context::{
        CallStackEntry, Context, ScopeCmdAlias, ScopeContent, ScopeFn, ScopeMethod, ScopeVar,
    },
    errors::{ExecActualErrorNature, ExecError, ExecInfoType, ExecInternalPropagation, ExecResult},
    expr::eval_expr,
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    props::{PropAccessMode, TailPropAccessPolicy, TailPropWritingPolicy, eval_props_access},
    typechecking::check_if_value_fits_type,
    values::{
        LocatedValue, NotComparableTypesErr, RangeValue, RuntimeCmdAlias, RuntimeFnBody,
        RuntimeFnSignature, RuntimeFnValue, RuntimeValue, are_values_equal,
    },
};

/// Run a program
///
/// The program may have been built from an AST, or parsed through [`reshell_parser::PROGRAM`].
///
/// STDIN is handled automatically, including Ctrl+C presses through the function
/// provided in [`crate::context::ContextCreationParams`].
///
/// The program will be checked using [`reshell_checker::check`], so the program does not need to
/// have been checked beforehand.
///
/// If the program ends with a wandering value, it will be returned as-is along with its original location.
/// Note that said location is the place the value originates from, not necessarily where it was returned from.
///
/// In case an error occurs, an [`ExecError`] value will be returned instead.
pub fn run_program(program: &Span<Program>, ctx: &mut Context) -> ExecResult<Option<LocatedValue>> {
    // Reset Ctrl+C requests
    ctx.reset_ctrl_c_press_indicator();

    let Program { content } = &program.data;

    match content.at.start.file_id {
        FileId::None | FileId::Internal | FileId::Custom(_) => {
            return Err(ctx.hard_error(content.at, "program must be backed by a source file"));
        }

        FileId::SourceFile(id) => assert!(ctx.files_map().get_file(id).is_some()),
    }

    // Check the program
    ctx.prepare_for_new_program(program).map_err(|err| {
        ctx.hard_error_with(program.at, ExecActualErrorNature::CheckingErr(err), |err| {
            err.add_info(
                ExecInfoType::Note,
                "Error was encountered before running the program",
            );
        })
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

/// Run a block, with the provided scope content
fn run_block(
    block: &Block,
    ctx: &mut Context,
    content: Option<ScopeContent>,
) -> ExecResult<Option<InstrRet>> {
    ctx.create_and_push_scope(block.scope_id, content.unwrap_or_else(ScopeContent::new));

    let result = run_block_in_current_scope(block, ctx);

    ctx.pop_scope();

    result
}

/// Run a block, with the provided scope content, parent scopes list, and call stack entry
pub(crate) fn run_block_detailed(
    block: &Block,
    ctx: &mut Context,
    scope_content: ScopeContent,
    parent_scopes: IndexSet<u64>,
    call_stack_entry: Option<CallStackEntry>,
) -> ExecResult<Option<InstrRet>> {
    ctx.create_and_push_scope_detailed(
        block.scope_id,
        scope_content,
        parent_scopes,
        call_stack_entry,
    );

    let result = run_block_in_current_scope(block, ctx);

    ctx.pop_scope();

    result
}

/// Run a block inside the current scope instead of creating a dedicated one
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

/// Run all the instructions in the current scope
fn run_instructions_in_current_scope(
    instructions: &[Span<Instruction>],
    ctx: &mut Context,
) -> ExecResult<Option<InstrRet>> {
    let mut wandering_value = None;

    for instr in instructions {
        wandering_value = None;

        // Handle any Ctrl+C press
        ctx.ensure_no_ctrl_c_press(instr.at)?;

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

/// Perform a first pass on a block's provided set of instructions to declare
/// static items like functions, methods, command aliases and type aliases,
/// as well as to evaluate included blocks from other source files.
fn block_first_pass(
    instructions: &[Span<Instruction>],
    block: &Block,
    ctx: &mut Context,
) -> ExecResult<()> {
    for instr in instructions {
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

/// Run a single instruction
fn run_instr(instr: &Span<Instruction>, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    let instr_ret = match &instr.data {
        Instruction::DeclareVar { names, init_expr } => {
            let init_value = eval_expr(&init_expr.data, ctx)?;

            if matches!(init_value, RuntimeValue::Void) {
                return Err(ctx.hard_error(init_expr.at, "cannot assign a void value"));
            }

            declare_vars(names, init_value, init_expr.at, None, ctx)?;

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
                return Err(ctx.hard_error(expr.at, "cannot assign a void value"));
            }

            if prop_acc.is_empty()
                && list_push.is_none()
                && let Some(enforced_type) = &var.enforced_type
                && !check_if_value_fits_type(&assign_value, enforced_type, ctx)
            {
                return Err(ctx.hard_error(
                    expr.at,
                    format!(
                        "variable has enforced type {}, but tried to assign a value of type: {}",
                        enforced_type.display(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                        assign_value
                            .compute_type()
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    ),
                ));
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

                                Err(ctx.hard_error(
                                    list_push.at,
                                    format!("cannot push a value as this is not a list but a {left_type}"),
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
            let cond_val =
                match eval_expr(&cond.data, ctx)? {
                    RuntimeValue::Bool(bool) => bool,
                    value => return Err(ctx.hard_error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    )),
                };

            let mut ret = None;

            if cond_val {
                ret = Some(run_block(body, ctx, None)?);
            } else {
                for branch in elsif {
                    let ElsIf { cond, body } = branch;

                    let cond_val = eval_expr(&cond.data, ctx)?;

                    let RuntimeValue::Bool(cond_val) = cond_val else {
                        return Err(ctx.hard_error(cond.at, format!("expected the condition to resolve to a boolean, found a {} instead", cond_val.compute_type().display(ctx.type_alias_store(), PrettyPrintOptions::inline()))));
                    };

                    if cond_val {
                        ret = Some(run_block(body, ctx, None)?);
                        break;
                    }
                }

                if ret.is_none()
                    && let Some(els) = els
                {
                    ret = Some(run_block(els, ctx, None)?);
                }
            }

            ret.flatten()
        }

        Instruction::ForLoop {
            destructure_as,
            iter_on,
            body,
        } => {
            match eval_expr(&iter_on.data, ctx)? {
                RuntimeValue::Range(range) => {
                    let RangeValue {
                        from,
                        to,
                        include_last_value,
                    } = range;

                    let range = from..(to + if include_last_value { 1 } else { 0 });

                    for value in range {
                        let mut loop_scope = ScopeContent::new();

                        declare_vars(
                            destructure_as,
                            RuntimeValue::Int(value),
                            iter_on.at,
                            Some((body.scope_id, &mut loop_scope)),
                            ctx,
                        )?;

                        match run_loop_block(body, ctx, Some(loop_scope))? {
                            LoopBlockResult::Continue => {}
                            LoopBlockResult::Break => break,
                            LoopBlockResult::FnReturn(loc_val) => {
                                return Ok(Some(InstrRet::FnReturn(loc_val)));
                            }
                        }
                    }
                }

                RuntimeValue::List(list) => {
                    for item in list.read(iter_on.at).iter() {
                        let mut loop_scope = ScopeContent::new();

                        declare_vars(
                            destructure_as,
                            item.clone(),
                            iter_on.at,
                            Some((body.scope_id, &mut loop_scope)),
                            ctx,
                        )?;

                        match run_loop_block(body, ctx, Some(loop_scope))? {
                            LoopBlockResult::Continue => {}
                            LoopBlockResult::Break => break,
                            LoopBlockResult::FnReturn(loc_val) => {
                                return Ok(Some(InstrRet::FnReturn(loc_val)));
                            }
                        }
                    }
                }

                value => {
                    return Err(ctx.hard_error(
                        iter_on.at,
                        format!(
                            "expected a list to iterate on, found a {} instead",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ));
                }
            }

            None
        }

        Instruction::ForLoopKeyed {
            key_iter_var,
            destructure_as,
            iter_on,
            body,
        } => {
            let map = match eval_expr(&iter_on.data, ctx)? {
                RuntimeValue::Map(map) => map,
                value => {
                    return Err(ctx.hard_error(
                        iter_on.at,
                        format!(
                            "expected a map, got a {}",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ));
                }
            };

            for (key, value) in map.read(iter_on.at).iter() {
                let mut loop_scope = ScopeContent::new();

                loop_scope.vars.insert(
                    key_iter_var.data.clone(),
                    ScopeVar {
                        name_at: RuntimeCodeRange::Parsed(key_iter_var.at),
                        decl_scope_id: body.scope_id,
                        is_mut: false,
                        enforced_type: None,
                        value: GcCell::new(LocatedValue::new(
                            RuntimeCodeRange::Parsed(key_iter_var.at),
                            RuntimeValue::String(key.clone()),
                        )),
                    },
                );

                declare_vars(
                    destructure_as,
                    value.clone(),
                    iter_on.at,
                    Some((body.scope_id, &mut loop_scope)),
                    ctx,
                )?;

                match run_loop_block(body, ctx, Some(loop_scope))? {
                    LoopBlockResult::Continue => {}
                    LoopBlockResult::Break => break,
                    LoopBlockResult::FnReturn(loc_val) => {
                        return Ok(Some(InstrRet::FnReturn(loc_val)));
                    }
                }
            }

            None
        }

        Instruction::WhileLoop { cond, body } => {
            loop {
                let cond_val = match eval_expr(&cond.data, ctx)? {
                    RuntimeValue::Bool(bool) => bool,
                    value => return Err(ctx.hard_error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    )),
                };

                if !cond_val {
                    break;
                }

                match run_loop_block(body, ctx, None)? {
                    LoopBlockResult::Continue => {}
                    LoopBlockResult::Break => break,
                    LoopBlockResult::FnReturn(loc_val) => {
                        return Ok(Some(InstrRet::FnReturn(loc_val)));
                    }
                }
            }

            None
        }

        Instruction::LoopContinue => {
            return Err(ExecError::InternalPropagation(
                ExecInternalPropagation::LoopContinuation,
            ));
        }

        Instruction::LoopBreak => {
            return Err(ExecError::InternalPropagation(
                ExecInternalPropagation::LoopBreakage,
            ));
        }

        Instruction::Match { expr, cases, els } => {
            let match_on = eval_expr(&expr.data, ctx)?;

            for MatchCase { matches, body } in cases {
                let case_value = eval_expr(&matches.data, ctx)?;

                let cmp = are_values_equal(&match_on, &case_value).map_err(
                    |NotComparableTypesErr { reason }| {
                        ctx.hard_error(
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
                    eval_expr(&expr.data, ctx)
                        .map(|value| LocatedValue::new(RuntimeCodeRange::Parsed(expr.at), value))
                })
                .transpose()?,
        )),

        Instruction::Throw(expr) => {
            let message = match eval_expr(&expr.data, ctx)? {
                RuntimeValue::String(string) => string,

                value => {
                    return Err(ctx.hard_error(
                        expr.at,
                        format!(
                            "expected a string, found a {}",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ));
                }
            };

            return Err(ctx.hard_error(
                instr.at,
                ExecActualErrorNature::Thrown {
                    at: RuntimeCodeRange::Parsed(instr.at),
                    message,
                },
            ));
        }

        Instruction::Try {
            try_body,
            catch_var,
            catch_body,
        } => {
            if let Err(err) = run_block(&try_body.data, ctx, None) {
                if let ExecError::ActualError(err) = &err
                    && let ExecActualErrorNature::Thrown { at, message } = &err.nature
                {
                    let mut scope = ScopeContent::new();

                    scope.vars.insert(
                        catch_var.data.clone(),
                        ScopeVar {
                            name_at: RuntimeCodeRange::Parsed(catch_var.at),
                            decl_scope_id: catch_body.scope_id,
                            is_mut: false,
                            enforced_type: None,
                            value: GcCell::new(LocatedValue::new(
                                *at,
                                RuntimeValue::String(message.clone()),
                            )),
                        },
                    );

                    run_block(catch_body, ctx, Some(scope))?;
                } else {
                    return Err(err);
                }
            }

            None
        }

        Instruction::CmdAliasDecl {
            name,
            content,
            content_scope_id,
        } => {
            // We can do this thanks to the checker
            assert!(
                !ctx.current_scope_content_mut()
                    .cmd_aliases
                    .contains_key(&name.data)
            );

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

/// Run a loop's body block
fn run_loop_block(
    body: &Block,
    ctx: &mut Context,
    loop_scope: Option<ScopeContent>,
) -> ExecResult<LoopBlockResult> {
    // Check block's execution result
    match run_block(body, ctx, loop_scope) {
        // Nothing to note
        Ok(None) => Ok(LoopBlockResult::Continue),

        // Wandering values can be dropped
        Ok(Some(InstrRet::WanderingValue(_))) => Ok(LoopBlockResult::Continue),

        // Propagate functions' return statements
        Ok(Some(InstrRet::FnReturn(value))) => Ok(LoopBlockResult::FnReturn(value)),

        // Internal propagation data
        Err(ExecError::InternalPropagation(propagation)) => {
            match propagation {
                // Loop continuation (do nothing)
                ExecInternalPropagation::LoopContinuation => Ok(LoopBlockResult::Continue),

                // Loop breakage
                ExecInternalPropagation::LoopBreakage => Ok(LoopBlockResult::Break),
            }
        }

        // Propagate other error types
        Err(err) => Err(err),
    }
}

/// Result of a loop's body block run
#[derive(Debug)]
enum LoopBlockResult {
    /// Continue the loop (or end if the condition is met)
    Continue,

    /// Break the loop immediately
    Break,

    /// Propagate a function return value
    FnReturn(Option<LocatedValue>),
}

/// Declare a single variable or a set of ones, inside the current scope or the provided one
fn declare_vars(
    names: &Span<ValueDestructuring>,
    value: RuntimeValue,
    value_at: CodeRange,
    mut scope: Option<(AstScopeId, &mut ScopeContent)>,
    ctx: &mut Context,
) -> ExecResult<()> {
    match &names.data {
        ValueDestructuring::Single(single) => {
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
                scope.as_mut().map(|(id, content)| (*id, &mut **content)),
                ctx,
            )?;
        }

        ValueDestructuring::Tuple(members) => {
            let list = match value {
                RuntimeValue::List(list) => list,

                _ => {
                    return Err(ctx.hard_error(
                        names.at,
                        format!(
                            "tried to spread a list but found a: {}",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ));
                }
            };

            let list = list.read_promise_no_write();

            if members.len() > list.len() {
                return Err(ctx.hard_error(
                    names.at,
                    format!(
                        "tried to spread {} elements, but provided list contains {}",
                        members.len(),
                        list.len()
                    ),
                ));
            }

            for (names, value) in members.iter().zip(list.iter().cloned()) {
                declare_vars(
                    names,
                    value,
                    value_at,
                    scope.as_mut().map(|(id, content)| (*id, &mut **content)),
                    ctx,
                )?;
            }
        }

        ValueDestructuring::MapOrStruct(members) => {
            let map = match value {
                RuntimeValue::Map(map) => map,

                RuntimeValue::Struct(members) => members,

                _ => {
                    return Err(ctx.hard_error(
                        names.at,
                        format!(
                            "tried to spread a map or struct but found a: {}",
                            value
                                .compute_type()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                        ),
                    ));
                }
            };

            let map = map.read_promise_no_write();

            for var in members {
                let ObjPropSpreading { typ, default_value } = var;

                let (name, binding, is_mut) = match typ {
                    ObjPropSpreadingType::RawKeyToConst { name, binding } => {
                        (name, binding.as_ref(), false)
                    }

                    ObjPropSpreadingType::RawKeyToMut { name } => (name, None, true),

                    ObjPropSpreadingType::LiteralKeyToConst {
                        literal_name,
                        binding,
                    } => (literal_name, Some(binding), false),
                };

                let value = match map.get(&name.data) {
                    Some(value) => value.clone(),

                    None => match default_value {
                        Some(default_value) => eval_expr(default_value, ctx)?,

                        None => {
                            return Err(ctx.hard_error(
                                name.at,
                                "this property was not found in provided value",
                            ));
                        }
                    },
                };

                match binding {
                    Some(ObjPropSpreadingBinding::BindTo { alias, is_mut }) => declare_var(
                        alias,
                        DeclareVarData {
                            is_mut: *is_mut,
                            value,
                            value_at,
                            enforced_type: None,
                        },
                        scope.as_mut().map(|(id, content)| (*id, &mut **content)),
                        ctx,
                    )?,

                    Some(ObjPropSpreadingBinding::Deconstruct(deconstruct)) => {
                        declare_vars(
                            deconstruct,
                            value,
                            value_at,
                            scope.as_mut().map(|(id, content)| (*id, &mut **content)),
                            ctx,
                        )?;
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
                            scope.as_mut().map(|(id, content)| (*id, &mut **content)),
                            ctx,
                        )?;
                    }
                }
            }
        }
    }

    Ok(())
}

/// Data for declaring a variable
struct DeclareVarData {
    /// Is the variable mutable?
    is_mut: bool,

    /// The variable's initial value
    value: RuntimeValue,

    /// The variable's initial value's original location
    value_at: CodeRange,

    /// The variable's (optional) enforced type
    enforced_type: Option<ValueType>,
}

/// Declare a single variable
fn declare_var(
    name: &Span<String>,
    data: DeclareVarData,
    scope: Option<(AstScopeId, &mut ScopeContent)>,
    ctx: &mut Context,
) -> ExecResult<()> {
    let DeclareVarData {
        is_mut,
        value,
        value_at,
        enforced_type,
    } = data;

    if let Some(enforced_type) = &enforced_type
        && !check_if_value_fits_type(&value, enforced_type, ctx)
    {
        return Err(ctx.hard_error(
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

    let (decl_scope_id, scope_content) = scope.unwrap_or_else(|| {
        (
            ctx.current_scope().ast_scope_id,
            ctx.current_scope_content_mut(),
        )
    });

    scope_content.vars.insert(
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

/// Represents the return content of an instruction
///
/// Ideally this should have been put in [`ExecErrorNature::InternalPropagation`],
/// but given [`ExecError`] values are always put inside a [`Box`], this would
/// have resulted in every function return and wandering value requiring a heap allocation.
/// Also
//
// Plus [`ExecError`] contains lot of irrelevant fields for these data, which would have
// wasted memory.
#[derive(Debug, Clone)]
pub enum InstrRet {
    FnReturn(Option<LocatedValue>),
    WanderingValue(LocatedValue),
}
