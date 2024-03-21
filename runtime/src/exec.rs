use std::{fmt::Debug, fs};

use parsy::{Eaten, FileId, Parser};
use reshell_parser::{
    ast::{Block, ElsIf, Instruction, Program},
    program,
};

use crate::{
    cmd::run_cmd,
    display::readable_value_type,
    errors::ExecResult,
    expr::eval_expr,
    files_map::ScopableFilePath,
    scoping::{Context, Scope, ScopeFn, ScopeVar},
    values::{LocatedValue, RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

pub fn run(program: &Program, ctx: &mut Context, init_scope: Scope) -> ExecResult<Scope> {
    let scopes_at_start = ctx.scopes().len();

    let (scope, _) = run_program(program, ctx, init_scope)?;

    assert!(ctx.scopes().len() == scopes_at_start);

    Ok(scope)
}

pub fn run_in_existing_scope(program: &Program, ctx: &mut Context) -> ExecResult<()> {
    let scopes_at_start = ctx.scopes().len();

    run_program_in_current_scope(program, ctx)?;

    assert!(ctx.scopes().len() == scopes_at_start);

    Ok(())
}

fn run_program(
    program: &Program,
    ctx: &mut Context,
    init_scope: Scope,
) -> ExecResult<(Scope, Option<InstrRet>)> {
    run_block(&program.content.data, ctx, init_scope)
}

fn run_program_in_current_scope(
    program: &Program,
    ctx: &mut Context,
) -> ExecResult<Option<InstrRet>> {
    run_block_in_current_scope(&program.content.data, ctx)
}

pub fn run_block(
    block: &Block,
    ctx: &mut Context,
    block_scope: Scope,
) -> ExecResult<(Scope, Option<InstrRet>)> {
    ctx.push_scope(block_scope);

    let instr_ret = run_block_in_current_scope(block, ctx)?;

    Ok((ctx.pop_scope(), instr_ret))
}

fn run_block_in_current_scope(block: &Block, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    for instr in &block.instructions {
        if let Some(ret) = run_instr(&instr, ctx)? {
            return Ok(Some(ret));
        }
    }

    Ok(None)
}

fn run_instr(instr: &Eaten<Instruction>, ctx: &mut Context) -> ExecResult<Option<InstrRet>> {
    match &instr.data {
        Instruction::Comment { content: _ } => {}

        Instruction::Include(path_expr) => {
            let file_path = match eval_expr(&path_expr.data, ctx)? {
                RuntimeValue::String(path) => path,
                value => {
                    return Err(ctx.error(
                        instr.at,
                        format!(
                            "expected a path (string), found a {}",
                            readable_value_type(&value)
                        ),
                    ))
                }
            };

            let from_dir = match ctx.current_file_path() {
                Some(path) => path.parent().unwrap().to_path_buf(),
                None => std::env::current_dir().map_err(|err| {
                    ctx.error(
                        instr.at,
                        format!("failed to get current working directory: {err}"),
                    )
                })?,
            };

            let file_path = from_dir.join(file_path);

            if !file_path.exists() {
                return Err(ctx.error(
                    path_expr.at,
                    format!("path '{}' does not exist", file_path.display()),
                ));
            }

            if !file_path.is_file() {
                return Err(ctx.error(
                    path_expr.at,
                    format!("path '{}' exists but is not a file", file_path.display()),
                ));
            }

            let source = fs::read_to_string(&file_path).map_err(|err| {
                ctx.error(
                    instr.at,
                    format!(
                        "failed to read content of file '{}': {}",
                        file_path.display(),
                        err
                    ),
                )
            })?;

            let file_scope = ctx.create_file_scope(
                ScopableFilePath::RealFile(file_path.clone()),
                source.clone(),
            );

            let parsed = program()
                .parse_str_as_file(&source, FileId::Id(file_scope.in_file_id))
                .map_err(|err| ctx.error(instr.at, err))?;

            let (_, instr_ret) = run_program(&parsed.data, ctx, file_scope)?;

            if let Some(instr_ret) = instr_ret {
                return Ok(Some(instr_ret));
            }
        }

        Instruction::DeclareVar {
            name,
            mutable,
            init_expr,
        } => {
            if ctx.current_scope().vars.contains_key(&name.data) {
                return Err(ctx.error(name.at, format!("duplicate variable declaration")));
            }

            let init_value = init_expr
                .as_ref()
                .map(|expr| {
                    eval_expr(&expr.data, ctx).map(|value| LocatedValue::new(value, expr.at))
                })
                .transpose()?;

            // TODO: ugly access
            ctx.current_scope_mut().vars.insert(
                name.data.clone(),
                ScopeVar {
                    // name: name.clone(),
                    declared_at: name.at,
                    is_mut: mutable.is_some(),
                    value: init_value,
                    forked: false,
                },
            );
        }

        Instruction::AssignVar { name, expr } => {
            let Some(var) = ctx
                .get_var(name) else {
                    return Err(ctx.error(name.at, "variable not found"));
                };

            if !var.is_mut {
                return Err(ctx.error(name.at, "this variable is not set as mutable"));
            }

            if var.forked {
                return Err(ctx.error(
                    name.at,
                    "cannot assign to this variable as it was not declared in the current process",
                ));
            }

            let value = eval_expr(&expr.data, ctx)?;

            ctx.get_var_mut(name).unwrap().value = Some(LocatedValue::new(value, expr.at));
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
                            readable_value_type(&value)
                        ),
                    ))
                }
            };

            if cond_val {
                run_block(&body.data, ctx, ctx.create_scope())?;
            } else {
                let mut run = false;

                for branch in elsif {
                    let ElsIf { cond, body } = &branch.data;

                    let cond_val = eval_expr(&cond.data, ctx)?;
                    let RuntimeValue::Bool(cond_val) = cond_val else {
                        return Err(ctx.error(cond.at, format!("expected the condition to resolve to a boolean, found a {} instead", readable_value_type(&cond_val))));
                    };

                    if cond_val {
                        run_block(&body.data, ctx, ctx.create_scope())?;
                        run = true;
                        break;
                    }
                }

                if !run {
                    if let Some(els) = els {
                        run_block(&els.data, ctx, ctx.create_scope())?;
                    }
                }
            }
        }

        Instruction::ForLoop {
            iter_var,
            iter_on,
            body,
        } => {
            match eval_expr(&iter_on.data, ctx)? {
                RuntimeValue::List(list) => {
                    for item in &list {
                        let mut loop_scope = ctx.create_scope();

                        loop_scope.vars.insert(
                            iter_var.data.clone(),
                            ScopeVar {
                                declared_at: iter_var.at,
                                is_mut: false,
                                value: Some(LocatedValue::new(
                                    // TODO: performance?
                                    item.clone(),
                                    iter_var.at,
                                )),
                                forked: false,
                            },
                        );

                        if let (_, Some(InstrRet::BreakLoop)) =
                            run_block(&body.data, ctx, loop_scope)?
                        {
                            break;
                        }
                    }
                }

                RuntimeValue::Range { from, to } => {
                    for i in from..=to {
                        let mut loop_scope = ctx.create_scope();

                        loop_scope.vars.insert(
                            iter_var.data.clone(),
                            ScopeVar {
                                declared_at: iter_var.at,
                                is_mut: false,
                                value: Some(LocatedValue::new(
                                    RuntimeValue::Int(i as i64),
                                    iter_var.at,
                                )),
                                forked: false,
                            },
                        );

                        if let (_, Some(InstrRet::BreakLoop)) =
                            run_block(&body.data, ctx, loop_scope)?
                        {
                            break;
                        }
                    }
                }

                value => {
                    return Err(ctx.error(
                        iter_on.at,
                        format!(
                            "expected a list or range to iterate on, found a {} instead",
                            readable_value_type(&value)
                        ),
                    ))
                }
            }
        }

        Instruction::WhileLoop { cond, body } => loop {
            let cond_val = match eval_expr(&cond.data, ctx)? {
                RuntimeValue::Bool(bool) => bool,
                value => {
                    return Err(ctx.error(
                        cond.at,
                        format!(
                            "expected the condition to resolve to a boolean, found a {} instead",
                            readable_value_type(&value)
                        ),
                    ))
                }
            };

            if !cond_val {
                break;
            }

            if let (_, Some(InstrRet::BreakLoop)) = run_block(&body.data, ctx, ctx.create_scope())?
            {
                break;
            }
        },

        Instruction::LoopContinue => return Ok(Some(InstrRet::ContinueLoop)),

        Instruction::LoopBreak => return Ok(Some(InstrRet::BreakLoop)),

        Instruction::Switch { cases: _ } => todo!(),

        Instruction::FnDecl {
            name,
            signature,
            body,
        } => {
            let fns = &mut ctx.current_scope_mut().fns;

            if fns.contains_key(&name.data) {
                return Err(ctx.error(name.at, format!("duplicate name declaration")));
            }

            fns.insert(
                name.data.clone(),
                ScopeFn {
                    declared_at: name.at,
                    value: RuntimeFnValue {
                        body: RuntimeFnBody::Block(body.clone()),
                        signature: signature.clone(),
                    },
                },
            );
        }

        Instruction::FnReturn { expr } => {
            return Ok(Some(InstrRet::FnReturn(
                expr.as_ref()
                    .map(|expr| {
                        eval_expr(&expr.data, ctx).map(|value| LocatedValue::new(value, expr.at))
                    })
                    .transpose()?,
            )))
        }

        Instruction::Throw(expr) => {
            return Ok(Some(InstrRet::Throwed(LocatedValue::new(
                eval_expr(&expr.data, ctx)?,
                expr.at,
            ))));
        }

        Instruction::CmdCall(call) => {
            run_cmd(call, ctx, false)?;
        }

        Instruction::BaseBlock(block) => {
            run_block(&block.data, ctx, ctx.create_scope())?;
        }
    }

    Ok(None)
}

#[derive(Debug, Clone)]
pub enum InstrRet {
    ContinueLoop,
    BreakLoop,
    FnReturn(Option<LocatedValue>),
    Throwed(LocatedValue),
}
