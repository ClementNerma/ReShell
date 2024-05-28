#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod errors;
mod state;

use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};
use parsy::{CodeRange, Eaten};
use reshell_parser::ast::{
    Block, CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdPath, CmdPipe, ComputedString,
    ComputedStringPiece, DoubleOp, ElsIf, ElsIfExpr, Expr, ExprInner, ExprInnerContent, ExprOp,
    FnArg, FnArgNames, FnCall, FnCallArg, FnSignature, Function, Instruction, LiteralValue,
    Program, PropAccess, PropAccessNature, SingleCmdCall, SingleOp, SwitchCase, Value,
};

pub use self::{
    errors::CheckerError,
    state::{DeclaredVar, Dependency, DependencyType, Scope},
};

use self::{errors::CheckerResult, state::State};

#[derive(Debug)]
pub struct CheckerOutput {
    pub fn_deps: IndexMap<CodeRange, IndexSet<Dependency>>,
    // TODO: scoped type aliases and cmd aliases???
}

pub fn check(program: &Program, native_lib_scope: Scope) -> CheckerResult<CheckerOutput> {
    let Program { content } = program;

    let mut state = State::new();
    state.push_scope(native_lib_scope);

    check_block(content, &mut state)?;

    Ok(CheckerOutput {
        fn_deps: state.fn_deps,
    })
}

fn check_block(block: &Eaten<Block>, state: &mut State) -> CheckerResult {
    check_block_with(block, state, |_| {})
}

fn check_block_with(
    block: &Eaten<Block>,
    state: &mut State,
    fill_scope: impl FnOnce(&mut Scope),
) -> CheckerResult {
    let Block {
        instructions,
        code_range,
    } = &block.data;

    let mut scope = Scope {
        code_range: *code_range,
        fn_args_at: None, // can be changed with "fill_scope"
        vars: HashMap::new(),
        fns: HashMap::new(),
        types: HashSet::new(),
        cmd_aliases: HashSet::new(),
    };

    fill_scope(&mut scope);

    state.push_scope(scope);

    for instr in instructions {
        check_instr(instr, state)?;
    }

    state.pop_scope();

    Ok(())
}

fn check_instr(instr: &Eaten<Instruction>, state: &mut State) -> CheckerResult {
    match &instr.data {
        Instruction::Comment { content: _ } => {}

        Instruction::DeclareVar {
            name,
            mutable,
            init_expr,
        } => {
            if state.curr_scope().vars.contains_key(&name.data) {
                return Err(CheckerError::new(name.at, "Duplicate variable declaration"));
            }

            if let Some(init_expr) = init_expr {
                check_expr(&init_expr.data, state)?;
            }

            state.curr_scope_mut().vars.insert(
                name.data.clone(),
                DeclaredVar {
                    name_at: name.at,
                    is_mut: mutable.is_some(),
                },
            );
        }

        Instruction::AssignVar {
            name,
            prop_acc,
            list_push: _,
            expr,
        } => {
            let var = state.register_var_usage_and_get(name)?;

            if !var.is_mut {
                return Err(CheckerError::new(
                    name.at,
                    "Cannot assign to immutable variable",
                ));
            }

            for nature in prop_acc {
                check_prop_access_nature(nature, state)?;
            }

            check_expr(&expr.data, state)?;
        }

        Instruction::IfCond {
            cond,
            body,
            elsif,
            els,
        } => {
            check_expr(&cond.data, state)?;
            check_block(body, state)?;

            for elsif in elsif {
                let ElsIf { cond, body } = &elsif.data;

                check_expr(&cond.data, state)?;
                check_block(body, state)?;
            }

            if let Some(els) = els {
                check_block(els, state)?;
            }
        }

        Instruction::ForLoop {
            iter_var,
            iter_on,
            body,
        } => {
            check_expr(&iter_on.data, state)?;
            check_block_with(body, state, |scope| {
                scope.vars.insert(
                    iter_var.data.clone(),
                    DeclaredVar {
                        name_at: iter_var.at,
                        is_mut: false,
                    },
                );
            })?;
        }

        Instruction::WhileLoop { cond, body } => {
            check_expr(&cond.data, state)?;
            check_block(body, state)?;
        }

        Instruction::LoopContinue => {
            // TODO: check if currently in a loop (entering a function breaks it)
        }

        Instruction::LoopBreak => {
            // TODO: check if currenty in a loop (same)
        }

        Instruction::Switch { expr, cases } => {
            check_expr(&expr.data, state)?;

            for case in cases {
                let SwitchCase { cond, body } = case;

                check_expr(&cond.data, state)?;
                check_block(body, state)?;
            }
        }

        Instruction::FnDecl { name, content } => {
            if state.curr_scope().fns.contains_key(&name.data) {
                return Err(CheckerError::new(name.at, "Duplicate function declaration"));
            }

            state
                .curr_scope_mut()
                .fns
                .insert(name.data.clone(), name.at);

            check_function(content, state)?;
        }

        Instruction::FnReturn { expr } => {
            // TODO: check if in a function

            if let Some(expr) = expr {
                check_expr(&expr.data, state)?;
            }
        }

        Instruction::Throw(expr) => {
            check_expr(&expr.data, state)?;
        }

        Instruction::Try {
            call,
            catch_var,
            catch_body,
        } => {
            check_fn_call(call, state)?;
            check_block_with(catch_body, state, |scope| {
                scope.vars.insert(
                    catch_var.data.clone(),
                    DeclaredVar {
                        name_at: catch_var.at,
                        is_mut: false,
                    },
                );
            })?;
        }

        Instruction::CmdAliasDecl { name, content } => todo!(),

        Instruction::TypeAliasDecl { name, content } => todo!(),

        Instruction::BaseBlock(block) => check_block(block, state)?,

        Instruction::CmdCall(call) => check_cmd_call(call, state)?,
    }

    Ok(())
}

fn check_expr(expr: &Expr, state: &mut State) -> CheckerResult {
    let Expr { inner, right_ops } = expr;

    check_expr_inner(inner, state)?;

    for op in right_ops {
        check_expr_op(op, state)?;
    }

    Ok(())
}

fn check_expr_inner(inner: &Eaten<ExprInner>, state: &mut State) -> CheckerResult {
    let ExprInner { content, prop_acc } = &inner.data;

    check_expr_inner_content(&content.data, state)?;

    for acc in prop_acc {
        check_prop_access(acc, state)?;
    }

    Ok(())
}

fn check_expr_inner_content(content: &ExprInnerContent, state: &mut State) -> CheckerResult {
    match content {
        ExprInnerContent::SingleOp { op, right } => {
            check_single_op(op, state)?;
            check_expr_inner_content(&right.data, state)?;
        }

        ExprInnerContent::ParenExpr(expr) => {
            check_expr(&expr.data, state)?;
        }

        ExprInnerContent::Value(value) => {
            check_value(value, state)?;
        }

        ExprInnerContent::Ternary {
            cond,
            body,
            elsif,
            els,
        } => {
            check_expr(&cond.data, state)?;
            check_expr(&body.data, state)?;

            for elsif in elsif {
                check_elsif_expr(elsif, state)?;
            }

            check_expr(&els.data, state)?;
        }
    }

    Ok(())
}

fn check_elsif_expr(elsif_expr: &Eaten<ElsIfExpr>, state: &mut State) -> CheckerResult {
    let ElsIfExpr { body, cond } = &elsif_expr.data;

    check_expr(&body.data, state)?;
    check_expr(&cond.data, state)?;

    Ok(())
}

fn check_expr_op(expr_op: &ExprOp, state: &mut State) -> CheckerResult {
    let ExprOp { op, with } = expr_op;

    check_double_op(op, state)?;
    check_expr_inner(with, state)?;

    Ok(())
}

fn check_single_op(single_op: &Eaten<SingleOp>, _: &mut State) -> CheckerResult {
    match &single_op.data {
        SingleOp::Neg => Ok(()),
    }
}

fn check_double_op(double_op: &Eaten<DoubleOp>, _: &mut State) -> CheckerResult {
    match &double_op.data {
        DoubleOp::Add => Ok(()),
        DoubleOp::Sub => Ok(()),
        DoubleOp::Mul => Ok(()),
        DoubleOp::Div => Ok(()),
        DoubleOp::And => Ok(()),
        DoubleOp::Or => Ok(()),
        DoubleOp::Eq => Ok(()),
        DoubleOp::Neq => Ok(()),
        DoubleOp::Lt => Ok(()),
        DoubleOp::Lte => Ok(()),
        DoubleOp::Gt => Ok(()),
        DoubleOp::Gte => Ok(()),
        DoubleOp::NullFallback => Ok(()),
    }
}

fn check_prop_access(prop_acc: &Eaten<PropAccess>, state: &mut State) -> CheckerResult {
    let PropAccess {
        nature,
        nullable: _,
    } = &prop_acc.data;

    check_prop_access_nature(nature, state)
}

fn check_prop_access_nature(nature: &Eaten<PropAccessNature>, state: &mut State) -> CheckerResult {
    match &nature.data {
        PropAccessNature::Key(key) => check_expr(&key.data, state),
        PropAccessNature::Prop(_) => Ok(()),
    }
}

fn check_value(value: &Eaten<Value>, state: &mut State) -> CheckerResult {
    match &value.data {
        Value::Null => Ok(()),
        Value::Literal(lit) => check_literal_value(lit, state),
        Value::ComputedString(computed_string) => check_computed_string(computed_string, state),
        Value::List(list) => {
            for item in list {
                check_expr(&item.data, state)?;
            }

            Ok(())
        }
        Value::Struct(members) => {
            for item in members.values() {
                check_expr(&item.data, state)?;
            }

            Ok(())
        }
        Value::Variable(var) => state.register_var_usage(var),
        Value::FnCall(fn_call) => check_fn_call(fn_call, state),
        Value::CmdOutput(cmd_call) => check_cmd_call(cmd_call, state),
        Value::CmdSuccess(cmd_call) => check_cmd_call(cmd_call, state),
        Value::FnAsValue(func_name) => state.register_fn_usage(func_name),
        Value::Closure(func) => check_function(func, state),
    }
}

fn check_literal_value(lit_value: &Eaten<LiteralValue>, _: &mut State) -> CheckerResult {
    match &lit_value.data {
        LiteralValue::Boolean(_) => Ok(()),
        LiteralValue::Integer(_) => Ok(()),
        LiteralValue::Float(_) => Ok(()),
    }
}

fn check_computed_string(
    computed_string: &Eaten<ComputedString>,
    state: &mut State,
) -> CheckerResult {
    let ComputedString { pieces } = &computed_string.data;

    for piece in pieces {
        check_computed_string_piece(piece, state)?;
    }

    Ok(())
}

fn check_computed_string_piece(
    piece: &Eaten<ComputedStringPiece>,
    state: &mut State,
) -> CheckerResult {
    match &piece.data {
        ComputedStringPiece::Literal(_) => Ok(()),
        ComputedStringPiece::Escaped(_) => Ok(()),
        ComputedStringPiece::Variable(var) => state.register_var_usage(var),
        ComputedStringPiece::Expr(expr) => check_expr(&expr.data, state),
        ComputedStringPiece::CmdCall(cmd_call) => check_cmd_call(cmd_call, state),
    }
}

fn check_fn_call(fn_call: &Eaten<FnCall>, state: &mut State) -> CheckerResult {
    let FnCall {
        is_var_name,
        name,
        call_args,
    } = &fn_call.data;

    if *is_var_name {
        state.register_var_usage(name)?;
    } else {
        state.register_fn_usage(name)?;
    }

    for arg in &call_args.data {
        check_fn_call_arg(arg, state)?;
    }

    Ok(())
}

fn check_fn_call_arg(arg: &Eaten<FnCallArg>, state: &mut State) -> CheckerResult {
    match &arg.data {
        FnCallArg::Expr(expr) => check_expr(&expr.data, state),
        FnCallArg::CmdArg(cmd_arg) => check_cmd_arg(cmd_arg, state),
    }
}

fn check_cmd_call(cmd_call: &Eaten<CmdCall>, state: &mut State) -> CheckerResult {
    let CmdCall { base, pipes } = &cmd_call.data;

    check_single_cmd_call(base, state)?;

    for pipe in pipes {
        check_cmd_pipe(pipe, state)?;
    }

    Ok(())
}

fn check_single_cmd_call(
    single_cmd_call: &Eaten<SingleCmdCall>,
    state: &mut State,
) -> CheckerResult {
    let SingleCmdCall {
        env_vars,
        method: _,
        path,
        args,
    } = &single_cmd_call.data;

    for env_var in &env_vars.data {
        check_cmd_env_var(env_var, state)?;
    }

    match &path.data {
        CmdPath::Raw(_) => {}
        CmdPath::ComputedString(computed_string) => check_computed_string(computed_string, state)?,
    }

    for arg in &args.data {
        check_cmd_arg(arg, state)?;
    }

    Ok(())
}

fn check_cmd_env_var(cmd_env_var: &Eaten<CmdEnvVar>, state: &mut State) -> CheckerResult {
    let CmdEnvVar { name: _, value } = &cmd_env_var.data;

    match &value.data {
        CmdEnvVarValue::Raw(_) => Ok(()),

        CmdEnvVarValue::ComputedString(computed_string) => {
            check_computed_string(computed_string, state)
        }

        CmdEnvVarValue::Expr(expr) => check_expr(&expr.data, state),
    }
}

fn check_cmd_pipe(pipe: &CmdPipe, state: &mut State) -> CheckerResult {
    let CmdPipe { pipe_type: _, cmd } = pipe;

    check_single_cmd_call(cmd, state)
}

fn check_cmd_arg(arg: &Eaten<CmdArg>, state: &mut State) -> CheckerResult {
    match &arg.data {
        CmdArg::LiteralValue(lit) => check_literal_value(lit, state),
        CmdArg::ComputedString(computed_string) => check_computed_string(computed_string, state),
        CmdArg::CmdCall(cmd_call) => check_cmd_call(cmd_call, state),
        CmdArg::ParenExpr(expr) => check_expr(&expr.data, state),
        CmdArg::VarName(var) => state.register_var_usage(var),
        CmdArg::FnAsValue(func) => state.register_fn_usage(func),
        CmdArg::Raw(_) => Ok(()),
        CmdArg::SpreadVar(var) => state.register_var_usage(var),
    }
}

fn check_function(func: &Function, state: &mut State) -> CheckerResult {
    let Function { signature, body } = func;

    let FnSignature { args, ret_type: _ } = signature;

    let mut vars = HashMap::new();

    for arg in &args.data {
        let FnArg {
            names,
            is_optional: _,
            is_rest: _,
            typ: _,
        } = arg;

        let var_name = match names {
            FnArgNames::NotFlag(name) => name.clone(),
            FnArgNames::ShortFlag(name) => name.map(|c| c.to_string()),
            FnArgNames::LongFlag(name) => name.clone(),
            FnArgNames::LongAndShortFlag { long, short: _ } => long.clone(),
        };

        let dup = vars.insert(
            var_name.data.clone(),
            DeclaredVar {
                name_at: var_name.at,
                is_mut: false,
            },
        );

        if dup.is_some() {
            return Err(CheckerError::new(var_name.at, "Duplicate argument name"));
        }
    }

    check_block_with(body, state, |scope| {
        scope.fn_args_at = Some(args.at);

        for (name, var) in vars {
            scope.vars.insert(name, var);
        }
    })
}
