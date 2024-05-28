//!
//! ReShell's checker
//!
//! This crate exposes a [`check`] function which is fed a parsed [`Program`].
//!
//! It checks the program's validity and collects data used to accelerate the runtime.
//!

#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod errors;
mod state;

pub mod output;

use std::collections::{HashMap, HashSet};

use parsy::{CodeRange, Eaten};
use reshell_parser::ast::{
    Block, CmdArg, CmdCall, CmdEnvVar, CmdFlagArg, CmdFlagValueArg, CmdPath, CmdPipe, CmdPipeType,
    CmdValueMakingArg, ComputedString, ComputedStringPiece, DoubleOp, ElsIf, ElsIfExpr, Expr,
    ExprInner, ExprInnerChaining, ExprInnerContent, ExprOp, FnArg, FnCall, FnCallArg,
    FnFlagArgNames, FnSignature, Function, FunctionBody, Instruction, LiteralValue, Program,
    PropAccess, PropAccessNature, RuntimeCodeRange, RuntimeEaten, SingleCmdCall, SingleOp,
    SingleValueType, StructTypeMember, SwitchCase, Value, ValueType,
};

use self::state::UsedItem;
pub use self::{
    errors::CheckerError,
    state::{CheckerScope, DeclaredCmdAlias, DeclaredFn, DeclaredVar, SpecialScopeType, State},
};

use crate::{errors::CheckerResult, output::*};

pub fn check(
    program: &Program,
    scopes: Vec<CheckerScope>,
    prev_output: Option<CheckerOutput>,
) -> CheckerResult<CheckerOutput> {
    let Program { content } = program;

    let mut state = State::new(prev_output);

    for scope in scopes {
        state.push_scope(scope);
    }

    check_block(content, &mut state)?;

    Ok(state.consume())
}

fn check_block(block: &Eaten<Block>, state: &mut State) -> CheckerResult {
    check_block_with(block, state, |_| {})
}

fn check_block_with(
    block: &Eaten<Block>,
    state: &mut State,
    fill_scope: impl FnOnce(&mut CheckerScope),
) -> CheckerResult {
    let mut scope = CheckerScope {
        code_range: RuntimeCodeRange::Parsed(block.at),
        special_scope_type: None, // can be changed later on with "fill_scope"
        vars: HashMap::new(),
        fns: HashMap::new(),
        cmd_aliases: HashMap::new(),
        type_aliases: HashMap::new(),
    };

    fill_scope(&mut scope);

    state.push_scope(scope);

    check_block_without_push(block, state)
}

fn check_block_without_push(block: &Eaten<Block>, state: &mut State) -> CheckerResult {
    check_block_in_current_scope(block, state)?;

    state.pop_scope();

    Ok(())
}

fn check_block_in_current_scope(block: &Eaten<Block>, state: &mut State) -> CheckerResult {
    let Block { instructions } = &block.data;

    assert_eq!(
        state.curr_scope().code_range.parsed_range().unwrap(),
        block.at
    );

    for instr in instructions {
        match &instr.data {
            Instruction::TypeAliasDecl { name, content } => {
                if state
                    .curr_scope_mut()
                    .type_aliases
                    .insert(name.data.clone(), name.at)
                    .is_some()
                {
                    return Err(CheckerError::new(
                        name.at,
                        "duplicate type alias declaration",
                    ));
                }

                check_value_type(&content.data, state)?;

                state.register_type_alias(name, content, block);
            }

            Instruction::FnDecl { name, content } => {
                if state.curr_scope().fns.contains_key(&name.data) {
                    return Err(CheckerError::new(name.at, "duplicate function declaration"));
                }

                if state.curr_scope_mut().cmd_aliases.contains_key(&name.data) {
                    return Err(CheckerError::new(
                        name.at,
                        "a command alias already uses this name",
                    ));
                }

                state.curr_scope_mut().fns.insert(
                    name.data.clone(),
                    DeclaredFn {
                        name_at: RuntimeCodeRange::Parsed(name.at),
                        is_method: content.signature.data.is_method(),
                    },
                );
            }

            Instruction::CmdAliasDecl { name, content } => {
                if state.curr_scope_mut().cmd_aliases.contains_key(&name.data) {
                    return Err(CheckerError::new(
                        name.at,
                        "duplicate command alias declaration",
                    ));
                }

                if state.curr_scope().fns.contains_key(&name.data) {
                    return Err(CheckerError::new(
                        name.at,
                        "a function already uses this name",
                    ));
                }

                state.curr_scope_mut().cmd_aliases.insert(
                    name.data.clone(),
                    DeclaredCmdAlias {
                        name_at: name.at,
                        content_at: content.at,
                        is_ready: false,
                    },
                );

                state.register_cmd_alias(content.clone());
            }

            _ => {}
        }
    }

    for instr in instructions {
        check_instr(instr, state)?;
    }

    Ok(())
}

fn check_instr(instr: &Eaten<Instruction>, state: &mut State) -> CheckerResult {
    match &instr.data {
        Instruction::DeclareVar {
            name,
            mutable,
            init_expr,
        } => {
            // if state.curr_scope().vars.contains_key(&name.data) {
            //     return Err(CheckerError::new(name.at, "duplicate variable declaration"));
            // }

            if let Some(init_expr) = init_expr {
                check_expr(&init_expr.data, state)?;
            }

            state.curr_scope_mut().vars.insert(
                name.data.clone(),
                DeclaredVar {
                    name_at: RuntimeCodeRange::Parsed(name.at),
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
            let dep = state.register_usage(name, DependencyType::Variable)?;

            match dep {
                UsedItem::Variable(var) => {
                    if !var.is_mut {
                        return Err(CheckerError::new(
                            name.at,
                            "variable was not declared as mutable",
                        ));
                    }
                }

                UsedItem::Function(_) | UsedItem::CmdAlias(_) => unreachable!(),
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
                scope.special_scope_type = Some(SpecialScopeType::Loop);

                scope.vars.insert(
                    iter_var.data.clone(),
                    DeclaredVar {
                        name_at: RuntimeCodeRange::Parsed(iter_var.at),
                        is_mut: false,
                    },
                );
            })?;
        }

        Instruction::ForLoopKeyed {
            key_iter_var,
            value_iter_var,
            iter_on,
            body,
        } => {
            check_expr(&iter_on.data, state)?;

            check_block_with(body, state, |scope| {
                scope.special_scope_type = Some(SpecialScopeType::Loop);

                scope.vars.insert(
                    key_iter_var.data.clone(),
                    DeclaredVar {
                        name_at: RuntimeCodeRange::Parsed(key_iter_var.at),
                        is_mut: false,
                    },
                );

                scope.vars.insert(
                    value_iter_var.data.clone(),
                    DeclaredVar {
                        name_at: RuntimeCodeRange::Parsed(value_iter_var.at),
                        is_mut: false,
                    },
                );
            })?;
        }

        Instruction::WhileLoop { cond, body } => {
            check_expr(&cond.data, state)?;
            check_block_with(body, state, |scope| {
                scope.special_scope_type = Some(SpecialScopeType::Loop);
            })?;
        }

        Instruction::LoopContinue => {
            if !matches!(
                state.nearest_special_scope_type(),
                Some(SpecialScopeType::Loop)
            ) {
                return Err(CheckerError::new(
                    instr.at,
                    "'continue' keyword can only be used inside a loop",
                ));
            }
        }

        Instruction::LoopBreak => {
            if !matches!(
                state.nearest_special_scope_type(),
                Some(SpecialScopeType::Loop)
            ) {
                return Err(CheckerError::new(
                    instr.at,
                    "'break' keyword can only be used inside a loop",
                ));
            }
        }

        Instruction::Switch { expr, cases } => {
            check_expr(&expr.data, state)?;

            for case in cases {
                let SwitchCase { cond, body } = case;

                check_expr(&cond.data, state)?;
                check_block(body, state)?;
            }
        }

        Instruction::FnDecl { name: _, content } => {
            // NOTE: function was already registered during init.

            check_function(content, state)?;
        }

        Instruction::FnReturn { expr } => {
            if !matches!(
                state.nearest_special_scope_type(),
                Some(SpecialScopeType::Function { args_at: _ })
            ) {
                return Err(CheckerError::new(
                    instr.at,
                    "'return' keyword can only be used inside a function",
                ));
            }

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
                        name_at: RuntimeCodeRange::Parsed(catch_var.at),
                        is_mut: false,
                    },
                );
            })?;
        }

        Instruction::CmdAliasDecl { name, content } => {
            // NOTE: command alias was already registered during init.

            state.prepare_deps(content.at);

            state.push_scope(CheckerScope {
                code_range: RuntimeCodeRange::Parsed(content.at),
                special_scope_type: Some(SpecialScopeType::CmdAlias),
                vars: HashMap::new(),
                fns: HashMap::new(),
                cmd_aliases: HashMap::new(),
                type_aliases: HashMap::new(),
            });

            check_single_cmd_call(content, state)?;

            state.pop_scope();

            state.mark_cmd_alias_as_ready(name);
        }

        Instruction::TypeAliasDecl {
            name: _,
            content: _,
        } => {
            // Already treated in first pass
        }

        Instruction::DoBlock(block) => check_block(block, state)?,

        Instruction::FnCall(fn_call) => check_fn_call(fn_call, state)?,

        Instruction::CmdCall(call) => check_cmd_call(call, state)?,

        Instruction::Imported(program) => {
            let curr_at = state.curr_scope().code_range;

            state.curr_scope_mut().code_range = RuntimeCodeRange::Parsed(program.data.content.at);

            check_block_in_current_scope(&program.data.content, state)?;

            state.curr_scope_mut().code_range = curr_at;
        }
    }

    Ok(())
}

fn check_expr_with(
    expr: &Eaten<impl AsRef<Expr>>,
    state: &mut State,
    fill_scope: impl FnOnce(&mut CheckerScope),
) -> CheckerResult {
    let mut scope = CheckerScope {
        code_range: RuntimeCodeRange::Parsed(expr.at),
        special_scope_type: None, // can be changed later on with "fill_scope"
        vars: HashMap::new(),
        fns: HashMap::new(),
        cmd_aliases: HashMap::new(),
        type_aliases: HashMap::new(),
    };

    fill_scope(&mut scope);

    state.push_scope(scope);

    check_expr(expr.data.as_ref(), state)?;

    state.pop_scope();

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
    let ExprInner { content, chainings } = &inner.data;

    check_expr_inner_content(&content.data, state)?;

    for chaining in chainings {
        check_expr_inner_chaining(&chaining.data, state)?;
    }

    Ok(())
}

fn check_expr_inner_chaining(chaining: &ExprInnerChaining, state: &mut State) -> CheckerResult {
    match chaining {
        ExprInnerChaining::PropAccess(prop_acc) => check_prop_access(prop_acc, state),
        ExprInnerChaining::MethodCall(method_call) => {
            match check_fn_call_and_get_item(method_call, state)? {
                UsedItem::Function(func) => {
                    if func.is_method {
                        Ok(())
                    } else {
                        Err(CheckerError::new(
                            method_call.data.name.at,
                            "this function is not a method",
                        ))
                    }
                }

                UsedItem::Variable(_) | UsedItem::CmdAlias(_) => unreachable!(),
            }
        }
        ExprInnerChaining::FnCall(fn_call) => check_fn_call(fn_call, state),
    }
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

        ExprInnerContent::FnAsValue(name) => {
            state.register_usage(name, DependencyType::Function)?;
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

        ExprInnerContent::Try {
            fn_call,
            catch_var,
            catch_expr,
        } => {
            check_fn_call(fn_call, state)?;

            check_expr_with(catch_expr, state, |scope| {
                scope.vars.insert(
                    catch_var.data.clone(),
                    DeclaredVar {
                        name_at: RuntimeCodeRange::Parsed(catch_var.at),
                        is_mut: false,
                    },
                );
            })?;
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
        Value::Variable(var) => {
            state.register_usage(var, DependencyType::Variable)?;
            Ok(())
        }
        Value::FnCall(fn_call) => check_fn_call(fn_call, state),
        Value::CmdOutput(cmd_call) => check_cmd_call(cmd_call, state),
        Value::CmdCall(cmd_call) => {
            state.register_cmd_call_value(cmd_call);
            check_cmd_call(cmd_call, state)
        }
        Value::FnAsValue(func_name) => {
            state.register_usage(func_name, DependencyType::Function)?;
            Ok(())
        }
        Value::Closure(func) => check_function(func, state),
    }
}

fn check_literal_value(lit_value: &Eaten<LiteralValue>, _: &mut State) -> CheckerResult {
    match &lit_value.data {
        LiteralValue::Boolean(_) => Ok(()),
        LiteralValue::Integer(_) => Ok(()),
        LiteralValue::Float(_) => Ok(()),
        LiteralValue::String(_) => Ok(()),
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
        ComputedStringPiece::Variable(var) => {
            state.register_usage(var, DependencyType::Variable)?;
            Ok(())
        }
        ComputedStringPiece::Expr(expr) => check_expr(&expr.data, state),
        ComputedStringPiece::CmdCall(cmd_call) => check_cmd_call(cmd_call, state),
    }
}

fn check_fn_call(fn_call: &Eaten<FnCall>, state: &mut State) -> CheckerResult {
    check_fn_call_and_get_item(fn_call, state).map(|_| ())
}

fn check_fn_call_and_get_item(
    fn_call: &Eaten<FnCall>,
    state: &mut State,
) -> CheckerResult<UsedItem> {
    let FnCall {
        is_var_name,
        name,
        call_args,
    } = &fn_call.data;

    let item = if *is_var_name {
        state.register_usage(name, DependencyType::Variable)?
    } else {
        state.register_usage(name, DependencyType::Function)?
    };

    for arg in &call_args.data {
        check_fn_call_arg(arg, state)?;
    }

    Ok(item)
}

fn check_fn_call_arg(arg: &Eaten<FnCallArg>, state: &mut State) -> CheckerResult {
    match &arg.data {
        FnCallArg::Expr(expr) => check_expr(&expr.data, state),
        FnCallArg::Flag { name: _, value } => check_expr(&value.data, state),
        FnCallArg::CmdArg(cmd_arg) => check_cmd_arg(cmd_arg, state),
    }
}

fn check_cmd_call(cmd_call: &Eaten<CmdCall>, state: &mut State) -> CheckerResult {
    let CmdCall { base, pipes } = &cmd_call.data;

    let calls = [(None, base)].into_iter().chain(
        pipes
            .iter()
            .map(|CmdPipe { pipe_type, cmd }| (Some(pipe_type), cmd)),
    );

    let mut chain_type = None;

    for (pipe_type, call) in calls {
        let call_type = check_single_cmd_call(call, state)?;

        let current_chain_type = match chain_type {
            None => {
                chain_type = Some(call_type);
                call_type
            }

            Some(chain_type) => {
                if chain_type != call_type {
                    return Err(CheckerError::new(
                        pipe_type.unwrap().at,
                        match (chain_type, call_type) {
                            (CmdPathTargetType::Function, CmdPathTargetType::ExternalCommand) => {
                                "cannot pipe a function's output into an external command"
                            }

                            (CmdPathTargetType::ExternalCommand, CmdPathTargetType::Function) => {
                                "cannot pipe an external command's output into a function"
                            }

                            _ => unreachable!(),
                        },
                    ));
                }

                chain_type
            }
        };

        if let Some(pipe_type) = pipe_type {
            match pipe_type.data {
                CmdPipeType::Stdout | CmdPipeType::Stderr => {
                    if current_chain_type == CmdPathTargetType::Function {
                        return Err(CheckerError::new(
                            pipe_type.at,
                            "cannot apply stdout/stderr pipe to a function",
                        ));
                    }
                }

                CmdPipeType::Value => {
                    if current_chain_type == CmdPathTargetType::ExternalCommand {
                        return Err(CheckerError::new(
                            pipe_type.at,
                            "value pipe can only be applied to functions",
                        ));
                    }
                }
            }
        }
    }

    Ok(())
}

fn check_single_cmd_call(
    single_cmd_call: &Eaten<SingleCmdCall>,
    state: &mut State,
) -> CheckerResult<CmdPathTargetType> {
    let SingleCmdCall {
        env_vars,
        path,
        args,
    } = &single_cmd_call.data;

    for env_var in &env_vars.data {
        check_cmd_env_var(env_var, state)?;
    }

    let mut developed_aliases = vec![];

    let target_type = match &path.data {
        CmdPath::Direct(_) => CmdPathTargetType::ExternalCommand,
        CmdPath::Expr(_) => CmdPathTargetType::Function,
        CmdPath::ComputedString(_) => CmdPathTargetType::ExternalCommand,
        CmdPath::RawString(name) => {
            enum Target {
                CmdAlias(DeclaredCmdAlias),
                Function,
            }

            let target = state.scopes().find_map(|scope| {
                scope
                    .cmd_aliases
                    .get(&name.data)
                    .map(|cmd_alias| Target::CmdAlias(*cmd_alias))
                    .or_else(|| scope.fns.get(&name.data).map(|_| Target::Function))
            });

            match target {
                Some(target) => match target {
                    Target::CmdAlias(cmd_alias) => {
                        let DeclaredCmdAlias {
                            name_at: _,
                            content_at,
                            is_ready,
                        } = cmd_alias;

                        if !is_ready {
                            return Err(CheckerError::new(
                                name.at,
                                "cannot reference a command alias before its declaration",
                            ));
                        }

                        developed_aliases.push(DevelopedCmdAliasCall {
                            content_at,
                            called_alias_name: name.clone(),
                        });

                        state.register_usage(name, DependencyType::CmdAlias)?;

                        let alias_inner_call = state.get_developed_cmd_call_at(content_at);
                        let alias_inner_call = alias_inner_call.as_ref().unwrap();

                        developed_aliases
                            .extend(alias_inner_call.developed_aliases.iter().cloned());

                        if alias_inner_call.is_function {
                            CmdPathTargetType::Function
                        } else {
                            CmdPathTargetType::ExternalCommand
                        }
                    }

                    Target::Function => CmdPathTargetType::Function,
                },

                None => CmdPathTargetType::ExternalCommand,
            }
        }
    };

    for arg in &args.data {
        check_cmd_arg(arg, state)?;
    }

    state.register_developed_single_cmd_call(
        single_cmd_call,
        DevelopedSingleCmdCall {
            at: single_cmd_call.at,
            is_function: match target_type {
                CmdPathTargetType::Function => true,
                CmdPathTargetType::ExternalCommand => false,
            },
            developed_aliases,
        },
    );

    Ok(target_type)
}

fn check_cmd_env_var(cmd_env_var: &Eaten<CmdEnvVar>, state: &mut State) -> CheckerResult {
    let CmdEnvVar { name: _, value } = &cmd_env_var.data;

    check_cmd_value_making_arg(&value.data, state)
}

fn check_cmd_arg(arg: &Eaten<CmdArg>, state: &mut State) -> CheckerResult {
    match &arg.data {
        CmdArg::ValueMaking(value_making) => check_cmd_value_making_arg(value_making, state),

        CmdArg::Flag(flag_arg) => {
            let CmdFlagArg { name: _, value } = flag_arg;

            match value {
                Some(CmdFlagValueArg {
                    value_sep: _,
                    value,
                }) => check_cmd_value_making_arg(&value.data, state),

                None => Ok(()),
            }
        }

        CmdArg::SpreadVar(var) => {
            state.register_usage(var, DependencyType::Variable)?;
            Ok(())
        }
    }
}

fn check_cmd_value_making_arg(arg: &CmdValueMakingArg, state: &mut State) -> CheckerResult {
    match arg {
        CmdValueMakingArg::LiteralValue(lit) => check_literal_value(lit, state),

        CmdValueMakingArg::ComputedString(computed_string) => {
            check_computed_string(computed_string, state)
        }

        CmdValueMakingArg::CmdOutput(cmd_call) => check_cmd_call(cmd_call, state),

        CmdValueMakingArg::ParenExpr(expr) => check_expr(&expr.data, state),

        CmdValueMakingArg::VarName(var) => {
            state.register_usage(var, DependencyType::Variable)?;
            Ok(())
        }

        CmdValueMakingArg::Raw(_) => Ok(()),
    }
}

fn check_function(func: &Function, state: &mut State) -> CheckerResult {
    let Function { signature, body } = func;

    state.register_function_body(body.clone());

    let checked_args = check_fn_signature(signature, state)?;

    let FnSignature { args, ret_type: _ } = &signature.data;

    let args = match args {
        RuntimeEaten::Parsed(args) => args,
        RuntimeEaten::Internal(_) => unreachable!(),
    };

    let mut vars = HashMap::with_capacity(checked_args.len());

    for checked_arg in checked_args {
        let CheckedFnArg {
            name_at,
            var_name,
            is_rest: _,
        } = checked_arg;

        let dup = vars.insert(
            var_name,
            DeclaredVar {
                name_at: RuntimeCodeRange::Parsed(name_at),
                is_mut: false,
            },
        );

        assert!(dup.is_none());
    }

    state.prepare_deps(body.at);

    let fill_scope = |scope: &mut CheckerScope| {
        scope.special_scope_type = Some(SpecialScopeType::Function { args_at: args.at });

        for (name, var) in vars {
            scope.vars.insert(name, var);
        }
    };

    match &body.data {
        FunctionBody::Expr(expr) => {
            // Check expr
            check_expr_with(expr, state, fill_scope)
        }

        FunctionBody::Block(block) => {
            // Check block
            check_block_with(block, state, fill_scope)
        }
    }
}

fn check_fn_arg(arg: &FnArg, state: &mut State) -> CheckerResult<CheckedFnArg> {
    let (name, name_at, alt_var_name, is_rest) = match arg {
        FnArg::Positional {
            name,
            is_optional: _,
            typ,
        } => {
            if let Some(typ) = typ {
                check_value_type(typ.data(), state)?;
            }

            (name.data().clone(), name.at(), None, false)
        }

        FnArg::PresenceFlag { names } => match names {
            FnFlagArgNames::ShortFlag(short) => (short.data().to_string(), short.at(), None, false),

            FnFlagArgNames::LongFlag(long) => (
                long.data().clone(),
                long.at(),
                Some(long_flag_var_name(long.data())),
                false,
            ),

            FnFlagArgNames::LongAndShortFlag { long, short: _ } => (
                long.data().clone(),
                long.at(),
                Some(long_flag_var_name(long.data())),
                false,
            ),
        },

        FnArg::NormalFlag {
            names,
            is_optional: _,
            typ,
        } => {
            if let Some(typ) = typ {
                check_value_type(typ.data(), state)?;
            }

            match names {
                FnFlagArgNames::ShortFlag(short) => {
                    (short.data().to_string(), short.at(), None, false)
                }

                FnFlagArgNames::LongFlag(long) => (
                    long.data().clone(),
                    long.at(),
                    Some(long_flag_var_name(long.data())),
                    false,
                ),

                FnFlagArgNames::LongAndShortFlag { long, short: _ } => (
                    long.data().clone(),
                    long.at(),
                    Some(long_flag_var_name(long.data())),
                    false,
                ),
            }
        }

        FnArg::Rest { name } => (name.data().clone(), name.at(), None, true),
    };

    Ok(CheckedFnArg {
        var_name: alt_var_name.unwrap_or_else(|| name.clone()),
        name_at: match name_at {
            RuntimeCodeRange::Parsed(at) => at,
            RuntimeCodeRange::Internal => unreachable!(),
        },
        is_rest,
    })
}

struct CheckedFnArg {
    name_at: CodeRange,
    var_name: String,
    is_rest: bool,
}

/// Compute the variable name for a long flag
/// Converst a raw flag name to a valid (variable) identifier
///
/// Example: `push-with-lease` -> `pushWithLease`
pub fn long_flag_var_name(name: &str) -> String {
    let mut var_name = String::with_capacity(name.len());

    let mut uppercase = false;

    for char in name.chars() {
        if char == '-' {
            uppercase = true;
        } else if uppercase {
            uppercase = false;

            for char in char.to_uppercase() {
                var_name.push(char);
            }
        } else {
            var_name.push(char);
        }
    }

    var_name
}

fn check_value_type(value_type: &ValueType, state: &mut State) -> CheckerResult {
    match value_type {
        ValueType::Single(typ) => check_single_value_type(typ.data(), state)?,
        ValueType::Union(types) => {
            for typ in types {
                check_single_value_type(typ.data(), state)?;
            }
        }
    }

    Ok(())
}

fn check_single_value_type(value_type: &SingleValueType, state: &mut State) -> CheckerResult {
    match value_type {
        SingleValueType::Any
        | SingleValueType::Null
        | SingleValueType::Bool
        | SingleValueType::Int
        | SingleValueType::Float
        | SingleValueType::String
        | SingleValueType::List
        | SingleValueType::Range
        | SingleValueType::Map
        | SingleValueType::Error
        | SingleValueType::UntypedStruct
        | SingleValueType::ArgSpread
        | SingleValueType::CmdCall => Ok(()),

        SingleValueType::TypedStruct(members) => {
            let mut names = HashSet::new();

            for member in members {
                let StructTypeMember { name, typ } = member.data();

                if !names.insert(name.data()) {
                    return Err(CheckerError::new(
                        name.eaten().unwrap().at,
                        "duplicate member in struct",
                    ));
                }

                check_value_type(typ.data(), state)?;
            }

            Ok(())
        }

        SingleValueType::Function(signature) => match signature {
            RuntimeEaten::Parsed(eaten) => check_fn_signature(eaten, state).map(|_| ()),
            RuntimeEaten::Internal(_) => Ok(()),
        },

        SingleValueType::TypeAlias(name) => state.register_type_alias_usage(name),

        SingleValueType::Custom(_) => unreachable!(),
    }
}

fn check_fn_signature(
    signature: &Eaten<FnSignature>,
    state: &mut State,
) -> CheckerResult<Vec<CheckedFnArg>> {
    state.register_function_signature(signature.clone());

    let FnSignature { args, ret_type } = &signature.data;

    let args = match args {
        RuntimeEaten::Parsed(args) => args,
        RuntimeEaten::Internal(_) => unreachable!(),
    };

    let mut used_idents = HashSet::new();
    let mut rest_arg_name_at = None;

    let mut checked_args = Vec::with_capacity(args.data.len());

    for arg in &args.data {
        let checked_arg = check_fn_arg(arg, state)?;

        let CheckedFnArg {
            var_name,
            name_at,
            is_rest,
        } = &checked_arg;

        if let Some(rest_arg_name_at) = rest_arg_name_at {
            return Err(CheckerError::new(
                rest_arg_name_at,
                "rest argument must be the very last of the function",
            ));
        }

        if *is_rest {
            rest_arg_name_at = Some(*name_at);
        }

        if !used_idents.insert(var_name.clone()) {
            return Err(CheckerError::new(
                *name_at,
                "Duplicate argument name in function",
            ));
        }

        checked_args.push(checked_arg);
    }

    if let Some(ret_type) = ret_type {
        check_value_type(ret_type.data(), state)?;
    }

    Ok(checked_args)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CmdPathTargetType {
    Function,
    ExternalCommand,
}
