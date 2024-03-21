#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod errors;
mod state;

use std::collections::{HashMap, HashSet};

use indexmap::IndexSet;
use parsy::Eaten;
use reshell_parser::ast::{
    Block, CmdArg, CmdCall, CmdEnvVar, CmdEnvVarValue, CmdPath, CmdPipe, ComputedString,
    ComputedStringPiece, DoubleOp, ElsIf, ElsIfExpr, Expr, ExprInner, ExprInnerContent, ExprOp,
    FnArg, FnArgNames, FnCall, FnCallArg, FnSignature, Function, Instruction, LiteralValue,
    Program, PropAccess, PropAccessNature, SingleCmdCall, SingleOp, SingleValueType,
    StructTypeMember, SwitchCase, Value, ValueType,
};

pub use self::{
    errors::CheckerError,
    state::{CheckerOutput, CheckerScope, DeclaredVar, Dependency, DependencyType},
};

use self::{
    errors::CheckerResult,
    state::{ScopeType, State},
};

pub fn check(
    program: &Program,
    native_lib_scope: CheckerScope,
    mut first_scope: CheckerScope,
) -> CheckerResult<CheckerOutput> {
    let Program { content } = program;

    let mut state = State::new();
    state.push_scope(native_lib_scope);

    first_scope.code_range = content.data.code_range;
    state.push_scope(first_scope);

    check_block_without_push(content, &mut state)?;

    Ok(state.collected)
}

fn check_block(block: &Eaten<Block>, state: &mut State) -> CheckerResult {
    check_block_with(block, state, |_| {})
}

fn check_block_with(
    block: &Eaten<Block>,
    state: &mut State,
    fill_scope: impl FnOnce(&mut CheckerScope),
) -> CheckerResult {
    let Block {
        instructions: _,
        code_range,
    } = &block.data;

    let mut scope = CheckerScope {
        code_range: *code_range,
        deps: false, // can be changed later on with "fill_scope"
        typ: None,   // can be changed later on with "fill_scope"
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
    let Block {
        instructions,
        code_range,
    } = &block.data;

    assert!(state.curr_scope().code_range == *code_range);

    for instr in instructions {
        if let Instruction::TypeAliasDecl { name, content } = &instr.data {
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

            state
                .collected
                .type_aliases
                .insert(name.at, content.clone());

            state
                .collected
                .type_aliases_decl
                .entry(*code_range)
                .or_default()
                .insert(name.data.clone(), name.at);
        }
    }

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
                return Err(CheckerError::new(name.at, "duplicate variable declaration"));
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
            let dep = state.register_usage(name, DependencyType::Variable)?;

            if !dep.is_mut {
                return Err(CheckerError::new(
                    name.at,
                    "variable was not declared as mutable",
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
                scope.typ = Some(ScopeType::Loop);

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
            check_block_with(body, state, |scope| {
                scope.typ = Some(ScopeType::Loop);
            })?;
        }

        Instruction::LoopContinue => {
            if !matches!(state.curr_scope_type(), Some(ScopeType::Loop)) {
                return Err(CheckerError::new(
                    instr.at,
                    "can only be used inside a loop",
                ));
            }
        }

        Instruction::LoopBreak => {
            if !matches!(state.curr_scope_type(), Some(ScopeType::Loop)) {
                return Err(CheckerError::new(
                    instr.at,
                    "can only be used inside a loop",
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

        Instruction::FnDecl { name, content } => {
            if state.curr_scope().fns.contains_key(&name.data) {
                return Err(CheckerError::new(name.at, "duplicate function declaration"));
            }

            if state.curr_scope().cmd_aliases.contains_key(&name.data) {
                return Err(CheckerError::new(
                    name.at,
                    "a command alias already uses this name",
                ));
            }

            state
                .curr_scope_mut()
                .fns
                .insert(name.data.clone(), name.at);

            check_function(content, state)?;
        }

        Instruction::FnReturn { expr } => {
            if !matches!(
                state.curr_scope_type(),
                Some(ScopeType::Function { args_at: _ })
            ) {
                return Err(CheckerError::new(
                    instr.at,
                    "can only be used inside a function",
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
                        name_at: catch_var.at,
                        is_mut: false,
                    },
                );
            })?;
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

            state
                .curr_scope_mut()
                .cmd_aliases
                .insert(name.data.clone(), name.at);

            state.collected.deps.insert(content.at, IndexSet::new());

            state.push_scope(CheckerScope {
                code_range: content.at,
                deps: true,
                typ: None,
                vars: HashMap::new(),
                fns: HashMap::new(),
                // TODO: inject this cmd alias into the created scope (allows inner usage) - first check if it doesn't create an infinite loop
                // and test this throroughtly!
                cmd_aliases: HashMap::new(),
                type_aliases: HashMap::new(),
            });

            check_single_cmd_call(content, state)?;

            state.pop_scope();
        }

        Instruction::TypeAliasDecl {
            name: _,
            content: _,
        } => {
            // Already treated in first pass
        }

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
        Value::Variable(var) => {
            state.register_usage(var, DependencyType::Variable)?;
            Ok(())
        }
        Value::FnCall(fn_call) => check_fn_call(fn_call, state),
        Value::CmdOutput(cmd_call) => check_cmd_call(cmd_call, state),
        Value::CmdSuccess(cmd_call) => check_cmd_call(cmd_call, state),
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
    let FnCall {
        is_var_name,
        name,
        call_args,
    } = &fn_call.data;

    if *is_var_name {
        state.register_usage(name, DependencyType::Variable)?;
    } else {
        state.register_usage(name, DependencyType::Function)?;
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

    let calls = [base]
        .into_iter()
        .chain(pipes.iter().map(|CmdPipe { pipe_type: _, cmd }| cmd));

    for cmd in calls {
        match check_single_cmd_call(cmd, state)? {
            SingleCmdCallTargetType::Function => {
                if !pipes.is_empty() {
                    return Err(CheckerError::new(
                        cmd.at,
                        "functions cannot be piped from or into",
                    ));
                }
            }
            SingleCmdCallTargetType::Command => {}
        }
    }

    Ok(())
}

fn check_single_cmd_call(
    single_cmd_call: &Eaten<SingleCmdCall>,
    state: &mut State,
) -> CheckerResult<SingleCmdCallTargetType> {
    let SingleCmdCall {
        env_vars,
        path,
        args,
    } = &single_cmd_call.data;

    for env_var in &env_vars.data {
        check_cmd_env_var(env_var, state)?;
    }

    let mut target_type = SingleCmdCallTargetType::Command;

    match &path.data {
        CmdPath::RawString(name) => {
            if state.is_fn(&name.data) {
                state.register_usage(name, DependencyType::Function)?;
                target_type = SingleCmdCallTargetType::Function;
            } else if state.is_cmd_alias(&name.data) {
                state.register_usage(name, DependencyType::CmdAlias)?;
            }
        }
        CmdPath::Direct(_) => {}
        CmdPath::Expr(expr) => check_expr(&expr.data, state)?,
        CmdPath::ComputedString(computed_string) => check_computed_string(computed_string, state)?,
    }

    for arg in &args.data {
        check_cmd_arg(arg, state)?;
    }

    Ok(target_type)
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

fn check_cmd_arg(arg: &Eaten<CmdArg>, state: &mut State) -> CheckerResult {
    match &arg.data {
        CmdArg::LiteralValue(lit) => check_literal_value(lit, state),
        CmdArg::ComputedString(computed_string) => check_computed_string(computed_string, state),
        CmdArg::CmdCall(cmd_call) => check_cmd_call(cmd_call, state),
        CmdArg::ParenExpr(expr) => check_expr(&expr.data, state),
        CmdArg::VarName(var) => {
            state.register_usage(var, DependencyType::Variable)?;
            Ok(())
        }
        CmdArg::FnAsValue(func) => {
            state.register_usage(func, DependencyType::Function)?;
            Ok(())
        }
        CmdArg::Raw(_) => Ok(()),
        CmdArg::SpreadVar(var) => {
            state.register_usage(var, DependencyType::Variable)?;
            Ok(())
        }
    }
}

fn check_function(func: &Function, state: &mut State) -> CheckerResult {
    let Function { signature, body } = func;

    check_fn_signature(signature, state)?;

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
            FnArgNames::Positional(name) => name.clone(),
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

    state
        .collected
        .deps
        .insert(body.data.code_range, IndexSet::new());

    check_block_with(body, state, |scope| {
        scope.deps = true;
        scope.typ = Some(ScopeType::Function { args_at: args.at });

        for (name, var) in vars {
            scope.vars.insert(name, var);
        }
    })
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
        | SingleValueType::UntypedStruct => Ok(()),

        SingleValueType::TypedStruct(members) => {
            for member in members {
                // TODO: ensure no duplicate member
                let StructTypeMember { name: _, typ } = member.data();

                check_value_type(typ.data(), state)?;
            }

            Ok(())
        }

        SingleValueType::Function(signature) => check_fn_signature(signature, state),

        SingleValueType::TypeAlias(name) => state.register_type_alias_usage(name),
    }
}

fn check_fn_signature(signature: &FnSignature, state: &mut State) -> CheckerResult {
    let FnSignature { args, ret_type } = signature;

    let mut used_idents = HashSet::new();

    for arg in &args.data {
        let FnArg {
            names,
            is_optional: _,
            is_rest: _,
            typ,
        } = arg;

        let (long, short) = match names {
            FnArgNames::Positional(name) => (Some(name), None),
            FnArgNames::ShortFlag(flag) => (None, Some(flag)),
            FnArgNames::LongFlag(flag) => (Some(flag), None),
            FnArgNames::LongAndShortFlag { long, short } => (Some(long), Some(short)),
        };

        if let Some(long) = long {
            if used_idents.contains(&long.data) {
                return Err(CheckerError::new(long.at, "duplicate argument name "));
            }

            used_idents.insert(long.data.clone());
        }

        if let Some(short) = short {
            let short_str = short.data.to_string();

            if used_idents.contains(&short_str) {
                return Err(CheckerError::new(short.at, "duplicate flag name"));
            }

            used_idents.insert(short_str);
        }

        if let Some(typ) = typ {
            check_value_type(&typ.data, state)?;
        }
    }

    if let Some(ret_type) = ret_type {
        check_value_type(&ret_type.data, state)?;
    }

    Ok(())
}

enum SingleCmdCallTargetType {
    Function,
    Command,
}