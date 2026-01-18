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
pub mod typechecking;

use std::collections::{HashMap, HashSet};

use parsy::{InputRange, Span};
use reshell_parser::ast::{
    AstScopeId, Block, CmdArg, CmdCall, CmdCallBase, CmdCaptureType, CmdEnvVar, CmdExternalPath,
    CmdFlagArg, CmdFlagValueArg, CmdOutputCapture, CmdPath, CmdPipe, CmdPipeType, CmdRawString,
    CmdRawStringPiece, CmdRedirects, CmdValueMakingArg, ComputedString, ComputedStringPiece, ElsIf,
    ElsIfExpr, Expr, ExprInner, ExprInnerChaining, ExprInnerContent, ExprOp, FnArg, FnCall,
    FnCallNature, FnSignature, FnSignatureArg, FnSignatureFlagArgNames, FnSignatureNormalFlagArg,
    FnSignaturePositionalArg, FnSignaturePresenceFlagArg, FnSignatureRestArg, Function,
    Instruction, ListItem, LiteralValue, MapItem, MapKey, MatchCase, MatchExprCase,
    ObjPropSpreading, ObjPropSpreadingBinding, ObjPropSpreadingType, Program, PropAccess,
    PropAccessNature, Range, RangeBound, RuntimeCodeRange, SingleCmdCall, SingleOp,
    SingleValueType, SingleVarDecl, SpreadValue, StructItem, StructTypeMember, TypeMatchCase,
    TypeMatchExprCase, Value, ValueDestructuring, ValueType,
};
use reshell_prettify::PrettyPrintable;

use self::typechecking::check_if_type_fits_type;
pub use self::{
    errors::CheckerError,
    state::{
        CheckerScope, DeclaredCmdAlias, DeclaredFn, DeclaredMethod, DeclaredVar, SpecialScopeType,
        TypeAliasStore,
    },
};
use crate::{errors::CheckerResult, output::*, state::State};

pub fn check(
    program: &Program,
    scopes: Vec<CheckerScope>,
    prev_output: &mut CheckerOutput,
) -> CheckerResult {
    let Program { content } = program;

    let mut state = State::new(prev_output);

    for scope in scopes {
        state.push_scope(scope);
    }

    check_block(&content.data, &mut state)?;

    Ok(())
}

fn check_block(block: &Block, state: &mut State) -> CheckerResult {
    check_block_with(block, state, |_, _| Ok(()))
}

fn check_block_with(
    block: &Block,
    state: &mut State,
    fill_scope: impl FnOnce(&mut CheckerScope, &mut State) -> CheckerResult,
) -> CheckerResult {
    let Block {
        scope_id,
        instructions,
    } = block;

    let mut scope = CheckerScope {
        id: *scope_id,
        special_scope_type: None, // can be changed later on with "fill_scope"
        vars: HashMap::new(),
        fns: HashMap::new(),
        methods: HashMap::new(),
        cmd_aliases: HashMap::new(),
        type_aliases: HashMap::new(),
    };

    fill_scope(&mut scope, state)?;

    state.push_scope(scope);

    // assert_eq!(
    //     state.curr_scope().code_range.parsed_range().unwrap(),
    //     block.at
    // );

    block_first_pass(instructions, block, state)?;

    for instr in instructions {
        check_instr(instr, state)?;
    }

    state.pop_scope();

    Ok(())
}

fn block_first_pass(
    instructions: &[Span<Instruction>],
    block: &Block,
    state: &mut State,
) -> CheckerResult {
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

            Instruction::FnDecl { name, content: _ } => {
                if state.curr_scope().fns.contains_key(&name.data) {
                    return Err(CheckerError::new(name.at, "duplicate function declaration"));
                }

                if state.curr_scope_mut().cmd_aliases.contains_key(&name.data) {
                    return Err(CheckerError::new(
                        name.at,
                        "a command alias already uses this name",
                    ));
                }

                let curr_scope_id = state.curr_scope().id;

                state.curr_scope_mut().fns.insert(
                    name.data.clone(),
                    DeclaredFn {
                        decl_at: RuntimeCodeRange::Parsed(name.at),
                        scope_id: curr_scope_id,
                    },
                );
            }

            Instruction::MethodDecl {
                name,
                on_type,
                content: _,
            } => {
                check_value_type(on_type, state)?;

                let curr_scope_id = state.curr_scope().id;

                if let Some(same_name_methods) = state.curr_scope().methods.get(&name.data) {
                    for (other_type, _) in same_name_methods.iter() {
                        if check_if_type_fits_type(on_type, other_type, state.type_alias_store())
                            || check_if_type_fits_type(
                                other_type,
                                on_type,
                                state.type_alias_store(),
                            )
                        {
                            return Err(CheckerError::new(
                                name.at,
                                "this method clashes with another same-name method applying on a compatible type",
                            ).with_detail(
                                format!("trying to declare a method for type     : {}", on_type.display_inline())
                            ).with_detail(
                                format!("...but a method exists for clashing type: {}", other_type.display_inline())
                            ));
                        }
                    }
                }

                state
                    .curr_scope_mut()
                    .methods
                    .entry(name.data.clone())
                    .or_default()
                    .push((
                        shared(on_type.clone()),
                        DeclaredMethod {
                            decl_at: RuntimeCodeRange::Parsed(name.at),
                            scope_id: curr_scope_id,
                        },
                    ));
            }

            Instruction::CmdAliasDecl {
                name,
                content,
                content_scope_id: _,
            } => {
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

                let curr_scope_id = state.curr_scope().id;

                state.curr_scope_mut().cmd_aliases.insert(
                    name.data.clone(),
                    DeclaredCmdAlias {
                        decl_at: name.at,
                        scope_id: curr_scope_id,
                        content_at: content.at,
                        is_ready: false,
                    },
                );

                state.register_cmd_alias(content.clone());
            }

            Instruction::Include(program) => {
                let Program { content } = program;
                let Block {
                    scope_id: _,
                    instructions,
                } = &content.data;

                block_first_pass(instructions, block, state)?;
            }

            _ => {}
        }
    }

    Ok(())
}

fn check_instr(instr: &Span<Instruction>, state: &mut State) -> CheckerResult {
    match &instr.data {
        Instruction::DeclareVar { names, init_expr } => {
            check_expr(&init_expr.data, state)?;

            register_destructuring(&names.data, None, state, None)?;
        }

        Instruction::AssignVar {
            name,
            prop_acc,
            list_push: _,
            expr,
        } => {
            state.register_var_usage(name)?;

            let var = state
                .scopes()
                .find_map(|scope| scope.vars.get(&name.data))
                .unwrap();

            if !var.is_mut {
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
                let ElsIf { cond, body } = elsif;

                check_expr(&cond.data, state)?;
                check_block(body, state)?;
            }

            if let Some(els) = els {
                check_block(els, state)?;
            }
        }

        Instruction::ForLoop {
            destructure_as,
            iter_on,
            body,
        } => {
            check_expr(&iter_on.data, state)?;

            check_block_with(body, state, |scope, state| {
                scope.special_scope_type = Some(SpecialScopeType::Loop);
                register_destructuring(&destructure_as.data, Some(scope), state, None)
            })?;
        }

        Instruction::ForLoopKeyed {
            key_iter_var,
            destructure_as,
            iter_on,
            body,
        } => {
            check_expr(&iter_on.data, state)?;

            check_block_with(body, state, |scope, state| {
                scope.special_scope_type = Some(SpecialScopeType::Loop);

                scope.vars.insert(
                    key_iter_var.data.clone(),
                    DeclaredVar {
                        decl_at: RuntimeCodeRange::Parsed(key_iter_var.at),
                        scope_id: scope.id,
                        is_mut: false,
                    },
                );

                register_destructuring(
                    &destructure_as.data,
                    Some(scope),
                    state,
                    Some(HashSet::from([key_iter_var.data.clone()])),
                )
            })?;
        }

        Instruction::WhileLoop { cond, body } => {
            check_expr(&cond.data, state)?;

            check_block_with(body, state, |scope, _| {
                scope.special_scope_type = Some(SpecialScopeType::Loop);
                Ok(())
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

        Instruction::Match { expr, cases, els } => {
            check_expr(&expr.data, state)?;

            for case in cases {
                let MatchCase { matches, body } = case;

                check_expr(&matches.data, state)?;
                check_block(body, state)?;
            }

            if let Some(els) = els {
                check_block(els, state)?;
            }
        }

        Instruction::TypeMatch { expr, cases, els } => {
            check_expr(&expr.data, state)?;

            for case in cases {
                let TypeMatchCase { matches, body } = case;

                check_value_type(matches, state)?;
                check_block(body, state)?;
            }

            if let Some(els) = els {
                check_block(els, state)?;
            }
        }

        Instruction::FnDecl { name: _, content } => {
            // NOTE: function was already registered during init.

            check_function(content, state)?;
        }

        Instruction::MethodDecl {
            name: _,
            on_type: _,
            content,
        } => {
            // NOTE: method was already registered during init.

            check_function(content, state)?;
        }

        Instruction::FnReturn { expr } => {
            if !state
                .scopes()
                .any(|scope| scope.special_scope_type == Some(SpecialScopeType::Function))
            {
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
            try_body,
            catch_var,
            catch_body,
        } => {
            check_block(&try_body.data, state)?;

            check_block_with(catch_body, state, |scope, _| {
                if let Some(catch_var) = catch_var {
                    scope.vars.insert(
                        catch_var.data.clone(),
                        DeclaredVar {
                            decl_at: RuntimeCodeRange::Parsed(catch_var.at),
                            scope_id: scope.id,
                            is_mut: false,
                        },
                    );
                }

                Ok(())
            })?;
        }

        Instruction::CmdAliasDecl {
            name,
            content,
            content_scope_id,
        } => {
            // NOTE: command alias was already registered during init.

            state.prepare_deps(*content_scope_id);

            state.push_scope(CheckerScope {
                id: *content_scope_id,
                special_scope_type: Some(SpecialScopeType::CmdAlias),
                vars: HashMap::new(),
                fns: HashMap::new(),
                methods: HashMap::new(),
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

        Instruction::CmdCall(call) => check_cmd_call(call, state)?,

        Instruction::Include(program) => {
            let Program { content } = program;
            let Block {
                instructions,
                scope_id: _,
            } = &content.data;

            for instr in instructions {
                check_instr(instr, state)?;
            }
        }
    }

    Ok(())
}

fn register_destructuring(
    names: &ValueDestructuring,
    scope: Option<&mut CheckerScope>,
    state: &mut State,
    used_idents: Option<HashSet<String>>,
) -> CheckerResult {
    return insert_vars(names, scope, &mut used_idents.unwrap_or_default(), state);

    fn insert_vars(
        names: &ValueDestructuring,
        mut scope: Option<&mut CheckerScope>,
        idents: &mut HashSet<String>,
        state: &mut State,
    ) -> CheckerResult {
        match names {
            ValueDestructuring::Single(single) => {
                insert_var(single, scope.as_deref_mut(), idents, state)?;
            }

            ValueDestructuring::Tuple(vars) => {
                for vars in vars {
                    insert_vars(&vars.data, scope.as_deref_mut(), idents, state)?;
                }
            }

            ValueDestructuring::MapOrStruct(vars) => {
                for var in vars {
                    let ObjPropSpreading { typ, default_value } = var;

                    match typ {
                        ObjPropSpreadingType::RawKeyToConst { name, binding } => match binding {
                            Some(binding) => {
                                insert_binding(binding, scope.as_deref_mut(), idents, state)?;
                            }

                            None => {
                                insert_var(
                                    &SingleVarDecl {
                                        name: name.clone(),
                                        is_mut: false,
                                        enforced_type: None,
                                    },
                                    scope.as_deref_mut(),
                                    idents,
                                    state,
                                )?;
                            }
                        },

                        ObjPropSpreadingType::RawKeyToMut { name } => {
                            insert_var(
                                &SingleVarDecl {
                                    name: name.clone(),
                                    is_mut: true,
                                    enforced_type: None,
                                },
                                scope.as_deref_mut(),
                                idents,
                                state,
                            )?;
                        }

                        ObjPropSpreadingType::LiteralKeyToConst {
                            literal_name: _,
                            binding,
                        } => {
                            insert_binding(binding, scope.as_deref_mut(), idents, state)?;
                        }
                    }

                    if let Some(default_value) = default_value {
                        check_expr(default_value, state)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn insert_binding(
        binding: &ObjPropSpreadingBinding,
        scope: Option<&mut CheckerScope>,
        idents: &mut HashSet<String>,
        state: &mut State,
    ) -> CheckerResult {
        match binding {
            ObjPropSpreadingBinding::BindTo { alias, is_mut } => insert_var(
                &SingleVarDecl {
                    name: alias.clone(),
                    is_mut: *is_mut,
                    enforced_type: None,
                },
                scope,
                idents,
                state,
            ),

            ObjPropSpreadingBinding::Deconstruct(span) => {
                insert_vars(&span.data, scope, idents, state)
            }
        }
    }

    fn insert_var(
        decl: &SingleVarDecl,
        scope: Option<&mut CheckerScope>,
        idents: &mut HashSet<String>,
        state: &mut State,
    ) -> CheckerResult {
        let SingleVarDecl {
            name,
            enforced_type,
            is_mut,
        } = decl;

        if let Some(enforced_type) = enforced_type {
            check_value_type(enforced_type, state)?;
        }

        if !idents.insert(name.data.clone()) {
            return Err(CheckerError::new(
                name.at,
                "duplicate identifier in declaration",
            ));
        }

        let scope = scope.unwrap_or_else(|| state.curr_scope_mut());

        let scope_id = scope.id;

        scope.vars.insert(
            name.data.clone(),
            DeclaredVar {
                decl_at: RuntimeCodeRange::Parsed(name.at),
                scope_id,
                is_mut: *is_mut,
            },
        );

        Ok(())
    }
}

fn check_expr_with(
    expr: &Expr,
    scope_id: AstScopeId,
    state: &mut State,
    fill_scope: impl FnOnce(&mut CheckerScope),
) -> CheckerResult {
    let mut scope = CheckerScope {
        id: scope_id,
        special_scope_type: None, // can be changed later on with "fill_scope"
        vars: HashMap::new(),
        fns: HashMap::new(),
        methods: HashMap::new(),
        cmd_aliases: HashMap::new(),
        type_aliases: HashMap::new(),
    };

    fill_scope(&mut scope);

    state.push_scope(scope);

    check_expr(expr, state)?;

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

fn check_expr_inner(inner: &Span<ExprInner>, state: &mut State) -> CheckerResult {
    let ExprInner { content, chainings } = &inner.data;

    check_expr_inner_content(content, state)?;

    for chaining in chainings {
        check_expr_inner_chaining(chaining, state)?;
    }

    Ok(())
}

fn check_expr_inner_chaining(chaining: &ExprInnerChaining, state: &mut State) -> CheckerResult {
    match chaining {
        ExprInnerChaining::PropAccess(prop_acc) => check_prop_access(prop_acc, state),
        ExprInnerChaining::MethodCall(method_call) => check_fn_call(method_call, state),
    }
}

fn check_expr_inner_content(content: &Span<ExprInnerContent>, state: &mut State) -> CheckerResult {
    match &content.data {
        ExprInnerContent::SingleOp {
            op,
            right,
            right_chainings,
        } => {
            check_single_op(op, state)?;
            check_expr_inner_content(right, state)?;

            for chaining in right_chainings {
                check_expr_inner_chaining(&chaining.data, state)?;
            }
        }

        ExprInnerContent::ParenExpr(expr) => {
            check_expr(expr, state)?;
        }

        ExprInnerContent::Value(value) => {
            check_value(value, state)?;
        }

        ExprInnerContent::FnAsValue(name) => {
            state.register_fn_usage(name)?;
        }

        ExprInnerContent::Ternary {
            cond,
            body,
            elsif,
            els,
        } => {
            check_expr(&cond.data, state)?;
            check_expr(body, state)?;

            for elsif in elsif {
                check_elsif_expr(elsif, state)?;
            }

            check_expr(els, state)?;
        }

        ExprInnerContent::Match { expr, cases, els } => {
            check_expr(expr, state)?;

            for MatchExprCase { matches, then } in cases {
                check_expr(&matches.data, state)?;
                check_expr(then, state)?;
            }

            check_expr(els, state)?;
        }

        ExprInnerContent::TypeMatch { expr, cases, els } => {
            check_expr(expr, state)?;

            for TypeMatchExprCase { matches, then } in cases {
                check_value_type(matches, state)?;
                check_expr(then, state)?;
            }

            check_expr(els, state)?;
        }

        ExprInnerContent::Try {
            try_expr,
            catch_var,
            catch_expr,
            catch_expr_scope_id,
        } => {
            check_expr(try_expr, state)?;

            check_expr_with(catch_expr, *catch_expr_scope_id, state, |scope| {
                if let Some(catch_var) = catch_var {
                    scope.vars.insert(
                        catch_var.data.clone(),
                        DeclaredVar {
                            decl_at: RuntimeCodeRange::Parsed(catch_var.at),
                            scope_id: scope.id,
                            is_mut: false,
                        },
                    );
                }
            })?;
        }

        ExprInnerContent::Throw(expr) => {
            check_expr(&expr.data, state)?;
        }

        ExprInnerContent::LoopContinue => {
            if !matches!(
                state.nearest_special_scope_type(),
                Some(SpecialScopeType::Loop)
            ) {
                return Err(CheckerError::new(
                    content.at,
                    "'continue' keyword can only be used inside a loop",
                ));
            }
        }

        ExprInnerContent::LoopBreak => {
            if !matches!(
                state.nearest_special_scope_type(),
                Some(SpecialScopeType::Loop)
            ) {
                return Err(CheckerError::new(
                    content.at,
                    "'break' keyword can only be used inside a loop",
                ));
            }
        }
    }

    Ok(())
}

fn check_elsif_expr(elsif_expr: &ElsIfExpr, state: &mut State) -> CheckerResult {
    let ElsIfExpr { body, cond } = elsif_expr;

    check_expr(body, state)?;
    check_expr(&cond.data, state)?;

    Ok(())
}

fn check_expr_op(expr_op: &ExprOp, state: &mut State) -> CheckerResult {
    match expr_op {
        ExprOp::DoubleOp { op: _, right_op } => check_expr_inner(right_op, state),
        ExprOp::TypeIs { right_op } => check_value_type(&right_op.data, state),
    }
}

fn check_single_op(single_op: &SingleOp, _: &mut State) -> CheckerResult {
    match single_op {
        SingleOp::Neg => Ok(()),
    }
}

fn check_prop_access(prop_acc: &PropAccess, state: &mut State) -> CheckerResult {
    let PropAccess {
        nature,
        nullable: _,
    } = prop_acc;

    check_prop_access_nature(nature, state)
}

fn check_prop_access_nature(nature: &Span<PropAccessNature>, state: &mut State) -> CheckerResult {
    match &nature.data {
        PropAccessNature::Key(key) => check_expr(&key.data, state),
        PropAccessNature::Prop(_) => Ok(()),
    }
}

fn check_value(value: &Value, state: &mut State) -> CheckerResult {
    match value {
        Value::Null => Ok(()),

        Value::Literal(lit) => check_literal_value(lit),

        Value::ComputedString(computed_string) => check_computed_string(computed_string, state),

        Value::Range(range) => check_range(range, state),

        Value::List(list) => check_list(list, state),

        Value::Map(members) => {
            for item in members {
                match item {
                    MapItem::Single { key, value } => {
                        check_map_key(&key.data, state)?;
                        check_expr(value, state)?;
                    }

                    MapItem::Spread(spread_value) => check_spread_value(&spread_value.data, state)?,
                }
            }

            Ok(())
        }

        Value::Struct(members) => {
            let mut explicit_fields = HashSet::with_capacity(members.len());

            for item in members {
                match item {
                    StructItem::Single { field, value } => {
                        if !explicit_fields.insert(&field.data) {
                            return Err(CheckerError::new(
                                field.at,
                                "duplicate identifier in struct",
                            ));
                        }

                        check_expr(value, state)?;
                    }

                    StructItem::Spread(spread_value) => {
                        check_spread_value(&spread_value.data, state)?;
                    }
                }
            }

            Ok(())
        }

        Value::Variable(var) => {
            state.register_var_usage(var)?;
            Ok(())
        }

        Value::FnCall(fn_call) => check_fn_call(fn_call, state),

        Value::CmdOutput(cmd_call) => check_cmd_capture(cmd_call, state),

        Value::CmdCall(cmd_call) => {
            state.register_cmd_call_value(cmd_call);
            check_cmd_call(cmd_call, state)
        }

        Value::FnAsValue(func_name) => {
            state.register_fn_usage(func_name)?;
            Ok(())
        }

        Value::Lambda(lambda) => check_function(lambda, state),
    }
}

fn check_literal_value(lit_value: &LiteralValue) -> CheckerResult {
    match lit_value {
        LiteralValue::Boolean(_) => Ok(()),
        LiteralValue::Integer(_) => Ok(()),
        LiteralValue::Float(_) => Ok(()),
        LiteralValue::String(_) => Ok(()),
    }
}

fn check_computed_string(computed_string: &ComputedString, state: &mut State) -> CheckerResult {
    let ComputedString { pieces } = computed_string;

    for piece in pieces {
        check_computed_string_piece(piece, state)?;
    }

    Ok(())
}

fn check_computed_string_piece(piece: &ComputedStringPiece, state: &mut State) -> CheckerResult {
    match &piece {
        ComputedStringPiece::Literal(_) => Ok(()),
        ComputedStringPiece::Escaped(_) => Ok(()),
        ComputedStringPiece::Variable(var) => {
            state.register_var_usage(var)?;
            Ok(())
        }
        ComputedStringPiece::Expr(expr) => check_expr(&expr.data, state),
        ComputedStringPiece::CmdOutput(capture) => check_cmd_capture(capture, state),
    }
}

fn check_range(range: &Range, state: &mut State) -> CheckerResult {
    let Range {
        from,
        to,
        include_last_value: _,
    } = range;

    check_range_bound(from, state)?;
    check_range_bound(to, state)?;

    Ok(())
}

fn check_range_bound(range_bound: &RangeBound, state: &mut State) -> CheckerResult {
    match range_bound {
        RangeBound::Literal(_) => Ok(()),
        RangeBound::Variable(var) => state.register_var_usage(var),
        RangeBound::Expr(expr) => check_expr(&expr.data, state),
    }
}

fn check_map_key(key: &MapKey, state: &mut State) -> CheckerResult {
    match key {
        MapKey::Raw(_) => Ok(()),
        MapKey::LiteralString(_) => Ok(()),
        MapKey::ComputedString(c_str) => check_computed_string(c_str, state),
        MapKey::Expr(expr) => check_expr(expr, state),
    }
}

fn check_list(items: &[ListItem], state: &mut State) -> CheckerResult {
    for item in items {
        match item {
            ListItem::Single(expr) => check_expr(expr, state)?,
            ListItem::Spread(spread_value) => check_spread_value(&spread_value.data, state)?,
        }
    }

    Ok(())
}

fn check_spread_value(spread_value: &SpreadValue, state: &mut State) -> CheckerResult {
    match spread_value {
        SpreadValue::Variable(var_name) => state.register_var_usage(var_name),
        SpreadValue::Expr(expr) => check_expr(expr, state),
    }
}

fn check_fn_call(fn_call: &Span<FnCall>, state: &mut State) -> CheckerResult {
    let FnCall {
        nature,
        name,
        call_args,
    } = &fn_call.data;

    match nature {
        FnCallNature::Variable => state.register_var_usage(name)?,
        FnCallNature::Method => state.register_method_usage(name)?,
        FnCallNature::NamedFunction => state.register_fn_usage(name)?,
    }

    for arg in &call_args.data {
        check_fn_call_arg(arg, state)?;
    }

    Ok(())
}

fn check_fn_call_arg(arg: &Span<FnArg>, state: &mut State) -> CheckerResult {
    match &arg.data {
        FnArg::Expr(expr) => check_expr(&expr.data, state),
        FnArg::Flag { name: _, value } => match value {
            Some(value) => check_expr(&value.data, state),
            None => Ok(()),
        },
    }
}

fn check_cmd_call(cmd_call: &Span<CmdCall>, state: &mut State) -> CheckerResult {
    let CmdCall { base, pipes } = &cmd_call.data;

    enum PrevCallType {
        Expr,
        ActualCall(CmdPathTargetType),
    }

    let mut prev_call_type = match &**base {
        CmdCallBase::Expr(expr) => {
            check_expr(&expr.data, state)?;
            PrevCallType::Expr
        }

        CmdCallBase::SingleCmdCall(call) => {
            let cmd_target_type = check_single_cmd_call(call, state)?;

            if let Some(redirects) = &call.data.redirects {
                check_cmd_redirect(
                    cmd_target_type,
                    redirects,
                    pipes.first().map(|pipe| pipe.pipe_type),
                )?;
            }

            PrevCallType::ActualCall(cmd_target_type)
        }
    };

    let mut pipes = pipes.iter().peekable();

    while let Some(CmdPipe { pipe_type, cmd }) = pipes.next() {
        let cmd_target_type = check_single_cmd_call(cmd, state)?;

        if let Some(redirects) = &cmd.data.redirects {
            check_cmd_redirect(
                cmd_target_type,
                redirects,
                pipes.peek().map(|pipe| pipe.pipe_type),
            )?;
        }

        match pipe_type.data {
            CmdPipeType::ValueOrStdout => {}

            // Ensure the piped call has an error output (STDERR)
            CmdPipeType::Stderr => match prev_call_type {
                PrevCallType::Expr => {
                    return Err(CheckerError::new(
                        pipe_type.at,
                        "expressions don't have an error output",
                    ));
                }

                PrevCallType::ActualCall(CmdPathTargetType::Function) => {
                    return Err(CheckerError::new(
                        pipe_type.at,
                        "functions don't have an error output",
                    ));
                }

                PrevCallType::ActualCall(CmdPathTargetType::ExternalCommand) => {
                    // OK
                }
            },
        }

        prev_call_type = PrevCallType::ActualCall(cmd_target_type);
    }

    Ok(())
}

fn check_cmd_redirect(
    cmd_target_type: CmdPathTargetType,
    redirects: &Span<CmdRedirects>,
    next_pipe: Option<Span<CmdPipeType>>,
) -> CheckerResult {
    match cmd_target_type {
        CmdPathTargetType::Function => {
            // TODO: allow stdout redirection (write string to file)
            return Err(CheckerError::new(
                redirects.at,
                "cannot redirect a function's output to a file",
            ));
        }

        CmdPathTargetType::ExternalCommand => {
            // Ensure that we're not redirecting a pipe that's going to be empty
            // (because it's being captured / ...)
            if let Some(next_pipe) = next_pipe {
                match next_pipe.data {
                    CmdPipeType::Stderr => match &redirects.data {
                        CmdRedirects::StdoutToFile(_) | CmdRedirects::StdoutToStderr => {}
                        CmdRedirects::StderrToFile(_)
                        | CmdRedirects::StderrToStdout
                        | CmdRedirects::StdoutAndStderrToFile(_)
                        | CmdRedirects::StdoutToFileAndStderrToFile {
                            path_for_stdout: _,
                            path_for_stderr: _,
                        } => {
                            return Err(CheckerError::new(
                                next_pipe.at,
                                "cannot pipe STDERR as it's being redirected elsewhere",
                            ));
                        }
                    },

                    CmdPipeType::ValueOrStdout => match &redirects.data {
                        CmdRedirects::StderrToFile(_) | CmdRedirects::StderrToStdout => {}
                        CmdRedirects::StdoutToFile(_)
                        | CmdRedirects::StdoutToStderr
                        | CmdRedirects::StdoutAndStderrToFile(_)
                        | CmdRedirects::StdoutToFileAndStderrToFile {
                            path_for_stdout: _,
                            path_for_stderr: _,
                        } => {
                            return Err(CheckerError::new(
                                next_pipe.at,
                                "cannot pipe STDOUT as it's being redirected elsewhere",
                            ));
                        }
                    },
                }
            }
        }
    }

    Ok(())
}

fn check_single_cmd_call(
    single_cmd_call: &Span<SingleCmdCall>,
    state: &mut State,
) -> CheckerResult<CmdPathTargetType> {
    let SingleCmdCall {
        env_vars,
        path,
        args,
        redirects,
    } = &single_cmd_call.data;

    for env_var in &env_vars.data {
        check_cmd_env_var(env_var, state)?;
    }

    let mut developed_aliases = vec![];

    let target_type = match &path.data {
        CmdPath::Raw(name) => check_if_cmd_or_fn_and_register(
            &name.forge_here(name.data.to_owned()),
            &mut developed_aliases,
            state,
        )?,

        CmdPath::External(path) => {
            match path {
                CmdExternalPath::RawString(r_str) => {
                    check_cmd_raw_string(&r_str.data, state)?;
                }
                CmdExternalPath::LiteralString(_) => {}
                CmdExternalPath::ComputedString(c_str) => {
                    check_computed_string(&c_str.data, state)?
                }
            }

            CmdPathTargetType::ExternalCommand
        }

        CmdPath::Method(name) => {
            if !state
                .scopes()
                .any(|scope| scope.methods.contains_key(&name.data))
            {
                return Err(CheckerError::new(
                    name.at,
                    format!("no method named '{}' found for any type", name.data),
                ));
            }

            state.register_method_usage(name)?;

            CmdPathTargetType::Function
        }
    };

    for arg in &args.data {
        check_cmd_arg(arg, state)?;
    }

    let is_function = match target_type {
        CmdPathTargetType::Function => true,
        CmdPathTargetType::ExternalCommand => false,
    };

    if is_function && let Some(env_var) = env_vars.data.first() {
        return Err(CheckerError::new(
            env_var.at,
            "environment variables are not supported for function calls",
        ));
    }

    if let Some(redirects) = redirects {
        match &redirects.data {
            CmdRedirects::StdoutToFile(span) => check_cmd_raw_string(&span.data, state)?,
            CmdRedirects::StderrToFile(span) => check_cmd_raw_string(&span.data, state)?,
            CmdRedirects::StderrToStdout => {}
            CmdRedirects::StdoutToStderr => {}
            CmdRedirects::StdoutAndStderrToFile(span) => check_cmd_raw_string(&span.data, state)?,
            CmdRedirects::StdoutToFileAndStderrToFile {
                path_for_stdout,
                path_for_stderr,
            } => {
                check_cmd_raw_string(&path_for_stdout.data, state)?;
                check_cmd_raw_string(&path_for_stderr.data, state)?;
            }
        }
    }

    state.register_developed_single_cmd_call(
        single_cmd_call,
        DevelopedSingleCmdCall {
            at: single_cmd_call.at,
            is_function,
            developed_aliases,
        },
    );

    Ok(target_type)
}

fn check_if_cmd_or_fn_and_register(
    name: &Span<String>,
    developed_aliases: &mut Vec<DevelopedCmdAliasCall>,
    state: &mut State,
) -> CheckerResult<CmdPathTargetType> {
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
                    decl_at: _,
                    scope_id: _,
                    content_at,
                    is_ready,
                } = cmd_alias;

                if !is_ready {
                    return Err(CheckerError::new(
                        name.at,
                        "cannot reference a command alias before its declaration",
                    ));
                }

                state.register_cmd_alias_usage(name)?;

                developed_aliases.push(DevelopedCmdAliasCall {
                    alias_called_at: name.clone(),
                    alias_content_at: content_at,
                });

                let alias_inner_call = state.get_developed_cmd_call_at(content_at);
                let alias_inner_call = alias_inner_call.as_ref().unwrap();

                developed_aliases.extend(alias_inner_call.developed_aliases.iter().cloned());

                Ok(if alias_inner_call.is_function {
                    CmdPathTargetType::Function
                } else {
                    CmdPathTargetType::ExternalCommand
                })
            }

            Target::Function => {
                state.register_fn_usage(name)?;
                Ok(CmdPathTargetType::Function)
            }
        },

        None => Ok(CmdPathTargetType::ExternalCommand),
    }
}

fn check_cmd_env_var(cmd_env_var: &Span<CmdEnvVar>, state: &mut State) -> CheckerResult {
    let CmdEnvVar { name: _, value } = &cmd_env_var.data;

    check_cmd_value_making_arg(&value.data, state)
}

fn check_cmd_arg(arg: &Span<CmdArg>, state: &mut State) -> CheckerResult {
    match &arg.data {
        CmdArg::ValueMaking(value_making) => check_cmd_value_making_arg(&value_making.data, state),

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

        CmdArg::Spread(arg) => check_spread_value(&arg.data, state),
    }
}

fn check_cmd_value_making_arg(arg: &CmdValueMakingArg, state: &mut State) -> CheckerResult {
    match arg {
        CmdValueMakingArg::LiteralValue(lit) => check_literal_value(lit),

        CmdValueMakingArg::Variable(name) => state.register_var_usage(name),

        CmdValueMakingArg::ComputedString(computed_string) => {
            check_computed_string(computed_string, state)
        }

        CmdValueMakingArg::List(items) => check_list(items, state),

        CmdValueMakingArg::InlineCmdCall(cmd_call) => {
            state.register_cmd_call_value(cmd_call);
            check_cmd_call(cmd_call, state)
        }

        CmdValueMakingArg::ParenExpr(expr) => check_expr(&expr.data, state),

        CmdValueMakingArg::CmdRawString(cc_str) => check_cmd_raw_string(&cc_str.data, state),

        CmdValueMakingArg::Lambda(lambda) => check_function(&lambda.data, state),
    }
}

fn check_cmd_raw_string(cc_str: &CmdRawString, state: &mut State) -> CheckerResult {
    let CmdRawString { pieces } = cc_str;

    for piece in pieces {
        match &piece.data {
            CmdRawStringPiece::Literal(_) => {}
            CmdRawStringPiece::Variable(var) => {
                state.register_var_usage(var)?;
            }
            CmdRawStringPiece::CmdCapturedOutput(cmd_call) => {
                check_cmd_capture(cmd_call, state)?;
            }
        }
    }

    Ok(())
}

fn check_cmd_capture(capture: &CmdOutputCapture, state: &mut State) -> CheckerResult {
    let CmdOutputCapture { capture, cmd_call } = capture;

    check_cmd_call(cmd_call, state)?;

    let final_redirects = match cmd_call.data.pipes.last() {
        Some(last_pipe) => last_pipe.cmd.data.redirects.as_ref(),
        None => match &*cmd_call.data.base {
            CmdCallBase::Expr(_) => None,
            CmdCallBase::SingleCmdCall(cmd) => cmd.data.redirects.as_ref(),
        },
    };

    if let Some(redirects) = final_redirects {
        match &capture.data {
            CmdCaptureType::Stdout => match &redirects.data {
                CmdRedirects::StderrToFile(_) | CmdRedirects::StderrToStdout => {}
                CmdRedirects::StdoutToFile(_)
                | CmdRedirects::StdoutToStderr
                | CmdRedirects::StdoutAndStderrToFile(_)
                | CmdRedirects::StdoutToFileAndStderrToFile {
                    path_for_stdout: _,
                    path_for_stderr: _,
                } => {
                    return Err(CheckerError::new(
                        capture.at,
                        "cannot capture STDOUT as it's being redirected elsewhere",
                    ));
                }
            },

            CmdCaptureType::Stderr => match &redirects.data {
                CmdRedirects::StdoutToFile(_) | CmdRedirects::StdoutToStderr => {}
                CmdRedirects::StderrToFile(_)
                | CmdRedirects::StderrToStdout
                | CmdRedirects::StdoutAndStderrToFile(_)
                | CmdRedirects::StdoutToFileAndStderrToFile {
                    path_for_stdout: _,
                    path_for_stderr: _,
                } => {
                    return Err(CheckerError::new(
                        capture.at,
                        "cannot capture STDERR as it's being redirected elsewhere",
                    ));
                }
            },
        }
    }

    Ok(())
}

fn check_function(func: &Function, state: &mut State) -> CheckerResult {
    let Function { signature, body } = func;

    state.register_function_body(body.clone());

    let checked_args = check_fn_signature(&Span::ate(signature.at, &signature.data), state)?;

    let mut vars = HashMap::with_capacity(checked_args.len());

    for checked_arg in checked_args {
        let CheckedFnArg {
            name_at,
            var_name,
            is_rest: _,
            has_explicit_type: _,
        } = checked_arg;

        let dup = vars.insert(
            var_name,
            DeclaredVar {
                decl_at: RuntimeCodeRange::Parsed(name_at),
                scope_id: body.data.scope_id,
                is_mut: false,
            },
        );

        assert!(dup.is_none());
    }

    state.prepare_deps(body.data.scope_id);

    check_block_with(&body.data, state, |scope: &mut CheckerScope, _| {
        scope.special_scope_type = Some(SpecialScopeType::Function);

        for (name, var) in vars {
            scope.vars.insert(name, var);
        }

        Ok(())
    })
}

fn check_fn_arg(arg: &FnSignatureArg, state: &mut State) -> CheckerResult<CheckedFnArg> {
    let (name, name_at, alt_var_name, is_rest, typ) = match arg {
        FnSignatureArg::Positional(FnSignaturePositionalArg {
            name,
            is_optional: _,
            typ,
        }) => {
            if let Some(typ) = typ {
                check_value_type(typ, state)?;
            }

            (name.data.clone(), name.at, None, false, typ.as_ref())
        }

        FnSignatureArg::PresenceFlag(FnSignaturePresenceFlagArg { names }) => match names {
            FnSignatureFlagArgNames::ShortFlag(short) => {
                (short.data.to_string(), short.at, None, false, None)
            }

            FnSignatureFlagArgNames::LongFlag(long) => (
                long.data.clone(),
                long.at,
                Some(long_flag_var_name(&long.data)),
                false,
                None,
            ),

            FnSignatureFlagArgNames::LongAndShortFlag { long, short: _ } => (
                long.data.clone(),
                long.at,
                Some(long_flag_var_name(&long.data)),
                false,
                None,
            ),
        },

        FnSignatureArg::NormalFlag(FnSignatureNormalFlagArg {
            names,
            is_optional: _,
            typ,
        }) => {
            check_value_type(typ, state)?;

            match names {
                FnSignatureFlagArgNames::ShortFlag(short) => {
                    (short.data.to_string(), short.at, None, false, Some(typ))
                }

                FnSignatureFlagArgNames::LongFlag(long) => (
                    long.data.clone(),
                    long.at,
                    Some(long_flag_var_name(&long.data)),
                    false,
                    Some(typ),
                ),

                FnSignatureFlagArgNames::LongAndShortFlag { long, short: _ } => (
                    long.data.clone(),
                    long.at,
                    Some(long_flag_var_name(&long.data)),
                    false,
                    Some(typ),
                ),
            }
        }

        FnSignatureArg::Rest(FnSignatureRestArg { name, typ }) => {
            if let Some(typ) = typ {
                check_value_type(&typ.data, state)?;

                if !matches!(
                    typ.data,
                    ValueType::Single(|SingleValueType::UntypedList| SingleValueType::TypedList(_),),
                ) {
                    return Err(CheckerError::new(
                        typ.at.parsed_range().unwrap(),
                        format!(
                            "rest types, when provided, must be either 'list' or 'list[...]', found: {}",
                            typ.data.display_inline()
                        ),
                    ));
                }
            }

            (
                name.data.clone(),
                name.at,
                None,
                true,
                typ.as_ref().map(|typ| &typ.data),
            )
        }
    };

    Ok(CheckedFnArg {
        var_name: alt_var_name.unwrap_or_else(|| name.clone()),
        name_at: match name_at {
            RuntimeCodeRange::Parsed(at) => at,
            RuntimeCodeRange::Internal(_) => unreachable!(),
        },
        is_rest,
        has_explicit_type: typ.is_some(),
    })
}

#[derive(Clone)]
struct CheckedFnArg {
    name_at: InputRange,
    var_name: String,
    is_rest: bool,
    has_explicit_type: bool,
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
        ValueType::Single(typ) => check_single_value_type(typ, state)?,
        ValueType::Union(types) => {
            for typ in types {
                check_single_value_type(typ, state)?;
            }
        }
    }

    Ok(())
}

fn check_single_value_type(value_type: &SingleValueType, state: &mut State) -> CheckerResult {
    match value_type {
        SingleValueType::Any
        | SingleValueType::Void
        | SingleValueType::Null
        | SingleValueType::Bool
        | SingleValueType::Int
        | SingleValueType::Float
        | SingleValueType::String
        | SingleValueType::StringLiteral(_)
        | SingleValueType::DateTime
        | SingleValueType::Instant
        | SingleValueType::Duration
        | SingleValueType::Regex
        | SingleValueType::Range
        | SingleValueType::Error
        | SingleValueType::CmdCall
        | SingleValueType::CmdArg
        | SingleValueType::UntypedList
        | SingleValueType::UntypedMap
        | SingleValueType::UntypedStruct => Ok(()),

        SingleValueType::TypedList(inner) | SingleValueType::TypedMap(inner) => {
            check_value_type(inner, state)
        }

        SingleValueType::TypedStruct(members) => {
            let mut names = HashSet::new();

            for member in members {
                let StructTypeMember {
                    name,
                    optional: _,
                    typ,
                } = member;

                if !names.insert(&name.data) {
                    return Err(CheckerError::new(
                        name.at.parsed_range().unwrap(),
                        "duplicate member in struct",
                    ));
                }

                check_value_type(typ, state)?;
            }

            Ok(())
        }

        SingleValueType::Function(signature) => match signature.as_parsed() {
            Some(eaten) => check_fn_signature(&eaten, state).map(|_| ()),
            None => Ok(()),
        },

        SingleValueType::TypeAlias(name) => state.register_type_alias_usage(name),
    }
}

fn check_fn_signature(
    signature: &Span<&FnSignature>,
    state: &mut State,
) -> CheckerResult<Vec<CheckedFnArg>> {
    state.register_function_signature(signature.map(FnSignature::clone));

    let FnSignature { args, ret_type } = &signature.data;

    assert!(args.as_parsed().is_some());

    let mut used_idents = HashSet::new();

    let mut rest_arg = None::<CheckedFnArg>;

    let mut checked_args = Vec::with_capacity(args.data.len());

    let mut had_optional = false;

    for (i, arg) in args.data.iter().enumerate() {
        let checked_arg = check_fn_arg(arg, state)?;

        let CheckedFnArg {
            var_name,
            name_at,
            is_rest,
            has_explicit_type: _,
        } = &checked_arg;

        if let Some(rest_arg) = &rest_arg
            && !rest_arg.has_explicit_type
        {
            return Err(CheckerError::new(
                rest_arg.name_at,
                "a rest argument must the last of the function unless it has an explicit type",
            ));
        }

        if let FnSignatureArg::Positional(FnSignaturePositionalArg {
            name,
            is_optional,
            typ: _,
        }) = arg
        {
            if rest_arg.is_some() {
                return Err(CheckerError::new(
                    name.at.parsed_range().unwrap(),
                    "no positional argument can follow a rest argument",
                ));
            }

            if *is_optional {
                had_optional = true;
            } else if had_optional {
                return Err(CheckerError::new(
                    name.at.parsed_range().unwrap(),
                    "cannot have a non-optional positional argument after an optional one",
                ));
            } else if i > 0 && name.data == "self" {
                return Err(CheckerError::new(
                    name.at.parsed_range().unwrap(),
                    "cannot have non-first 'self' argument as it is reserved for methods",
                ));
            }
        }

        if *is_rest {
            rest_arg = Some(checked_arg.clone());
        }

        if !used_idents.insert(var_name.clone()) {
            return Err(CheckerError::new(
                *name_at,
                "duplicate argument name in function",
            ));
        }

        checked_args.push(checked_arg);
    }

    if let Some(ret_type) = ret_type {
        check_value_type(&ret_type.data, state)?;
    }

    Ok(checked_args)
}

#[derive(Debug, Clone, Copy)]
enum CmdPathTargetType {
    Function,
    ExternalCommand,
}
