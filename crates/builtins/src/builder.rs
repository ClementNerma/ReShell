use std::collections::HashMap;

use indexmap::IndexSet;
use reshell_parser::ast::FnSignature;

use reshell_runtime::{
    context::{ScopeContent, ScopeFn, ScopeVar},
    gc::{GcCell, GcReadOnlyCell},
    values::{
        CapturedDependencies, LocatedValue, RuntimeFnBody, RuntimeFnSignature, RuntimeFnValue,
        RuntimeValue,
    },
};

use super::{
    content::define_native_lib,
    helper::InternalFunction,
    utils::{forge_internal_loc, forge_internal_token},
};

pub struct NativeLibDefinition {
    pub functions: Vec<InternalFunction>,
    pub vars: Vec<BuiltinVar>,
}

pub struct BuiltinVar {
    pub name: &'static str,
    pub is_mut: bool,
    pub init_value: RuntimeValue,
}

pub fn build_native_lib_content() -> ScopeContent {
    let NativeLibDefinition { functions, vars } = define_native_lib();

    ScopeContent {
        fns: functions
            .into_iter()
            .map(
                |InternalFunction {
                     name,
                     args,
                     run,
                     ret_type,
                 }| {
                    (
                        name.to_owned(),
                        ScopeFn {
                            name_at: forge_internal_loc(),
                            value: GcReadOnlyCell::new(RuntimeFnValue {
                                signature: RuntimeFnSignature::Owned(FnSignature {
                                    args: forge_internal_token(args),
                                    ret_type: ret_type
                                        .map(|ret_type| forge_internal_token(Box::new(ret_type))),
                                }),
                                body: RuntimeFnBody::Internal(run),
                                parent_scopes: IndexSet::new(),
                                captured_deps: CapturedDependencies::default(),
                            }),
                        },
                    )
                },
            )
            .collect(),

        vars: vars
            .into_iter()
            .map(
                |BuiltinVar {
                     name,
                     is_mut,
                     init_value,
                 }| {
                    (
                        name.to_owned(),
                        ScopeVar {
                            name_at: forge_internal_loc(),
                            is_mut,
                            value: GcCell::new(Some(LocatedValue::new(
                                init_value,
                                forge_internal_loc(),
                            ))),
                        },
                    )
                },
            )
            .collect(),

        cmd_aliases: HashMap::new(),
    }
}
