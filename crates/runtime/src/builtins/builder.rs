use std::collections::HashMap;

use indexmap::IndexSet;
use reshell_parser::ast::FnSignature;

use crate::{
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
    pub functions: Vec<(&'static str, InternalFunction)>,
    pub vars: Vec<(&'static str, bool, RuntimeValue)>,
}

pub fn build_native_lib_content() -> ScopeContent {
    let NativeLibDefinition { functions, vars } = define_native_lib();

    ScopeContent {
        fns: functions
            .into_iter()
            .map(
                |(
                    name,
                    InternalFunction {
                        args,
                        run,
                        ret_type,
                    },
                )| {
                    (
                        name.to_owned(),
                        ScopeFn {
                            declared_at: forge_internal_loc(),
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
            .map(|(name, is_mut, value)| {
                (
                    name.to_owned(),
                    ScopeVar {
                        declared_at: forge_internal_loc(),
                        is_mut,
                        value: GcCell::new(Some(LocatedValue::new(value, forge_internal_loc()))),
                    },
                )
            })
            .collect(),

        cmd_aliases: HashMap::new(),
    }
}
