//! This module builds the native library's content

use std::{
    collections::HashMap,
    ffi::OsString,
    path::PathBuf,
    sync::{Arc, OnceLock},
};

use indexmap::IndexSet;
use reshell_parser::{
    NATIVE_LIB_AST_SCOPE_ID,
    ast::{FnSignature, ValueType},
};
use reshell_runtime::{
    context::{ScopeContent, ScopeFn, ScopeMethod, ScopeVar},
    gc::GcCell,
    values::{LocatedValue, RuntimeFnBody, RuntimeFnSignature, RuntimeFnValue, RuntimeValue},
};

use crate::{
    functions::native_functions, helpers::fns::InternalFunction, methods::native_methods,
    utils::INTERNAL_CODE_RANGE, vars::native_vars,
};

/// Parameters of the native library
pub struct NativeLibParams {
    pub home_dir: Option<PathBuf>,
    pub script_args: Vec<OsString>,
}

/// Definition a a native variable
pub struct BuiltinVar {
    pub name: &'static str,
    pub is_mut: bool,
    pub init_value: RuntimeValue,
    pub enforced_type: ValueType,
}

/// Build the content of the native library
pub fn build_native_lib_content(params: NativeLibParams) -> ScopeContent {
    ScopeContent {
        fns: <[InternalFunction; _]>::into_iter(native_functions())
            .map(|func| {
                let InternalFunction {
                    name,
                    args,
                    method_on_type: _,
                    run,
                    ret_type,
                } = func;

                // TODO: requires locking, which isn't ideal
                let captured_deps = OnceLock::new();
                captured_deps.set(ScopeContent::new()).unwrap();

                (
                    name.to_owned(),
                    ScopeFn {
                        decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                        name_at: INTERNAL_CODE_RANGE,
                        value: Arc::new(RuntimeFnValue {
                            is_method: false,

                            signature: RuntimeFnSignature::Owned(FnSignature {
                                args,
                                ret_type: Some(Box::new(ret_type)),
                            }),
                            body: RuntimeFnBody::Internal(run),
                            parent_scopes: IndexSet::new(),
                            captured_deps,
                        }),
                    },
                )
            })
            .collect(),

        methods: native_methods()
            .into_iter()
            .fold(HashMap::new(), |mut map, func| {
                let InternalFunction {
                    name,
                    args,
                    method_on_type,
                    run,
                    ret_type,
                } = func;

                let on_type = method_on_type.unwrap();

                // TODO: requires locking, which isn't ideal
                let captured_deps = OnceLock::new();
                captured_deps.set(ScopeContent::new()).unwrap();

                map.entry(name.to_owned()).or_default().push(ScopeMethod {
                    name_at: INTERNAL_CODE_RANGE,
                    decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                    on_type: Arc::new(on_type),
                    value: Arc::new(RuntimeFnValue {
                        is_method: true,

                        signature: RuntimeFnSignature::Owned(FnSignature {
                            args,
                            ret_type: Some(Box::new(ret_type)),
                        }),
                        body: RuntimeFnBody::Internal(run),
                        parent_scopes: IndexSet::new(),
                        captured_deps,
                    }),
                });

                map
            }),

        vars: native_vars(params)
            .into_iter()
            .map(|var| {
                let BuiltinVar {
                    name,
                    is_mut,
                    init_value,
                    enforced_type,
                } = var;

                (
                    name.to_owned(),
                    ScopeVar {
                        name_at: INTERNAL_CODE_RANGE,
                        decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                        is_mut,
                        enforced_type: Some(enforced_type),
                        value: GcCell::new(LocatedValue::new(INTERNAL_CODE_RANGE, init_value)),
                    },
                )
            })
            .collect(),

        cmd_aliases: HashMap::new(),
    }
}
