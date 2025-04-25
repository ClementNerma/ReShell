//! This module builds the native library's content

use std::{collections::HashMap, ffi::OsString, path::PathBuf};

use indexmap::IndexSet;
use reshell_parser::{
    ast::{FnSignature, ValueType},
    scope::NATIVE_LIB_AST_SCOPE_ID,
};
use reshell_runtime::{
    context::{ScopeContent, ScopeFn, ScopeMethod, ScopeVar},
    gc::{GcCell, GcOnceCell, GcReadOnlyCell},
    values::{
        CapturedDependencies, LocatedValue, RuntimeFnBody, RuntimeFnSignature, RuntimeFnValue,
        RuntimeValue,
    },
};

use crate::{
    functions::native_functions,
    helpers::fns::InternalFunction,
    methods::native_methods,
    utils::{INTERNAL_CODE_RANGE, internal_runtime_span},
    vars::native_vars,
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
        fns: native_functions()
            .into_iter()
            .map(|func| {
                let InternalFunction {
                    name,
                    args,
                    method_on_type: _,
                    run,
                    ret_type,
                } = func;

                (
                    name.to_owned(),
                    ScopeFn {
                        decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                        name_at: INTERNAL_CODE_RANGE,
                        value: GcReadOnlyCell::new(RuntimeFnValue {
                            is_method: false,

                            signature: RuntimeFnSignature::Owned(FnSignature {
                                args: internal_runtime_span(args),
                                ret_type: Some(internal_runtime_span(Box::new(ret_type))),
                            }),
                            body: RuntimeFnBody::Internal(run),
                            parent_scopes: IndexSet::new(),
                            captured_deps: GcOnceCell::new_init(CapturedDependencies::default()),
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

                map.entry(name.to_owned()).or_default().push(ScopeMethod {
                    name_at: INTERNAL_CODE_RANGE,
                    decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                    on_type: GcReadOnlyCell::new(on_type),
                    value: GcReadOnlyCell::new(RuntimeFnValue {
                        is_method: true,

                        signature: RuntimeFnSignature::Owned(FnSignature {
                            args: internal_runtime_span(args),
                            ret_type: Some(internal_runtime_span(Box::new(ret_type))),
                        }),
                        body: RuntimeFnBody::Internal(run),
                        parent_scopes: IndexSet::new(),
                        captured_deps: GcOnceCell::new_init(CapturedDependencies::default()),
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
