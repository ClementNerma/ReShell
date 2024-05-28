use std::{collections::HashMap, path::PathBuf};

use indexmap::IndexSet;
use reshell_parser::{
    ast::{FnSignature, RuntimeCodeRange, RuntimeEaten},
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

use super::{content::define_native_lib, helper::InternalFunction};

/// Parameters of the native library
pub struct NativeLibParams {
    pub home_dir: Option<PathBuf>,
}

/// Definition of the native library
pub struct NativeLibDefinition {
    pub functions: Vec<InternalFunction>,
    pub methods: Vec<InternalFunction>,
    pub vars: Vec<BuiltinVar>,
}

/// Definition a a native variable
pub struct BuiltinVar {
    pub name: &'static str,
    pub is_mut: bool,
    pub init_value: RuntimeValue,
}

/// Build the content of the native library
pub fn build_native_lib_content(params: NativeLibParams) -> ScopeContent {
    let NativeLibDefinition {
        functions,
        methods,
        vars,
    } = define_native_lib(params);

    ScopeContent {
        fns: functions
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
                        value: GcReadOnlyCell::new(RuntimeFnValue {
                            signature: RuntimeFnSignature::Owned(FnSignature {
                                args: RuntimeEaten::Internal(
                                    args,
                                    "native library's type generator",
                                ),
                                ret_type: ret_type.map(|ret_type| {
                                    RuntimeEaten::Internal(
                                        Box::new(ret_type),
                                        "native library's type generator",
                                    )
                                }),
                            }),
                            body: RuntimeFnBody::Internal(run),
                            parent_scopes: IndexSet::new(),
                            captured_deps: GcOnceCell::new_init(CapturedDependencies::default()),
                        }),
                    },
                )
            })
            .collect(),

        methods: methods
            .into_iter()
            .map(|func| {
                let InternalFunction {
                    name,
                    args,
                    method_on_type,
                    run,
                    ret_type,
                } = func;

                let on_type = method_on_type.unwrap();

                (
                    (name.to_owned(), on_type.clone()),
                    ScopeMethod {
                        decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                        applyable_type: on_type.clone(),
                        value: GcReadOnlyCell::new(RuntimeFnValue {
                            signature: RuntimeFnSignature::Owned(FnSignature {
                                args: RuntimeEaten::Internal(
                                    args,
                                    "native library's type generator",
                                ),
                                ret_type: ret_type.map(|ret_type| {
                                    RuntimeEaten::Internal(
                                        Box::new(ret_type),
                                        "native library's type generator",
                                    )
                                }),
                            }),
                            body: RuntimeFnBody::Internal(run),
                            parent_scopes: IndexSet::new(),
                            captured_deps: GcOnceCell::new_init(CapturedDependencies::default()),
                        }),
                    },
                )
            })
            .collect(),

        vars: vars
            .into_iter()
            .map(|var| {
                let BuiltinVar {
                    name,
                    is_mut,
                    init_value,
                } = var;

                (
                    name.to_owned(),
                    ScopeVar {
                        decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                        is_mut,
                        value: GcCell::new(LocatedValue::new(
                            init_value,
                            RuntimeCodeRange::Internal("native library's type generator"),
                        )),
                    },
                )
            })
            .collect(),

        cmd_aliases: HashMap::new(),
    }
}
