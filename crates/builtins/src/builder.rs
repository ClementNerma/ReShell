use std::{collections::HashMap, path::PathBuf};

use indexmap::IndexSet;
use reshell_parser::{
    ast::{FnSignature, RuntimeEaten, SingleValueType, ValueType},
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

use crate::helpers::fns::InternalFunction;

use super::content::define_native_lib;

/// Create a [`RuntimeEaten`] data with internal location
pub fn internal_runtime_eaten<T>(data: T) -> RuntimeEaten<T> {
    RuntimeEaten::Internal(data, "native library's builder")
}

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
    pub enforced_type: Vec<SingleValueType>,
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
                        name_at: internal_runtime_eaten(()).at(),
                        value: GcReadOnlyCell::new(RuntimeFnValue {
                            is_method: false,

                            signature: RuntimeFnSignature::Owned(FnSignature {
                                args: internal_runtime_eaten(args),
                                ret_type: ret_type
                                    .map(|ret_type| internal_runtime_eaten(Box::new(ret_type))),
                            }),
                            body: RuntimeFnBody::Internal(run),
                            parent_scopes: IndexSet::new(),
                            captured_deps: GcOnceCell::new_init(CapturedDependencies::default()),
                        }),
                    },
                )
            })
            .collect(),

        methods: methods.into_iter().fold(HashMap::new(), |mut map, func| {
            let InternalFunction {
                name,
                args,
                method_on_type,
                run,
                ret_type,
            } = func;

            let on_type = method_on_type.unwrap();

            map.entry(name.to_owned()).or_default().push(ScopeMethod {
                name_at: internal_runtime_eaten(()).at(),
                decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                on_type: GcReadOnlyCell::new(on_type),
                value: GcReadOnlyCell::new(RuntimeFnValue {
                    is_method: true,

                    signature: RuntimeFnSignature::Owned(FnSignature {
                        args: internal_runtime_eaten(args),
                        ret_type: ret_type
                            .map(|ret_type| internal_runtime_eaten(Box::new(ret_type))),
                    }),
                    body: RuntimeFnBody::Internal(run),
                    parent_scopes: IndexSet::new(),
                    captured_deps: GcOnceCell::new_init(CapturedDependencies::default()),
                }),
            });

            map
        }),

        vars: vars
            .into_iter()
            .map(|var| {
                let BuiltinVar {
                    name,
                    is_mut,
                    init_value,
                    mut enforced_type,
                } = var;

                (
                    name.to_owned(),
                    ScopeVar {
                        name_at: internal_runtime_eaten(()).at(),
                        decl_scope_id: NATIVE_LIB_AST_SCOPE_ID,
                        is_mut,
                        enforced_type: match enforced_type.len() {
                            0 => None,
                            1 => Some(ValueType::Single(internal_runtime_eaten(
                                enforced_type.remove(0),
                            ))),
                            _ => Some(ValueType::Union(
                                enforced_type
                                    .into_iter()
                                    .map(internal_runtime_eaten)
                                    .collect(),
                            )),
                        },
                        value: GcCell::new(LocatedValue::new(
                            init_value,
                            internal_runtime_eaten(()).at(),
                        )),
                    },
                )
            })
            .collect(),

        cmd_aliases: HashMap::new(),
    }
}
