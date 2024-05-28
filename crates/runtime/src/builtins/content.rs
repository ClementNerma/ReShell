use std::collections::HashMap;

use indexmap::IndexSet;
use reshell_parser::ast::FnSignature;

use crate::{
    builtins::builder::{Arg, IntType, OptionalArg, RequiredArg},
    context::{ScopeContent, ScopeFn, ScopeVar},
    define_internal_fn,
    gc::{GcCell, GcReadOnlyCell},
    values::{CapturedDependencies, LocatedValue, RuntimeFnBody, RuntimeFnValue, RuntimeValue},
};

use super::{
    builder::InternalFunction,
    prompt::GEN_PROMPT_VAR_NAME,
    utils::{forge_internal_loc, forge_internal_token},
};

pub fn build_native_lib_content() -> ScopeContent {
    let functions = [(
        "exit",
        define_internal_fn!(
            Args [ArgsAt] ( code: OptionalArg<IntType> => Arg::positional("code") ),
            |at, Args { code }, ArgsAt { code: code_at }, ctx| {
                let code = code
                    .map(|code|
                        u8::try_from(code)
                            .map_err(|_| ctx.error(code_at.unwrap(), format!("code must be in 0..255, got {code}")))
                    )
                    .transpose()?;

                Err(ctx.exit(at, code))
            }
        ),
    )];

    let vars = [(GEN_PROMPT_VAR_NAME, false, RuntimeValue::Null)];

    ScopeContent {
        fns: functions
            .into_iter()
            .map(|(name, InternalFunction { args, run })| {
                (
                    name.to_owned(),
                    ScopeFn {
                        declared_at: forge_internal_loc(),
                        value: GcReadOnlyCell::new(RuntimeFnValue {
                            signature: FnSignature {
                                args: forge_internal_token(args),
                                ret_type: None, // TODO
                            },
                            body: RuntimeFnBody::Internal(run),
                            parent_scopes: IndexSet::new(),
                            captured_deps: CapturedDependencies::new(),
                        }),
                    },
                )
            })
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
