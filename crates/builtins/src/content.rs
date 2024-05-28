use std::path::MAIN_SEPARATOR;

use reshell_parser::ast::SingleValueType;
use reshell_runtime::{
    compat::{TargetFamily, PATH_VAR_SEP, TARGET_FAMILY},
    values::RuntimeValue,
};

use crate::{
    builder::{BuiltinVar, NativeLibDefinition, NativeLibParams},
    functions::native_functions,
    methods::native_methods,
    repl_fns::{
        completer::{completer_signature, GEN_COMPLETIONS_VAR_NAME},
        on_dir_jump::{dir_jump_handler_signature, ON_DIR_JUMP_VAR_NAME},
        prompt::{prompt_renderer_signature, GEN_PROMPT_VAR_NAME},
    },
};

/// Generate definitions of the native library
pub fn define_native_lib(params: NativeLibParams) -> NativeLibDefinition {
    let NativeLibParams { home_dir } = params;

    NativeLibDefinition {
        functions: native_functions(),

        methods: native_methods(),

        vars: vec![
            // Prompt generation function
            BuiltinVar {
                name: GEN_PROMPT_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
                enforced_type: vec![
                    SingleValueType::Function(internal_runtime_eaten(prompt_renderer_signature())),
                    SingleValueType::Null,
                ],
            },
            // Completion generation function
            BuiltinVar {
                name: GEN_COMPLETIONS_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
                enforced_type: vec![
                    SingleValueType::Function(internal_runtime_eaten(completer_signature())),
                    SingleValueType::Null,
                ],
            },
            // Directory jump listener
            BuiltinVar {
                name: ON_DIR_JUMP_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
                enforced_type: vec![
                    SingleValueType::Function(internal_runtime_eaten(dir_jump_handler_signature())),
                    SingleValueType::Null,
                ],
            },
            // Platform-specific PATH variable separator
            BuiltinVar {
                name: "PATH_VAR_SEP",
                is_mut: false,
                init_value: RuntimeValue::String(PATH_VAR_SEP.to_string()),
                enforced_type: vec![SingleValueType::String],
            },
            // Platform-specific path separator
            BuiltinVar {
                name: "PATH_SEP",
                is_mut: false,
                init_value: RuntimeValue::String(MAIN_SEPARATOR.to_string()),
                enforced_type: vec![SingleValueType::String],
            },
            // Path to the current user's home directory
            BuiltinVar {
                name: "HOME",
                is_mut: false,
                init_value: match home_dir.and_then(|dir| dir.to_str().map(str::to_owned)) {
                    Some(dir) => RuntimeValue::String(dir),
                    None => RuntimeValue::Null,
                },
                enforced_type: vec![SingleValueType::String, SingleValueType::Null],
            },
            // OS family
            BuiltinVar {
                name: "IS_PLATFORM_WINDOWS",
                is_mut: false,
                init_value: RuntimeValue::Bool(match TARGET_FAMILY {
                    TargetFamily::Windows => true,
                    TargetFamily::Unix => false,
                }),
                enforced_type: vec![SingleValueType::Bool],
            },
            BuiltinVar {
                name: "IS_PLATFORM_UNIX",
                is_mut: false,
                init_value: RuntimeValue::Bool(match TARGET_FAMILY {
                    TargetFamily::Windows => false,
                    TargetFamily::Unix => true,
                }),
                enforced_type: vec![SingleValueType::Bool],
            },
        ],
    }
}
