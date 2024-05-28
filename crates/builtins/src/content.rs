use std::path::MAIN_SEPARATOR;

use reshell_runtime::{
    compat::{TargetFamily, PATH_VAR_SEP, TARGET_FAMILY},
    values::RuntimeValue,
};

use crate::{
    builder::{BuiltinVar, NativeLibDefinition, NativeLibParams},
    functions::native_functions,
    methods::native_methods,
    repl_fns::{on_dir_jump::ON_DIR_JUMP_VAR_NAME, prompt::GEN_PROMPT_VAR_NAME},
};

/// Generate definitions of the native library
pub fn define_native_lib(params: NativeLibParams) -> NativeLibDefinition {
    let NativeLibParams { home_dir } = params;

    NativeLibDefinition {
        functions: native_functions(),

        methods: native_methods(),

        vars: vec![
            // Prompt generation variable
            BuiltinVar {
                name: GEN_PROMPT_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
            },
            // Directory jump listener
            BuiltinVar {
                name: ON_DIR_JUMP_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
            },
            // Platform-specific PATH variable separator
            BuiltinVar {
                name: "PATH_VAR_SEP",
                is_mut: false,
                init_value: RuntimeValue::String(PATH_VAR_SEP.to_string()),
            },
            // Platform-specific path separator
            BuiltinVar {
                name: "PATH_SEP",
                is_mut: false,
                init_value: RuntimeValue::String(MAIN_SEPARATOR.to_string()),
            },
            // Path to the current user's home directory
            BuiltinVar {
                name: "HOME",
                is_mut: false,
                init_value: match home_dir.and_then(|dir| dir.to_str().map(str::to_owned)) {
                    Some(dir) => RuntimeValue::String(dir),
                    None => RuntimeValue::Null,
                },
            },
            // OS family
            BuiltinVar {
                name: "IS_PLATFORM_WINDOWS",
                is_mut: false,
                init_value: RuntimeValue::Bool(match TARGET_FAMILY {
                    TargetFamily::Windows => true,
                    TargetFamily::Unix => false,
                }),
            },
            BuiltinVar {
                name: "IS_PLATFORM_UNIX",
                is_mut: false,
                init_value: RuntimeValue::Bool(match TARGET_FAMILY {
                    TargetFamily::Windows => false,
                    TargetFamily::Unix => true,
                }),
            },
        ],
    }
}
