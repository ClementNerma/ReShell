//! This module contains the declaration of all builtin variables

pub mod repl;

use std::{ffi::OsStr, path::MAIN_SEPARATOR};

use reshell_parser::ast::{SingleValueType, ValueType};
use reshell_runtime::{
    compat::{TargetFamily, PATH_VAR_SEP, TARGET_FAMILY},
    gc::GcCell,
    values::RuntimeValue,
};

use self::repl::{ReplConfig, REPL_CONFIG_VAR_NAME};
use crate::{
    builder::{BuiltinVar, NativeLibParams},
    helpers::args::{TypedValueEncoder, TypedValueParser},
};

pub fn native_vars(params: NativeLibParams) -> Vec<BuiltinVar> {
    let NativeLibParams {
        home_dir,
        script_args,
    } = params;

    vec![
        // REPL configuration
        BuiltinVar {
            name: REPL_CONFIG_VAR_NAME,
            is_mut: true,
            init_value: ReplConfig::encode(ReplConfig {
                on_dir_jump: None,
                generate_prompt: None,
                generate_completions: None,
            }),
            enforced_type: ReplConfig::value_type(),
        },
        // Platform-specific PATH variable separator
        BuiltinVar {
            name: "PATH_VAR_SEP",
            is_mut: false,
            init_value: RuntimeValue::String(PATH_VAR_SEP.to_string()),
            enforced_type: ValueType::Single(SingleValueType::String),
        },
        // Platform-specific path separator
        BuiltinVar {
            name: "PATH_SEP",
            is_mut: false,
            init_value: RuntimeValue::String(MAIN_SEPARATOR.to_string()),
            enforced_type: ValueType::Single(SingleValueType::String),
        },
        // Path to the current user's home directory
        BuiltinVar {
            name: "HOME",
            is_mut: false,
            init_value: match home_dir.and_then(|dir| dir.to_str().map(str::to_owned)) {
                Some(dir) => RuntimeValue::String(dir),
                None => RuntimeValue::Null,
            },
            enforced_type: ValueType::Union(vec![SingleValueType::String, SingleValueType::Null]),
        },
        // Name of the current OS
        BuiltinVar {
            name: "OS_NAME",
            is_mut: false,
            init_value: RuntimeValue::String(std::env::consts::OS.to_owned()),
            enforced_type: ValueType::Single(SingleValueType::String),
        },
        // Name of the current OS family
        BuiltinVar {
            name: "OS_FAMILY_NAME",
            is_mut: false,
            init_value: RuntimeValue::String(std::env::consts::FAMILY.to_owned()),
            enforced_type: ValueType::Single(SingleValueType::String),
        },
        // Name of the machine's architecture
        BuiltinVar {
            name: "ARCH_NAME",
            is_mut: false,
            init_value: RuntimeValue::String(std::env::consts::ARCH.to_owned()),
            enforced_type: ValueType::Single(SingleValueType::String),
        },
        // OS family
        BuiltinVar {
            name: "IS_PLATFORM_WINDOWS",
            is_mut: false,
            init_value: RuntimeValue::Bool(match TARGET_FAMILY {
                TargetFamily::Windows => true,
                TargetFamily::Unix => false,
            }),
            enforced_type: ValueType::Single(SingleValueType::Bool),
        },
        BuiltinVar {
            name: "IS_PLATFORM_UNIX",
            is_mut: false,
            init_value: RuntimeValue::Bool(match TARGET_FAMILY {
                TargetFamily::Windows => false,
                TargetFamily::Unix => true,
            }),
            enforced_type: ValueType::Single(SingleValueType::Bool),
        },
        // Minimum valid integer
        BuiltinVar {
            name: "INT_MIN",
            is_mut: false,
            init_value: RuntimeValue::Int(i64::MIN),
            enforced_type: ValueType::Single(SingleValueType::Int),
        },
        // Maximum valid integer
        BuiltinVar {
            name: "INT_MAX",
            is_mut: false,
            init_value: RuntimeValue::Int(i64::MAX),
            enforced_type: ValueType::Single(SingleValueType::Int),
        },
        // Minimum valid floating-point number
        BuiltinVar {
            name: "FLOAT_MIN",
            is_mut: false,
            init_value: RuntimeValue::Float(f64::MIN),
            enforced_type: ValueType::Single(SingleValueType::Float),
        },
        // Maximum valid floating-point number
        BuiltinVar {
            name: "FLOAT_MAX",
            is_mut: false,
            init_value: RuntimeValue::Float(f64::MAX),
            enforced_type: ValueType::Single(SingleValueType::Float),
        },
        // Shell arguments
        BuiltinVar {
            name: "SHELL_ARGS",
            is_mut: false,
            init_value: RuntimeValue::List(GcCell::new(
                script_args
                    .into_iter()
                    .map(|str| RuntimeValue::String(OsStr::to_string_lossy(&str).into_owned()))
                    .collect(),
            )),
            enforced_type: ValueType::Union(vec![SingleValueType::TypedList(Box::new(
                ValueType::Single(SingleValueType::String),
            ))]),
        },
    ]
}
