//!
//! Function to trigger when the current directory changes
//!

use std::path::Path;

use reshell_parser::ast::{FnSignature, RuntimeCodeRange};
use reshell_runtime::{context::Context, errors::ExecResult, values::RuntimeValue};

use crate::{
    helpers::{args::TypedValueParser, types::StringType},
    utils::{call_fn_checked, forge_basic_fn_signature},
};

pub static ON_DIR_JUMP_VAR_NAME: &str = "onDirectoryJump";

/// Render the prompt (used for the REPL)
pub fn trigger_directory_jump_event(ctx: &mut Context, new_current_dir: &Path) -> ExecResult<()> {
    // Don't trigger for non-UTF-8 paths
    let Some(new_current_dir) = new_current_dir.to_str() else {
        return Ok(());
    };

    let on_dir_jump = ctx
        .native_lib_scope_content()
        .vars
        .get(ON_DIR_JUMP_VAR_NAME)
        .unwrap()
        .clone();

    let on_dir_jump_fn = on_dir_jump.value.read(RuntimeCodeRange::Internal(
        "calling directory jump function",
    ));

    if matches!(on_dir_jump_fn.value, RuntimeValue::Null) {
        return Ok(());
    };

    call_fn_checked(
        &on_dir_jump_fn,
        &dir_jump_handler_signature(),
        vec![RuntimeValue::String(new_current_dir.to_owned())],
        ctx,
    )?;

    Ok(())
}

/// Generate prompt rendering function's signature
pub fn dir_jump_handler_signature() -> FnSignature {
    forge_basic_fn_signature(vec![("new_current_dir", StringType::value_type())], None)
}
