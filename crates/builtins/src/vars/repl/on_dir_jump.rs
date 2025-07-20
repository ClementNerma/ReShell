//!
//! Function to trigger when the current directory changes
//!

use std::path::Path;

use reshell_runtime::{
    context::Context,
    errors::ExecResult,
    values::{LocatedValue, RuntimeValue},
};

use super::get_repl_config;
use crate::{
    declare_typed_fn_handler,
    helpers::{
        args::TypedValueParser,
        types::{StringType, VoidType},
    },
    utils::{INTERNAL_CODE_RANGE, call_fn_checked},
};

declare_typed_fn_handler!(
    /// Generate prompt rendering function's signature
    pub DirectoryJumpHandlerFn(new_current_dir: StringType) -> VoidType
);

/// Render the prompt (used for the REPL)
pub fn trigger_directory_jump_event(ctx: &mut Context, new_current_dir: &Path) -> ExecResult<()> {
    // Don't trigger for non-UTF-8 paths
    let Some(new_current_dir) = new_current_dir.to_str() else {
        return Ok(());
    };

    let Some(on_dir_jump) = get_repl_config(ctx)?.on_dir_jump else {
        return Ok(());
    };

    call_fn_checked(
        &LocatedValue::new(INTERNAL_CODE_RANGE, RuntimeValue::Function(on_dir_jump)),
        &DirectoryJumpHandlerFn::signature(),
        vec![RuntimeValue::String(new_current_dir.to_owned())],
        ctx,
    )?;

    Ok(())
}
