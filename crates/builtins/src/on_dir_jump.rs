use std::path::Path;

use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{context::Context, errors::ExecResult, values::RuntimeValue};

use crate::{
    helper::TypingDirectCreation,
    type_handlers::StringType,
    utils::{call_fn_checked, forge_basic_fn_signature},
};

pub static ON_DIR_JUMP_VAR_NAME: &str = "onDirectoryJump";

/// Render the prompt (used for the REPL)
pub fn trigger_directory_jump_event(ctx: &mut Context, new_current_dir: &Path) -> ExecResult<()> {
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
    }

    let expected_signature = forge_basic_fn_signature(
        vec![("new_current_dir", StringType::direct_underlying_type())],
        None,
    );

    call_fn_checked(
        &on_dir_jump_fn,
        &expected_signature,
        vec![RuntimeValue::String(
            // TODO: lossy?
            new_current_dir.to_string_lossy().into_owned(),
        )],
        ctx,
    )?;

    Ok(())
}
