use std::borrow::Cow;

use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{context::Context, errors::ExecResult, gc::GcCell, values::RuntimeValue};

use crate::{
    helper::{Typing, TypingDirectCreation},
    type_handlers::{DetachedListType, StringType, TypedStruct2Type},
    utils::{call_fn_checked, forge_basic_fn_signature},
};

pub static GEN_COMPLETIONS_VAR_NAME: &str = "generateCompletions";

/// Generate completions (used for the REPL)
pub fn generate_completions(
    cmd_pieces: &[Cow<str>],
    ctx: &mut Context,
) -> ExecResult<Option<Vec<(String, String)>>> {
    let completer_var = ctx
        .native_lib_scope_content()
        .vars
        .get(GEN_COMPLETIONS_VAR_NAME)
        .unwrap()
        .clone();

    let completer_var_value = completer_var.value.read(RuntimeCodeRange::Internal(
        "calling completions generation function",
    ));

    if matches!(completer_var_value.value, RuntimeValue::Null) {
        return Ok(None);
    }

    let ret_type = DetachedListType::new(TypedStruct2Type::new(
        ("raw_string", StringType::new_direct()),
        ("description", StringType::new_direct()),
    ));

    let expected_signature = forge_basic_fn_signature(
        vec![("line", StringType::direct_underlying_type())],
        Some(ret_type.underlying_type()),
    );

    let completion_args = vec![RuntimeValue::List(GcCell::new(
        cmd_pieces
            .iter()
            .map(|piece| RuntimeValue::String(piece.clone().into_owned()))
            .collect(),
    ))];

    let ret_val = call_fn_checked(
        &completer_var_value,
        &expected_signature,
        completion_args,
        ctx,
    )?;

    let ret_val = ret_val.ok_or_else(|| {
        ctx.error(
            RuntimeCodeRange::Internal("calling completion generation function"),
            "completion generation function did not return a value",
        )
    })?;

    let results = ret_type.parse(ret_val.value).map_err(|err| {
        ctx.error(
            ret_val.from,
            format!("type error in completion function's return value: {err}"),
        )
    })?;

    Ok(Some(results))
}
