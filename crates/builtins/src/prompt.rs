use std::collections::HashMap;

use reshell_runtime::{context::Context, errors::ExecResult, gc::GcCell, values::RuntimeValue};

use crate::{
    helper::{Typing, TypingDirectCreation},
    type_handlers::{
        BoolType, ExactIntType, IntType, NullableType, StringType, TypedStruct1Type,
        TypedStruct3Type, TypedStruct4Type,
    },
    utils::{call_fn_checked, forge_basic_fn_signature},
};

pub static GEN_PROMPT_VAR_NAME: &str = "generatePrompt";

/// Render the prompt (used for the REPL)
pub fn render_prompt(
    ctx: &mut Context,
    last_cmd_status: Option<LastCmdStatus>,
) -> ExecResult<Option<PromptRendering>> {
    let prompt_var = ctx
        .native_lib_scope_content()
        .vars
        .get(GEN_PROMPT_VAR_NAME)
        .unwrap()
        .clone();

    let prompt_var_value = prompt_var.value.read(prompt_var.name_at);

    if matches!(prompt_var_value.value, RuntimeValue::Null) {
        return Ok(None);
    }

    let ret_type = TypedStruct4Type::new(
        ("prompt_left", NullableType::<StringType>::new_direct()),
        ("prompt_right", NullableType::<StringType>::new_direct()),
        ("prompt_indicator", NullableType::<StringType>::new_direct()),
        (
            "prompt_multiline_indicator",
            NullableType::<StringType>::new_direct(),
        ),
    );

    let expected_signature = forge_basic_fn_signature(
        vec![(
            "prompt_data",
            TypedStruct1Type::new((
                "last_cmd_status",
                TypedStruct3Type::new(
                    ("success", BoolType),
                    ("exit_code", NullableType::<IntType>::new_direct()),
                    ("duration_ms", ExactIntType::<i64>::new_direct()),
                ),
            ))
            .underlying_type(),
        )],
        Some(ret_type.underlying_type()),
    );

    let last_cmd_status = match last_cmd_status {
        None => RuntimeValue::Null,
        Some(status) => {
            let LastCmdStatus {
                success,
                exit_code,
                duration_ms,
            } = status;

            RuntimeValue::Struct(GcCell::new(HashMap::from([
                ("success".to_string(), RuntimeValue::Bool(success)),
                (
                    "exit_code".to_string(),
                    match exit_code {
                        Some(code) => RuntimeValue::Int(code.into()),
                        None => RuntimeValue::Null,
                    },
                ),
                (
                    "duration_ms".to_string(),
                    RuntimeValue::Int(i64::try_from(duration_ms).unwrap_or(i64::MAX)),
                ),
            ])))
        }
    };

    let prompt_data = RuntimeValue::Struct(GcCell::new(HashMap::from([(
        "last_cmd_status".to_string(),
        last_cmd_status,
    )])));

    let ret_val = call_fn_checked(
        &prompt_var_value,
        &expected_signature,
        vec![prompt_data],
        ctx,
    )?;

    let ret_val = ret_val.ok_or_else(|| {
        ctx.error(
            prompt_var.name_at,
            "prompt generation function did not return a value",
        )
    })?;

    let (prompt_left, prompt_right, prompt_indicator, prompt_multiline_indicator) =
        ret_type.parse(ret_val.value).map_err(|err| {
            ctx.error(
                ret_val.from,
                format!("type error in prompt function's return value: {err}"),
            )
        })?;

    Ok(Some(PromptRendering {
        prompt_left,
        prompt_right,
        prompt_indicator,
        prompt_multiline_indicator,
    }))
}

#[derive(Debug)]
pub struct LastCmdStatus {
    pub success: bool,
    pub exit_code: Option<i32>,
    pub duration_ms: u128,
}

#[derive(Default)]
pub struct PromptRendering {
    pub prompt_left: Option<String>,
    pub prompt_right: Option<String>,
    pub prompt_indicator: Option<String>,
    pub prompt_multiline_indicator: Option<String>,
}
