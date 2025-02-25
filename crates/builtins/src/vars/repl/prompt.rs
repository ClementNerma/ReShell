//!
//! Function to call to generate the REPL's prompt
//!

use indexmap::IndexMap;
use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{context::Context, errors::ExecResult, gc::GcCell, values::RuntimeValue};

use crate::{
    declare_typed_fn_handler, declare_typed_struct_handler,
    helpers::{
        args::TypedValueParser,
        types::{BoolType, ExactIntType, NullableType, StringType},
    },
    utils::call_fn_checked,
};

pub static GEN_PROMPT_VAR_NAME: &str = "generatePrompt";

declare_typed_fn_handler!(
    pub PromptRenderer(prompt_data: PromptData) -> PromptRendering
);

declare_typed_struct_handler!(
    pub LastCmdStatus {
        pub success: BoolType,
        pub exit_code: NullableType<ExactIntType<u8>>,
        pub duration_ms: ExactIntType<i64>
    }
);

declare_typed_struct_handler!(
    pub PromptData {
        #[allow(dead_code)] last_cmd_status: LastCmdStatus
    }
);

declare_typed_struct_handler!(
    #[derive(Default)]
    pub PromptRendering {
        #[allow(dead_code)] pub prompt_left: NullableType<StringType>,
        #[allow(dead_code)] pub prompt_right: NullableType<StringType>,
        #[allow(dead_code)] pub prompt_indicator: NullableType<StringType>,
        #[allow(dead_code)] pub prompt_multiline_indicator: NullableType<StringType>
    }
);

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

    let prompt_var_value = prompt_var.value.read(RuntimeCodeRange::Internal(
        "calling prompt generation function",
    ));

    if matches!(prompt_var_value.value, RuntimeValue::Null) {
        return Ok(None);
    }

    let last_cmd_status = match last_cmd_status {
        None => RuntimeValue::Null,
        Some(status) => {
            let LastCmdStatus {
                success,
                exit_code,
                duration_ms,
            } = status;

            RuntimeValue::Struct(GcCell::new(IndexMap::from([
                ("success".to_string(), RuntimeValue::Bool(success)),
                (
                    "exitCode".to_string(),
                    match exit_code {
                        Some(code) => RuntimeValue::Int(code.into()),
                        None => RuntimeValue::Null,
                    },
                ),
                ("durationMs".to_string(), RuntimeValue::Int(duration_ms)),
            ])))
        }
    };

    let prompt_data = RuntimeValue::Struct(GcCell::new(IndexMap::from([(
        "lastCmdStatus".to_string(),
        last_cmd_status,
    )])));

    let ret_val = call_fn_checked(
        &prompt_var_value,
        &PromptRenderer::signature(),
        vec![prompt_data],
        ctx,
    )?;

    let ret_val = ret_val.ok_or_else(|| {
        ctx.error(
            RuntimeCodeRange::Internal("calling prompt generation function"),
            "prompt generation function did not return a value",
        )
    })?;

    PromptRendering::parse(ret_val.value)
        .map(Some)
        .map_err(|err| {
            ctx.error(
                ret_val.from,
                format!("type error in prompt function's return value: {err}"),
            )
        })
}
