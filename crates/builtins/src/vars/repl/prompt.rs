//!
//! Function to call to generate the REPL's prompt
//!

use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{
    context::Context,
    errors::ExecResult,
    values::{LocatedValue, RuntimeValue},
};

use super::get_repl_config;
use crate::{
    declare_typed_fn_handler, declare_typed_struct_handler,
    helpers::{
        args::{TypedValueEncoder, TypedValueParser},
        types::{BoolType, ExactIntType, NullableType, StringType},
    },
    utils::{INTERNAL_CODE_RANGE, call_fn_checked},
};

declare_typed_fn_handler!(
    pub PromptRendererFn(prompt_data: PromptData) -> PromptRendering
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
        #[allow(dead_code)] last_cmd_status: NullableType<LastCmdStatus>
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
    let Some(prompt_renderer) = get_repl_config(ctx).generate_prompt else {
        return Ok(None);
    };

    let ret_val = call_fn_checked(
        &LocatedValue::new(INTERNAL_CODE_RANGE, RuntimeValue::Function(prompt_renderer)),
        &PromptRendererFn::signature(),
        vec![PromptData::encode(PromptData { last_cmd_status })],
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
