use std::collections::HashMap;

use reshell_parser::ast::{FnArg, FnArgNames, FnSignature};

use reshell_runtime::{
    context::Context,
    errors::ExecResult,
    gc::GcCell,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::RuntimeValue,
};

use crate::{
    helper::{ArgTyping, ArgTypingDirectCreation},
    type_handlers::{
        BoolType, ExactIntType, IntType, NullType, StringType, TypedStruct1Type, TypedStruct3Type,
        TypedStruct4Type, Union2Type,
    },
    utils::{call_fn_checked, forge_basic_fn_signature},
};

pub static GEN_PROMPT_VAR_NAME: &str = "gen_prompt";

pub fn render_prompt(
    ctx: &mut Context,
    last_cmd_status: Option<LastCmdStatus>,
) -> ExecResult<Option<PromptRendering>> {
    let prompt_var = ctx
        .scopes()
        .get(&0)
        .unwrap()
        .content
        .vars
        .get(GEN_PROMPT_VAR_NAME)
        .unwrap()
        .clone();

    let prompt_var_value = prompt_var.value.read();
    let prompt_var_value = prompt_var_value.as_ref().unwrap();

    if matches!(prompt_var_value.value, RuntimeValue::Null) {
        return Ok(None);
    }

    let expected_signature = forge_basic_fn_signature(
        vec![(
            "prompt_data",
            TypedStruct1Type::new((
                "last_cmd_status",
                TypedStruct3Type::new(
                    ("success", BoolType),
                    ("exit_code", Union2Type::<IntType, NullType>::new_direct()),
                    ("duration_ms", ExactIntType::<i64>::new_direct()),
                ),
            ))
            .underlying_type(),
        )],
        Some(
            TypedStruct4Type::new(
                (
                    "prompt_left",
                    Union2Type::<StringType, NullType>::new_direct(),
                ),
                (
                    "prompt_right",
                    Union2Type::<StringType, NullType>::new_direct(),
                ),
                (
                    "prompt_indicator",
                    Union2Type::<StringType, NullType>::new_direct(),
                ),
                (
                    "prompt_multiline_indicator",
                    Union2Type::<StringType, NullType>::new_direct(),
                ),
            )
            .underlying_type(),
        ),
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
                    RuntimeValue::Int(duration_ms as i64),
                ),
            ])))
        }
    };

    let prompt_data = RuntimeValue::Struct(GcCell::new(HashMap::from([(
        "last_cmd_status".to_string(),
        last_cmd_status,
    )])));

    let ret_val = call_fn_checked(
        prompt_var_value,
        &expected_signature,
        vec![prompt_data],
        ctx,
    )?;

    let ret_val = ret_val.ok_or_else(|| {
        ctx.error(
            prompt_var.declared_at,
            "prompt generation function did not return a value",
        )
    })?;

    let RuntimeValue::Struct(rendering) = ret_val.value else {
        return Err(ctx.error(
            ret_val.from,
            format!(
                "expected the prompt generation function to return a struct, found a {}",
                ret_val
                    .value
                    .get_type()
                    .render_colored(ctx, PrettyPrintOptions::inline())
            ),
        ));
    };

    macro_rules! get_options {
        ($from: ident @ $from_at: expr => $($ident: ident),+) => {{
            let mut out = PromptRendering::default();

            $(
                out.$ident = match $from.read().get(stringify!($ident)) {
                    None => return Err(ctx.error(
                        $from_at,
                        format!("missing option {} for prompt generation", stringify!($ident))
                    )),

                    Some(value) => match value {
                        RuntimeValue::Null => None,
                        RuntimeValue::String(string) => Some(string.clone()),
                        value => return Err(ctx.error(
                            $from_at,
                            format!("expected option {} to be a string for prompt generation, found a {}", stringify!($ident), value.get_type().render_colored(ctx, PrettyPrintOptions::inline()))
                        ))
                    }
                };
            )+

            // TODO: reject unknown keys?

            out
        }};
    }

    Ok(Some(get_options!(
        rendering @ ret_val.from =>
        prompt_left,
        prompt_right,
        prompt_indicator,
        prompt_multiline_indicator
    )))
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
