//!
//! Function to call to generate completions
//!

use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{context::Context, errors::ExecResult, gc::GcCell, values::RuntimeValue};

use crate::{
    declare_typed_fn_handler, declare_typed_struct_handler,
    helpers::{
        args::TypedValueParser,
        types::{DetachedListType, NullType, StringType, Union2Type},
    },
    utils::call_fn_checked,
};

pub static GEN_COMPLETIONS_VAR_NAME: &str = "generateCompletions";

pub enum CompletionStringSegment {
    VariableName(String),
    String(String),
}

declare_typed_struct_handler!(CompleterResultType {
    description: StringType,
    value: StringType
});

type CompleterReturnType = DetachedListType<CompleterResultType>;

/// Generated completion
pub struct GeneratedCompletion {
    pub description: String,
    pub value: String,
}

/// Generate completions (used for the REPL)
pub fn generate_completions(
    cmd_pieces: &[Vec<CompletionStringSegment>],
    ctx: &mut Context,
) -> ExecResult<Option<Vec<GeneratedCompletion>>> {
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

    let vec = vec![RuntimeValue::List(GcCell::new(
        cmd_pieces
            .iter()
            .map(|segments| {
                let mut joined = String::new();

                for segment in segments {
                    match segment {
                        CompletionStringSegment::String(string) => {
                            joined.push_str(string);
                        }

                        CompletionStringSegment::VariableName(_) => {
                            return RuntimeValue::Null;
                        }
                    }
                }

                RuntimeValue::String(joined)
            })
            .collect(),
    ))];

    let completion_args = vec;

    let ret_val = call_fn_checked(
        &completer_var_value,
        &CompleterFn::signature(),
        completion_args,
        ctx,
    )?;

    let ret_val = ret_val.ok_or_else(|| {
        ctx.error(
            RuntimeCodeRange::Internal("calling completion generation function"),
            "completion generation function did not return a value",
        )
    })?;

    let results = CompleterReturnType::parse(ret_val.value).map_err(|err| {
        ctx.error(
            ret_val.from,
            format!("type error in completion function's return value: {err}"),
        )
    })?;

    Ok(Some(
        results
            .into_iter()
            .map(
                |CompleterResultType { description, value }| GeneratedCompletion {
                    description,
                    value,
                },
            )
            .collect(),
    ))
}

declare_typed_fn_handler!(
    // Completer function's signature
    pub CompleterFn(line: DetachedListType<Union2Type<StringType, NullType>>) -> CompleterReturnType
);
