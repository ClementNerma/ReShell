//!
//! Function to call to generate completions
//!

use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{
    context::Context,
    errors::ExecResult,
    gc::GcCell,
    values::{LocatedValue, RuntimeValue},
};

use super::get_repl_config;
use crate::{
    declare_typed_fn_handler, declare_typed_struct_handler,
    helpers::{
        args::TypedValueParser,
        types::{DetachedListType, NullType, StringType, Union2Type},
    },
    utils::{INTERNAL_CODE_RANGE, call_fn_checked},
};

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

declare_typed_fn_handler!(
    /// Completer function's signature
    pub CompleterFn(line: DetachedListType<Union2Type<StringType, NullType>>) -> CompleterReturnType
);

/// Generate completions (used for the REPL)
pub fn generate_completions(
    cmd_pieces: &[Vec<CompletionStringSegment>],
    ctx: &mut Context,
) -> ExecResult<Option<Vec<GeneratedCompletion>>> {
    let Some(completer_fn) = get_repl_config(ctx)?.generate_completions else {
        return Ok(None);
    };

    let completion_args = vec![RuntimeValue::List(GcCell::new(
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

    let ret_val = call_fn_checked(
        &LocatedValue::new(INTERNAL_CODE_RANGE, RuntimeValue::Function(completer_fn)),
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
