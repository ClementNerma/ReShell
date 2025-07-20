//!
//! Set of functions that will be used and called by the REPL
//!

use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{context::Context, errors::ExecResult};

use self::{completer::CompleterFn, on_dir_jump::DirectoryJumpHandlerFn, prompt::PromptRendererFn};
use crate::{
    declare_typed_struct_handler,
    helpers::{
        args::TypedValueParser,
        types::{NonEncodableWrapper, NullableType},
    },
};

pub mod completer;
pub mod on_dir_jump;
pub mod prompt;

declare_typed_struct_handler!(
    pub ReplConfig {
        pub generate_prompt: NullableType<NonEncodableWrapper<PromptRendererFn>>,
        pub generate_completions:  NullableType<NonEncodableWrapper<CompleterFn>>,
        pub on_dir_jump: NullableType<NonEncodableWrapper<DirectoryJumpHandlerFn>>
    }
);

pub static REPL_CONFIG_VAR_NAME: &str = "reshell";

pub fn get_repl_config(ctx: &mut Context) -> ExecResult<ReplConfig> {
    let var = ctx
        .native_lib_scope_content()
        .vars
        .get(REPL_CONFIG_VAR_NAME)
        .unwrap();

    let internal_at = RuntimeCodeRange::Internal("REPL config fetcher");

    ReplConfig::parse(var.value.read(internal_at).value.clone()).map_err(|err| {
        ctx.error(
            internal_at,
            format!("REPL configuration variable does not have the expected shape: {err}"),
        )
    })
}
