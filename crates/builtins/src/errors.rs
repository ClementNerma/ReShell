use std::fmt::Display;

use reshell_parser::ast::RuntimeCodeRange;
use reshell_runtime::{context::Context, errors::ExecResult};

pub trait FallibleAtRuntime {
    type Ok;

    fn context(
        self,
        message: &str,
        at: impl Into<RuntimeCodeRange>,
        ctx: &Context,
    ) -> ExecResult<Self::Ok>;

    fn with_context(
        self,
        message: impl FnOnce() -> String,
        at: impl Into<RuntimeCodeRange>,
        ctx: &Context,
    ) -> ExecResult<Self::Ok>;
}

impl<T, E: Display> FallibleAtRuntime for Result<T, E> {
    type Ok = T;

    fn context(
        self,
        message: &str,
        at: impl Into<RuntimeCodeRange>,
        ctx: &Context,
    ) -> ExecResult<T> {
        self.map_err(|err| ctx.error(at, format!("{message}: {err}")))
    }

    fn with_context(
        self,
        message: impl FnOnce() -> String,
        at: impl Into<RuntimeCodeRange>,
        ctx: &Context,
    ) -> ExecResult<T> {
        self.map_err(|err| ctx.error(at, format!("{}: {err}", message())))
    }
}

impl<T> FallibleAtRuntime for Option<T> {
    type Ok = T;

    fn context(
        self,
        message: &str,
        at: impl Into<RuntimeCodeRange>,
        ctx: &Context,
    ) -> ExecResult<T> {
        self.ok_or_else(|| ctx.error(at, message.to_owned()))
    }

    fn with_context(
        self,
        message: impl FnOnce() -> String,
        at: impl Into<RuntimeCodeRange>,
        ctx: &Context,
    ) -> ExecResult<T> {
        self.ok_or_else(|| ctx.error(at, message()))
    }
}
