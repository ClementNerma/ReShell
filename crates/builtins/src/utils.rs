//! This module contains various utilities to force internal spans, call function values or extract returned values

use reshell_checker::typechecking::check_if_fn_signature_fits_another;
use reshell_parser::ast::{
    FnCallNature, FnSignature, FnSignatureArg, FnSignaturePositionalArg, RuntimeCodeRange,
    RuntimeSpan, ValueType,
};
use reshell_prettify::PrettyPrintable;
use reshell_runtime::{
    cmd::{CmdArgResult, SingleCmdArgResult},
    context::Context,
    errors::ExecResult,
    functions::{FnCallInfos, FnPossibleCallArgs, call_fn_value},
    values::{LocatedValue, RuntimeFnSignature, RuntimeValue},
};

use crate::helpers::args::TypedValueParser;

/// A [`RuntimeCodeRange`] with an internal location
pub static INTERNAL_CODE_RANGE: RuntimeCodeRange =
    RuntimeCodeRange::Internal("native library's builder");

/// Create a [`RuntimeSpan`] data with internal location
pub fn internal_runtime_span<T>(data: T) -> RuntimeSpan<T> {
    RuntimeSpan::internal("native library's builder", data)
}

/// Forge a basic function signature (only positional arguments, no optionality, no flags, no rest)
pub fn forge_basic_fn_signature(
    args: Vec<(&'static str, ValueType)>,
    ret_type: Option<ValueType>,
) -> FnSignature {
    FnSignature {
        args: internal_runtime_span(
            args.into_iter()
                .map(|(name, typ)| {
                    FnSignatureArg::Positional(FnSignaturePositionalArg {
                        name: internal_runtime_span(name.into()),
                        is_optional: false,
                        typ: Some(typ),
                    })
                })
                .collect(),
        ),

        ret_type: ret_type.map(|ret_type| internal_runtime_span(Box::new(ret_type))),
    }
}

/// Call a function and perform all required checks to ensure
/// both the arguments and the return type are valid
///
/// The `loc_val` parameter is used to indicate where the function value comes from.
/// If the provided value is not a Function, an error will be returned.
///
/// This function returns the typechecked value returned by the called function (if any).
pub fn call_fn_checked(
    loc_val: &LocatedValue,
    expected_signature: &FnSignature,
    args: Vec<RuntimeValue>,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    call_fn_checked_with_parsed_args(
        loc_val,
        expected_signature,
        FnPossibleCallArgs::Internal {
            args: args
                .into_iter()
                .map(|arg| {
                    CmdArgResult::Single(SingleCmdArgResult::Basic(LocatedValue::new(
                        loc_val.from,
                        arg,
                    )))
                })
                .collect(),
            at: "<internal function caller>",
        },
        ctx,
    )
}

pub fn call_fn_checked_with_parsed_args(
    loc_val: &LocatedValue,
    expected_signature: &FnSignature,
    args: FnPossibleCallArgs,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    let func = match &loc_val.value {
        RuntimeValue::Function(func) => func,
        value => {
            return Err(ctx.hard_error(
                loc_val.from,
                format!(
                    "type mismatch: expected a {}, found a {}",
                    expected_signature.display_inline(),
                    value.compute_type().display_inline()
                ),
            ));
        }
    };

    let signature = match &func.signature {
        RuntimeFnSignature::Shared(shared) => &shared.data,
        RuntimeFnSignature::Owned(owned) => owned,
    };

    if !check_if_fn_signature_fits_another(signature, expected_signature, ctx.type_alias_store()) {
        return Err(ctx.hard_error(
            loc_val.from,
            format!(
                "type mismatch: expected a {}, found a {}",
                expected_signature.display_inline(),
                loc_val.value.compute_type().display_inline()
            ),
        ));
    }

    call_fn_value(
        loc_val.from,
        func,
        FnCallInfos {
            nature: FnCallNature::NamedFunction,
            piped: None,
            args,
        },
        ctx,
    )
}

/// Parse a function's optional return value
///
/// This function uses a [`Typing`] parser to handle the provided value
pub fn expect_returned_value<T, P: TypedValueParser<Parsed = T>>(
    value: Option<LocatedValue>,
    at: RuntimeCodeRange,
    ctx: &Context,
) -> ExecResult<T> {
    let loc_val = value.ok_or_else(|| ctx.hard_error(at, "function did not return a value"))?;

    P::parse(loc_val.value)
        .map_err(|err| ctx.hard_error(at, format!("function returned wrong value type: {err}")))
}

/// Define runtime function modules
#[macro_export]
macro_rules! functions_set {
    (fn $fn_name: ident => { $(mod $mod_name: ident;)+ }) => {
        $( mod $mod_name; )+

        pub fn $fn_name() -> [$crate::helpers::fns::InternalFunction; $crate::functions_set!(@count $($mod_name)+)] {
            [ $( self::$mod_name::build_fn() ),+ ]
        }
    };

    (@count) => (0usize);
    (@count $x:tt $($xs:tt)* ) => (1usize + $crate::functions_set!(@count $($xs)*));

}
