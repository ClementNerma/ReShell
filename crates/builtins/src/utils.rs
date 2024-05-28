use reshell_parser::ast::{FnArg, FnSignature, RuntimeCodeRange, RuntimeEaten, ValueType};

use reshell_runtime::{
    cmd::{CmdArgResult, CmdSingleArgResult},
    context::Context,
    errors::ExecResult,
    functions::{call_fn_value, FnPossibleCallArgs},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    typechecker::check_fn_signature_equality,
    values::{LocatedValue, RuntimeFnSignature, RuntimeValue},
};

use crate::helper::Typing;

/// Forge a basic function signature (only positional arguments, no optionality, no flags, no rest)
pub fn forge_basic_fn_signature(
    args: Vec<(impl Into<String>, ValueType)>,
    ret_type: Option<ValueType>,
) -> FnSignature {
    FnSignature {
        args: RuntimeEaten::Internal(
            args.into_iter()
                .map(|(name, typ)| FnArg::Positional {
                    name: RuntimeEaten::Internal(name.into(), "native library's type generator"),
                    is_optional: false,
                    typ: Some(RuntimeEaten::Internal(
                        typ,
                        "native library's type generator",
                    )),
                })
                .collect(),
            "native library's type generator",
        ),

        ret_type: ret_type.map(|ret_type| {
            RuntimeEaten::Internal(Box::new(ret_type), "native library's type generator")
        }),
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
    let func = match &loc_val.value {
        RuntimeValue::Function(func) => func,
        value => {
            return Err(ctx.error(
                loc_val.from,
                format!(
                    "type mismatch: expected a {}, found a {}",
                    expected_signature.render_colored(ctx, PrettyPrintOptions::inline()),
                    value
                        .get_type()
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            ))
        }
    };

    let signature = match &func.signature {
        RuntimeFnSignature::Shared(shared) => &shared.data,
        RuntimeFnSignature::Owned(owned) => owned,
    };

    if !check_fn_signature_equality(signature, expected_signature, ctx) {
        return Err(ctx.error(
            loc_val.from,
            format!(
                "type mismatch: expected a {}, found a {}",
                expected_signature.render_colored(ctx, PrettyPrintOptions::inline()),
                loc_val
                    .value
                    .get_type()
                    .render_colored(ctx, PrettyPrintOptions::inline())
            ),
        ));
    }

    call_fn_value(
        loc_val.from,
        func,
        FnPossibleCallArgs::Internal(
            args.into_iter()
                .map(|arg| {
                    CmdArgResult::Single(CmdSingleArgResult::Basic(LocatedValue::new(
                        arg,
                        loc_val.from,
                    )))
                })
                .collect(),
        ),
        ctx,
    )
}

/// Parse a function's optional return value
///
/// This function uses a [`Typing`] parser to handle the provided value
pub fn expect_returned_value<T>(
    value: Option<LocatedValue>,
    at: RuntimeCodeRange,
    type_parser: impl Typing<Parsed = T>,
    ctx: &Context,
) -> ExecResult<T> {
    let loc_val = value.unwrap_or_else(|| {
        ctx.panic(
            at,
            "got no return value (should have been caught before returning)",
        )
    });

    let typed = type_parser.parse(loc_val.value).unwrap_or_else(|err| {
        ctx.panic(
            at,
            format!("got invalid return value (should have been caught before returning): {err}"),
        )
    });

    Ok(typed)
}
