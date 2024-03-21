use reshell_parser::ast::{
    FnArg, FnArgNames, FnSignature, RuntimeCodeRange, RuntimeEaten, ValueType,
};

use reshell_runtime::{
    context::Context,
    errors::ExecResult,
    functions::{call_fn_value, FnPossibleCallArgs},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    typechecker::check_fn_signature_equality,
    values::{LocatedValue, RuntimeFnSignature, RuntimeValue},
};

pub fn forge_basic_fn_signature(
    args: Vec<(impl Into<String>, ValueType)>,
    ret_type: Option<ValueType>,
) -> FnSignature {
    FnSignature {
        args: RuntimeEaten::Internal(
            args.into_iter()
                .map(|(name, typ)| FnArg {
                    names: FnArgNames::Positional(RuntimeEaten::Internal(name.into())),
                    is_optional: false,
                    is_rest: false,
                    typ: Some(RuntimeEaten::Internal(typ)),
                })
                .collect(),
        ),

        ret_type: ret_type.map(|ret_type| RuntimeEaten::Internal(Box::new(ret_type))),
    }
}

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

    if !check_fn_signature_equality(signature, expected_signature, ctx)? {
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
        RuntimeCodeRange::Internal,
        func,
        FnPossibleCallArgs::Internal(
            args.into_iter()
                .map(|arg| LocatedValue::new(arg, RuntimeCodeRange::Internal))
                .collect(),
        ),
        ctx,
    )
}
