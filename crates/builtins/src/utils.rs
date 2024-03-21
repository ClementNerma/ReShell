use parsy::{CodeRange, Eaten, FileId, Location};
use reshell_parser::ast::{FnArg, FnArgNames, FnSignature, ValueType};

use reshell_runtime::{
    context::Context,
    errors::ExecResult,
    functions::{call_fn_value, FnCallResult, FnPossibleCallArgs},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    typechecker::check_fn_signature_equality,
    values::{LocatedValue, RuntimeFnSignature, RuntimeValue},
};

pub fn forge_internal_loc() -> CodeRange {
    CodeRange::new(
        Location {
            file_id: FileId::Internal,
            offset: 0,
        },
        0,
    )
}

pub fn forge_internal_token<T>(data: T) -> Eaten<T> {
    Eaten {
        at: forge_internal_loc(),
        data,
    }
}

pub fn forge_basic_fn_signature(
    args: Vec<(impl Into<String>, ValueType)>,
    ret_type: Option<ValueType>,
) -> FnSignature {
    FnSignature {
        args: forge_internal_token(
            args.into_iter()
                .map(|(name, typ)| FnArg {
                    names: FnArgNames::Positional(forge_internal_token(name.into())),
                    is_optional: false,
                    is_rest: false,
                    typ: Some(forge_internal_token(typ)),
                })
                .collect(),
        ),

        ret_type: ret_type.map(|ret_type| forge_internal_token(Box::new(ret_type))),
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

    let ret = call_fn_value(
        forge_internal_loc(),
        func,
        FnPossibleCallArgs::Direct {
            at: forge_internal_loc(),
            args: args
                .into_iter()
                .map(|arg| LocatedValue::new(arg, forge_internal_loc()))
                .collect(),
        },
        ctx,
    )?;

    match ret {
        FnCallResult::Success { returned } => Ok(returned),
        FnCallResult::Thrown(LocatedValue { value, from }) => Err(ctx.error(
            from,
            format!(
                "function call thrown a value: {}",
                value.render_uncolored(ctx, PrettyPrintOptions::inline())
            ),
        )),
    }
}
