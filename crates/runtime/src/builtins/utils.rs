use parsy::{CodeRange, Eaten, FileId, Location};
use reshell_parser::ast::FnSignature;

use crate::{
    context::Context,
    errors::ExecResult,
    functions::{call_fn_value, FnCallResult, FnPossibleCallArgs},
    pretty::{PrettyPrintOptions, PrettyPrintable},
    typechecker::check_fn_equality,
    values::{LocatedValue, RuntimeValue},
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

    if !check_fn_equality(&func.signature, expected_signature, ctx)? {
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
