use std::cmp::Ordering;

use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    define_internal_fn,
    helpers::shared::{ComparableType, ComparableValueType},
};

define_internal_fn!(
    "max",

    (
        a: RequiredArg<WithOriginalValue<ComparableValueType>> = Arg::positional("a"),
        b: RequiredArg<WithOriginalValue<ComparableValueType>> = Arg::positional("b")
    )

    -> ComparableValueType
);

fn run() -> Runner {
    Runner::new(|at, Args { a, b }, _, ctx| ordered(a, b, true, at, ctx).map(Some))
}

pub fn ordered(
    (a, a_orig): (ComparableType, RuntimeValue),
    (b, b_orig): (ComparableType, RuntimeValue),
    greater: bool,
    at: RuntimeCodeRange,
    ctx: &Context,
) -> ExecResult<RuntimeValue> {
    let ord = match (&a, &b) {
        (ComparableType::String(a), ComparableType::String(b)) => Ok(a.cmp(b)),
        (ComparableType::String(_), _) => Err(()),

        (ComparableType::Int(a), ComparableType::Int(b)) => Ok(a.cmp(b)),
        (ComparableType::Int(_), _) => Err(()),

        (ComparableType::Duration(a), ComparableType::Duration(b)) => Ok(a.cmp(b)),
        (ComparableType::Duration(_), _) => Err(()),

        (ComparableType::DateTime(a), ComparableType::DateTime(b)) => Ok(a.cmp(b)),
        (ComparableType::DateTime(_), _) => Err(()),
    };

    match ord {
        Ok(ord) => Ok(match ord {
            Ordering::Less => {
                if greater {
                    b_orig
                } else {
                    a_orig
                }
            }

            Ordering::Equal => a_orig,

            Ordering::Greater => {
                if greater {
                    a_orig
                } else {
                    b_orig
                }
            }
        }),

        Err(()) => Err(ctx.hard_error(
            at,
            format!(
                "cannot compare a {} and a {}",
                a.original_value_type()
                    .display(PrettyPrintOptions::inline()),
                b.original_value_type()
                    .display(PrettyPrintOptions::inline())
            ),
        )),
    }
}
