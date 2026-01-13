use crate::{define_internal_fn, helpers::shared::ComparableValueType};

use super::max::ordered;

define_internal_fn!(
    "min",

    (
        a: RequiredArg<WithOriginalValue<ComparableValueType>> = Arg::positional("a"),
        b: RequiredArg<WithOriginalValue<ComparableValueType>> = Arg::positional("b")
    )

    -> ComparableValueType
);

fn run() -> Runner {
    Runner::new(|at, Args { a, b }, _, ctx| ordered(a, b, false, at, ctx).map(Some))
}
