use std::sync::Arc;

use regex::Regex;
use reshell_runtime::gc::GcReadOnlyCell;

use crate::{define_internal_fn, types::RegexValue};

define_internal_fn!(
    "regex",

    (
        pattern: RequiredArg<StringType> = Arg::positional("pattern")
    )

    -> CustomType<RegexValue>
);

fn run() -> Runner {
    Runner::new(|_, Args { pattern }, args_at, ctx| {
        let regex =
            Regex::new(&pattern).map_err(|err| ctx.throw(args_at.pattern, format!("{err}")))?;

        let regex =
            RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(RegexValue(Arc::new(regex)))));

        Ok(Some(regex))
    })
}
