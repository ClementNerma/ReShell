use std::{sync::Arc, time::Instant};

use crate::types::InstantValue;

crate::define_internal_fn!(
    "instant",

    ()

    -> CustomType<InstantValue>
);

fn run() -> Runner {
    Runner::new(|_, Args {}, _, _| {
        let value = InstantValue(Instant::now());

        Ok(Some(RuntimeValue::Custom(Arc::new(value))))
    })
}
