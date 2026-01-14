use std::time::Instant;

crate::define_internal_fn!(
    "instant",

    ()

    -> InstantType
);

fn run() -> Runner {
    Runner::new(|_, Args {}, _, _| Ok(Some(RuntimeValue::Instant(Instant::now()))))
}
