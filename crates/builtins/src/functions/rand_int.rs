crate::define_internal_fn!(
    "randInt",

    ()

    -> Some(IntType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args {}, _, _| {
        let rand = rand::random::<i64>();
        Ok(Some(RuntimeValue::Int(rand)))
    })
}
