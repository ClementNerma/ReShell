crate::define_internal_fn!(
    "randInt",

    (
        low_bound: OptionalArg<IntType> = Arg::positional("lowBound"),
        high_bound: OptionalArg<IntType> = Arg::positional("highBound")
    )

    -> IntType
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             low_bound,
             high_bound,
         },
         _,
         _| {
            let num = match (low_bound, high_bound) {
                (Some(low_bound), Some(high_bound)) => rand::random_range(low_bound..=high_bound),
                (Some(low_bound), None) => rand::random_range(low_bound..=i64::MAX),
                (None, Some(high_bound)) => rand::random_range(i64::MIN..=high_bound),
                (None, None) => rand::random(),
            };

            Ok(Some(RuntimeValue::Int(num)))
        },
    )
}
