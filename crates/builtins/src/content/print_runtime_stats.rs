use reshell_runtime::size::ComputableSize;

crate::define_internal_fn!(
    //
    // clone a value
    //

    "__print_runtime_stats",

    ()

    -> None
);

fn run() -> Runner {
    Runner::new(|_, Args {}, _, ctx| {
        let lines = [
            "========== Runtime stats ==========".to_string(),
            format!("=> Memory usage: {} bytes", ctx.compute_total_size()),
            "===================================".to_string(),
        ];

        println!("\n{}\n", lines.join("\n"));

        Ok(None)
    })
}
