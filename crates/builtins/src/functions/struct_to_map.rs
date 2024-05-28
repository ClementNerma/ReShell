use reshell_runtime::gc::GcCell;

crate::define_internal_fn!(
    //
    // convert a struct to a map
    //

    "structToMap",

    (
        obj: RequiredArg<UntypedStructType> = Arg::positional("obj")
    )

    -> Some(UntypedMapType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|_, Args { obj }, ArgsAt { obj: obj_at }, _| {
        let entries = obj.read(obj_at).clone();

        Ok(Some(RuntimeValue::Map(GcCell::new(entries))))
    })
}
