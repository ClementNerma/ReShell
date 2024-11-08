use crate::{
    declare_typed_fn_handler,
    utils::{call_fn_checked, expect_returned_value, forge_basic_fn_signature},
};

crate::define_internal_fn!(
    //
    // map over a list
    //

    "fold",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        init: RequiredArg<AnyType> = Arg::positional("init"),
        folder: RequiredArg<FolderFn> = Arg::positional("folder")
    )

    -> Some(AnyType::value_type())
);

declare_typed_fn_handler!(FolderFn => forge_basic_fn_signature(
    vec![
        ("acc", AnyType::value_type()),
        ("value", ExactIntType::<usize>::value_type()),
    ],
    Some(NullableType::<AnyType>::value_type()),
));

fn run() -> Runner {
    Runner::new(|_, Args { list, init, folder }, args_at, ctx| {
        let folder = LocatedValue::new(args_at.folder, RuntimeValue::Function(folder));

        let list = list.read(args_at.folder);

        let mut folded = init.clone();

        for value in list.iter() {
            folded = call_fn_checked(
                &folder,
                &FolderFn::signature(),
                vec![folded.clone(), value.clone()],
                ctx,
            )
            .and_then(|ret| expect_returned_value::<_, AnyType>(ret, args_at.folder, ctx))?;
        }

        Ok(Some(folded))
    })
}
