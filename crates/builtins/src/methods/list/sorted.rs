use reshell_runtime::{
    errors::ExecInfoType,
    gc::{GcCell, GcReadOnlyCell},
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::functions::{DateTimeValue, DurationValue};

crate::define_internal_fn!(
    //
    // sort a list's items into a new list
    //
    "sorted",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self()
    )

     -> Some(UntypedListType::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args { list }, _, ctx| {
        let list = list.read_promise_no_write();

        macro_rules! try_sort_type {
            ($($typ: ty => $remap: expr),+) => {{
                $(
                    let typ = <$typ>::new_single_direct();

                    if let Ok(mut items) = list.iter().cloned().map(|item| SingleTyping::parse(&typ, item)).collect::<Result<Vec<_>, _>>() {
                        items.sort();

                        return Ok(Some(RuntimeValue::List(GcCell::new(
                            items.into_iter().map($remap).collect(),
                        ))));
                    }
                )+

                Err(
                    ctx.throw(at, "only lists containing items of a comparable type can be sorted")
                     $(.with_info(
                        ExecInfoType::Note,
                        format!(
                            "comparable type: {}",
                            <$typ>::new_single_direct()
                                .underlying_single_type()
                                .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            )
                        )
                    )+
                )
            }};
        }

        try_sort_type!(
            StringType => RuntimeValue::String,
            IntType => RuntimeValue::Int,
            CustomType<DurationValue> => |item| RuntimeValue::Custom(GcReadOnlyCell::new(item)),
            CustomType<DateTimeValue> => |item| RuntimeValue::Custom(GcReadOnlyCell::new(item))
        )
    })
}
