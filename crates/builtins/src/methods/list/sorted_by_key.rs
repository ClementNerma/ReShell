use reshell_parser::ast::SingleValueType;
use reshell_runtime::{gc::GcCell, values::CustomValueType};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    declare_typed_fn_handler,
    functions::{DateTimeValue, DurationValue},
    utils::{call_fn_checked, expect_returned_value},
};

use super::sorted::ComparableValueType;

crate::define_internal_fn!(
    //
    // sort a list's items into a new list
    //
    "sortedByKey",

    (
        list: RequiredArg<UntypedListType> = Arg::method_self(),
        keyer: RequiredArg<KeyerFn> = Arg::positional("keyer")
    )

     -> UntypedListType
);

declare_typed_fn_handler!(KeyerFn(value: ComparableValueType) -> ComparableValueType);

fn run() -> Runner {
    Runner::new(|_, Args { list, keyer }, args_at, ctx| {
        let keyer = LocatedValue::new(args_at.keyer, RuntimeValue::Function(keyer));
        let keyer_signature = KeyerFn::signature();

        let list = list.read(args_at.keyer);

        let Some(first) = list.first() else {
            return Ok(Some(RuntimeValue::List(GcCell::new(vec![]))));
        };

        enum Keys {
            Strings(Vec<(usize, String)>),
            Integers(Vec<(usize, i64)>),
            Durations(Vec<(usize, Box<DurationValue>)>),
            DateTimes(Vec<(usize, Box<DateTimeValue>)>),
        }

        let mut get_item_key = |item: RuntimeValue| {
            let ret_val = call_fn_checked(&keyer, &keyer_signature, vec![item], ctx)?;
            expect_returned_value::<_, ComparableValueType>(ret_val, args_at.keyer, ctx)
        };

        let mut keys = match get_item_key(first.clone())? {
            Union4Result::A(first) => Keys::Strings(vec![(0, first)]),
            Union4Result::B(first) => Keys::Integers(vec![(0, first)]),
            Union4Result::C(first) => Keys::Durations(vec![(0, first)]),
            Union4Result::D(first) => Keys::DateTimes(vec![(0, first)]),
        };

        for (i, value) in list.iter().enumerate().skip(1) {
            let key = get_item_key(value.clone())?;

            let gen_err = |keys: &Keys,
                           key: <ComparableValueType as TypedValueParser>::Parsed,
                           ctx: &mut Context| {
                let first_item_type = match &keys {
                    Keys::Strings(_) => SingleValueType::String,
                    Keys::Integers(_) => SingleValueType::Int,
                    Keys::Durations(_) => SingleValueType::Custom(DurationValue::typename_static()),
                    Keys::DateTimes(_) => SingleValueType::Custom(DateTimeValue::typename_static()),
                };

                ctx.error(
                    args_at.keyer,
                    format!(
                        "first call to this function returned a {}, but {i} call(s) later it returned a {}",
                        first_item_type.render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline()),
                        key.from_value_type().render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline())
                    )
                )
            };

            match &mut keys {
                Keys::Strings(vec) => match key {
                    Union4Result::A(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, key, ctx)),
                },

                Keys::Integers(vec) => match key {
                    Union4Result::B(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, key, ctx)),
                },

                Keys::Durations(vec) => match key {
                    Union4Result::C(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, key, ctx)),
                },

                Keys::DateTimes(vec) => match key {
                    Union4Result::D(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, key, ctx)),
                },
            }
        }

        let sorted_values: Vec<RuntimeValue> = match keys {
            Keys::Strings(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }

            Keys::Integers(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }

            Keys::Durations(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }

            Keys::DateTimes(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }
        };

        Ok(Some(RuntimeValue::List(GcCell::new(sorted_values))))
    })
}
