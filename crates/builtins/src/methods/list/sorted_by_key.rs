use std::{sync::Arc, time::Duration};

use jiff::Zoned;
use reshell_parser::ast::SingleValueType;
use reshell_prettify::PrettyPrintable;
use reshell_runtime::{errors::ExecError, gc::GcCell};

use crate::{
    declare_typed_fn_handler,
    helpers::shared::{ComparableType, ComparableValueType},
    utils::{call_fn_checked, expect_returned_value},
};

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

declare_typed_fn_handler!(KeyerFn(value: AnyType) -> ComparableValueType);

fn run() -> Runner {
    Runner::new(|_, Args { list, keyer }, args_at, ctx| {
        let keyer = LocatedValue::new(args_at.keyer, RuntimeValue::Function(keyer));
        let keyer_signature = KeyerFn::signature();

        let list = list.read(args_at.keyer);

        let Some(first) = list.first() else {
            return Ok(Some(RuntimeValue::List(GcCell::new(vec![]))));
        };

        /// Represents the type of keys that are being used to sort the list.
        ///
        /// As a sortable list must be uniform, this enum is used to track the
        /// type of the first key, and then check that all subsequent keys are
        /// of the same type.
        ///
        /// The name "keys" is related to the function's name, `sortedByKey`.
        enum SortingKeys {
            Strings(Vec<(usize, String)>),
            Integers(Vec<(usize, i64)>),
            Durations(Vec<(usize, Duration)>),
            DateTimes(Vec<(usize, Arc<Zoned>)>),
        }

        let mut get_item_key = |item: RuntimeValue| {
            let ret_val = call_fn_checked(&keyer, &keyer_signature, vec![item], ctx)?;
            expect_returned_value::<_, ComparableValueType>(ret_val, args_at.keyer, ctx)
        };

        let mut keys = match get_item_key(first.clone())? {
            ComparableType::String(first) => SortingKeys::Strings(vec![(0, first)]),
            ComparableType::Int(first) => SortingKeys::Integers(vec![(0, first)]),
            ComparableType::Duration(first) => SortingKeys::Durations(vec![(0, first)]),
            ComparableType::DateTime(first) => SortingKeys::DateTimes(vec![(0, first)]),
        };

        fn gen_err(
            keys: &SortingKeys,
            (i, key): (usize, <ComparableValueType as TypedValueParser>::Parsed),
            keyer_fn_at: RuntimeCodeRange,
            ctx: &mut Context,
        ) -> ExecError {
            let first_item_type = match &keys {
                SortingKeys::Strings(_) => SingleValueType::String,
                SortingKeys::Integers(_) => SingleValueType::Int,
                SortingKeys::DateTimes(_) => SingleValueType::DateTime,
                SortingKeys::Durations(_) => SingleValueType::Duration,
            };

            ctx.hard_error(
                keyer_fn_at,
                format!(
                    "first call to this function returned a {}, but {i} call(s) later it returned a {}",
                    first_item_type.display_inline(),
                    key.original_value_type().display_inline()
                )
            )
        }

        for (i, value) in list.iter().enumerate().skip(1) {
            let key = get_item_key(value.clone())?;

            match &mut keys {
                SortingKeys::Strings(vec) => match key {
                    ComparableType::String(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, (i, key), args_at.keyer, ctx)),
                },

                SortingKeys::Integers(vec) => match key {
                    ComparableType::Int(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, (i, key), args_at.keyer, ctx)),
                },

                SortingKeys::Durations(vec) => match key {
                    ComparableType::Duration(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, (i, key), args_at.keyer, ctx)),
                },

                SortingKeys::DateTimes(vec) => match key {
                    ComparableType::DateTime(value) => {
                        vec.push((i, value));
                    }
                    _ => return Err(gen_err(&keys, (i, key), args_at.keyer, ctx)),
                },
            }
        }

        let sorted_values: Vec<RuntimeValue> = match keys {
            SortingKeys::Strings(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }

            SortingKeys::Integers(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }

            SortingKeys::Durations(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }

            SortingKeys::DateTimes(mut vec) => {
                vec.sort_by(|(_, a), (_, b)| a.cmp(b));

                vec.iter()
                    .map(|(i, _)| list.get(*i).unwrap().clone())
                    .collect()
            }
        };

        Ok(Some(RuntimeValue::List(GcCell::new(sorted_values))))
    })
}
