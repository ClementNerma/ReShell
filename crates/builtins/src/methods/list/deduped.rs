use std::{
    collections::HashSet,
    hash::{DefaultHasher, Hash, Hasher},
};

use reshell_runtime::gc::GcCell;

use crate::{declare_typed_union_handler, define_internal_fn};

define_internal_fn!(
    // Return a copy of a list with deduplicated entries

    "deduped",

    (
        list: RequiredArg<DeduplicateListType> = Arg::method_self()
    )

    -> DeduplicateListType
);

declare_typed_union_handler!(pub DeduplicateListType => enum DeduplicateList {
    String(DetachedListType<StringType>),
    Int(DetachedListType<IntType>)
});

fn run() -> Runner {
    Runner::new(|_, Args { list }, _, _| {
        let out = match list {
            DeduplicateList::String(strings) => list_from_dedupped(strings, RuntimeValue::String),
            DeduplicateList::Int(ints) => list_from_dedupped(ints, RuntimeValue::Int),
        };

        Ok(Some(out))
    })
}

fn list_from_dedupped<T: Eq + Hash>(
    mut values: Vec<T>,
    wrap: impl Fn(T) -> RuntimeValue,
) -> RuntimeValue {
    let mut found = HashSet::new();

    values.retain(|value| {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        found.insert(hasher.finish())
    });

    RuntimeValue::List(GcCell::new(values.into_iter().map(wrap).collect()))
}
