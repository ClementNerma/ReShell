use crate::functions_set;

functions_set! {
    fn list_methods => {
        mod all;
        mod any;
        mod append;
        mod at;
        mod concat;
        mod contains;
        mod deduped;
        mod each;
        mod expect_at;
        mod filter;
        mod filter_map;
        mod find;
        mod flat_map;
        mod flattened;
        mod fold;
        mod is_empty;
        mod join;
        mod last;
        mod len;
        mod map;
        mod parallel_each;
        mod parallel_map;
        mod pop;
        mod prepend;
        mod push;
        mod reduce;
        mod remove_at;
        mod reversed;
        mod shuffled;
        mod slice;
        mod sorted;
        mod sorted_by_key;
        mod sum;
    }
}
