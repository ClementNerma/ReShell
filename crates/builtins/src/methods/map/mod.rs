use crate::functions_set;

functions_set! {
    fn map_methods => {
        mod get;
        mod has;
        mod is_empty;
        mod keys;
        mod len;
        mod remove;
        mod to_struct;
        mod values;
    }
}
