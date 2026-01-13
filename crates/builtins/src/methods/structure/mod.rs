use crate::functions_set;

functions_set! {
    fn structure_methods => {
        mod fields;
        mod get;
        mod has;
        mod len;
        mod to_map;
        mod values;
    }
}
