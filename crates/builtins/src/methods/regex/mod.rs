use crate::functions_set;

functions_set! {
    fn regex_methods => {
        mod capture;
        mod matches;
        mod replace;
    }
}
