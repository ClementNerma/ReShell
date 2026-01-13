use crate::functions_set;

functions_set! {
    fn duration => {
        mod seconds;
        mod subsec_micros;
        mod subsec_millis;
        mod subsec_nanos;
    }
}
