use crate::functions_set;

functions_set! {
    fn datetime_methods => {
        mod midnight;
        mod to_string;
        mod tomorrow;
        mod with_day;
        mod with_hours;
        mod with_minutes;
        mod with_month;
        mod with_seconds;
        mod with_year;
        mod yesterday;
    }
}
