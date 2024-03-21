mod command;
mod datetime;
mod duration;
mod instant;
mod list;
mod map;
mod progress_bar;
mod regex;
mod spread;
mod string;

use crate::helper::InternalFunction;

/// List all native methods
pub fn native_methods() -> Vec<InternalFunction> {
    vec![
        self::command::succeeds::build_fn(),
        self::datetime::to_string::build_fn(),
        self::duration::seconds::build_fn(),
        self::duration::subsec_micros::build_fn(),
        self::duration::subsec_millis::build_fn(),
        self::duration::subsec_nanos::build_fn(),
        self::instant::elapsed::build_fn(),
        self::list::append::build_fn(),
        self::list::at::build_fn(),
        self::list::contains::build_fn(),
        self::list::filter::build_fn(),
        self::list::fold::build_fn(),
        self::list::for_each::build_fn(),
        self::list::get::build_fn(),
        self::list::is_empty::build_fn(),
        self::list::join::build_fn(),
        self::list::len::build_fn(),
        self::list::map::build_fn(),
        self::list::pop::build_fn(),
        self::list::prepend::build_fn(),
        self::list::reduce::build_fn(),
        self::list::remove_at::build_fn(),
        self::list::reverse::build_fn(),
        self::list::slice::build_fn(),
        self::list::sort::build_fn(),
        self::map::is_empty::build_fn(),
        self::map::keys::build_fn(),
        self::map::values::build_fn(),
        self::progress_bar::clear::build_fn(),
        self::progress_bar::inc::build_fn(),
        self::regex::captures::build_fn(),
        self::regex::matches::build_fn(),
        self::spread::is_empty::build_fn(),
        self::string::chars::build_fn(),
        self::string::contains::build_fn(),
        self::string::ends_with::build_fn(),
        self::string::find_str::build_fn(),
        self::string::is_empty::build_fn(),
        self::string::len::build_fn(),
        self::string::lines::build_fn(),
        self::string::parse_float::build_fn(),
        self::string::parse_int::build_fn(),
        self::string::reverse::build_fn(),
        self::string::replace::build_fn(),
        self::string::split::build_fn(),
        self::string::starts_with::build_fn(),
        self::string::substr::build_fn(),
        self::string::to_lowercase::build_fn(),
        self::string::to_uppercase::build_fn(),
        self::string::trim::build_fn(),
    ]
}
