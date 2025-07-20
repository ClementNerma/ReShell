//!
//! List of methods for the builtin types
//!

mod any;
mod cmdcall;
mod datetime;
mod duration;
mod instant;
mod list;
mod map;
mod progress_bar;
mod regex;
mod string;
mod stringifyable;
mod structure;

pub use stringifyable::to_string::{StringifyableType, stringify_value};

use crate::helpers::fns::InternalFunction;

/// List all native methods
pub fn native_methods() -> Vec<InternalFunction> {
    vec![
        //
        // All value types
        //
        self::any::inspect::build_fn(),
        self::any::to_json::build_fn(),
        self::any::transform::build_fn(),
        self::any::typename::build_fn(),
        //
        // Command calls
        //
        self::cmdcall::capture::build_fn(),
        self::cmdcall::succeeds::build_fn(),
        self::cmdcall::run::build_fn(),
        //
        // Date-times
        //
        self::datetime::to_string::build_fn(),
        self::datetime::tomorrow::build_fn(),
        self::datetime::with_day::build_fn(),
        self::datetime::with_hours::build_fn(),
        self::datetime::with_minutes::build_fn(),
        self::datetime::with_month::build_fn(),
        self::datetime::with_seconds::build_fn(),
        self::datetime::with_year::build_fn(),
        self::datetime::yesterday::build_fn(),
        //
        // Durations
        //
        self::duration::seconds::build_fn(),
        self::duration::subsec_micros::build_fn(),
        self::duration::subsec_millis::build_fn(),
        self::duration::subsec_nanos::build_fn(),
        //
        // Instants
        //
        self::instant::elapsed::build_fn(),
        //
        // Lists
        //
        self::list::any::build_fn(),
        self::list::all::build_fn(),
        self::list::append::build_fn(),
        self::list::at::build_fn(),
        self::list::concat::build_fn(),
        self::list::contains::build_fn(),
        self::list::each::build_fn(),
        self::list::filter::build_fn(),
        self::list::filter_map::build_fn(),
        self::list::find::build_fn(),
        self::list::flat_map::build_fn(),
        self::list::flattened::build_fn(),
        self::list::fold::build_fn(),
        self::list::is_empty::build_fn(),
        self::list::join::build_fn(),
        self::list::last::build_fn(),
        self::list::len::build_fn(),
        self::list::map::build_fn(),
        self::list::pop::build_fn(),
        self::list::prepend::build_fn(),
        self::list::push::build_fn(),
        self::list::reduce::build_fn(),
        self::list::remove_at::build_fn(),
        self::list::reversed::build_fn(),
        self::list::shuffled::build_fn(),
        self::list::slice::build_fn(),
        self::list::sum::build_fn(),
        self::list::sorted_by_key::build_fn(),
        self::list::sorted::build_fn(),
        //
        // Maps
        //
        self::map::has::build_fn(),
        self::map::is_empty::build_fn(),
        self::map::get::build_fn(),
        self::map::keys::build_fn(),
        self::map::len::build_fn(),
        self::map::remove::build_fn(),
        self::map::to_struct::build_fn(),
        self::map::values::build_fn(),
        //
        // Progress bars
        //
        self::progress_bar::clear::build_fn(),
        self::progress_bar::finish::build_fn(),
        self::progress_bar::inc::build_fn(),
        //
        // Regexes
        //
        self::regex::capture::build_fn(),
        self::regex::matches::build_fn(),
        self::regex::replace::build_fn(),
        //
        // Strings
        //
        self::string::chars::build_fn(),
        self::string::contains::build_fn(),
        self::string::ends_with::build_fn(),
        self::string::find_str::build_fn(),
        self::string::is_empty::build_fn(),
        self::string::len::build_fn(),
        self::string::lines::build_fn(),
        self::string::pad_end::build_fn(),
        self::string::pad_start::build_fn(),
        self::string::parse_float::build_fn(),
        self::string::parse_json::build_fn(),
        self::string::parse_int::build_fn(),
        self::string::parse_toml::build_fn(),
        self::string::reversed::build_fn(),
        self::string::repeat::build_fn(),
        self::string::replace::build_fn(),
        self::string::split::build_fn(),
        self::string::starts_with::build_fn(),
        self::string::substr::build_fn(),
        self::string::to_lowercase::build_fn(),
        self::string::to_uppercase::build_fn(),
        self::string::trim_end::build_fn(),
        self::string::trim_start::build_fn(),
        self::string::trim::build_fn(),
        //
        // Stringifyables (union type)
        //
        self::stringifyable::to_string::build_fn(),
        //
        // Structs
        //
        self::structure::fields::build_fn(),
        self::structure::get::build_fn(),
        self::structure::has::build_fn(),
        self::structure::len::build_fn(),
        self::structure::to_map::build_fn(),
        self::structure::values::build_fn(),
    ]
}
