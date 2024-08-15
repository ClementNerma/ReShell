//!
//! List of methods for the builtin types
//!

mod any;
mod command;
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

pub use stringifyable::to_string::{stringify_value, StringifyableType};

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
        self::command::succeeds::build_fn(),
        //
        // Date-times
        //
        self::datetime::to_string::build_fn(),
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
        self::list::append::build_fn(),
        self::list::at::build_fn(),
        self::list::concat::build_fn(),
        self::list::contains::build_fn(),
        self::list::each::build_fn(),
        self::list::filter::build_fn(),
        self::list::filter_map::build_fn(),
        self::list::fold::build_fn(),
        self::list::get::build_fn(),
        self::list::is_empty::build_fn(),
        self::list::join::build_fn(),
        self::list::len::build_fn(),
        self::list::map::build_fn(),
        self::list::pop::build_fn(),
        self::list::prepend::build_fn(),
        self::list::reduce::build_fn(),
        self::list::remove_at::build_fn(),
        self::list::reversed::build_fn(),
        self::list::shuffled::build_fn(),
        self::list::slice::build_fn(),
        self::list::sorted::build_fn(),
        //
        // Maps
        //
        self::map::has::build_fn(),
        self::map::is_empty::build_fn(),
        self::map::keys::build_fn(),
        self::map::len::build_fn(),
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
        self::regex::captures::build_fn(),
        self::regex::matches::build_fn(),
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
        self::string::parse_float::build_fn(),
        self::string::parse_int::build_fn(),
        self::string::reverse::build_fn(),
        self::string::repeat::build_fn(),
        self::string::replace::build_fn(),
        self::string::split::build_fn(),
        self::string::starts_with::build_fn(),
        self::string::substr::build_fn(),
        self::string::to_lowercase::build_fn(),
        self::string::to_uppercase::build_fn(),
        self::string::trim::build_fn(),
        //
        // Stringifyables (union type)
        //
        self::stringifyable::to_string::build_fn(),
        //
        // Structs
        //
        self::structure::to_map::build_fn(),
    ]
}
