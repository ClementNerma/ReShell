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
mod range;
mod regex;
mod string;
mod stringifyable;
mod structure;

pub use self::stringifyable::{StringifyableType, stringify_value};

use crate::helpers::fns::InternalFunction;

/// List all native methods
pub fn native_methods() -> Vec<InternalFunction> {
    let mut out = vec![];

    out.extend(self::any::any_methods());
    out.extend(self::cmdcall::cmd_call_methods());
    out.extend(self::datetime::datetime_methods());
    out.extend(self::duration::duration());
    out.extend(self::instant::instant_methods());
    out.extend(self::list::list_methods());
    out.extend(self::map::map_methods());
    out.extend(self::progress_bar::progress_bar_methods());
    out.extend(self::range::range_methods());
    out.extend(self::regex::regex_methods());
    out.extend(self::string::string_methods());
    out.extend(self::stringifyable::stringifyable_methods());
    out.extend(self::structure::structure_methods());

    out
}
