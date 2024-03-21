use crate::helper::InternalFunction;

mod datetime;
mod duration;
mod instant;
mod progress_bar;
mod regex;

/// List all native typemaking functions
pub fn native_typemaking_functions() -> Vec<InternalFunction> {
    vec![
        // Collect functions from individual modules
        self::datetime::build_fn(),
        self::instant::build_fn(),
        self::progress_bar::build_fn(),
        self::regex::build_fn(),
    ]
}

pub mod types {
    pub use super::{
        datetime::DateTimeValue, duration::DurationValue, instant::InstantValue,
        progress_bar::ProgressBarValue, regex::RegexValue,
    };
}
