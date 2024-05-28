use crate::ast::{FnFlagArgNames, RuntimeEaten};

impl FnFlagArgNames {
    pub fn short_flag(&self) -> Option<RuntimeEaten<char>> {
        match self {
            FnFlagArgNames::ShortFlag(flag) => Some(*flag),
            FnFlagArgNames::LongFlag(_) => None,
            FnFlagArgNames::LongAndShortFlag { long: _, short } => Some(*short),
        }
    }

    pub fn long_flag(&self) -> Option<&RuntimeEaten<String>> {
        match self {
            FnFlagArgNames::ShortFlag(_) => None,
            FnFlagArgNames::LongFlag(flag) => Some(flag),
            FnFlagArgNames::LongAndShortFlag { long, short: _ } => Some(long),
        }
    }
}
