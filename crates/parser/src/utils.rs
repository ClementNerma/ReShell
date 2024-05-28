use crate::ast::{FnArgNames, RuntimeEaten};

impl FnArgNames {
    pub fn is_flag(&self) -> bool {
        match self {
            FnArgNames::Positional(_) => false,
            FnArgNames::ShortFlag(_) => true,
            FnArgNames::LongFlag(_) => true,
            FnArgNames::LongAndShortFlag { long: _, short: _ } => true,
        }
    }

    pub fn short_flag(&self) -> Option<RuntimeEaten<char>> {
        match self {
            FnArgNames::Positional(_) => None,
            FnArgNames::ShortFlag(flag) => Some(*flag),
            FnArgNames::LongFlag(_) => None,
            FnArgNames::LongAndShortFlag { long: _, short } => Some(*short),
        }
    }

    pub fn long_flag(&self) -> Option<&RuntimeEaten<String>> {
        match self {
            FnArgNames::Positional(_) => None,
            FnArgNames::ShortFlag(_) => None,
            FnArgNames::LongFlag(flag) => Some(flag),
            FnArgNames::LongAndShortFlag { long, short: _ } => Some(long),
        }
    }
}
