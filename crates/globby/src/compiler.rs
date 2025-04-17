use regex::bytes::Regex;

use crate::parser::{CharacterClass, CharsMatcher, RawComponent, SingleCharMatcher};

// TODO: remove a lot of string allocations!

#[derive(Debug, Clone)]
pub enum Component {
    Regex(Regex),
    Wildcard,
}

pub enum CaseSensitivity {
    Sensitive,
    Insensitive,
}

pub fn compile_component(component: &RawComponent, case_sensitivity: CaseSensitivity) -> Component {
    let regex = match component {
        RawComponent::Wildcard => return Component::Wildcard,

        RawComponent::Literal(lit) => regex::escape(lit),

        RawComponent::Suite(chars_matchers) => {
            join_iter(chars_matchers.iter().map(compile_chars_matcher), "")
        }
    };

    Component::Regex(
        Regex::new(&format!(
            "{}^{regex}$",
            match case_sensitivity {
                CaseSensitivity::Sensitive => "",
                CaseSensitivity::Insensitive => "(?i)",
            }
        ))
        .unwrap(),
    )
}

fn compile_chars_matcher(chars_matcher: &CharsMatcher) -> String {
    match chars_matcher {
        CharsMatcher::AnyChars => ".*".to_owned(),
        CharsMatcher::AnyCharOrNot => ".?".to_owned(),
        CharsMatcher::Literal(lit) => regex::escape(lit),
        CharsMatcher::OneOfChars(single_char_matchers) => format!(
            "[{}]",
            join_iter(
                single_char_matchers
                    .iter()
                    .copied()
                    .map(compile_single_char_matcher),
                ""
            )
        ),
        CharsMatcher::NoneOfChars(single_char_matchers) => format!(
            "[^{}]",
            join_iter(
                single_char_matchers
                    .iter()
                    .copied()
                    .map(compile_single_char_matcher),
                ""
            )
        ),
        CharsMatcher::OneOfGroups(items) => format!(
            "({})",
            join_iter(
                items
                    .iter()
                    .map(|group| join_iter(group.iter().map(compile_chars_matcher), "")),
                "|",
            )
        ),
    }
}

fn compile_single_char_matcher(char_matcher: SingleCharMatcher) -> String {
    match char_matcher {
        SingleCharMatcher::Literal(lit) => regex::escape(&lit.to_string()),

        SingleCharMatcher::Class(character_class) => match character_class {
            CharacterClass::Alpha => "[:alpha:]".to_owned(),
            CharacterClass::Digit => "[:digit:]".to_owned(),
            CharacterClass::Alphanumeric => "[:alnum:]".to_owned(),
            CharacterClass::Uppercase => "[:upper:]".to_owned(),
            CharacterClass::Lowercase => "[:lower:]".to_owned(),
            CharacterClass::Whitespace => "[:space:]".to_owned(),
            CharacterClass::Any => ".".to_owned(),
        },
    }
}

fn join_iter(iter: impl Iterator<Item = impl AsRef<str>>, join: &str) -> String {
    let mut out = String::new();

    for (i, item) in iter.enumerate() {
        if i > 0 {
            out.push_str(join);
        }

        out.push_str(item.as_ref());
    }

    out
}
