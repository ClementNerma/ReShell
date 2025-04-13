use regex::bytes::Regex;

use crate::parser::{
    CharacterClass, CharsMatcher, CharsMatcherNature, ComponentRepetition, RawComponent,
    SingleCharMatcher,
};

// TODO: remove a lot of string allocations!

#[derive(Debug)]
pub enum Component {
    Regex(Regex),
    Wildcard,
}

pub fn compile_component(component: &RawComponent) -> Component {
    let regex = match component {
        RawComponent::Wildcard => return Component::Wildcard,

        RawComponent::Literal(lit) => regex::escape(lit),

        RawComponent::Suite(chars_matchers) => {
            join_iter(chars_matchers.iter().map(compile_chars_matcher), "")
        }

        RawComponent::OneOf(items) => format!(
            "({})",
            join_iter(
                items
                    .iter()
                    .map(|group| join_iter(group.iter().map(compile_chars_matcher), "")),
                "|",
            )
        ),
    };

    Component::Regex(Regex::new(&format!("^{regex}$")).unwrap())
}

fn compile_chars_matcher(chars_matcher: &CharsMatcher) -> String {
    let CharsMatcher { nature, repetition } = chars_matcher;

    let base = match nature {
        CharsMatcherNature::AnyChars => ".*".to_owned(),
        CharsMatcherNature::AnyCharOrNot => ".?".to_owned(),
        CharsMatcherNature::Literal(lit) => regex::escape(lit),
        CharsMatcherNature::OneOfChars(single_char_matchers) => format!(
            "[{}]",
            join_iter(
                single_char_matchers
                    .iter()
                    .copied()
                    .map(compile_single_char_matcher),
                ""
            )
        ),
        CharsMatcherNature::NoneOfChars(single_char_matchers) => format!(
            "[^{}]",
            join_iter(
                single_char_matchers
                    .iter()
                    .copied()
                    .map(compile_single_char_matcher),
                ""
            )
        ),
    };

    let repetition = match repetition {
        ComponentRepetition::ExactlyOnce => "",
        ComponentRepetition::AtMostOnce => "?",
        ComponentRepetition::Any => "*",
    };

    format!("{base}{repetition}")
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
