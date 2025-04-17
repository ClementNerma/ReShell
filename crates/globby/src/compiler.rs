use regex::bytes::Regex;

use crate::parser::{CharacterClass, CharsMatcher, RawComponent, SingleCharMatcher};

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
            let mut out = String::new();

            for matcher in chars_matchers {
                compile_chars_matcher(matcher, &mut out);
            }

            out
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

fn compile_chars_matcher(chars_matcher: &CharsMatcher, out: &mut String) {
    match chars_matcher {
        CharsMatcher::AnyChars => out.push_str(".*"),
        CharsMatcher::AnyCharOrNot => out.push_str(".?"),
        CharsMatcher::Literal(lit) => out.push_str(&regex::escape(lit)),
        CharsMatcher::OneOfChars(single_char_matchers) => {
            out.push('[');

            for matcher in single_char_matchers {
                compile_single_char_matcher(*matcher, out);
            }

            out.push(']');
        }
        CharsMatcher::NoneOfChars(single_char_matchers) => {
            out.push_str("[^");

            for matcher in single_char_matchers {
                compile_single_char_matcher(*matcher, out);
            }

            out.push(']');
        }
        CharsMatcher::OneOfGroups(matchers) => {
            out.push('(');

            for (i, matchers) in matchers.iter().enumerate() {
                if i > 0 {
                    out.push('|');
                }

                for matcher in matchers {
                    compile_chars_matcher(matcher, out);
                }
            }

            out.push(')');
        }
    }
}

fn compile_single_char_matcher(char_matcher: SingleCharMatcher, out: &mut String) {
    match char_matcher {
        SingleCharMatcher::Literal(lit) => out.push_str(&regex::escape(&lit.to_string())),

        SingleCharMatcher::Class(character_class) => out.push_str(match character_class {
            CharacterClass::Alpha => "[:alpha:]",
            CharacterClass::Digit => "[:digit:]",
            CharacterClass::Alphanumeric => "[:alnum:]",
            CharacterClass::Uppercase => "[:upper:]",
            CharacterClass::Lowercase => "[:lower:]",
            CharacterClass::Whitespace => "[:space:]",
            CharacterClass::Any => ".",
        }),
    }
}
