use std::{collections::HashSet, sync::LazyLock};

use parsy::{Parser, char, choice, end, filter, just, not, silent_choice};

pub static PATTERN_PARSER: LazyLock<Box<dyn Parser<RawPattern> + Send + Sync>> =
    LazyLock::new(|| {
        let normal_char = filter(|c| !SPECIAL_CHARS.contains(&c));

        let chars_matcher = choice::<CharsMatcher, _>((
            //
            // Literal characters
            //
            normal_char
                .repeated_into_container::<String>()
                .at_least(1)
                .map(|string| CharsMatcher {
                    nature: CharsMatcherNature::Literal(string),
                    repetition: ComponentRepetition::ExactlyOnce,
                }),
            //
            // Wildcard
            //
            char('*')
                .followed_by(not(char('*')).critical(
                    "Wildcard components '**' must be preceded by and followed by a path separator",
                ))
                .map(|_| CharsMatcher {
                    nature: CharsMatcherNature::AnyChars,
                    repetition: ComponentRepetition::ExactlyOnce,
                }),
            //
            // Optional universal character (or not)
            //
            char('?').map(|_| CharsMatcher {
                nature: CharsMatcherNature::AnyCharOrNot,
                repetition: ComponentRepetition::ExactlyOnce,
            }),
            //
            // Character alternates
            //
            char('[')
                .ignore_then(char('^').or_not())
                .then(choice::<Vec<SingleCharMatcher>, _>((
                    //
                    // Character class
                    //
                    char(':')
                        .ignore_then(
                            choice::<CharacterClass, _>((
                                just("alpha").to(CharacterClass::Alpha),
                                just("digit").to(CharacterClass::Digit),
                                just("alphanumeric").to(CharacterClass::Alphanumeric),
                                just("uppercase").to(CharacterClass::Uppercase),
                                just("lowercase").to(CharacterClass::Lowercase),
                                just("whitespace").to(CharacterClass::Whitespace),
                                just("any").to(CharacterClass::Any),
                            ))
                            .critical("expected a valid character class"),
                        )
                        .then_ignore(char(':').critical_auto_msg())
                        .map(SingleCharMatcher::Class)
                        .map(|class| vec![class]),
                    //
                    // Various characters
                    //
                    choice::<SingleCharMatcher, _>((
                        char('\\')
                            .ignore_then(
                                filter(|c| SPECIAL_CHARS.contains(&c) && c != '/' && c != '\\')
                                    .critical("expected a special character to escape"),
                            )
                            .map(SingleCharMatcher::Literal),
                        filter(|c| c != '/' && c != '\\').map(SingleCharMatcher::Literal),
                    ))
                    .repeated_into_vec()
                    .at_least(1)
                    .critical("expected at least one character to match"),
                )))
                .then_ignore(char(']').critical_auto_msg())
                .map(|(neg, chars)| {
                    if neg.is_some() {
                        CharsMatcherNature::NoneOfChars(chars)
                    } else {
                        CharsMatcherNature::OneOfChars(chars)
                    }
                })
                .then(
                    choice::<ComponentRepetition, _>((
                        char('*').to(ComponentRepetition::Any),
                        char('?').to(ComponentRepetition::AtMostOnce),
                    ))
                    .or_not()
                    .map(|rep| rep.unwrap_or(ComponentRepetition::ExactlyOnce)),
                )
                .map(|(nature, repetition)| CharsMatcher { nature, repetition }),
        ));

        let component = choice::<RawComponent, _>((
            //
            // Wildcard
            //
            just("**")
                .followed_by(silent_choice((char('/'), char('\\'), end())).critical(
                    "Wildcard components '**' must be preceded and followed by path separators",
                ))
                .map(|_| RawComponent::Wildcard),
            //
            // Alternate groups
            //
            char('{')
                .ignore_then(
                    chars_matcher
                        .repeated_into_vec()
                        .at_least(1)
                        .separated_by_into_vec(char('|'))
                        .at_least(2)
                        .critical("expected at least 2 alternative matchers"),
                )
                .then_ignore(char('}').critical_auto_msg())
                .map(RawComponent::OneOf),
            //
            // Character matchers
            //
            chars_matcher.repeated_into_vec().map(RawComponent::Suite),
        ));

        let pattern = silent_choice((char('/'), char('\\')))
            .or_not()
            // TODO: handle empty components
            .then(component.separated_by_into_vec(silent_choice((char('/'), char('\\')))))
            .full()
            .map(|(is_absolute, components)| RawPattern {
                is_absolute: is_absolute.is_some(),
                components,
            });

        Box::new(pattern)
    });

static SPECIAL_CHARS: LazyLock<HashSet<char>> =
    LazyLock::new(|| HashSet::from(['[', ']', '{', '}', '*', '?', '\\', '/']));

pub struct RawPattern {
    pub is_absolute: bool,
    pub components: Vec<RawComponent>,
}

#[derive(Debug)]
pub enum RawComponent {
    Suite(Vec<CharsMatcher>),
    OneOf(Vec<Vec<CharsMatcher>>),
    Wildcard,
}

#[derive(Debug)]
pub struct CharsMatcher {
    pub nature: CharsMatcherNature,
    pub repetition: ComponentRepetition,
}

#[derive(Debug)]
pub enum CharsMatcherNature {
    AnyChars,
    AnyCharOrNot,
    Literal(String),
    OneOfChars(Vec<SingleCharMatcher>),
    NoneOfChars(Vec<SingleCharMatcher>),
}

#[derive(Debug, Clone, Copy)]
pub enum SingleCharMatcher {
    Literal(char),
    Class(CharacterClass),
}

#[derive(Debug, Clone, Copy)]
pub enum CharacterClass {
    Alpha,
    Digit,
    Alphanumeric,
    Uppercase,
    Lowercase,
    Whitespace,
    Any,
}

#[derive(Debug, Clone, Copy)]
pub enum ComponentRepetition {
    ExactlyOnce,
    AtMostOnce,
    Any,
}
