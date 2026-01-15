use parsy::{
    Parser, ParserConstUtils,
    parsers::{
        alphanumeric,
        helpers::{char, choice, filter, silent_choice, whitespaces},
    },
};

const fn comment() -> impl Parser<()> + Send + Sync + Copy {
    char('#')
        .then(filter(|c| c != '\r' && c != '\n').repeated())
        .to(())
}

pub const fn ms() -> impl Parser<()> + Send + Sync + Copy {
    silent_choice((
        comment(),
        filter(|c| c.is_whitespace() && c != '\r' && c != '\n'),
    ))
    .repeated()
    .to(())
}

pub const fn msnl() -> impl Parser<()> + Send + Sync + Copy {
    silent_choice((comment(), filter(|c| c.is_whitespace())))
        .repeated()
        .to(())
}

pub const fn s() -> impl Parser<()> + Send + Sync + Copy {
    whitespaces().no_newline().at_least_one()
}

pub const fn possible_ident_char() -> impl Parser<char> + Send + Sync + Copy {
    choice((char('_'), alphanumeric()))
}

pub const fn first_ident_char() -> impl Parser<char> + Send + Sync + Copy {
    filter(|c| c == '_' || c.is_alphabetic())
}

pub const fn ident() -> impl Parser<String> + Send + Sync + Copy {
    first_ident_char()
        .then(filter(|c| c == '_' || c.is_alphanumeric()).repeated())
        .collect_string()
}

pub const fn var_name() -> impl Parser<String> + Send + Sync + Copy {
    char('$').ignore_then(ident())
}
