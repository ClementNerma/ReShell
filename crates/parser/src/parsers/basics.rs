use parsy::{
    Parser,
    chars::alphanumeric,
    helpers::{char, choice, filter, silent_choice, whitespaces},
};

pub fn comment() -> impl Parser<()> + Copy {
    char('#')
        .then(filter(|c| c != '\r' && c != '\n').repeated())
        .to(())
}

pub fn ms() -> impl Parser<()> + Copy {
    silent_choice((
        comment(),
        filter(|c| c.is_whitespace() && c != '\r' && c != '\n'),
    ))
    .repeated()
    .to(())
}

pub fn msnl() -> impl Parser<()> + Copy {
    silent_choice((comment(), filter(|c| c.is_whitespace())))
        .repeated()
        .to(())
}

pub fn s() -> impl Parser<()> + Copy {
    whitespaces().no_newline().at_least_one()
}

pub fn possible_ident_char() -> impl Parser<char> + Copy {
    choice((char('_'), alphanumeric()))
}

pub fn first_ident_char() -> impl Parser<char> + Copy {
    filter(|c| c == '_' || c.is_alphabetic())
}

pub fn ident() -> impl Parser<String> + Copy {
    first_ident_char()
        .then(filter(|c| c == '_' || c.is_alphanumeric()).repeated())
        .collect_string()
}

pub fn var_name() -> impl Parser<String> + Copy {
    char('$').ignore_then(ident())
}

#[macro_export]
macro_rules! use_basic_parsers {
    ($($idents: ident),+) => {
        $( let $idents = $crate::parsers::basics::$idents(); )+
    }
}
