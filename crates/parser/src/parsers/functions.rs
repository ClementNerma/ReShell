use parsy::{
    Parser, ParserConstUtils,
    parsers::helpers::{char, choice, filter, just, not},
};

use super::{
    FN_ARG_LONG_FLAG_NAME_IDENTIFIER, FN_FLAG_ARG_SIGNATURE_NAMES, VALUE_TYPE, first_ident_char,
    ident, ms, msnl, possible_ident_char,
};
use crate::{
    ast::{
        FnSignature, FnSignatureArg, FnSignatureFlagArgNames, FnSignatureNormalFlagArg,
        FnSignaturePositionalArg, FnSignaturePresenceFlagArg, FnSignatureRestArg, RuntimeSpan,
    },
    parsers::FN_SIGNATURE_ARG,
};

pub fn fn_arg_long_flag_name_identifier() -> impl Parser<String> + Send + Sync {
    first_ident_char
        .then(filter(|c| c == '_' || c == '-' || c.is_alphanumeric()).repeated())
        .collect_string()
}

pub fn fn_flag_arg_signature_names() -> impl Parser<FnSignatureFlagArgNames> + Send + Sync {
    let fn_arg_long_flag_name = just("--")
        .ignore_then(
            FN_ARG_LONG_FLAG_NAME_IDENTIFIER
                .static_ref()
                .validate_or_critical(
                    |name| name != "it" && name != "self",
                    "Cannot declare a flag with reserved name 'it' or 'self'",
                )
                .spanned()
                .critical("expected a flag name (identifier)"),
        )
        .followed_by(not(possible_ident_char).critical("unexpected symbol after long flag name"));

    let fn_arg_short_flag_name = char('-')
        .ignore_then(
            first_ident_char
                .spanned()
                .critical("expected a single-character identifier"),
        )
        .followed_by(not(possible_ident_char).critical("expected a single-character identifier"));

    choice::<FnSignatureFlagArgNames, _>((
        // Long *and* short flags
        fn_arg_long_flag_name
            .map(RuntimeSpan::from)
            .then_ignore(ms)
            .then_ignore(char('('))
            .then(fn_arg_short_flag_name.map(RuntimeSpan::from))
            .then_ignore(char(')').critical_auto_msg())
            .map(|(long, short)| FnSignatureFlagArgNames::LongAndShortFlag { short, long }),
        // Long flag only
        fn_arg_long_flag_name
            .map(RuntimeSpan::from)
            .map(FnSignatureFlagArgNames::LongFlag),
        // Long flag only
        fn_arg_short_flag_name
            .map(RuntimeSpan::from)
            .map(FnSignatureFlagArgNames::ShortFlag),
    ))
}

pub fn fn_signature_arg() -> impl Parser<FnSignatureArg> + Send + Sync {
    choice::<FnSignatureArg, _>((
        // Positional
        ident
            .validate_or_critical(
                |name| name != "it",
                "Cannot declare an argument with reserved name 'it'",
            )
            .spanned()
            .map(RuntimeSpan::from)
            .then_ignore(msnl)
            .then(char('?').or_not())
            .then(
                char(':')
                    .ignore_then(msnl)
                    .ignore_then(
                        VALUE_TYPE
                            .static_ref()
                            .critical("expected a type for the argument"),
                    )
                    .or_not(),
            )
            .map(|((name, is_optional), typ)| {
                FnSignatureArg::Positional(FnSignaturePositionalArg {
                    name,
                    is_optional: is_optional.is_some(),
                    typ,
                })
            }),
        // Normal flags
        FN_FLAG_ARG_SIGNATURE_NAMES
            .static_ref()
            .then_ignore(msnl)
            .then(char('?').or_not())
            .then_ignore(char(':'))
            .then_ignore(msnl)
            .then(
                VALUE_TYPE
                    .static_ref()
                    .critical("expected a type for the flag argument"),
            )
            .map(|((names, is_optional), typ)| {
                FnSignatureArg::NormalFlag(FnSignatureNormalFlagArg {
                    names,
                    is_optional: is_optional.is_some(),
                    typ,
                })
            }),
        // Presence flags
        FN_FLAG_ARG_SIGNATURE_NAMES
            .static_ref()
            .then_ignore(msnl)
            .then_ignore(
                char('?').critical("expected either a '?' or a ':' marker after flag argument"),
            )
            .map(|names| FnSignatureArg::PresenceFlag(FnSignaturePresenceFlagArg { names })),
        // Rest
        just("...")
            .ignore_then(ident)
            .spanned()
            .map(RuntimeSpan::from)
            .then(
                msnl.ignore_then(char(':'))
                    .ignore_then(msnl)
                    .ignore_then(
                        VALUE_TYPE
                            .static_ref()
                            .critical("expected a type for the rest parameter")
                            .spanned()
                            .map(RuntimeSpan::from),
                    )
                    .or_not(),
            )
            .map(|(name, typ)| FnSignatureArg::Rest(FnSignatureRestArg { name, typ })),
    ))
}

pub fn fn_signature() -> impl Parser<FnSignature> + Send + Sync {
    char('(')
        .ignore_then(
            FN_SIGNATURE_ARG
                .static_ref()
                .separated_by_into_vec(char(',').padded_by(msnl)),
        )
        .then_ignore(msnl)
        .then_ignore(char(')').critical_auto_msg())
        .then(
            msnl.ignore_then(just("->"))
                .ignore_then(ms)
                .ignore_then(
                    VALUE_TYPE
                        .static_ref()
                        .map(Box::new)
                        .critical("expected a type"),
                )
                .then_ignore(ms)
                .or_not(),
        )
        .map(|(args, ret_type)| FnSignature { args, ret_type })
}
