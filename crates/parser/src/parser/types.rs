use parsy::{
    Parser,
    helpers::{char, choice, just, recursive_shared},
    timed::LazilyDefined,
};

use crate::{
    ast::{RuntimeSpan, SingleValueType, StructTypeMember, ValueType},
    parser::{functions::FN_SIGNATURE, values::LITERAL_STRING},
    use_basic_parsers,
};

pub static VALUE_TYPE: LazilyDefined<ValueType> = LazilyDefined::new(|| {
    use_basic_parsers!(possible_ident_char, msnl, ident, ms);

    recursive_shared::<ValueType, _>(|value_type| {
        let single_value_type = choice::<SingleValueType, _>((
            just("any")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Any),
            just("null")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Null),
            just("bool")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Bool),
            just("int")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Int),
            just("float")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Float),
            just("string")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::String),
            just("error")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::Error),
            just("cmdcall")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::CmdCall),
            just("fn")
                .ignore_then(FN_SIGNATURE.static_ref())
                .spanned()
                .map(RuntimeSpan::from)
                .map(SingleValueType::Function),
            just("list[")
                .ignore_then(value_type.clone().map(Box::new))
                .then_ignore(char(']').critical_auto_msg())
                .map(SingleValueType::TypedList),
            just("list")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::UntypedList),
            just("map[")
                .ignore_then(value_type.clone().map(Box::new))
                .then_ignore(char(']').critical_auto_msg())
                .map(SingleValueType::TypedMap),
            just("map")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::UntypedMap),
            char('{')
                .ignore_then(msnl)
                .ignore_then(
                    ident
                        .spanned()
                        .map(RuntimeSpan::from)
                        .then_ignore(ms)
                        .then(char('?').or_not())
                        .then_ignore(ms)
                        .then_ignore(char(':').critical_auto_msg())
                        .then_ignore(msnl)
                        .then(value_type.clone().critical("expected a value type"))
                        .map(|((name, optional), typ)| StructTypeMember {
                            name,
                            optional: optional.is_some(),
                            typ,
                        })
                        .separated_by_into_vec(char(',').padded_by(msnl)),
                )
                .then_ignore(msnl)
                .then_ignore(char('}').critical_auto_msg())
                .map(SingleValueType::TypedStruct),
            just("struct")
                .not_followed_by(possible_ident_char)
                .map(|_| SingleValueType::UntypedStruct),
            ident.spanned().map(SingleValueType::TypeAlias),
            LITERAL_STRING
                .static_ref()
                .map(SingleValueType::StringLiteral),
        ));

        choice((
            // Union type
            char('(')
                .ignore_then(
                    single_value_type
                        .clone()
                        .separated_by_into_vec(char('|').padded_by(msnl))
                        .at_least(1)
                        .critical("expected a type union"),
                )
                .then_ignore(char(')').critical_auto_msg())
                .map(ValueType::Union),
            // Single type
            single_value_type.map(ValueType::Single),
        ))
    })
    .erase_type()
});
