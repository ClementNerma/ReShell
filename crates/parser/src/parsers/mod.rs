//! This module exposes the main parser for ReShell source code ([`PROGRAM`]),
//! as well as its required context ([`ParserContext`]).
//!
//! The parser is built using [`parsy`]'s utility types.

use std::{collections::HashSet, sync::LazyLock};

use parsy::{
    FileId, Parser, ParserConstUtils, ParserNonConstUtils, Span,
    parsers::{LazilyDefined, helpers::get_context},
};

use crate::{
    ast::{
        Block, CmdCall, CmdOutputCapture, ComputedString, EscapableChar, Expr, FnCall, FnSignature,
        FnSignatureArg, FnSignatureFlagArgNames, Function, Instruction, ListItem, LiteralValue,
        Program, PropAccessNature, Range, SingleCmdCall, SpreadValue, Value, ValueType,
    },
    files_map::SourceFile,
};

mod basics;
mod blocks;
mod cmd_calls;
mod exprs;
mod functions;
mod instrs;
mod types;
mod values;

/// The parsing context, required to parse a ReShell program
pub struct ParserContext {
    pub load_file: FileLoader,
}

/// A callback to load files when an 'include' statement is encountered
pub type FileLoader = Box<dyn Fn(String, FileId) -> Result<SourceFile, String> + Send + Sync>;

/// Parser for a ReShell program
///
/// Requires to set up a parser context by providing a [`ParserContext`] to [`parsy::ParserInput::new_with_ctx`]
pub static PROGRAM: LazilyDefined<Program> = LazilyDefined::new(|| {
    get_context::<ParserContext>()
        .ignore_then(
            RAW_BLOCK
                .static_ref()
                .spanned()
                .padded_by(msnl)
                .full()
                .critical("unexpected symbol")
                .map(|content| Program { content }),
        )
        .erase_type()
});

/// Now this is a complex one
///
/// To avoid using dynamic dispatch for all parsers (e.g. [`Box<dyn Parser>`]),
/// we use static types everywhere (no `dyn`). But, because we use [`parsy`] which
/// is a _parser combinators_ library, the types are extremely long and full of generics.
///
/// Also, they will change every time we change the slightest single thing in a parser,
/// and it will also change the type of all parsers that depend on it.
///
/// So, to avoid this problem, we use Rust's *marvelous* `type_alias_impl_trait`, which allows
/// defining a type alias, which can be used to "hide" the complex type when building a function,
/// a `const` or a `static`.
///
/// Parsers can then be defined as:
///
/// ```no_run
/// // We also need Send + Sync for shared recursive parsers
/// type OpaqueType = impl Parser<Parsed> + Send + Sync;
///
/// #[define_opaque(OpaqueType)]
/// type SpecificParser: LazyLock<OpaqueType> = LazyLock::new(|| /* build the parser here */);
/// ```
///
/// Given this is pretty verbose, the `lazy_parsers` macro takes care of everything for us:
/// it takes the name of the static to build, guesses the parsed type by camel-case-ing the
/// static's name, and takes the parser's body, which in order to make things more isolated
/// are simple function pointers calls.
///
/// Some small utilities don't require allocation and can be copied cheaply ; the macro has
/// a special branch for these.
macro_rules! lazy_parsers {
    // Set up eager parsers (built at compile time, implement `Copy`)
    (@eager $(static $static_name:ident $((-> $parsed_type:ty))? = $body:expr;)+) => {
        $(
            ::paste::paste! {
                type [<$static_name:camel "OpaqueImplType">] = impl Parser<lazy_parsers!(@parsed: $static_name | $($parsed_type)?)> + Send + Sync + Copy;

                #[define_opaque([<$static_name:camel "OpaqueImplType">])]
                #[allow(non_upper_case_globals)]
                static $static_name: [<$static_name:camel "OpaqueImplType">] = $body;
            }
        )+
    };

    // Set up lazy parsers (built once at runtime)
    (@lazy $(static $static_name:ident $((-> $parsed_type:ty))? = $body:expr;)+) => {
        $(
            ::paste::paste! {
                type [<$static_name:camel "OpaqueImplType">] = impl Parser<lazy_parsers!(@parsed: $static_name | $($parsed_type)?)> + Send + Sync;

                #[define_opaque([<$static_name:camel "OpaqueImplType">])]
                static $static_name: ::std::sync::LazyLock<[<$static_name:camel "OpaqueImplType">]> = ::std::sync::LazyLock::new({ || $body });
            }
        )+
    };

    (@parsed: $static_name:ident |) => {
        ::paste::paste! { [<$static_name:camel>] }
    };

    (@parsed: $static_name:ident | $parsed_type:ty) => {
        $parsed_type
    };
}

// Build all eager parsers
lazy_parsers! { @eager
    // Basic utilities
    static ms (-> ()) = self::basics::ms();
    static msnl (-> ()) = self::basics::msnl();
    static s (-> ()) = self::basics::s();
    static possible_ident_char (-> char) = self::basics::possible_ident_char();
    static first_ident_char (-> char) = self::basics::first_ident_char();
    static ident (-> String) = self::basics::ident();
    static var_name (-> String) = self::basics::var_name();
}

// Build all lazy parsers
lazy_parsers! { @lazy
    // Blocks
    static RAW_BLOCK (-> Block) = self::blocks::raw_block();
    static BLOCK = self::blocks::block();

    // Command calls
    static SINGLE_CMD_CALL = self::cmd_calls::single_cmd_call();
    static CMD_CALL = self::cmd_calls::cmd_call();

    // Expressions
    static PROP_ACCESS_NATURE = self::exprs::prop_access_nature();
    static EXPR = self::exprs::expr();

    // Functions
    static FN_ARG_LONG_FLAG_NAME_IDENTIFIER (-> String) = self::functions::fn_arg_long_flag_name_identifier();
    static FN_FLAG_ARG_SIGNATURE_NAMES (-> FnSignatureFlagArgNames) = self::functions::fn_flag_arg_signature_names();
    static FN_SIGNATURE_ARG = self::functions::fn_signature_arg();
    static FN_SIGNATURE = self::functions::fn_signature();

    // Instructions
    static INSTRUCTION (-> Span<Instruction>) = self::instrs::instruction();

    // Types
    static VALUE_TYPE = self::types::value_type();

    // Values
    static ESCAPABLE_CHAR (-> EscapableChar) = self::values::escapable_char();
    static LITERAL_STRING (-> String) = self::values::literal_string();
    static LITERAL_INT (-> i64) = self::values::literal_int();
    static LITERAL_VALUE = self::values::literal_value();
    static COMPUTED_STRING = self::values::computed_string();
    static RANGE = self::values::range();
    static LAMBDA (-> Function) = self::values::lambda();
    static CMD_CAPTURE (-> CmdOutputCapture) = self::values::cmd_capture();
    static INLINE_CMD_CALL (-> Span<CmdCall>) = self::values::inline_cmd_call();
    static SPREAD_VALUE = self::values::spread_value();
    static FN_CALL = self::values::fn_call();
    static LIST_VALUE (-> Vec<ListItem>) = self::values::list_value();
    static VALUE = self::values::value();
}

/// List of delimiter characters
pub static DELIMITER_CHARS: LazyLock<HashSet<char>> = LazyLock::new(|| {
    HashSet::from([
        '(', ')', '[', ']', '{', '}', '<', '>', ';', '|', '\'', '"', '`', '$', '#', '^',
    ])
});

// Usage: .debug(simple_debug) after any parser
#[allow(dead_code)]
fn simple_debug<T: std::fmt::Debug>(d: parsy::parsers::DebugType<'_, '_, T>) {
    println!("{d:#?}");
}
