use colored::Colorize;
use terminal_size::{terminal_size, Width};

use crate::{
    builder::BuiltinVar,
    define_internal_fn,
    helper::{Arg, ArgTyping, ArgTypingDirectCreation, OptionalArg, RequiredArg},
    type_handlers::{
        AnyType, DetachedListType, ErrorType, ExactIntType, FloatType, IntType, MapType, RangeType,
        StringType, Tuple2Type, Union2Result, Union2Type, Union3Result, Union3Type,
        UntypedStructType,
    },
};

use reshell_runtime::{
    display::dbg_loc,
    gc::GcCell,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::RuntimeValue,
};

use super::{builder::NativeLibDefinition, prompt::GEN_PROMPT_VAR_NAME, utils::forge_internal_loc};

pub fn define_native_lib() -> NativeLibDefinition {
    NativeLibDefinition {
        functions: vec![
            define_internal_fn!(
                //
                // Create a map (optionally from a list of entries)
                //

                "map",

                Args [ArgsAt] (
                    entries: OptionalArg<Union2Type<
                        UntypedStructType,
                        DetachedListType<Tuple2Type<StringType, AnyType>>
                    >> =>
                        Arg::positional("entries")
                )

                -> Some(MapType::new_direct().underlying_type()),

                |_, Args { entries }, _, _| {
                    let map = match entries {
                        None => HashMap::new(),
                        Some(entries) => match entries {
                            Union2Result::A(obj) => obj.read().clone(),
                            Union2Result::B(tuples) => tuples.into_iter().collect()
                        }
                    };

                    Ok(Some(RuntimeValue::Map(GcCell::new(map))))
                }
            ),
            define_internal_fn!(
                //
                // Exit the program (optionally with an error code)
                //

                "exit",

                Args [ArgsAt] (
                    code: OptionalArg<IntType> => Arg::positional("code")
                )

                -> None,

                |at, Args { code }, ArgsAt { code: code_at }, ctx| {
                    let code = code
                        .map(|code|
                            u8::try_from(code)
                                .map_err(|_| ctx.error(code_at.unwrap(), format!("code must be in 0..255, got {code}")))
                        )
                        .transpose()?;

                    Err(ctx.exit(at, code))
                }
            ),
            define_internal_fn!(
                //
                // Display a message
                //

                "echo",

                Args [ArgsAt] (
                    message: RequiredArg<Union3Type<StringType, IntType, FloatType>> => Arg::positional("message")
                )

                -> None,

                |_, Args { message }, _, _| {
                    println!("{}", match message {
                        Union3Result::A(string) => string,
                        Union3Result::B(int) => int.to_string(),
                        Union3Result::C(float) => float.to_string()
                    });

                    Ok(None)
                }
            ),
            define_internal_fn!(
                //
                // Debug a value
                //

                "dbg",

                Args [ArgsAt] (
                    value: RequiredArg<AnyType> => Arg::positional("value"),
                    tab_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("tab-size"),
                    max_line_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("max-line-size")
                )

                -> None,

                |at, Args { value, tab_size, max_line_size }, _, ctx| {
                    let at = format!("dbg [{}]:", dbg_loc(at, ctx.files_map()));

                    println!("{} {}", at.bright_magenta(), value.render_colored(ctx, PrettyPrintOptions {
                        pretty: true,
                        line_prefix_size: at.chars().count(),
                        max_line_size: max_line_size.or_else(|| terminal_size().map(|(Width(width), _)| usize::from(width))).unwrap_or(30),
                        tab_size: tab_size.unwrap_or(4)
                    }));

                    Ok(None)
                }
            ),
            //
            // Debug a value's type
            //
            define_internal_fn!(
                "dbg-type",

                Args [ArgsAt] (
                    value: RequiredArg<AnyType> => Arg::positional("value"),
                    tab_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("tab-size"),
                    max_line_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("max-line-size")
                )

                -> None,

                |at, Args { value, tab_size, max_line_size }, _, ctx| {
                    let at = format!("dbg-type [{}]:", dbg_loc(at, ctx.files_map()).bright_yellow());

                    println!("{} {}", at.bright_magenta(), value.get_type().render_colored(ctx, PrettyPrintOptions {
                        pretty: true,
                        line_prefix_size: at.chars().count(),
                        max_line_size: max_line_size.or_else(|| terminal_size().map(|(Width(width), _)| usize::from(width))).unwrap_or(30),
                        tab_size: tab_size.unwrap_or(4)
                    }));

                    Ok(None)
                }
            ),
            define_internal_fn!(
                //
                // Create a range value
                //

                "range",

                Args [ArgsAt] (
                    from: RequiredArg<ExactIntType<usize>> => Arg::positional("from"),
                    to: RequiredArg<ExactIntType<usize>> => Arg::positional("to")
                )

                -> Some(RangeType::new_direct().underlying_type()),

                |_, Args { from, to }, _, _| {
                    Ok(Some(RuntimeValue::Range { from, to }))
                }
            ),
            define_internal_fn!(
                //
                // Create an error value
                //

                "error",

                Args [ArgsAt] (
                    content: RequiredArg<StringType> => Arg::positional("content")
                )

                -> Some(ErrorType::new_direct().underlying_type()),

                |at, Args { content }, _, _| {
                    Ok(Some(RuntimeValue::Error { at, msg: content }))
                }
            ),
        ],

        vars: vec![
            // Prompt generation variable
            BuiltinVar {
                name: GEN_PROMPT_VAR_NAME,
                is_mut: true,
                init_value: RuntimeValue::Null,
            },
        ],
    }
}
