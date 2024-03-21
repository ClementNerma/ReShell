use colored::Colorize;
use terminal_size::{terminal_size, Width};

use crate::{
    define_internal_fn,
    helper::{Arg, ArgTyping, ArgTypingDirectCreation, OptionalArg, RequiredArg},
    type_handlers::{
        AnyType, DetachedListType, ExactIntType, FloatType, IntType, MapType, StringType,
        Tuple2Type, Union2Result, Union2Type, Union3Result, Union3Type, UntypedStructType,
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
            (
                "map",
                define_internal_fn!(
                    Args [ArgsAt] (
                        entries: OptionalArg<Union2Type<
                            UntypedStructType,
                            DetachedListType<Tuple2Type<StringType, AnyType>>
                        >> =>
                            Arg::positional("entries")
                    ) ->
                        Some(MapType::new_direct().underlying_type()),

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
            ),
            (
                "exit",
                define_internal_fn!(
                    Args [ArgsAt] (
                        code: OptionalArg<IntType> => Arg::positional("code")
                    ) -> None,

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
            ),
            (
                "echo",
                define_internal_fn!(
                    Args [ArgsAt] (
                        message: RequiredArg<Union3Type<StringType, IntType, FloatType>> => Arg::positional("message")
                    ) -> None,

                    |_, Args { message }, _, _| {
                        println!("{}", match message {
                            Union3Result::A(string) => string,
                            Union3Result::B(int) => int.to_string(),
                            Union3Result::C(float) => float.to_string()
                        });

                        Ok(None)
                    }
                ),
            ),
            (
                "dbg",
                define_internal_fn!(
                    Args [ArgsAt] (
                        value: RequiredArg<AnyType> => Arg::positional("value"),
                        tab_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("tab-size"),
                        max_line_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("max-line-size")
                    ) -> None,

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
            ),
            (
                "dbg-type",
                define_internal_fn!(
                    Args [ArgsAt] (
                        value: RequiredArg<AnyType> => Arg::positional("value"),
                        tab_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("tab-size"),
                        max_line_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("max-line-size")
                    ) -> None,

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
            ),
        ],

        vars: vec![(GEN_PROMPT_VAR_NAME, true, RuntimeValue::Null)],
    }
}
