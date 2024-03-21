use colored::Colorize;
use terminal_size::{terminal_size, Height, Width};

use crate::{
    builtins::{
        helper::{Arg, OptionalArg, RequiredArg},
        type_handlers::{AnyType, FloatType, IntType, StringType, Union3Result, Union3Type},
    },
    define_internal_fn,
    display::dbg_loc,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::RuntimeValue,
};

use super::{builder::NativeLibDefinition, prompt::GEN_PROMPT_VAR_NAME, utils::forge_internal_loc};

pub fn define_native_lib() -> NativeLibDefinition {
    NativeLibDefinition {
        functions: vec![
            (
                "exit",
                define_internal_fn!(
                    Args [ArgsAt] ( code: OptionalArg<IntType> => Arg::positional("code") ),

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
                    Args [ArgsAt] ( message: RequiredArg<Union3Type<StringType, IntType, FloatType>> => Arg::positional("message") ),

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
                    Args [ArgsAt] ( value: RequiredArg<AnyType> => Arg::positional("value") ),

                    |at, Args { value }, _, ctx| {
                        let at = format!("dbg [{}]:", dbg_loc(at, ctx.files_map()));

                        println!("{} {}", at.bright_magenta(), value.render_colored(ctx, PrettyPrintOptions {
                            pretty: true,
                            line_prefix_size: at.chars().count(),
                            max_line_size: match terminal_size() {
                                Some((Width(cols), Height(_))) => cols.into(),
                                None => 30 // todo: make this a static?
                            },
                            tab_size: 4 // todo: make this configurable?
                        }));

                        Ok(None)
                    }
                ),
            ),
        ],

        vars: vec![(GEN_PROMPT_VAR_NAME, false, RuntimeValue::Null)],
    }
}
