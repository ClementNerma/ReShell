use std::path::Path;

use colored::Colorize;
use terminal_size::{terminal_size, Height, Width};

use crate::{
    builder::BuiltinVar,
    define_internal_fn,
    helper::{Arg, ArgNames, OptionalArg, RequiredArg, TypingDirectCreation},
    type_handlers::{
        AnyType, BoolType, DetachedListType, ErrorType, ExactIntType, FloatType, IntType, MapType,
        NullType, NullableType, RangeType, StringType, Tuple2Type, TypedFunctionType, Union2Result,
        Union2Type, Union3Result, Union3Type, UntypedListType, UntypedStructType,
    },
    utils::{call_fn_checked, forge_basic_fn_signature},
};

use reshell_runtime::{
    display::dbg_loc,
    gc::GcCell,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::RuntimeValue,
};

use super::{builder::NativeLibDefinition, prompt::GEN_PROMPT_VAR_NAME};

pub fn define_native_lib() -> NativeLibDefinition {
    NativeLibDefinition {
        functions: vec![
            define_internal_fn!(
                //
                // Create a map (optionally from a list of entries)
                //

                "map",

                Args [ArgsAt] (
                    entries: OptionalArg<
                        Union2Type<
                            UntypedStructType,
                            DetachedListType<Tuple2Type<StringType, AnyType>>
                        >
                    > = Arg::positional("entries")
                )

                -> Some(MapType::direct_underlying_type()),

                |_, Args { entries }, ArgsAt { entries: entries_at }, _| {
                    let map = match entries {
                        None => HashMap::new(),
                        Some(entries) => match entries {
                            Union2Result::A(obj) => obj.read(entries_at.unwrap()).clone(),
                            Union2Result::B(tuples) => tuples.into_iter().collect()
                        }
                    };

                    Ok(Some(RuntimeValue::Map(GcCell::new(map))))
                }
            ),
            define_internal_fn!(
                //
                // Display a message
                //

                "echo",

                Args [ArgsAt] (
                    message: RequiredArg<Union3Type<StringType, IntType, FloatType>> = Arg::positional("message")
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
                    value: RequiredArg<AnyType> = Arg::positional("value"),
                    tab_size: OptionalArg<ExactIntType<usize>> = Arg::long_flag("tab-size"),
                    max_line_size: OptionalArg<ExactIntType<usize>> = Arg::long_flag("max-line-size")
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
                "dbg_type",

                Args [ArgsAt] (
                    value: RequiredArg<AnyType> = Arg::positional("value"),
                    tab_size: OptionalArg<ExactIntType<usize>> = Arg::long_flag("tab-size"),
                    max_line_size: OptionalArg<ExactIntType<usize>> = Arg::long_flag("max-line-size")
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
                    from: RequiredArg<ExactIntType<usize>> = Arg::positional("from"),
                    to: RequiredArg<ExactIntType<usize>> = Arg::positional("to")
                )

                -> Some(RangeType::direct_underlying_type()),

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
                    content: RequiredArg<StringType> = Arg::positional("content")
                )

                -> Some(ErrorType::direct_underlying_type()),

                |at, Args { content }, _, ctx| {
                    match at {
                        RuntimeCodeRange::CodeRange(at) => Ok(Some(RuntimeValue::Error { at, msg: content })),
                        RuntimeCodeRange::Internal => Err(ctx.error(at, "cannot generate an error from an internal location"))
                    }
                }
            ),
            define_internal_fn!(
                //
                // Get the length of a string or the number of entries in a map / list
                //

                "len",

                Args [ArgsAt] (
                    content: RequiredArg<Union3Type<StringType, UntypedListType, MapType>> = Arg::positional("value")
                )

                -> Some(ExactIntType::<usize>::direct_underlying_type()),

                |_, Args { content }, ArgsAt { content: content_at }, ctx| {
                    let len = match content {
                        Union3Result::A(str) => str.len(),
                        Union3Result::B(list) => list.read(content_at).len(),
                        Union3Result::C(map) => map.read(content_at).len(),
                    };

                    let len = i64::try_from(len).map_err(|_| ctx.error(content_at, format!("length is too big to fit in the integer type ({len})")))?;

                    Ok(Some(RuntimeValue::Int(len)))
                }
            ),
            define_internal_fn!(
                //
                // slice a list
                //

                "slice",

                Args [ArgsAt] (
                    list: RequiredArg<UntypedListType> = Arg::positional("list"),
                    from: RequiredArg<ExactIntType<usize>> = Arg::positional("from"),
                    length: OptionalArg<ExactIntType<usize>> = Arg::positional("length")
                )

                -> Some(UntypedListType::direct_underlying_type()),

                |_, Args { list, from, length }, ArgsAt { list: list_at, .. }, _| {
                    let sliced = list.read(list_at).iter().skip(from).take(length.unwrap_or(usize::MAX)).cloned().collect::<Vec<_>>();

                    Ok(Some(RuntimeValue::List(GcCell::new(sliced))))
                }
            ),
            define_internal_fn!(
                //
                // pop a value from a list
                //

                "pop",

                Args [ArgsAt] (
                    list: RequiredArg<UntypedListType> = Arg::positional("list")
                )

                -> Some(Union2Type::<AnyType, NullType>::direct_underlying_type()),

                |_, Args { list }, ArgsAt { list: list_at }, ctx| {
                    Ok(Some(list.write(list_at, ctx)?.pop().unwrap_or(RuntimeValue::Null)))
                }
            ),
            define_internal_fn!(
                //
                // map over a list
                //

                "listmap",

                Args [ArgsAt] (
                    list: RequiredArg<UntypedListType> = Arg::positional("list"),
                    mapper @ mapper_type: RequiredArg<TypedFunctionType> = Arg::new(ArgNames::Positional("mapper"), TypedFunctionType::new(forge_basic_fn_signature(
                        vec![
                            ("index", ExactIntType::<usize>::direct_underlying_type()),
                            ("value", AnyType::direct_underlying_type()),
                        ],
                        Some(AnyType::direct_underlying_type()
                    ))))
                )

                -> Some(UntypedListType::direct_underlying_type()),

                move |_, Args { list, mapper }, ArgsAt { list: _, mapper: mapper_at }, ctx| {
                    let mapper = LocatedValue::new(RuntimeValue::Function(mapper), mapper_at);

                    let mapped = list
                        .read(mapper_at)
                        .iter()
                        .enumerate()
                        .map(|(index, value)| -> ExecResult<RuntimeValue> {
                            let ret = call_fn_checked(
                                &mapper,
                                mapper_type.base_typing().signature(),
                                vec![
                                    RuntimeValue::Int(index.try_into().expect("list contains too many elements to be represented by an integer")),
                                    value.clone()
                                ],
                                ctx
                            )?;

                            Ok(ret.expect("internal error: mapper did not return an error (should have been caught before returning)").value)
                        })
                        .collect::<Result<_, _>>()?;

                    Ok(Some(RuntimeValue::List(GcCell::new(mapped))))
                }
            ),
            define_internal_fn!(
                //
                // Check if a path exists
                //

                "pathExists",

                Args [ArgsAt] (
                    path: RequiredArg<StringType> = Arg::positional("path")
                )

                -> Some(BoolType::direct_underlying_type()),

                |_, Args { path }, _, _| {
                    Ok(Some(RuntimeValue::Bool(Path::new(&path).exists())))
                }
            ),
            define_internal_fn!(
                //
                // Check if a directory exists
                //

                "dirExists",

                Args [ArgsAt] (
                    path: RequiredArg<StringType> = Arg::positional("path")
                )

                -> Some(BoolType::direct_underlying_type()),

                |_, Args { path }, _, _| {
                    Ok(Some(RuntimeValue::Bool(Path::new(&path).is_dir())))
                }
            ),
            define_internal_fn!(
                //
                // Check if a file exists
                //

                "fileExists",

                Args [ArgsAt] (
                    path: RequiredArg<StringType> = Arg::positional("path")
                )

                -> Some(BoolType::direct_underlying_type()),

                |_, Args { path }, _, _| {
                    Ok(Some(RuntimeValue::Bool(Path::new(&path).is_file())))
                }
            ),
            define_internal_fn!(
                //
                // Get the value of an environment variable
                //

                "env",

                Args [ArgsAt] (
                    var_name: RequiredArg<StringType> = Arg::positional("var_name"),
                    lossy: OptionalArg<BoolType> = Arg::long_flag("lossy")
                )

                -> Some(StringType::direct_underlying_type()),

                |_, Args { var_name, lossy }, ArgsAt { var_name: var_name_at, .. }, ctx| {
                    let var_value = std::env::var_os(&var_name).ok_or_else(|| ctx.error(var_name_at, format!("environment variable '{var_name}' is not set")))?;

                    let var_value = if lossy == Some(true) {
                        var_value.to_string_lossy().into_owned()
                    } else {
                        var_value
                            .to_str()
                            .ok_or_else(|| ctx.error(var_name_at, format!("environment variable '{var_name}' contains invalid UTF-8 characters: '{}'", var_value.to_string_lossy())))?
                            .to_owned()
                    };

                    Ok(Some(RuntimeValue::String(var_value)))
                }
            ),
            define_internal_fn!(
                //
                // Set an environment variable's value
                //

                "set_env",

                Args [ArgsAt] (
                    var_name: RequiredArg<StringType> = Arg::positional("var_name"),
                    value: RequiredArg<StringType> = Arg::positional("value")
                )

                -> None,

                |_, Args { var_name, value }, _, _| {
                    std::env::set_var(var_name, value);
                    Ok(None)
                }
            ),
            define_internal_fn!(
                //
                // Change the current directory
                //

                "cd",

                Args [ArgsAt] (
                    path: RequiredArg<StringType> = Arg::positional("path")
                )

                -> None,

                |at, Args { path }, ArgsAt { path: path_at }, ctx| {
                    let trimmed_path = path.trim_end_matches(['/', '\\']);

                    let path = Path::new(if trimmed_path.is_empty() { path.as_str() } else { trimmed_path });

                    if !path.is_dir() {
                        return Err(ctx.error(path_at, format!("directory '{}' does not exist", path.display())))
                    }

                    std::env::set_current_dir(path)
                        .map_err(|err| ctx.error(at, format!("failed to change current directory: {err}")))?;

                    Ok(None)
                }
            ),
            define_internal_fn!(
                //
                // Get the current number of colums of the terminal
                //

                "term_cols",

                Args [ArgsAt] ()

                -> Some(NullableType::<ExactIntType<usize>>::direct_underlying_type()),

                |_, _, _, _| {
                    let cols = match terminal_size::terminal_size() {
                        Some((Width(width), Height(_))) => RuntimeValue::Int(width as i64),
                        None => RuntimeValue::Null
                    };

                    Ok(Some(cols))
                }
            ),
            define_internal_fn!(
                //
                // Get the current number of rows of the terminal
                //

                "term_rows",

                Args [ArgsAt] ()

                -> Some(NullableType::<ExactIntType<usize>>::direct_underlying_type()),

                |_, _, _, _| {
                    let rows = match terminal_size::terminal_size() {
                        Some((Width(_), Height(height))) => RuntimeValue::Int(height as i64),
                        None => RuntimeValue::Null
                    };

                    Ok(Some(rows))
                }
            ),
            define_internal_fn!(
                //
                // Get the current directory
                //

                "current_dir",

                Args [ArgsAt] (
                    lossy: OptionalArg<BoolType> = Arg::long_flag("lossy")
                )

                -> Some(StringType::direct_underlying_type()),

                |at, Args { lossy }, _, ctx| {
                    let current_dir = std::env::current_dir()
                        .map_err(|err| ctx.error(at, format!("failed to get current directory: {err}")))?;

                    let current_dir = if lossy != Some(true) {
                        current_dir.to_str().ok_or_else(|| ctx.error(at, format!("current directoy contains invalid UTF-8 characters: '{}'", current_dir.display())))?
                            .to_string()
                    } else {
                        current_dir.to_string_lossy().to_string()
                    };

                    Ok(Some(RuntimeValue::String(current_dir)))
                }
            ),
            define_internal_fn!(
                //
                // Exit the program (optionally with an error code)
                //

                "exit",

                Args [ArgsAt] (
                    code: OptionalArg<ExactIntType<u8>> = Arg::positional("code")
                )

                -> None,

                |at, Args { code }, _, ctx| {
                    Err(ctx.exit(at, code))
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
