use colored::Colorize;
use terminal_size::{terminal_size, Width};

use crate::{
    builder::BuiltinVar,
    define_internal_fn,
    helper::{Arg, ArgNames, ArgTypingDirectCreation, OptionalArg, RequiredArg},
    type_handlers::{
        AnyType, DetachedListType, ErrorType, ExactIntType, FloatType, IntType, MapType, NullType,
        RangeType, StringType, Tuple2Type, TypedFunctionType, Union2Result, Union2Type,
        Union3Result, Union3Type, UntypedListType, UntypedStructType,
    },
    utils::{call_fn_checked, forge_basic_fn_signature},
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

                Args [ArgsAt, ArgsTy] (
                    entries: OptionalArg<Union2Type<
                        UntypedStructType,
                        DetachedListType<Tuple2Type<StringType, AnyType>>
                    >> =>
                        Arg::positional("entries")
                )

                -> Some(MapType::direct_underlying_type()),

                |_, Args { entries }, ArgsAt { entries: entries_at }, _, _| {
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
                // Exit the program (optionally with an error code)
                //

                "exit",

                Args [ArgsAt, ArgsTy] (
                    code: OptionalArg<ExactIntType<u8>> => Arg::positional("code")
                )

                -> None,

                |at, Args { code }, _, _, ctx| {
                    Err(ctx.exit(at, code))
                }
            ),
            define_internal_fn!(
                //
                // Display a message
                //

                "echo",

                Args [ArgsAt, ArgsTy] (
                    message: RequiredArg<Union3Type<StringType, IntType, FloatType>> => Arg::positional("message")
                )

                -> None,

                |_, Args { message }, _, _, _| {
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

                Args [ArgsAt, ArgsTy] (
                    value: RequiredArg<AnyType> => Arg::positional("value"),
                    tab_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("tab-size"),
                    max_line_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("max-line-size")
                )

                -> None,

                |at, Args { value, tab_size, max_line_size }, _, _, ctx| {
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

                Args [ArgsAt, ArgsTy] (
                    value: RequiredArg<AnyType> => Arg::positional("value"),
                    tab_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("tab-size"),
                    max_line_size: OptionalArg<ExactIntType<usize>> => Arg::long_flag("max-line-size")
                )

                -> None,

                |at, Args { value, tab_size, max_line_size }, _, _, ctx| {
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

                Args [ArgsAt, ArgsTy] (
                    from: RequiredArg<ExactIntType<usize>> => Arg::positional("from"),
                    to: RequiredArg<ExactIntType<usize>> => Arg::positional("to")
                )

                -> Some(RangeType::direct_underlying_type()),

                |_, Args { from, to }, _, _, _| {
                    Ok(Some(RuntimeValue::Range { from, to }))
                }
            ),
            define_internal_fn!(
                //
                // Create an error value
                //

                "error",

                Args [ArgsAt, ArgsTy] (
                    content: RequiredArg<StringType> => Arg::positional("content")
                )

                -> Some(ErrorType::direct_underlying_type()),

                |at, Args { content }, _, _, _| {
                    Ok(Some(RuntimeValue::Error { at, msg: content }))
                }
            ),
            define_internal_fn!(
                //
                // Get the length of a string or the number of entries in a map / list
                //

                "len",

                Args [ArgsAt, ArgsTy] (
                    content: RequiredArg<Union3Type<StringType, UntypedListType, MapType>> => Arg::positional("value")
                )

                -> Some(ExactIntType::<usize>::direct_underlying_type()),

                |_, Args { content }, ArgsAt { content: content_at }, _, ctx| {
                    let len = match content {
                        Union3Result::A(str) => str.len(),
                        Union3Result::B(list) => list.read(content_at.unwrap()).len(),
                        Union3Result::C(map) => map.read(content_at.unwrap()).len(),
                    };

                    let len = i64::try_from(len).map_err(|_| ctx.error(content_at.unwrap_or_else(forge_internal_loc), format!("length is too big to fit in the integer type ({len})")))?;

                    Ok(Some(RuntimeValue::Int(len)))
                }
            ),
            define_internal_fn!(
                //
                // slice a list
                //

                "slice",

                Args [ArgsAt, ArgsTy] (
                    list: RequiredArg<UntypedListType> => Arg::positional("list"),
                    from: RequiredArg<ExactIntType<usize>> => Arg::positional("from"),
                    length: OptionalArg<ExactIntType<usize>> => Arg::positional("length")
                )

                -> Some(UntypedListType::direct_underlying_type()),

                |_, Args { list, from, length }, ArgsAt { list: list_at, .. }, _, _| {
                    let sliced = list.read(list_at.unwrap()).iter().skip(from).take(length.unwrap_or(usize::MAX)).cloned().collect::<Vec<_>>();

                    Ok(Some(RuntimeValue::List(GcCell::new(sliced))))
                }
            ),
            define_internal_fn!(
                //
                // pop a value from a list
                //

                "pop",

                Args [ArgsAt, ArgsTy] (
                    list: RequiredArg<UntypedListType> => Arg::positional("list")
                )

                -> Some(Union2Type::<AnyType, NullType>::direct_underlying_type()),

                |_, Args { list }, ArgsAt { list: list_at }, _, ctx| {
                    Ok(Some(list.write(list_at.unwrap(), ctx)?.pop().unwrap_or(RuntimeValue::Null)))
                }
            ),
            define_internal_fn!(
                //
                // map over a list
                //

                "listmap",

                Args [ArgsAt, ArgsTy] (
                    list: RequiredArg<UntypedListType> => Arg::positional("list"),
                    mapper: RequiredArg<TypedFunctionType> => Arg::new(ArgNames::Positional("mapper"), TypedFunctionType::new(forge_basic_fn_signature(
                        vec![
                            ("index", ExactIntType::<usize>::direct_underlying_type()),
                            ("value", AnyType::direct_underlying_type()),
                        ],
                        Some(AnyType::direct_underlying_type()
                    ))))
                )

                -> Some(UntypedListType::direct_underlying_type()),

                |_, Args { list, mapper }, ArgsAt { list: _, mapper: mapper_at }, ArgsTy { list: _, mapper: mapper_ty }, ctx| {
                    let mapper = LocatedValue::new(RuntimeValue::Function(mapper), mapper_at.unwrap_or_else(forge_internal_loc));

                    let mapped = list
                        .read(mapper_at.unwrap())
                        .iter()
                        .enumerate()
                        .map(|(index, value)| -> ExecResult<RuntimeValue> {
                            let ret = call_fn_checked(
                                &mapper,
                                mapper_ty.base_typing().signature(),
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
