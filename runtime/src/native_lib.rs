use std::fs;
use std::time::Instant;
use std::{collections::HashMap, env::VarError, path::Path};

use colored::Colorize;
use fork::{fork, Fork};
use glob::glob;
use indexmap::{IndexMap, IndexSet};
use parsy::{CodeRange, Eaten, FileId, Location, MaybeEaten, Parser};
use reshell_parser::ast::{FnArg, FnArgNames, FnSignature, SingleValueType, ValueType};
use reshell_parser::program;
use terminal_size::{terminal_size, Height, Width};

use crate::context::{Context, Scope, ScopeContent, ScopeFn, ScopeRange, ScopeVar};
use crate::display::dbg_loc;
use crate::errors::ExecResult;
use crate::files_map::ScopableFilePath;
use crate::functions::{call_fn_value, fail_if_thrown, FnPossibleCallArgs};
use crate::gc::GcCell;
use crate::pretty::{PrettyPrintOptions, PrettyPrintable};
use crate::typechecker::check_fn_equality;
use crate::values::{LocatedValue, RuntimeFnBody, RuntimeFnValue, RuntimeValue};

macro_rules! native_fn {
    ($name: ident ( $($arg_name: ident $({ rest: $rest_type: ident })? $((--$arg_long: ident))? $((-$arg_short: ident))?: $arg_type: ident $([$arg_from: ident])?),* ) $([$ctx: ident $(, $at: ident)?])? $(-> ($ret_type: ident $(| $ret_other_type: ident)*))? $body: block) => {{
        fn run(
            #[allow(unused_variables)]
            at: CodeRange,

            #[allow(unused_variables)]
            #[allow(unused_mut)]
            mut args: HashMap<String, LocatedValue>,

            #[allow(unused_variables)]
            ctx: &mut Context
        ) -> ExecResult<Option<LocatedValue>> {
            $(
                let arg = args.remove(stringify!($arg_name)).unwrap();

                let $arg_name = extract_arg_from_type!(arg, $arg_type $(, $rest_type)?);

                $( let $arg_from = arg.from; )?
            )*

            $(
                let $ctx = ctx;

                $( let $at = at; )?
            )?

            #[allow(unused_variables)]
            let ret: ExecResult<Option<RuntimeValue>> = $body;

            #[allow(unreachable_code)]
            ret.map(|ret| ret.map(|value| LocatedValue::new(value, forge_internal_loc())))
        }

        (
            stringify!($name).to_string(),
            ScopeFn {
                declared_at: forge_internal_token(()).at,
                value: GcCell::new(RuntimeFnValue {
                    signature: FnSignature {
                        args: vec![
                            $(
                                FnArg {
                                    names: forge_arg_names!($arg_name, $(--$arg_long)? $(-$arg_short)?),
                                    is_rest: arg_rest!($($rest_type)?),
                                    is_optional: false,
                                    typ: Some(forge_internal_token(ValueType::Single(MaybeEaten::Raw(SingleValueType::$arg_type)))),
                                }
                            ),*
                        ],

                        ret_type: value_type!($($ret_type $(| $ret_other_type)*)?)
                    },
                    body: RuntimeFnBody::Internal(run),
                    parent_scopes: IndexSet::new()
                }),
            }
        )
    }};
}

macro_rules! forge_arg_names {
    ($arg_name: ident, --$long_flag: ident -$short_flag: ident) => {
        FnArgNames::LongAndShortFlag {
            long: forge_internal_token(stringify!($long_flag)),
            short: forge_internal_token(stringify!($short_flag)),
        }
    };

    ($arg_name: ident, --$long_flag: ident) => {
        FnArgNames::LongFlag(forge_internal_token(stringify!($long_flag).to_string()))
    };

    ($arg_name: ident, -$short_flag: ident) => {
        FnArgNames::LongFlag(forge_internal_token(stringify!($short_flag).to_string()))
    };

    ($arg_name: ident,) => {
        FnArgNames::NotFlag(forge_internal_token(stringify!($arg_name).to_string()))
    };
}

macro_rules! extract_arg_from_type {
    ($arg: ident, Any $(, $rest: ident)?) => {
        $arg.value
    };

    ($arg: ident, UntypedStruct) => {
        match $arg.value {
            RuntimeValue::Struct(items) => items,
            _ => unreachable!(),
        }
    };

    ($arg: ident, List, $rest_type: ident) => {
        match $arg.value {
            RuntimeValue::List(items) => items
                .into_iter()
                .map(|item| match item {
                    RuntimeValue::$rest_type(string) => string,
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>(),
            _ => unreachable!(),
        }
    };

    ($arg: ident, $arg_type: ident $(, $rest: ident)?) => {
        match $arg.value {
            RuntimeValue::$arg_type(value) => value,
            _ => unreachable!(),
        }
    };
}

macro_rules! value_type {
    () => {
        None
    };

    ($typ: ident $(| $other_typ: ident),+) => {
        Some(forge_internal_token(Box::new(ValueType::Union(
            vec![ MaybeEaten::Raw(SingleValueType::$typ), $( MaybeEaten::Raw(SingleValueType::$other_typ) ),+ ]
        ))))
    };

    ($typ: ident) => {
        Some(forge_internal_token(Box::new(ValueType::Single(
            MaybeEaten::Raw(SingleValueType::$typ),
        ))))
    };
}

macro_rules! arg_rest {
    ($rest_type: ident) => {
        true
    };

    () => {
        false
    };
}

macro_rules! native_var {
    ($name: expr $(=> mut: $is_mut: expr)?, $value: expr) => {
        (
            $name.to_string(),
            ScopeVar {
                declared_at: forge_internal_loc(),
                is_mut: $($is_mut)?,
                value: Some(LocatedValue::new($value, forge_internal_loc())),
            },
        )
    };
}

pub fn generate_native_lib() -> Scope {
    let native_fns = [
        //
        // create a map
        //
        native_fn!(map (entries: UntypedStruct) -> (Map) {
            Ok(Some(RuntimeValue::Map(entries)))
        }),
        //
        // print text
        //
        native_fn!(print (text: String) {
            print!("{text}");
            Ok(None)
        }),
        //
        // print text + newline
        //
        native_fn!(println (text: String) {
            println!("{text}");
            Ok(None)
        }),
        //
        // debug a value
        //
        native_fn!(dbg (value: Any [at]) [ctx] {
            // TODO: disable color if we're not in a terminal
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
        }),
        //
        // create an error
        //
        native_fn!(error (msg: String [msg_from]) -> (Error) {
            Ok(Some(RuntimeValue::Error { at: msg_from, msg }))
        }),
        //
        // create a range
        //
        native_fn!(range (from: Int, to: Int) -> (Range) {
            Ok(Some(RuntimeValue::Range { from: from as usize, to: to as usize }))
        }),
        //
        // slice a list
        //
        native_fn!(slice (list: List, from: Int, to: Int) -> (List) {
            // TODO: track references
            let items = list.read().iter().skip(from as usize).take(to as usize).cloned().collect();

            Ok(Some(RuntimeValue::List(GcCell::new(items))))
        }),
        //
        // get the length of a list
        //
        native_fn!(count (list: List) -> (Int) {
            Ok(Some(RuntimeValue::Int(list.read().len() as i64)))
        }),
        //
        // get the length of a string
        //
        native_fn!(strlen (string: String) -> (Int) {
            Ok(Some(RuntimeValue::Int(string.len() as i64)))
        }),
        //
        // get a string as an array of characters
        //
        native_fn!(chars (string: String) -> (List) {
            Ok(Some(RuntimeValue::List(GcCell::new(string.chars().map(|c| RuntimeValue::String(c.to_string())).collect()))))
        }),
        //
        // get a substring
        //
        native_fn!(substr (string: String, from: Int, to: Int) -> (String) {
            Ok(Some(RuntimeValue::String(string.chars().skip(from as usize).take(to as usize).collect())))
        }),
        //
        // turn a string uppercase
        //
        native_fn!(uppercase (string: String) -> (String) {
            Ok(Some(RuntimeValue::String(string.to_uppercase())))
        }),
        //
        // turn a string lowercase
        //
        native_fn!(uppercase (string: String) -> (String) {
            Ok(Some(RuntimeValue::String(string.to_lowercase())))
        }),
        //
        // replace a string
        //
        native_fn!(replace (string: String, pattern: String, with: String) -> (String) {
            Ok(Some(RuntimeValue::String(string.replace(&pattern, &with))))
        }),
        //
        // read an environment variable
        //
        native_fn!(env (name: String [name_from]) [ctx] -> (String) {
            match std::env::var(&name) {
                Ok(value) => Ok(Some(RuntimeValue::String(value))),
                Err(VarError::NotPresent) => Err(ctx.error(name_from, format!("environment variable '{name}' is not set"))),
                Err(VarError::NotUnicode(_)) => Err(ctx.error(name_from, format!("environment variable '{name}' does not contain valid UTF-8 content")))
            }
        }),
        //
        // set an environment variable
        //
        native_fn!(set_env (name: String, value: String) {
            std::env::set_var(name, value);
            Ok(None)
        }),
        //
        // get path of the current script
        //
        native_fn!(current_script_path () [ctx] -> (String | Null) {
            Ok(Some(match ctx.current_file_path() {
                None => RuntimeValue::Null,
                Some(path) => RuntimeValue::String(path.to_string_lossy().to_string())
            }))
        }),
        //
        // get the width of the terminal
        //
        native_fn!(term_cols () -> (Int | Null) {
            Ok(Some(match terminal_size() {
                Some((Width(cols), Height(_))) => RuntimeValue::Int(cols.into()),
                None => RuntimeValue::Null
            }))
        }),
        //
        // get the height of the terminal
        //
        native_fn!(term_rows () -> (Int | Null) {
            Ok(Some(match terminal_size() {
                Some((Width(_), Height(rows))) => RuntimeValue::Int(rows.into()),
                None => RuntimeValue::Null
            }))
        }),
        //
        // run a closure detached from the terminal
        //
        native_fn!(detached (closure: Any [closure_at]) [ctx, at] {
            let fork = fork().map_err(|_| ctx.error(at, "failed to fork the process (unknown error occurred)"))?;

            if let Fork::Child = fork {
                call_fn_checked(&LocatedValue::new(closure, closure_at), &FnSignature { args: vec![], ret_type: None }, vec![], ctx)?;

                std::process::exit(0);
            }

            Ok(None)
        }),
        //
        // get PID of the current process
        //
        native_fn!(pid () -> (Int) {
            Ok(Some(RuntimeValue::Int(std::process::id().into())))
        }),
        //
        // change current directory
        //
        native_fn!(cd (path: String [path_at]) [ctx] {
            match std::env::set_current_dir(path) {
                Ok(()) => Ok(None),
                Err(err) => Err(ctx.error(path_at, format!("failed to change current directory: {err}")))
            }
        }),
        //
        // Include another script
        //
        native_fn!(include (path: String [path_at]) [ctx, at] {
            let from_dir = match ctx.current_file_path() {
                Some(path) => path.parent().unwrap().to_path_buf(),
                None => std::env::current_dir().map_err(|err| {
                    ctx.error(
                        at,
                        format!("failed to get current working directory: {err}"),
                    )
                })?,
            };

            let file_path = from_dir.join(path);

            if !file_path.exists() {
                return Err(ctx.error(
                    path_at,
                    format!("path '{}' does not exist", file_path.display()),
                ));
            }

            if !file_path.is_file() {
                return Err(ctx.error(
                    path_at,
                    format!("path '{}' exists but is not a file", file_path.display()),
                ));
            }

            let source = fs::read_to_string(&file_path).map_err(|err| {
                ctx.error(
                    at,
                    format!(
                        "failed to read content of file '{}': {}",
                        file_path.display(),
                        err
                    ),
                )
            })?;

            let file_id = ctx.register_file(ScopableFilePath::RealFile(file_path), source.clone());

            let parsed = program()
                .parse_str_as_file(&source, FileId::Id(file_id))
                .map_err(|err| ctx.error(at, err))?;

            // TODO: how to create a scope with an ID that doesn't conflict with existing scopes?
            todo!();

            // run_program(&parsed.data, ctx, Scope {
            //     id:
            // })?;
        }),
        //
        // check if path exists
        //
        native_fn!(pathExists (path: String) -> (Bool) {
            Ok(Some(RuntimeValue::Bool(Path::new(&path).exists())))
        }),
        //
        // check if file exists
        //
        native_fn!(fileExists (path: String) -> (Bool) {
            Ok(Some(RuntimeValue::Bool(Path::new(&path).is_file())))
        }),
        //
        // check if directory exists
        //
        native_fn!(dirExists (path: String) -> (Bool) {
            Ok(Some(RuntimeValue::Bool(Path::new(&path).is_dir())))
        }),
        //
        // list all items matching a glob
        //
        native_fn!(glob (pattern: String) [ctx, at] -> (List) {
            let files = glob(&pattern).map_err(|err| ctx.error(at, format!("failed to run glob: {err}")))?;

            Ok(Some(RuntimeValue::List(GcCell::new(
                files
                    .map(|entry| entry.map(|entry| {
                        RuntimeValue::String(
                            // TODO: what to do with lossy strings?
                            entry.to_string_lossy().into_owned()
                        )
                    }))
                    .collect::<Result<_, _>>()
                    .map_err(|err| ctx.error(at, format!("Failed to get informations on an item: {err}")))?
            ))))
        }),
        //
        // measure time taken by a closure to run (result is in milliseconds)
        //
        native_fn!(howlong (closure: Any [closure_at]) [ctx] -> (Int) {
            let start = Instant::now();

            call_fn_checked(&LocatedValue::new(closure, closure_at), &FnSignature { args: vec![], ret_type: None }, vec![], ctx)?;

            Ok(Some(RuntimeValue::Int(start.elapsed().as_millis() as i64)))
        }),
        //
        // exit the program
        //
        native_fn!(exit (code: Int) {
            std::process::exit(code as i32);
        }),
        // //
        // // create a directory
        // //
        // native_fn!(mkdir (paths { rest: String }: List, direct (-d): Bool, fail_if_exists (-f): Bool [f_at]) [ctx, at] {
        //     let res = paths.iter().try_for_each(|path| {
        //         let path = Path::new(&path);

        //         if path.exists() {
        //             if !path.is_dir() {
        //                 Err(ctx.error(at, "path already exists and is not a directory"))
        //             } else if fail_if_exists {
        //                 Err(ctx.error(f_at, "path already exists"))
        //             } else {
        //                 Ok(())
        //             }
        //         } else if direct {
        //             fs::create_dir(path).map_err(|err| ctx.error(at, format!("failed to create directory: {err}")))
        //         } else {
        //             fs::create_dir_all(path).map_err(|err| ctx.error(at, format!("failed to create directory: {err}")))
        //         }
        //     });

        //     res.map(|_| None)
        // }),
    ];

    let native_vars = [
        //
        // prompt generation function
        //
        native_var!(GEN_PROMPT_VAR_NAME => mut: true, RuntimeValue::Null),
    ];

    let mut content = ScopeContent::new();

    for (name, func) in native_fns {
        content.fns.insert(name, func);
    }

    for (name, var) in native_vars {
        content.vars.insert(name, GcCell::new(var));
    }

    Scope {
        id: 0,
        range: ScopeRange::Global,
        parent_scopes: IndexSet::new(),
        call_stack_entry: None,
        content,
    }
}

pub fn render_prompt(
    ctx: &mut Context,
    last_cmd_status: Option<LastCmdStatus>,
) -> ExecResult<Option<PromptRendering>> {
    let prompt_var = ctx
        .scopes()
        .get(&0)
        .unwrap()
        .content
        .vars
        .get(GEN_PROMPT_VAR_NAME)
        .unwrap()
        .clone();

    let prompt_var = prompt_var.read();

    let prompt_var_value = prompt_var.value.as_ref().unwrap();

    if matches!(prompt_var_value.value, RuntimeValue::Null) {
        return Ok(None);
    }

    let expected_signature = FnSignature {
        args: vec![FnArg {
            names: FnArgNames::NotFlag(forge_internal_token("prompt_data".to_string())),
            is_optional: false,
            is_rest: false,
            typ: Some(forge_internal_token(ValueType::Single(
                // TODO: fully typed struct
                MaybeEaten::Raw(SingleValueType::UntypedStruct),
            ))),
        }],
        ret_type: Some(forge_internal_token(Box::new(ValueType::Single(
            // TODO: fully typed struct
            MaybeEaten::Raw(SingleValueType::UntypedStruct),
        )))),
    };

    let last_cmd_status = match last_cmd_status {
        None => RuntimeValue::Null,
        Some(status) => {
            let LastCmdStatus {
                success,
                exit_code,
                duration_ms,
            } = status;

            RuntimeValue::Struct(GcCell::new(IndexMap::from([
                ("success".to_string(), RuntimeValue::Bool(success)),
                (
                    "exit_code".to_string(),
                    match exit_code {
                        Some(code) => RuntimeValue::Int(code.into()),
                        None => RuntimeValue::Null,
                    },
                ),
                (
                    "duration_ms".to_string(),
                    RuntimeValue::Int(duration_ms as i64),
                ),
            ])))
        }
    };

    let prompt_data = RuntimeValue::Struct(GcCell::new(IndexMap::from([(
        "last_cmd_status".to_string(),
        last_cmd_status,
    )])));

    let ret_val = call_fn_checked(
        prompt_var_value,
        &expected_signature,
        vec![prompt_data],
        ctx,
    )?;

    let ret_val = ret_val.ok_or_else(|| {
        ctx.error(
            prompt_var.declared_at,
            "prompt generation function did not return a value",
        )
    })?;

    let RuntimeValue::Struct(rendering) = ret_val.value else {
        return Err(ctx.error(
            ret_val.from,
            format!(
                "expected the prompt generation function to return a struct, found a {}",
                ret_val
                    .value
                    .get_type()
                    .render_colored(ctx, PrettyPrintOptions::inline())
            ),
        ));
    };

    macro_rules! get_options {
        ($from: ident @ $from_at: expr => $($ident: ident),+) => {{
            let mut out = PromptRendering::default();

            $(
                out.$ident = match $from.read().get(stringify!($ident)) {
                    None => return Err(ctx.error(
                        $from_at,
                        format!("missing option {} for prompt generation", stringify!($ident))
                    )),

                    Some(value) => match value {
                        RuntimeValue::Null => None,
                        RuntimeValue::String(string) => Some(string.clone()),
                        value => return Err(ctx.error(
                            $from_at,
                            format!("expected option {} to be a string for prompt generation, found a {}", stringify!($ident), value.get_type().render_colored(ctx, PrettyPrintOptions::inline()))
                        ))
                    }
                };
            )+

            // TODO: reject unknown keys?

            out
        }};
    }

    Ok(Some(get_options!(
        rendering @ ret_val.from =>
        prompt_left,
        prompt_right,
        prompt_indicator,
        prompt_multiline_indicator
    )))
}

fn call_fn_checked(
    loc_val: &LocatedValue,
    expected_signature: &FnSignature,
    args: Vec<RuntimeValue>,
    ctx: &mut Context,
) -> ExecResult<Option<LocatedValue>> {
    let func = match &loc_val.value {
        RuntimeValue::Function(func) => func,
        value => {
            return Err(ctx.error(
                loc_val.from,
                format!(
                    "type mismatch: expected a {}, found a {}",
                    expected_signature.render_colored(ctx, PrettyPrintOptions::inline()),
                    value
                        .get_type()
                        .render_colored(ctx, PrettyPrintOptions::inline())
                ),
            ))
        }
    };

    if !check_fn_equality(&func.read().signature, expected_signature, ctx)? {
        return Err(ctx.error(
            loc_val.from,
            format!(
                "type mismatch: expected a {}, found a {}",
                expected_signature.render_colored(ctx, PrettyPrintOptions::inline()),
                loc_val
                    .value
                    .get_type()
                    .render_colored(ctx, PrettyPrintOptions::inline())
            ),
        ));
    }

    let ret = call_fn_value(
        forge_internal_loc(),
        func,
        FnPossibleCallArgs::Direct {
            at: forge_internal_loc(),
            args: args
                .into_iter()
                .map(|arg| LocatedValue::new(arg, forge_internal_loc()))
                .collect(),
        },
        ctx,
    )?;

    fail_if_thrown(ret, ctx)
}

#[derive(Debug)]
pub struct LastCmdStatus {
    pub success: bool,
    pub exit_code: Option<i32>,
    pub duration_ms: u128,
}

#[derive(Default)]
pub struct PromptRendering {
    pub prompt_left: Option<String>,
    pub prompt_right: Option<String>,
    pub prompt_indicator: Option<String>,
    pub prompt_multiline_indicator: Option<String>,
    // prompt_history_search_indicator: Option<String>,
}

fn forge_internal_loc() -> CodeRange {
    CodeRange {
        start: Location {
            file_id: FileId::Internal,
            offset: 0,
        },
        len: 0,
    }
}

fn forge_internal_token<T>(data: T) -> Eaten<T> {
    Eaten {
        at: forge_internal_loc(),
        data,
    }
}

static GEN_PROMPT_VAR_NAME: &str = "gen_prompt";
