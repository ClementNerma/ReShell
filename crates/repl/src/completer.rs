//!
//! Completion module.
//!
//! Provides various functions for generating completions for a given input.
//!
//! Can complete command names, arguments content, variables, and paths.
//!

use std::{
    borrow::Cow,
    collections::HashSet,
    fs,
    path::{MAIN_SEPARATOR, MAIN_SEPARATOR_STR},
};

use reedline::{
    ColumnarMenu, Completer as RlCompleter, MenuBuilder, ReedlineMenu, Span, Suggestion,
};
use reshell_builtins::repl::completer::GeneratedCompletion;
use reshell_globby::{PatternOpts, glob_current_dir};
use reshell_parser::DELIMITER_CHARS;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};
use reshell_runtime::{
    compat::{PATH_VAR_SEP, TARGET_FAMILY, TargetFamily},
    context::Context,
};

use crate::{
    repl::SHARED_CONTEXT,
    utils::{
        cmd_pieces::{CmdPiece, compute_command_pieces},
        jaro_winkler::jaro_winkler_distance,
        path_globifier::{GlobPathOut, GlobPathStartsWith, globify_path},
    },
};

pub static COMPLETION_MENU_NAME: &str = "completion_menu";

pub fn create_completer(external_completer: Option<ExternalCompleter>) -> Box<dyn RlCompleter> {
    Box::new(Completer { external_completer })
}

pub fn create_completion_menu() -> ReedlineMenu {
    let menu = ColumnarMenu::default().with_name(COMPLETION_MENU_NAME);
    ReedlineMenu::EngineCompleter(Box::new(menu))
}

pub type ExternalCompleter =
    Box<dyn Fn(&[Vec<UnescapedSegment>], &mut Context) -> Vec<ExternalCompletion> + Send + Sync>;

#[derive(Debug)]
pub struct ExternalCompletion {
    pub description: String,
    pub value: String,
}

impl From<GeneratedCompletion> for ExternalCompletion {
    fn from(value: GeneratedCompletion) -> Self {
        let GeneratedCompletion { description, value } = value;
        Self { description, value }
    }
}

pub struct Completer {
    external_completer: Option<ExternalCompleter>,
}

impl RlCompleter for Completer {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        generate_completions(
            line,
            pos,
            SHARED_CONTEXT.lock().unwrap().as_mut().unwrap(),
            self.external_completer.as_ref(),
        )
        // TODO: error reporting
        .ok()
        .unwrap_or_default()
    }
}

type SortableSuggestion = (String, Suggestion);

pub fn generate_completions(
    line: &str,
    pos: usize,
    ctx: &mut Context,
    external_completer: Option<&ExternalCompleter>,
) -> Result<Vec<Suggestion>, String> {
    // Parse the last command present in the provided line
    // (up to the cursor's position)
    // into a list made of the command name and its arguments
    let cmd_pieces = compute_command_pieces(&line[..pos]);

    // Get the word the cursor is currently positioned at
    let CmdPiece {
        start: word_start,
        // This contains the word until the cursor's position,
        // so it may be incomplete
        content: word,
    } = cmd_pieces.last().copied().unwrap_or(CmdPiece {
        start: 0,
        content: "",
    });

    // Compute the word's end location
    let word_end = word_start + word.len();

    // Make a span that delimitates the word in the provided line string
    let span = Span {
        start: word_start,
        end: word_end,
    };

    // Extract the word
    let word = &line[word_start..word_end];

    // Check if the current word is a command's beginning,
    // which means it's a command's name and not an argument
    let cmd_beginning = cmd_pieces.len() <= 1;

    // Check if the word we're completing is a command name that's piped another command
    let is_pipe_cmd_name = line[..word_start].trim_end().ends_with('|')
        && !line[..word_start].trim_end().ends_with("||");

    // Safety check for the logic
    if is_pipe_cmd_name {
        assert!(cmd_beginning);
    }

    let mode = if is_pipe_cmd_name {
        // Check if we're completing a special instruction
        // e.g. a loop, a variable declaration, etc.
        //
        // If so, this is incorrect as we're currently trying to complete a command
        // that comes after a pipe (|).
        //
        // As a result, we return an empty set of completions (as the current content is invalid anyway)
        if complete_special_instructions(&cmd_pieces).is_some() {
            return Ok(vec![]);
        }

        // Otherwise, we try to complete the command's name
        CompletionMode::CmdName
    } else {
        complete_special_instructions(&cmd_pieces).unwrap_or(CompletionMode::Any)
    };

    // Get the character coming right after the word's end
    let next_char = line[word_end..].chars().next();

    match mode {
        // Return no completion result
        CompletionMode::None => return Ok(vec![]),

        // Return a single completion result
        CompletionMode::Single(single) => {
            return Ok(vec![Suggestion {
                value: single.to_owned(),
                description: None,
                style: None,
                extra: None,
                span,
                append_whitespace: true,
            }]);
        }

        // Complete as a command's name
        CompletionMode::CmdName => return complete_cmd_name(word, next_char, span, ctx),

        // Complete as an external command's name
        CompletionMode::ExternalCmdName => {
            let results = build_external_cmd_completions(word, next_char, span)
                .ok()
                .flatten()
                .unwrap_or_default();

            return Ok(sort_results(word, results));
        }

        // Complete as an expression or normally
        CompletionMode::Expr | CompletionMode::Any => {
            // Try to complete a variable's name
            if let Some(s_word) = word.strip_prefix('$') {
                return if s_word.chars().any(|c| !c.is_alphanumeric() && c != '_') {
                    complete_path(&unescape_str(word), span, ctx)
                } else {
                    Ok(complete_var_name(s_word, Some("$"), span, ctx))
                };
            }

            // Try to complete a function-as-a-value's name
            if let Some(s_word) = word.strip_prefix('@') {
                return Ok(sort_results(
                    word,
                    build_fn_completions(s_word, next_char, Some("@"), None, span, ctx).collect(),
                ));
            }

            // Otherwise, if the current word does not contain a path separator or a string delimiter...
            if !word.contains(['/', '\\', '\'', '"']) {
                match mode {
                    CompletionMode::Any => {
                        // Then it may be a command's name
                        if cmd_beginning {
                            return complete_cmd_name(word, next_char, span, ctx);
                        }
                    }

                    // Or it may be a function's name inside an expression
                    // (external commands, aliases and paths are nonsense here)
                    //
                    // If it's not, then we can't perform any completion
                    CompletionMode::Expr => {
                        return Ok(sort_results(
                            word,
                            build_fn_completions(word, next_char, None, Some("("), span, ctx)
                                .collect(),
                        ));
                    }

                    _ => unreachable!(),
                }
            }
        }
    };

    // If we're in a command's argument...
    if !cmd_beginning {
        // Bring the (optional) external completer
        if let Some(external_completer) = external_completer {
            // Unescape all strings to simplify to allow the external completer
            // (which doesn't know ReShell's syntax) to work correctly
            let cmd_pieces = cmd_pieces
                .iter()
                .map(|piece| unescape_str(piece.content))
                .collect::<Vec<_>>();

            // Call the external completer
            let completions = external_completer(&cmd_pieces, ctx);

            // If it returned some results...
            if !completions.is_empty() {
                // Return them
                return Ok(completions
                    .into_iter()
                    .map(|comp| {
                        let ExternalCompletion { value, description } = comp;

                        Suggestion {
                            value: escape_str(&value, None).into_owned(),
                            description: Some(description),
                            style: None,
                            extra: None,
                            span,
                            append_whitespace: true,
                        }
                    })
                    .collect());
            }
        }
    }

    match mode {
        // If everything else failed, try to complete the current word as a path
        CompletionMode::Any => complete_path(&unescape_str(word), span, ctx),

        CompletionMode::None
        | CompletionMode::Single(_)
        | CompletionMode::Expr
        | CompletionMode::CmdName
        | CompletionMode::ExternalCmdName => unreachable!(),
    }
}

fn complete_cmd_name(
    word: &str,
    next_char: Option<char>,
    span: Span,
    ctx: &Context,
) -> Result<Vec<Suggestion>, String> {
    // First try to complete as a method's name
    if let Some(word) = word.strip_prefix('.') {
        let comp =
            build_method_completions(word, next_char, Some("."), span, ctx).collect::<Vec<_>>();

        // If we've got at least one result, return it
        if !comp.is_empty() {
            return Ok(sort_results(word, comp));
        }
    }

    // Complete as a function's name...
    let mut cmd_comp =
        build_fn_completions(word, next_char, None, None, span, ctx).collect::<Vec<_>>();

    // ...and as an external command's name
    if let Some(cmds) = build_external_cmd_completions(word, next_char, span)? {
        cmd_comp.extend(cmds);
    }

    // Return the results (even if we've got nothing)
    Ok(sort_results(word, cmd_comp))
}

fn build_fn_completions<'a>(
    word: &str,
    next_char: Option<char>,
    add_prefix: Option<&'static str>,
    add_suffix: Option<&'static str>,
    span: Span,
    ctx: &'a Context,
) -> impl Iterator<Item = SortableSuggestion> + 'a {
    let word = word.to_lowercase();

    let append_whitespace =
        add_suffix.is_none() && next_char != Some(' ') && next_char != Some('(');

    let mut used_names = HashSet::new();

    ctx.visible_scopes_content()
        .flat_map(|scope| scope.fns.iter())
        .filter(move |(name, _)| name.to_lowercase().contains(&word))
        .filter(move |(name, _)| used_names.insert(*name))
        .map(move |(name, func)| {
            (
                name.clone(),
                Suggestion {
                    value: format!(
                        "{}{name}{}",
                        add_prefix.unwrap_or_default(),
                        add_suffix.unwrap_or_default()
                    ),
                    description: Some(
                        func.value
                            .signature
                            .inner()
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            .to_string(),
                    ),
                    style: None,
                    extra: None,
                    span,
                    append_whitespace,
                },
            )
        })
}

fn build_method_completions<'a>(
    word: &str,
    next_char: Option<char>,
    add_prefix: Option<&str>,
    span: Span,
    ctx: &'a Context,
) -> impl Iterator<Item = SortableSuggestion> + 'a {
    let word = word.to_lowercase();

    let append_whitespace = next_char != Some(' ') && next_char != Some('(');

    let add_prefix = add_prefix.map(str::to_owned);

    ctx.visible_scopes_content()
        .flat_map(|scope| scope.methods.iter())
        .filter(move |(name, _)| name.to_lowercase().contains(&word))
        .flat_map(move |(name, methods)| {
            let add_prefix = add_prefix.clone();

            methods.iter().map(move |method| {
                (
                    name.clone(),
                    Suggestion {
                        value: match add_prefix {
                            Some(ref prefix) => format!("{prefix}{name}"),
                            None => name.clone(),
                        },
                        description: Some(
                            method
                                .value
                                .signature
                                .inner()
                                .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                                .to_string(),
                        ),
                        style: None,
                        extra: None,
                        span,
                        append_whitespace,
                    },
                )
            })
        })
}

fn build_external_cmd_completions(
    word: &str,
    next_char: Option<char>,
    span: Span,
) -> Result<Option<Vec<SortableSuggestion>>, String> {
    if word.contains('/') || word.contains('\\') {
        return Ok(None);
    }

    let word = word.to_lowercase();

    let path = std::env::var_os("PATH").ok_or("PATH variable is not set")?;
    let path = path
        .to_str()
        .ok_or("PATH variable contains is not a valid UTF-8 string")?;

    let path_dirs = path.split(PATH_VAR_SEP).filter(|entry| !entry.is_empty());

    let append_whitespace = next_char != Some(' ');

    let mut indexed = HashSet::new();
    let mut results = Vec::<(String, Suggestion)>::new();

    for dir in path_dirs {
        let Ok(entries) = fs::read_dir(dir) else {
            continue;
        };

        for item in entries {
            let Ok(item) = item else {
                continue;
            };

            let Ok(file_type) = item.file_type() else {
                continue;
            };

            if !file_type.is_file() && !file_type.is_symlink() {
                continue;
            }

            let item_name = item.file_name();

            let Some(item_name) = item_name.to_str() else {
                continue;
            };

            let item_name_lc = item_name.to_lowercase();

            if !item_name_lc.contains(&word) {
                continue;
            }

            match TARGET_FAMILY {
                TargetFamily::Windows => {
                    let Some((_, ext)) = item_name_lc.rsplit_once('.') else {
                        continue;
                    };

                    if ext != "bat" && ext != "cmd" && ext != "exe" {
                        continue;
                    }

                    if !indexed.insert(item_name.to_owned()) {
                        continue;
                    }
                }

                TargetFamily::Unix => {
                    if !indexed.insert(item_name_lc) {
                        continue;
                    }
                }
            }

            results.push((
                item_name.to_owned(),
                Suggestion {
                    value: escape_str(item_name, None).into_owned(),
                    description: None,
                    style: None,
                    extra: None,
                    span,
                    append_whitespace,
                },
            ))
        }
    }

    Ok(Some(results))
}

fn complete_var_name(
    word: &str,
    add_prefix: Option<&str>,
    span: Span,
    ctx: &Context,
) -> Vec<Suggestion> {
    let word = word.to_lowercase();

    let mut used_names = HashSet::new();

    let results = ctx
        .visible_scopes_content()
        .flat_map(|scope| scope.vars.iter())
        .filter(|(name, _)| name.to_lowercase().contains(&word))
        .filter(|(name, _)| used_names.insert(*name))
        .map(|(name, item)| {
            (
                name.clone(),
                Suggestion {
                    value: match add_prefix {
                        Some(prefix) => format!("{prefix}{name}"),
                        None => name.clone(),
                    },
                    description: Some(
                        item.value
                            .read_promise_no_write()
                            .value
                            .compute_type()
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
                            .to_string(),
                    ),
                    style: None,
                    extra: None,
                    span,
                    append_whitespace: false,
                },
            )
        })
        .collect::<Vec<_>>();

    sort_results(&word, results)
}

fn complete_path(
    path: &[UnescapedSegment],
    span: Span,
    ctx: &Context,
) -> Result<Vec<Suggestion>, String> {
    let globified =
        globify_path(path, ctx).map_err(|err| format!("Failed to globify path {path:?}: {err}"))?;

    let GlobPathOut {
        glob_pattern,
        starts_with,
    } = globified;

    let entries = glob_current_dir(
        &glob_pattern,
        PatternOpts {
            case_insensitive: true,
        },
    )
    .map_err(|err| format!("Failed to get glob results for path {path:?}: {err}"))?;

    let paths = entries.collect::<Vec<_>>();

    let mut results = vec![];

    for entry in paths {
        let Ok(path) = entry else {
            continue;
        };

        let Ok(metadata) = path.metadata() else {
            continue;
        };

        let file_type = metadata.file_type();

        // NOTE: Invalid UTF-8 paths will be displayed lossily (no other way to handle this)
        let mut path_str = path.to_string_lossy().to_string();

        if file_type.is_dir() {
            path_str.push(MAIN_SEPARATOR);
        }

        let value = match &starts_with {
            Some(GlobPathStartsWith::Variable { name, value }) => {
                let corrected_value = value.as_str().replace(['/', '\\'], MAIN_SEPARATOR_STR);

                match path_str.strip_prefix(&corrected_value) {
                    Some(stripped_path) => escape_str(stripped_path, Some(&format!("${name}"))),
                    None => escape_str(&path_str, None),
                }
            }

            // If the path after the home dir portion doesn't need escaping, we can use a tilde `~`
            // Otherwise, we need to use the home directory's full path
            Some(GlobPathStartsWith::HomeDirTilde(home_dir)) => {
                let stripped_path = path_str
                    .strip_prefix(&format!("{home_dir}{MAIN_SEPARATOR}"))
                    .unwrap();

                if needs_escaping(stripped_path) == EscapingType::None {
                    Cow::Owned(format!("~{MAIN_SEPARATOR}{stripped_path}"))
                } else {
                    Cow::Owned(
                        escape_str(&format!("{home_dir}/{stripped_path}"), None).into_owned(),
                    )
                }
            }

            Some(GlobPathStartsWith::CurrentDir) => {
                if !path_str.starts_with("../") && !path_str.starts_with("..\\") {
                    escape_str(&path_str, Some(&format!(".{MAIN_SEPARATOR}")))
                } else {
                    escape_str(&path_str, None)
                }
            }

            None => escape_str(&path_str, None),
        };

        let value = value.into_owned();

        results.push((
            path_str,
            Suggestion {
                value,
                description: None,
                style: None,
                extra: None,
                span,
                append_whitespace: !file_type.is_dir(),
            },
        ));
    }

    Ok(sort_results(&glob_pattern, results))
}

fn sort_results(input: &str, values: Vec<(String, Suggestion)>) -> Vec<Suggestion> {
    let input = input.replace(['*', '?', '[', ']'], "");

    let input_lc = input.to_lowercase();

    let (mut matching_start, mut non_matching_start): (Vec<_>, Vec<_>) = values
        .into_iter()
        .partition(|(value, _)| value.to_lowercase().starts_with(&input_lc));

    matching_start.sort_by(|(a, _), (b, _)| {
        jaro_winkler_distance(&input, a)
            .partial_cmp(&jaro_winkler_distance(&input, b))
            .unwrap()
    });

    // If we have suggestions that have a common start...
    if !matching_start.is_empty() {
        // Ignore all other ones
        non_matching_start.clear();
    } else {
        // Otherwise, sort the non-start-matching suggestions as well
        non_matching_start.sort_by(|(a, _), (b, _)| {
            jaro_winkler_distance(&input, a)
                .partial_cmp(&jaro_winkler_distance(&input, b))
                .unwrap()
        });
    }

    matching_start
        .into_iter()
        .chain(non_matching_start)
        .map(|(_, value)| value)
        .collect()
}

/// Description of how to complete a provided input
#[derive(Debug)]
enum CompletionMode {
    /// Complete without doing anything special
    Any,

    // No completion is possible
    None,

    /// Only a single completion is possible
    Single(&'static str),

    // Complete as an expression
    Expr,

    // Command name (internal or external)
    CmdName,

    // Complete as an external command's name
    ExternalCmdName,
}

fn complete_special_instructions(cmd_pieces: &[CmdPiece]) -> Option<CompletionMode> {
    if cmd_pieces.len() == 1 {
        return None;
    }

    let cmd_name = cmd_pieces.first()?.content;

    let comp = match cmd_name {
        "for" => {
            match cmd_pieces.len() {
                // for [<...>]
                2 => CompletionMode::None,

                // for <...> <in?>
                3 => CompletionMode::Single("in"),

                // for <...> <in?> <...>
                _ => CompletionMode::Expr,
            }
        }

        "if" | "while" | "do" | "try" | "throw" | "match" | "return" => CompletionMode::Expr,

        "continue" | "break" | "fn" => CompletionMode::None,

        "alias" => {
            match cmd_pieces.len() {
                // alias [<...>]
                2 => CompletionMode::None,

                // alias <...> <=?>
                3 => CompletionMode::Single("="),

                _ => {
                    // TODO: complete like usual but with "alias <...> =" stripped
                    CompletionMode::Expr
                }
            }
        }

        "type" => {
            match cmd_pieces.len() {
                // alias [<...>]
                2 => CompletionMode::None,

                // alias <...> <=?>
                3 => CompletionMode::Single("="),

                _ => {
                    // TODO: complete like usual but with "type <...> =" stripped
                    CompletionMode::None
                }
            }
        }

        "include" => match cmd_pieces.len() {
            // include <...>
            2 => {
                // TODO: wrap in single quotes
                CompletionMode::None
            }

            // include <...> ...
            _ => CompletionMode::None,
        },

        // TODO: improve this
        "let" => match cmd_pieces.len() {
            2 => CompletionMode::None,

            // let <...> <=?>
            3 => CompletionMode::Single("="),

            _ => CompletionMode::Expr,
        },

        // TODO: complete like usual but with "^" stripped
        //       and add the "^"  back
        //       and ignore command aliases
        "^" => match cmd_pieces.len() {
            1 => CompletionMode::None,

            2 => CompletionMode::ExternalCmdName,

            _ => CompletionMode::Any,
        },

        // Not a special instruction
        _ => return None,
    };

    Some(comp)
}

#[derive(Debug)]
pub enum UnescapedSegment {
    VariableName(String),
    String(String),
}

fn unescape_str(str: &str) -> Vec<UnescapedSegment> {
    let mut out = vec![];

    let mut segment = String::new();
    let mut escaping = false;
    let mut reading_var_name = None::<String>;

    let opening_char = str.chars().next().filter(|c| *c == '\'' || *c == '"');

    for c in str.chars().skip(if opening_char.is_some() { 1 } else { 0 }) {
        if escaping {
            segment.push(c);
            escaping = false;
            continue;
        }

        if reading_var_name.is_some() {
            if c.is_alphanumeric() || c == '_' {
                reading_var_name.as_mut().unwrap().push(c);
                continue;
            }

            out.push(UnescapedSegment::VariableName(
                reading_var_name.take().unwrap(),
            ));
        }

        // Handle backslashes in strings
        if opening_char.is_some() && c == '\\' {
            escaping = true;
        }
        // Handle string closing
        else if opening_char == Some(c) {
            // NOTE: the assertion below was removed to account for the following scenario:
            // `ls /tmp/<TAB>` (user asks for completion)
            // `ls '/tmp/a b'` (suggestion selected)
            // 'ls '/tmp/a b'/c<TAB>` (user continues input then asks for completion again)
            // => previously, panicked because the closing single quote wasn't at the end of the string
            // assert_eq!(i + 1, str.chars().count());
        }
        // Handle dollar symbols in non-single-quoted strings (= double quoted + raw)
        else if c == '$' && matches!(opening_char, Some('"') | None) {
            if !segment.is_empty() {
                out.push(UnescapedSegment::String(segment));
                segment = String::new();
            }

            reading_var_name = Some(String::new());
        } else {
            segment.push(c);
        }
    }

    if !segment.is_empty() {
        out.push(UnescapedSegment::String(segment));
    }

    out
}

// [`Ord`] is derived to enable comparing two escaping types together
// If we have e.g. a string that needs double quoting and another that needs simple quoting,
// and we want to concatenate both, then we need the highest one => double quoting
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum EscapingType {
    None,
    SingleQuotes,
    DoubleQuotes,
}

fn needs_escaping(str: &str) -> EscapingType {
    if !str
        .chars()
        // TODO: in path completion for instance, if we use a variable it shouldn't be escapeded
        // but if the dir / file name contains a '$' it SHOULD be escaped
        // find a way to handle that
        .any(|c| c.is_whitespace() || (DELIMITER_CHARS.contains(&c) && c != '$'))
    {
        EscapingType::None
    }
    //
    // Technically, we don't need to do this if the '$' is escaped
    // But for the sake of not developing a backslash-counting algorithm,
    // we don't handle it here (for now).
    else if str.contains('$') {
        EscapingType::DoubleQuotes
    }
    //
    // If we don't need variables, we can use single-quote escaping
    else {
        EscapingType::SingleQuotes
    }
}

fn escape_str<'a>(str: &'a str, prefix: Option<&str>) -> Cow<'a, str> {
    let escaping_type = match prefix {
        Some(prefix) => needs_escaping(&format!("{prefix}{str}")),
        None => needs_escaping(str),
    };

    match escaping_type {
        EscapingType::None => match prefix {
            Some(prefix) => Cow::Owned(format!("{prefix}{str}")),
            None => Cow::Borrowed(str),
        },

        EscapingType::DoubleQuotes => {
            let mut escaped = String::with_capacity(str.len());

            escaped.push('"');

            if let Some(prefix) = prefix {
                escaped.push_str(prefix);
            }

            for c in str.chars() {
                if c == '"' || c == '$' || c == '\\' {
                    escaped.push('\\');
                }

                escaped.push(c);
            }

            escaped.push('"');

            Cow::Owned(escaped)
        }

        EscapingType::SingleQuotes => {
            let mut escaped = String::with_capacity(str.len());
            escaped.push('\'');

            if let Some(prefix) = prefix {
                escaped.push_str(prefix);
            }

            for c in str.chars() {
                if c == '\'' || c == '\\' {
                    escaped.push('\\');
                }

                escaped.push(c);
            }

            escaped.push('\'');

            Cow::Owned(escaped)
        }
    }
}
