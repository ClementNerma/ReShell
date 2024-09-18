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
    error::Error,
    fs,
    path::{MAIN_SEPARATOR, MAIN_SEPARATOR_STR},
};

use glob::{glob_with, MatchOptions};
use reedline::{
    ColumnarMenu, Completer as RlCompleter, MenuBuilder, ReedlineMenu, Span, Suggestion,
};
use reshell_builtins::repl::completer::GeneratedCompletion;
use reshell_parser::DELIMITER_CHARS;
use reshell_runtime::{
    compat::{TargetFamily, PATH_VAR_SEP, TARGET_FAMILY},
    context::Context,
};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    repl::SHARED_CONTEXT,
    utils::{
        cmd_pieces::{compute_command_pieces, CmdPiece},
        jaro_winkler::jaro_winkler_distance,
        path_globifier::{globify_path, GlobPathOut, GlobPathStartsWith},
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
    }
}

type SortableSuggestion = (String, Suggestion);

pub fn generate_completions(
    line: &str,
    pos: usize,
    ctx: &mut Context,
    external_completer: Option<&ExternalCompleter>,
) -> Vec<Suggestion> {
    let cmd_pieces = compute_command_pieces(&line[..pos]);

    let CmdPiece {
        start: word_start,
        content: word,
    } = cmd_pieces.last().copied().unwrap_or(CmdPiece {
        start: 0,
        content: "",
    });

    let word_end = word_start + word.len();

    let span = Span {
        start: word_start,
        end: word_end,
    };

    let word = &line[word_start..word_end];

    let cmd_beginning = cmd_pieces.len() <= 1;

    let after_cmd_separator = line[..word_start].trim_end().ends_with(['|', ';'])
        && !line[..word_start].trim_end().ends_with("||");

    let next_char = line[word_end..].chars().next();

    if after_cmd_separator && !word.starts_with(['\'', '"']) {
        if let Some(s_word) = word.strip_prefix('.') {
            return sort_results(
                word,
                build_method_completions(s_word, Some("."), span, ctx).collect(),
            );
        }

        let mut cmd_comp =
            build_fn_completions(word, next_char, None, None, span, ctx).collect::<Vec<_>>();

        cmd_comp.extend(
            build_cmd_completions(word, next_char, span)
                .ok()
                .flatten()
                .unwrap_or_default(),
        );

        return sort_results(word, cmd_comp);
    }

    let mode = complete_special_instructions(&cmd_pieces);

    match mode {
        CompletionMode::None => return vec![],

        CompletionMode::Single(single) => {
            return vec![Suggestion {
                value: single.to_owned(),
                description: None,
                style: None,
                extra: None,
                span,
                append_whitespace: true,
            }]
        }

        CompletionMode::Default | CompletionMode::Expr | CompletionMode::CmdName => {}
    }

    if matches!(mode, CompletionMode::Default | CompletionMode::Expr) {
        if let Some(s_word) = word.strip_prefix('$') {
            return if s_word.chars().any(|c| !c.is_alphanumeric() && c != '_') {
                complete_path(&unescape_str(word), span, ctx)
            } else {
                complete_var_name(s_word, Some("$"), span, ctx)
            };
        }

        if let Some(s_word) = word.strip_prefix('@') {
            return sort_results(
                word,
                build_fn_completions(s_word, next_char, Some("@"), None, span, ctx).collect(),
            );
        }
    }

    if !word.contains(['/', '\\', '\'', '"']) {
        if matches!(mode, CompletionMode::CmdName)
            || (cmd_beginning && !matches!(mode, CompletionMode::Expr))
        {
            let mut cmd_comp = match mode {
                CompletionMode::CmdName => vec![],
                _ => build_fn_completions(word, next_char, None, None, span, ctx).collect(),
            };

            cmd_comp.extend(
                build_cmd_completions(word, next_char, span)
                    .ok()
                    .flatten()
                    .unwrap_or_default(),
            );

            return sort_results(word, cmd_comp);
        }

        if matches!(mode, CompletionMode::Expr) {
            return sort_results(
                word,
                build_fn_completions(word, next_char, None, Some("("), span, ctx).collect(),
            );
        }
    }

    if !cmd_beginning {
        if let Some(external_completer) = external_completer {
            // let cmd_start = cmd_pieces.first().map_or(0, |piece| piece.start);
            // let cmd = &line[cmd_start..pos];

            let cmd_pieces = cmd_pieces
                .iter()
                .map(|piece| unescape_str(piece.content))
                .collect::<Vec<_>>();

            let completions = external_completer(&cmd_pieces, ctx);

            if !completions.is_empty() {
                return completions
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
                    .collect();
            }
        }
    }

    if matches!(mode, CompletionMode::Default) {
        complete_path(&unescape_str(word), span, ctx)
    } else {
        vec![]
    }
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
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline()),
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
    add_prefix: Option<&str>,
    span: Span,
    ctx: &'a Context,
) -> impl Iterator<Item = SortableSuggestion> + 'a {
    let word = word.to_lowercase();
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
                            method.value.signature.inner().render_colored(
                                ctx.type_alias_store(),
                                PrettyPrintOptions::inline(),
                            ),
                        ),
                        style: None,
                        extra: None,
                        span,
                        append_whitespace: false,
                    },
                )
            })
        })
}

fn build_cmd_completions(
    word: &str,
    next_char: Option<char>,
    span: Span,
) -> Result<Option<Vec<SortableSuggestion>>, Box<dyn Error>> {
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
                            .render_colored(ctx.type_alias_store(), PrettyPrintOptions::inline()),
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

fn complete_path(path: &[UnescapedSegment], span: Span, ctx: &Context) -> Vec<Suggestion> {
    let Ok(globified) = globify_path(path, ctx) else {
        // TODO: error handling
        return vec![];
    };

    let GlobPathOut {
        glob_pattern,
        starts_with,
    } = globified;

    let Ok(entries) = glob_with(
        &glob_pattern,
        MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: false,
            require_literal_separator: true,
        },
    ) else {
        return vec![];
    };

    let paths = entries.collect::<Vec<_>>();

    let mut results = vec![];

    for path in paths {
        let Ok(path) = path else {
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

            Some(GlobPathStartsWith::HomeDirTilde(home_dir)) => escape_str(
                path_str
                    .strip_prefix(&format!("{home_dir}{MAIN_SEPARATOR}"))
                    .unwrap(),
                Some(&format!("~{MAIN_SEPARATOR}")),
            ),

            Some(GlobPathStartsWith::CurrentDir) => {
                escape_str(&path_str, Some(&format!(".{MAIN_SEPARATOR}")))
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

    sort_results(&glob_pattern, results)
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

#[derive(Debug)]
enum CompletionMode {
    Default,
    Single(&'static str),
    None,
    Expr,
    CmdName,
}

fn complete_special_instructions(cmd_pieces: &[CmdPiece]) -> CompletionMode {
    if cmd_pieces.len() == 1 {
        return CompletionMode::Default;
    }

    let Some(cmd_name) = cmd_pieces.first().map(|piece| piece.content) else {
        return CompletionMode::Default;
    };

    match cmd_name {
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
        //       => and ignore aliases / functions
        "^" => match cmd_pieces.len() {
            1 => CompletionMode::None,

            2 => CompletionMode::CmdName,

            _ => CompletionMode::Default,
        },

        // Not a special instruction
        _ => CompletionMode::Default,
    }
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

fn escape_str<'a>(str: &'a str, prefix: Option<&str>) -> Cow<'a, str> {
    if !str
        .chars()
        .chain(prefix.unwrap_or_default().chars())
        .any(|c| c.is_whitespace() || (DELIMITER_CHARS.contains(&c) && c != '$'))
    {
        match prefix {
            Some(prefix) => Cow::Owned(format!("{prefix}{str}")),
            None => Cow::Borrowed(str),
        }
    }
    //
    // Technically, we don't need to do this if the '$' is escaped
    // But for the sake of not developing a backslash-counting algorithm,
    // we don't handle it here (for now).
    else if str.contains('$') || prefix.is_some_and(|prefix| prefix.contains('$')) {
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
    //
    // If we don't need variables, we can use single-quote escaping
    else {
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
