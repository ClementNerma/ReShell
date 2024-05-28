use std::{
    borrow::Cow,
    collections::HashSet,
    error::Error,
    fs,
    path::{MAIN_SEPARATOR, MAIN_SEPARATOR_STR},
    sync::LazyLock,
};

use glob::{glob_with, MatchOptions};
use reedline::{
    ColumnarMenu, Completer as RlCompleter, MenuBuilder, ReedlineMenu, Span, Suggestion,
};
use regex::{Captures, Regex};
use reshell_parser::DELIMITER_CHARS;
use reshell_runtime::{
    compat::{TargetFamily, PATH_VAR_SEP, TARGET_FAMILY},
    context::Context,
    pretty::{PrettyPrintOptions, PrettyPrintable},
    values::RuntimeValue,
};

use crate::utils::jaro_winkler::jaro_winkler_distance;

pub static COMPLETION_MENU_NAME: &str = "completion_menu";

pub fn create_completer(
    completer: impl Fn(CompletionContext) -> Vec<Suggestion> + Send + 'static,
) -> Box<dyn RlCompleter> {
    Box::new(Completer { completer })
}

pub fn create_completion_menu() -> ReedlineMenu {
    let menu = ColumnarMenu::default().with_name(COMPLETION_MENU_NAME);
    ReedlineMenu::EngineCompleter(Box::new(menu))
}

pub struct CompletionContext {
    pub line: String,
    pub pos: usize,
}

pub struct Completer<F: Fn(CompletionContext) -> Vec<Suggestion> + Send + 'static> {
    completer: F,
}

impl<F: Fn(CompletionContext) -> Vec<Suggestion> + Send + 'static> RlCompleter for Completer<F> {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        (self.completer)(CompletionContext {
            line: line.to_owned(),
            pos,
        })
    }
}

type SortableSuggestion = (String, Suggestion);

pub fn generate_completions(
    completion_context: CompletionContext,
    ctx: &Context,
) -> Vec<Suggestion> {
    let CompletionContext { line, pos } = completion_context;

    let split = |c: char| c.is_whitespace() && !DELIMITER_CHARS.contains(&c);

    let word_start = match line[..pos].rfind(split) {
        Some(index) => index + 1,
        None => 0,
    };

    let word_end = match line[word_start..pos].find(split) {
        Some(index) => index,
        None => pos,
    };

    let after_space = word_start > 0
        && matches!(line[word_start - 1..].chars().next(), Some(c) if c.is_whitespace());

    let word = &line[word_start..word_end];

    let span = Span {
        start: word_start,
        end: word_end,
    };

    if word == "~" {
        return vec![];
    }

    let next_char = line[word_end..].chars().next();

    if let Some(s_word) = word.strip_prefix('$') {
        return if s_word.chars().any(|c| !c.is_alphanumeric() && c != '_') {
            complete_path(word, span, ctx)
        } else {
            complete_var_name(s_word, Some("$"), span, ctx)
        };
    }

    if let Some(s_word) = word.strip_prefix('@') {
        return complete_fn_name(s_word, next_char, Some("@"), span, ctx);
    }

    if line[..word_start].trim_end().ends_with(" |") {
        let mut cmd_comp = build_cmd_completions(word, next_char, span)
            .ok()
            .flatten()
            .unwrap_or_default();

        cmd_comp.extend(build_fn_completions(word, next_char, None, span, ctx));
        cmd_comp.extend(build_method_completions(word, next_char, None, span, ctx));

        return sort_results(word, cmd_comp);
    }

    if !after_space && !word.contains(['/', '\\']) {
        let mut cmd_comp = build_cmd_completions(word, next_char, span)
            .ok()
            .flatten()
            .unwrap_or_default();

        cmd_comp.extend(build_fn_completions(word, next_char, None, span, ctx));

        return sort_results(word, cmd_comp);
    }

    complete_path(word, span, ctx)
}

fn build_fn_completions<'a>(
    word: &str,
    next_char: Option<char>,
    add_prefix: Option<&str>,
    span: Span,
    ctx: &'a Context,
) -> impl Iterator<Item = SortableSuggestion> + 'a {
    let word = word.to_lowercase();
    let add_prefix = add_prefix.map(str::to_owned);

    let append_whitespace = next_char != Some(' ') && next_char != Some('(');

    ctx.visible_scopes_content()
        .flat_map(|scope| scope.fns.iter())
        .filter(move |(name, _)| name.to_lowercase().contains(&word))
        .map(move |(name, func)| {
            (
                name.clone(),
                Suggestion {
                    value: match add_prefix {
                        Some(ref prefix) => format!("{prefix}{name}"),
                        None => name.clone(),
                    },
                    description: Some(
                        func.value
                            .signature
                            .inner()
                            .render_colored(ctx, PrettyPrintOptions::inline()),
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
    let add_prefix = add_prefix.map(str::to_owned);

    let append_whitespace = next_char != Some(' ') && next_char != Some('(');

    ctx.visible_scopes_content()
        .flat_map(|scope| scope.methods.iter())
        .filter(move |((name, _), _)| name.to_lowercase().contains(&word))
        .map(move |((name, _), func)| {
            (
                name.clone(),
                Suggestion {
                    value: match add_prefix {
                        Some(ref prefix) => format!("{prefix}{name}"),
                        None => name.clone(),
                    },
                    description: Some(
                        func.value
                            .signature
                            .inner()
                            .render_colored(ctx, PrettyPrintOptions::inline()),
                    ),
                    style: None,
                    extra: None,
                    span,
                    append_whitespace,
                },
            )
        })
}

fn complete_fn_name(
    word: &str,
    next_char: Option<char>,
    add_prefix: Option<&str>,
    span: Span,
    ctx: &Context,
) -> Vec<Suggestion> {
    sort_results(
        word,
        build_fn_completions(word, next_char, add_prefix, span, ctx).collect(),
    )
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
                    value: escape_raw(item_name).into_owned(),
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

    let results = ctx
        .visible_scopes_content()
        .flat_map(|scope| scope.vars.iter())
        .filter(|(name, _)| name.to_lowercase().contains(&word))
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
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline()),
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

fn complete_path(word: &str, span: Span, ctx: &Context) -> Vec<Suggestion> {
    let mut failed = false;
    let mut start_var = None;

    let word = VAR_IN_PATH_REGEX.replace_all(word, |cap: &Captures<'_>| {
        let var_name = cap.get(1).unwrap().as_str();

        for scope in ctx.visible_scopes_content() {
            if let Some(var) = scope.vars.get(var_name) {
                let value = var.value.read_promise_no_write();

                let value = match &value.value {
                    RuntimeValue::Int(int) => int.to_string(),
                    RuntimeValue::Float(float) => float.to_string(),
                    RuntimeValue::String(string) => string.clone(),

                    _ => break,
                };

                if cap.get(0).unwrap().start() == 0 {
                    start_var = Some((cap.get(1).unwrap().as_str().to_owned(), value.clone()));
                }

                return value;
            }
        }

        failed = true;
        String::new()
    });

    if failed {
        return vec![];
    }

    let mut search = word
        .split(['/', '\\'])
        .filter(|segment| !segment.is_empty() && *segment != ".")
        .map(|segment| {
            if segment != ".." && !segment.ends_with(':') {
                format!("*{segment}*")
            } else {
                segment.to_owned()
            }
        })
        .collect::<Vec<_>>();

    let starts_with_home_dir = if word.starts_with("~/") || word.starts_with("~\\") {
        let Some(home_dir) = ctx.home_dir() else {
            return vec![];
        };

        search[0] = home_dir
            .to_string_lossy()
            .trim_end_matches(['/', '\\'])
            .to_string();

        Some(home_dir)
    } else {
        None
    };

    if word.starts_with(['/', '\\']) {
        search.insert(0, String::new());
    }

    let mut search = search.join(MAIN_SEPARATOR_STR);

    if search.is_empty() {
        search.push('*');
    } else if word.ends_with(['/', '\\']) {
        search.push(MAIN_SEPARATOR);
        search.push('*');
    }

    let glob_options = MatchOptions {
        case_sensitive: false,
        require_literal_leading_dot: true,
        require_literal_separator: true,
    };

    let Ok(entries) = glob_with(&search, glob_options) else {
        return vec![];
    };

    let Ok(paths) = entries.collect::<Result<Vec<_>, _>>() else {
        return vec![];
    };

    let starts_with_dot_slash = word.starts_with("./") || word.starts_with(".\\");

    let mut results = vec![];

    for path in paths {
        let Ok(metadata) = path.metadata() else {
            return vec![];
        };

        let file_type = metadata.file_type();

        // NOTE: Invalid UTF-8 paths will be displayed lossily (no other way to handle this)
        let mut path_str = path.to_string_lossy().to_string();

        if file_type.is_dir() {
            path_str.push(MAIN_SEPARATOR);
        }

        if let Some(home_dir) = starts_with_home_dir {
            path_str = format!(
                "~{MAIN_SEPARATOR}{}",
                path_str
                    .strip_prefix(&format!("{}{MAIN_SEPARATOR}", home_dir.display()))
                    .unwrap()
            )
        }

        let path = if starts_with_dot_slash {
            format!(".{MAIN_SEPARATOR}{path_str}")
        } else {
            path_str.clone()
        };

        let value = match &start_var {
            Some((var_name, var_value)) => {
                let stripped_path = path_str.strip_prefix(var_value.as_str()).unwrap();
                let mut escaped = escape_raw(stripped_path).into_owned();

                let insertion_point = if escaped.starts_with('"') { 1 } else { 0 };

                escaped.insert(insertion_point, '$');
                escaped.insert_str(insertion_point + 1, var_name);

                escaped
            }

            None => escape_raw(&path).into_owned(),
        };

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

    sort_results(&search, results)
}

fn sort_results(input: &str, values: Vec<(String, Suggestion)>) -> Vec<Suggestion> {
    let input = input.replace(['*', '?'], "");

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

fn escape_raw(str: &str) -> Cow<str> {
    if str
        .chars()
        .any(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c))
    {
        let mut out = String::with_capacity(str.len() + 2);
        out.push('"');

        for c in str.chars() {
            if c == '"' || c == '$' || c == '\\' {
                out.push('\\');
            }

            out.push(c);
        }

        out.push('"');

        Cow::Owned(out)
    } else {
        Cow::Borrowed(str)
    }
}

static VAR_IN_PATH_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("\\$([[:alpha:]_][[:alnum:]_]*)").unwrap());
