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
use reshell_parser::DELIMITER_CHARS;
use reshell_runtime::{
    compat::{TargetFamily, PATH_VAR_SEP, TARGET_FAMILY},
    context::Context,
    pretty::{PrettyPrintOptions, PrettyPrintable},
};

use crate::{
    repl::SHARED_CONTEXT,
    utils::{
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
    Box<dyn Fn(&str, &mut Context) -> Vec<ExternalCompletion> + Send + Sync>;

#[derive(Debug)]
pub struct ExternalCompletion {
    pub raw_string: String,
    pub description: String,
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
    let word_start = pos - find_breakpoint_backward(&line[..pos]);
    let word_end = word_start + find_breakpoint_forward(&line[word_start..]);

    let word = &line[word_start..word_end];

    let span = Span {
        start: word_start,
        end: word_end,
    };

    let after_space = word_start > 0
        && matches!(line[word_start - 1..].chars().next(), Some(c) if c.is_whitespace());

    if word == "~" {
        return vec![];
    }

    let after_cmd_separator = line[..word_start].trim_end().ends_with(['|', ';'])
        && !line[..word_start].trim_end().ends_with("||");

    let next_char = line[word_end..].chars().next();

    if let Some(s_word) = word.strip_prefix('$') {
        return if s_word.chars().any(|c| !c.is_alphanumeric() && c != '_') {
            complete_path(word, span, ctx)
        } else {
            complete_var_name(&unescape(s_word), Some("$"), span, ctx)
        };
    }

    // TODO: only complete if inside expression
    if let Some(s_word) = word.strip_prefix('@') {
        return sort_results(
            &unescape(word),
            build_fn_completions(&unescape(s_word), next_char, Some("@"), span, ctx).collect(),
        );
    }

    if after_cmd_separator {
        let word = unescape(word);

        if let Some(s_word) = word.strip_prefix('.') {
            return sort_results(
                &word,
                build_method_completions(&unescape(s_word), Some("."), span, ctx).collect(),
            );
        }

        let mut cmd_comp = build_cmd_completions(&word, next_char, span)
            .ok()
            .flatten()
            .unwrap_or_default();

        cmd_comp.extend(build_fn_completions(&word, next_char, None, span, ctx));

        return sort_results(&word, cmd_comp);
    }

    if !after_space && !word.contains(['/', '\\']) {
        let word = unescape(word);

        let mut cmd_comp = build_cmd_completions(&word, next_char, span)
            .ok()
            .flatten()
            .unwrap_or_default();

        cmd_comp.extend(build_fn_completions(&word, next_char, None, span, ctx));

        return sort_results(&word, cmd_comp);
    }

    if after_space {
        if let Some(external_completer) = external_completer {
            let completions = external_completer(line, ctx);

            if !completions.is_empty() {
                return completions
                    .into_iter()
                    .map(|comp| {
                        let ExternalCompletion {
                            raw_string,
                            description,
                        } = comp;

                        Suggestion {
                            value: raw_string,
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
    add_prefix: Option<&str>,
    span: Span,
    ctx: &'a Context,
) -> impl Iterator<Item = SortableSuggestion> + 'a {
    let word = word.to_lowercase();
    let add_prefix = add_prefix.map(str::to_owned);

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
                    append_whitespace: false,
                },
            )
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
    let Ok(globified) = globify_path(word, ctx) else {
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
            require_literal_leading_dot: true,
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
                    Some(stripped_path) => {
                        let mut escaped = escape_raw(stripped_path).into_owned();

                        let insertion_point = if escaped.starts_with('"') { 1 } else { 0 };

                        escaped.insert(insertion_point, '$');
                        escaped.insert_str(insertion_point + 1, name);

                        escaped
                    }

                    None => escape_raw(&path_str).into_owned(),
                }
            }

            Some(GlobPathStartsWith::HomeDirTilde(home_dir)) => escape_raw(&format!(
                "~{MAIN_SEPARATOR}{}",
                path_str
                    .strip_prefix(&format!("{home_dir}{MAIN_SEPARATOR}"))
                    .unwrap()
            ))
            .into_owned(),

            Some(GlobPathStartsWith::CurrentDir) => {
                escape_raw(&format!(".{MAIN_SEPARATOR}{path_str}")).into_owned()
            }

            None => escape_raw(&path_str).into_owned(),
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

fn find_breakpoint_forward(str: &str) -> usize {
    let mut escaping = false;
    let mut offset = 0;

    for c in str.chars() {
        if c == '\r' || c == '\n' {
            break;
        }

        offset += c.len_utf8();

        if escaping {
            escaping = false;
        } else if c == '\\' {
            escaping = true;
        } else if needs_escaping(c) && c != '$' {
            break;
        }
    }

    offset
}

fn find_breakpoint_backward(str: &str) -> usize {
    let chars = str.chars().rev().collect::<Vec<_>>();
    let mut escaping = false;

    for (i, c) in chars.iter().enumerate() {
        if escaping {
            escaping = false;
            continue;
        }

        let mut prev_backslashes = 0;

        for c in chars[i..].iter().skip(1) {
            if *c == '\\' {
                prev_backslashes += 1;
            } else {
                break;
            }
        }

        if prev_backslashes % 2 != 0 {
            escaping = true;
        } else if c.is_whitespace() || (DELIMITER_CHARS.contains(c) && *c != '$') {
            return i;
        }
    }

    str.len()
}

fn unescape(str: &str) -> String {
    let mut unescaped = String::new();
    let mut escaping = false;

    for c in str.chars() {
        if escaping {
            escaping = false;
        } else if c == '\\' {
            escaping = true;
        } else {
            unescaped.push(c);
        }
    }

    unescaped
}

fn escape_raw(str: &str) -> Cow<str> {
    if str.chars().any(needs_escaping) {
        let mut escaped = String::with_capacity(str.len());

        for c in str.chars() {
            if needs_escaping(c) {
                escaped.push('\\');
            }

            escaped.push(c);
        }

        Cow::Owned(escaped)
    } else {
        Cow::Borrowed(str)
    }
}

fn needs_escaping(c: char) -> bool {
    c.is_whitespace() || DELIMITER_CHARS.contains(&c) || c == '\\'
}
