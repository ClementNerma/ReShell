use std::{
    borrow::Cow,
    collections::HashSet,
    error::Error,
    fs,
    path::{Path, MAIN_SEPARATOR, MAIN_SEPARATOR_STR},
};

use glob::{glob_with, MatchOptions};
use reedline::{ColumnarMenu, Completer as RlCompleter, ReedlineMenu, Span, Suggestion};
use reshell_parser::{ast::RuntimeCodeRange, delimiter_chars};
use reshell_runtime::{
    context::Context,
    pretty::{PrettyPrintOptions, PrettyPrintable},
};

use crate::{
    compat::{TargetFamily, TARGET_FAMILY},
    utils::lev_distance::levenshtein_distance,
};

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

    let delimiter_chars = delimiter_chars();

    let split = |c: char| c.is_whitespace() && !delimiter_chars.contains(&c);

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

    if let Some(word) = word.strip_prefix('$') {
        return complete_var_name(word, Some("$"), span, ctx);
    }

    if let Some(word) = word.strip_prefix('@') {
        return complete_fn_name(word, Some("@"), span, ctx);
    }

    if !after_space && !word.contains(['/', '\\']) {
        let mut cmd_comp = build_cmd_completions(word, span)
            .ok()
            .flatten()
            .unwrap_or_default();

        cmd_comp.extend(build_fn_completions(word, None, span, ctx));

        return sort_results(word, cmd_comp);
    }

    complete_path(word, span, ctx)
}

fn build_fn_completions<'a>(
    word: &str,
    add_prefix: Option<&str>,
    span: Span,
    ctx: &'a Context,
) -> impl Iterator<Item = SortableSuggestion> + 'a {
    let word = word.to_lowercase();
    let add_prefix = add_prefix.map(str::to_owned);

    ctx.visible_scopes()
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
                    extra: None,
                    span,
                    append_whitespace: true,
                },
            )
        })
}

fn complete_fn_name(
    word: &str,
    add_prefix: Option<&str>,
    span: Span,
    ctx: &Context,
) -> Vec<Suggestion> {
    sort_results(
        word,
        build_fn_completions(word, add_prefix, span, ctx).collect(),
    )
}

fn build_cmd_completions(
    word: &str,
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

    let path_var_sep = match TARGET_FAMILY {
        TargetFamily::Windows => ';',
        TargetFamily::Unix => ':',
    };

    let current_dir = std::env::current_dir()?;

    let path_dirs = [current_dir.to_str()]
        .into_iter()
        .flatten()
        .chain(path.split(path_var_sep).filter(|entry| !entry.is_empty()));

    let delimiter_chars = delimiter_chars();

    let mut results = Vec::<(String, Suggestion)>::new();

    for dir in path_dirs {
        if !Path::new(dir).is_dir() {
            continue;
        }

        let items = fs::read_dir(dir)?.collect::<Result<Vec<_>, _>>()?;

        for item in items {
            if !item.file_type()?.is_file()
                && (!item.file_type()?.is_symlink() || !fs::read_link(&item.path())?.is_file())
            {
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
                    let Some((filename, ext)) = item_name_lc.rsplit_once('.') else {
                        continue;
                    };

                    let exe_exts = ["bat", "cmd", "exe"];

                    if !exe_exts.contains(&ext) {
                        continue;
                    }

                    let possible_names = [filename.to_owned()]
                        .into_iter()
                        .chain(
                            ["bat", "cmd", "exe"]
                                .into_iter()
                                .map(|ext| format!("{filename}.{ext}")),
                        )
                        .collect::<HashSet<_>>();

                    if results
                        .iter()
                        .any(|(other, _)| possible_names.contains(other))
                    {
                        continue;
                    }
                }

                TargetFamily::Unix => {
                    if results.iter().any(|(other, _)| &item_name_lc == other) {
                        continue;
                    }
                }
            }

            results.push((
                item_name_lc,
                Suggestion {
                    value: escape_raw(item_name, &delimiter_chars).into_owned(),
                    description: None,
                    extra: None,
                    span,
                    append_whitespace: true,
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
        .visible_scopes()
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
                    description: Some(match item.value.read(RuntimeCodeRange::Internal).as_ref() {
                        Some(loc_val) => loc_val
                            .value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline()),
                        None => "<value not set>".to_string(),
                    }),
                    extra: None,
                    span,
                    append_whitespace: true,
                },
            )
        })
        .collect::<Vec<_>>();

    sort_results(&word, results)
}

fn complete_path(word: &str, span: Span, ctx: &Context) -> Vec<Suggestion> {
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
        search.insert(0, MAIN_SEPARATOR.to_string());
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

    let delimiter_chars = delimiter_chars();

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

        results.push((
            path_str,
            Suggestion {
                value: escape_raw(&path, &delimiter_chars).into_owned(),
                description: None,
                extra: None,
                span,
                append_whitespace: !file_type.is_dir(),
            },
        ));
    }

    sort_results(&search, results)
}

fn sort_results(input: &str, mut values: Vec<(String, Suggestion)>) -> Vec<Suggestion> {
    values.sort_by_key(|(candidate, _)| levenshtein_distance(input, candidate));
    values.into_iter().map(|(_, value)| value).collect()
}

fn escape_raw<'a>(str: &'a str, delimiter_chars: &HashSet<char>) -> Cow<'a, str> {
    if str
        .chars()
        .any(|c| c.is_whitespace() || delimiter_chars.contains(&c))
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
