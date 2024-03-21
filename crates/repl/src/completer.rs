use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fs,
    path::{Path, MAIN_SEPARATOR, MAIN_SEPARATOR_STR},
    sync::{Arc, Mutex},
};

use glob::{glob_with, MatchOptions};
use reedline::{ColumnarMenu, Completer as RlCompleter, ReedlineMenu, Span, Suggestion};
use reshell_runtime::{
    context::{Context, ScopeContent},
    pretty::{PrettyPrintOptions, PrettyPrintable},
};

use crate::{
    compat::{TargetFamily, TARGET_FAMILY},
    utils::lev_distance::levenshtein_distance,
};

pub static COMPLETION_MENU_NAME: &str = "completion_menu";

pub fn create_completer(completion_data: Arc<Mutex<CompletionData>>) -> Box<dyn RlCompleter> {
    Box::new(Completer { completion_data })
}

pub fn create_completion_menu() -> ReedlineMenu {
    let menu = ColumnarMenu::default().with_name(COMPLETION_MENU_NAME);
    ReedlineMenu::EngineCompleter(Box::new(menu))
}

pub struct Completer {
    completion_data: Arc<Mutex<CompletionData>>,
}

impl RlCompleter for Completer {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let completion_data = self.completion_data.lock().unwrap();

        let split = |c: char| {
            c.is_whitespace()
                || c == '('
                || c == ')'
                || c == '['
                || c == ']'
                || c == '{'
                || c == '}'
                || c == '"'
        };

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
            return complete_var_name(word, Some("$"), span, &completion_data);
        }

        if let Some(word) = word.strip_prefix('@') {
            return complete_fn_name(word, Some("@"), span, &completion_data);
        }

        if !after_space && !word.contains(['/', '\\']) {
            let mut cmd_comp = build_cmd_completions(word, span, &completion_data)
                .ok()
                .flatten()
                .unwrap_or_default();

            cmd_comp.extend(build_fn_completions(word, None, span, &completion_data));

            return sort_results(word, cmd_comp);
        }

        complete_path(word, span, completion_data.home_dir.as_deref())
    }
}

type SortableSuggestion = (String, Suggestion);

fn build_fn_completions<'a>(
    word: &str,
    add_prefix: Option<&str>,
    span: Span,
    completion_data: &'a CompletionData,
) -> impl Iterator<Item = SortableSuggestion> + 'a {
    let word = word.to_lowercase();
    let add_prefix = add_prefix.map(str::to_owned);

    completion_data
        .scopes
        .iter()
        .flat_map(|scope| scope.fns.iter())
        .filter(move |(name, _)| name.to_lowercase().contains(&word))
        .map(move |(name, signature)| {
            (
                name.clone(),
                Suggestion {
                    value: match add_prefix {
                        Some(ref prefix) => format!("{prefix}{name}"),
                        None => name.clone(),
                    },
                    description: Some(signature.clone()),
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
    completion_data: &CompletionData,
) -> Vec<Suggestion> {
    sort_results(
        word,
        build_fn_completions(word, add_prefix, span, completion_data).collect(),
    )
}

fn build_cmd_completions(
    word: &str,
    span: Span,
    completion_data: &CompletionData,
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

    let path_dirs = [completion_data.home_dir.as_deref()]
        .into_iter()
        .flatten()
        .chain(path.split(path_var_sep).filter(|entry| !entry.is_empty()));

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
                    let possible_names = [item_name_lc.clone()]
                        .into_iter()
                        .chain(
                            ["bat", "cmd", "exe"]
                                .into_iter()
                                .map(|ext| format!("{item_name_lc}.{ext}")),
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
                    // TODO: improve escaping
                    value: if item_name.contains(' ') {
                        format!("\"{item_name}\"")
                    } else {
                        item_name.to_owned()
                    },
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
    completion_data: &CompletionData,
) -> Vec<Suggestion> {
    let word = word.to_lowercase();

    let results = completion_data
        .scopes
        .iter()
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
                    description: Some(match item {
                        Some(value_type) => value_type.clone(),
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

fn complete_path(word: &str, span: Span, home_dir: Option<&str>) -> Vec<Suggestion> {
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
        let Some(home_dir) = home_dir else {
            return vec![];
        };

        search[0] = home_dir.trim_end_matches(['/', '\\']).to_string();

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
                    .strip_prefix(&format!("{home_dir}{MAIN_SEPARATOR}"))
                    .unwrap()
            )
        }

        let mut value = if starts_with_dot_slash {
            format!(".{MAIN_SEPARATOR}{path_str}")
        } else {
            path_str.clone()
        };

        if value.contains(' ') {
            value = format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""));
        }

        results.push((
            path_str,
            Suggestion {
                value,
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

pub struct CompletionData {
    scopes: Vec<CompletionDataScope>,
    home_dir: Option<String>,
}

impl CompletionData {
    pub fn generate_from_context(ctx: &Context) -> Self {
        Self {
            home_dir: ctx.home_dir().map(|dir| dir.to_string_lossy().into_owned()),
            scopes: ctx
                .visible_scopes()
                .map(|scope| CompletionDataScope::new(scope, ctx))
                .collect(),
        }
    }

    pub fn update_with(&mut self, ctx: &Context) {
        *self = Self::generate_from_context(ctx);
    }
}

pub struct CompletionDataScope {
    // TODO: requires computing type of all vars ahead of time (costly)
    pub vars: HashMap<String, Option<String>>,
    pub fns: HashMap<String, String>,
}

impl CompletionDataScope {
    fn new(scope: &ScopeContent, ctx: &Context) -> Self {
        Self {
            fns: scope
                .fns
                .iter()
                .map(|(name, func)| {
                    (
                        name.clone(),
                        func.value
                            .signature
                            .inner()
                            .render_colored(ctx, PrettyPrintOptions::inline()),
                    )
                })
                .collect(),

            vars: scope
                .vars
                .iter()
                .map(|(name, var)| {
                    (
                        name.clone(),
                        var.value.with_ref(|value| {
                            value.as_ref().map(|loc_val| {
                                loc_val
                                    .value
                                    .render_colored(ctx, PrettyPrintOptions::inline())
                            })
                        }),
                    )
                })
                .collect(),
        }
    }
}
