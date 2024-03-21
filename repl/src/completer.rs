use glob::{glob_with, MatchOptions};
use reedline::{ColumnarMenu, Completer as RlCompleter, ReedlineMenu, Span, Suggestion};
use reshell_runtime::pretty::{PrettyPrintOptions, PrettyPrintable};

use crate::state::RUNTIME_CONTEXT;

pub static COMPLETION_MENU_NAME: &str = "completion_menu";

pub fn create_completer() -> Box<dyn RlCompleter> {
    Box::new(Completer {
        home_dir: dirs::home_dir()
            .and_then(|home_dir| home_dir.to_str().map(|str| str.to_string())),
    })
}

pub fn create_completion_menu() -> ReedlineMenu {
    let menu = ColumnarMenu::default().with_name(COMPLETION_MENU_NAME);
    ReedlineMenu::EngineCompleter(Box::new(menu))
}

pub struct Completer {
    home_dir: Option<String>,
}

impl RlCompleter for Completer {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let after_last_ws = match line[..pos].rfind(char::is_whitespace) {
            Some(index) => index + 1,
            None => 0,
        };

        let Some(word) = line[after_last_ws..].split_whitespace().next() else {
            return vec![];
        };

        let span = Span {
            start: after_last_ws,
            end: after_last_ws + word.len(),
        };

        if word == "~" {
            return vec![];
        }

        if let Some(word_pr) = word.strip_prefix('$') {
            let ctx = &RUNTIME_CONTEXT.read().unwrap();

            let word_lc = word_pr.to_lowercase();

            return ctx
                .visible_scopes()
                .flat_map(|scope| scope.content.vars.iter())
                .filter(|(name, _)| name.to_lowercase().contains(word_lc.as_str()))
                .map(|(name, item)| Suggestion {
                    value: format!("${name}"),
                    description: Some(match &item.read().value {
                        Some(located_val) => located_val
                            .value
                            .get_type()
                            .render_colored(ctx, PrettyPrintOptions::inline()),

                        None => "<value not set>".to_string(),
                    }),
                    extra: None,
                    span,
                    append_whitespace: true,
                })
                .collect::<Vec<_>>();
        }

        if let Some(word_pr) = word.strip_prefix('@') {
            let ctx = &RUNTIME_CONTEXT.read().unwrap();

            let word_lc = word_pr.to_lowercase();

            return ctx
                .visible_scopes()
                .flat_map(|scope| scope.content.fns.iter())
                .filter(|(name, _)| name.to_lowercase().contains(word_lc.as_str()))
                .map(|(name, item)| Suggestion {
                    value: format!("@{name}"),
                    description: Some(
                        item.value
                            .read()
                            .signature
                            .render_uncolored(ctx, PrettyPrintOptions::inline()),
                    ),
                    extra: None,
                    span,
                    append_whitespace: true,
                })
                .collect::<Vec<_>>();
        }

        let mut search = word
            .split(['/', '\\'])
            .filter(|segment| !segment.is_empty() && *segment != ".")
            .map(|segment| {
                if segment != ".." {
                    format!("*{segment}*")
                } else {
                    segment.to_string()
                }
            })
            .collect::<Vec<_>>();

        let starts_with_home_dir = if word.starts_with("~/") {
            let Some(home_dir) = self.home_dir.as_ref() else {
                return vec![];
            };

            search[0] = home_dir.trim_end_matches('/').to_string();

            Some(home_dir)
        } else {
            None
        };

        if word.starts_with('/') {
            search.insert(0, "/".to_string());
        }

        let mut search = search.join("/");

        if search.is_empty() {
            search.push('*');
        } else if word.ends_with('/') {
            search.push_str("/*");
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

        let starts_with_dot_slash = word.starts_with("./");

        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        enum FileType {
            Directory,
            File,
            Unknown,
        }

        let mut out = vec![];

        for path in paths {
            let Ok(metadata) = path.metadata() else {
                return vec![];
            };

            let file_type = metadata.file_type();

            let file_type_enum = if file_type.is_dir() {
                FileType::Directory
            } else if file_type.is_file() {
                FileType::File
            } else {
                FileType::Unknown
            };

            // NOTE: Invalid UTF-8 paths will be displayed lossily (no other way to handle this)
            let mut path_str = path.to_string_lossy().to_string();

            if file_type.is_dir() {
                path_str.push('/');
            }

            if let Some(home_dir) = starts_with_home_dir {
                path_str = format!(
                    "~/{}",
                    path_str.strip_prefix(&format!("{home_dir}/")).unwrap()
                )
            }

            let mut value = if starts_with_dot_slash {
                format!("./{path_str}")
            } else {
                path_str
            };

            if value.contains(' ') {
                value = format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""));
            }

            out.push((
                file_type_enum,
                Suggestion {
                    value,
                    description: Some(
                        match file_type_enum {
                            FileType::Directory => "Dir",
                            FileType::File => "File",
                            FileType::Unknown => "?",
                        }
                        .to_string(),
                    ),
                    extra: None,
                    span,
                    append_whitespace: !file_type.is_dir(),
                },
            ));
        }

        out.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.value.cmp(&b.1.value)));

        out.into_iter().map(|(_, sugg)| sugg).collect()
    }
}
