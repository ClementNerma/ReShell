use std::{
    fs::{self, OpenOptions},
    io::{BufRead, BufReader, Write},
    path::PathBuf,
};

use indexmap::IndexMap;
use reedline::{
    CommandLineSearch, History as RlHistory, HistoryItem, HistoryItemId, HistorySessionId,
    ListMenu, MenuBuilder, ReedlineError, ReedlineErrorVariants, ReedlineMenu, SearchDirection,
    SearchFilter, SearchQuery,
};
use reshell_runtime::conf::RuntimeConf;

use crate::{paths::HISTORY_PATH, print_err, print_warn};

pub static HISTORY_MENU_NAME: &str = "history_menu";

pub fn create_history(runtime_conf: &RuntimeConf) -> Box<dyn RlHistory> {
    if runtime_conf.history.enabled {
        match create_history_inner(runtime_conf) {
            Ok(history) => return Box::new(history),
            Err(err) => {
                print_err(err);
                print_warn("History will not be saved to disk for this session.");
            }
        }
    }

    Box::new(History::in_memory())
}

fn create_history_inner(runtime_conf: &RuntimeConf) -> Result<History, String> {
    let history_path = HISTORY_PATH
        .as_ref()
        .ok_or("Failed to determine path to history file")?;

    if runtime_conf.history.enabled {
        History::with_file(history_path.clone())
            .map_err(|err| format!("Failed to load history: {err}"))
    } else {
        Ok(History::in_memory())
    }
}

pub fn create_history_menu() -> ReedlineMenu {
    let menu = ListMenu::default().with_name(HISTORY_MENU_NAME);
    ReedlineMenu::HistoryMenu(Box::new(menu))
}

/// Custom history implementation for [`reedline`] using an API proposal (currently unmerged)
pub struct History {
    entries: IndexMap<HistoryItemId, String>,
    file: Option<PathBuf>,
}

impl History {
    fn with_file(path: PathBuf) -> Result<Self, String> {
        let mut entries = IndexMap::new();

        let file = OpenOptions::new()
            .create(true)
            .truncate(false)
            .write(true)
            .read(true)
            .open(&path)
            .map_err(|err| format!("Failed to open history file: {err}"))?;

        for (i, line) in BufReader::new(file).lines().enumerate() {
            let line = line.map_err(|err| format!("Failed to read line {}: {err}", i + 1))?;

            entries.insert(HistoryItemId(i.try_into().unwrap()), line.to_owned());
        }

        Ok(Self {
            entries,
            file: Some(path),
        })
    }

    fn in_memory() -> Self {
        Self {
            entries: IndexMap::new(),
            file: None,
        }
    }

    fn construct_entry(id: HistoryItemId, command_line: String) -> HistoryItem {
        HistoryItem {
            id: Some(id),
            command_line,
            start_timestamp: None,
            session_id: None,
            hostname: None,
            cwd: None,
            duration: None,
            exit_status: None,
            more_info: None,
        }
    }
}

impl RlHistory for History {
    fn save(&mut self, mut h: HistoryItem) -> reedline::Result<HistoryItem> {
        if let Some(file) = &self.file {
            let mut file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(file)
                .map_err(ReedlineErrorVariants::IOError)
                .map_err(ReedlineError)?;

            file.write_all(format!("{}\n", h.command_line).as_bytes())
                .map_err(ReedlineErrorVariants::IOError)
                .map_err(ReedlineError)?;
        }

        let id = HistoryItemId(self.entries.last().map_or(0, |(last, _)| last.0 + 1));
        h.id = Some(id);

        let dup = self.entries.insert(id, h.command_line.clone());
        assert!(dup.is_none());

        Ok(h)
    }

    fn load(&self, id: HistoryItemId) -> reedline::Result<HistoryItem> {
        let cmd_line = self.entries.get(&id).cloned().ok_or({
            ReedlineError(ReedlineErrorVariants::OtherHistoryError(
                "Provided entry does not exist",
            ))
        })?;

        Ok(Self::construct_entry(id, cmd_line))
    }

    fn count(&self, query: SearchQuery) -> reedline::Result<i64> {
        self.search(query)?.len().try_into().map_err(|_| {
            ReedlineError(ReedlineErrorVariants::OtherHistoryError(
                "too many results (exceeds i64::MAX)",
            ))
        })
    }

    fn search(&self, query: SearchQuery) -> reedline::Result<Vec<HistoryItem>> {
        let SearchQuery {
            direction,
            start_time,
            end_time,
            start_id,
            end_id,
            limit,
            filter,
        } = query;

        let SearchFilter {
            command_line,
            hostname,
            cwd_exact,
            cwd_prefix,
            exit_successful,
            session,
            ..
        } = filter;

        if start_time.is_some()
            || end_time.is_some()
            || hostname.is_some()
            || cwd_exact.is_some()
            || cwd_prefix.is_some()
            || exit_successful.is_some()
            || session.is_some()
        {
            return Err(ReedlineError(
                ReedlineErrorVariants::HistoryFeatureUnsupported {
                    history: "SimpleFileHistory",
                    feature: "filtering by extra info",
                },
            ));
        }

        let (start_id, end_id) = {
            if let SearchDirection::Backward = direction {
                (end_id, start_id)
            } else {
                (start_id, end_id)
            }
        };

        let start_idx = match start_id {
            Some(from_id) => {
                self.entries.get_index_of(&from_id).ok_or(ReedlineError(
                    ReedlineErrorVariants::OtherHistoryError(
                        "provided 'start_id' item was not found",
                    ),
                ))? + 1
            }
            None => 0,
        };

        let end_idx = match end_id {
            Some(to_id) => self
                .entries
                .get_index_of(&to_id)
                .ok_or(ReedlineError(ReedlineErrorVariants::OtherHistoryError(
                    "provided 'end_id' item was not found",
                )))?
                .wrapping_sub(1),
            None => self.entries.len().saturating_sub(1),
        };

        if start_idx > end_idx {
            return Ok(vec![]);
        }

        let iter = self
            .entries
            .iter()
            .skip(start_idx)
            .take(1 + end_idx - start_idx);

        let limit = limit
            .and_then(|limit| usize::try_from(limit).ok())
            .unwrap_or(usize::MAX);

        let filter = |(id, cmd): (&HistoryItemId, &String)| {
            let cmd_lc = cmd.to_lowercase();

            let str_matches = match &command_line {
                Some(CommandLineSearch::Prefix(p)) => cmd_lc.starts_with(&p.to_lowercase()),
                Some(CommandLineSearch::Substring(p)) => cmd_lc.contains(&p.to_lowercase()),
                Some(CommandLineSearch::Exact(p)) => cmd_lc == p.to_lowercase(),
                None => true,
            };

            if !str_matches {
                return None;
            }

            // TODO
            // if let Some(str) = &filter.not_command_line {
            //     if &cmd_lc == str {
            //         return None;
            //     }
            // }

            Some(Self::construct_entry(*id, cmd.clone()))
        };

        Ok(match query.direction {
            SearchDirection::Backward => iter.rev().filter_map(filter).take(limit).collect(),
            SearchDirection::Forward => iter.filter_map(filter).take(limit).collect(),
        })
    }

    fn update(
        &mut self,
        _id: HistoryItemId,
        _updater: &dyn Fn(HistoryItem) -> HistoryItem,
    ) -> reedline::Result<()> {
        Err(ReedlineError(
            ReedlineErrorVariants::HistoryFeatureUnsupported {
                history: "SimpleFileHistory",
                feature: "updating entries",
            },
        ))
    }

    fn clear(&mut self) -> reedline::Result<()> {
        self.entries.clear();

        if let Some(file) = &self.file {
            if file.is_file() {
                fs::remove_file(file)
                    .map_err(ReedlineErrorVariants::IOError)
                    .map_err(ReedlineError)?
            }
        }

        Ok(())
    }

    fn delete(&mut self, _h: HistoryItemId) -> reedline::Result<()> {
        Err(ReedlineError(
            ReedlineErrorVariants::HistoryFeatureUnsupported {
                history: "SimpleFileHistory",
                feature: "removing entries",
            },
        ))
    }

    fn sync(&mut self) -> std::io::Result<()> {
        // Nothing to do, sync already happens in real time!
        // Also we don't want to load concurrent history entries
        Ok(())
    }

    fn session(&self) -> Option<HistorySessionId> {
        None
    }
}
