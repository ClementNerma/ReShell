use reedline::{FileBackedHistory, History, ListMenu, ReedlineMenu, SqliteBackedHistory};
use reshell_runtime::conf::RuntimeConf;

use crate::{paths::HISTORY_PATH, print_err, print_warn};

pub static HISTORY_MENU_NAME: &str = "history_menu";

pub fn create_history(runtime_conf: &RuntimeConf) -> Box<dyn History> {
    if runtime_conf.history.enabled {
        match create_history_inner(runtime_conf) {
            Ok(history) => return Box::new(history),
            Err(err) => {
                print_err(err);
                print_warn("History will not be saved to disk for this session.");
            }
        }
    }

    Box::new(FileBackedHistory::new(10_000))
}

fn create_history_inner(runtime_conf: &RuntimeConf) -> Result<SqliteBackedHistory, String> {
    let history_path = HISTORY_PATH
        .as_ref()
        .ok_or("Failed to determine path to history file")?;

    if runtime_conf.history.enabled {
        SqliteBackedHistory::with_file(history_path.clone(), None, None)
    } else {
        SqliteBackedHistory::in_memory()
    }
    .map_err(|err| format!("Failed to create history: {err}"))
}

pub fn create_history_menu() -> ReedlineMenu {
    let menu = ListMenu::default().with_name(HISTORY_MENU_NAME);
    ReedlineMenu::HistoryMenu(Box::new(menu))
}
