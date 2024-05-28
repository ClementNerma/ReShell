use reedline::{FileBackedHistory, History as RlHistory, ListMenu, MenuBuilder, ReedlineMenu};
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

    Box::new(FileBackedHistory::new(10_000).unwrap())
}

fn create_history_inner(runtime_conf: &RuntimeConf) -> Result<FileBackedHistory, String> {
    let history_path = HISTORY_PATH
        .as_ref()
        .ok_or("Failed to determine path to history file")?;

    if runtime_conf.history.enabled {
        FileBackedHistory::with_file(
            // TODO: make this configurable
            10_000,
            history_path.clone(),
        )
    } else {
        FileBackedHistory::new(
            // TODO: make this configurable
            10_000,
        )
    }
    .map_err(|err| format!("Failed to create history: {err}"))
}

pub fn create_history_menu() -> ReedlineMenu {
    let menu = ListMenu::default().with_name(HISTORY_MENU_NAME);
    ReedlineMenu::HistoryMenu(Box::new(menu))
}
