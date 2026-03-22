use reedline::{FileBackedHistory, History as RlHistory, ListMenu, MenuBuilder, ReedlineMenu};
use reshell_runtime::conf::RuntimeConf;

use crate::{paths::HISTORY_PATH, print_err, print_warn};

pub static HISTORY_MENU_NAME: &str = "history_menu";
pub static HISTORY_CAPACITY: usize = usize::MAX - 1;

pub fn create_history(runtime_conf: &RuntimeConf) -> Box<dyn RlHistory> {
    if runtime_conf.history.enabled {
        match create_history_inner(runtime_conf) {
            Ok(history) => return history,
            Err(err) => {
                print_err(err);
                print_warn("History will not be saved to disk for this session.");
            }
        }
    }

    Box::new(FileBackedHistory::new(HISTORY_CAPACITY).unwrap())
}

fn create_history_inner(runtime_conf: &RuntimeConf) -> Result<Box<dyn RlHistory>, String> {
    let history_path = HISTORY_PATH
        .as_ref()
        .ok_or("Failed to determine path to history file")?;

    if runtime_conf.history.enabled {
        FileBackedHistory::with_file(HISTORY_CAPACITY, history_path.clone())
            .map(|history| Box::new(history) as Box<dyn RlHistory>)
            .map_err(|err| format!("Failed to load history: {err}"))
    } else {
        Ok(Box::new(FileBackedHistory::new(HISTORY_CAPACITY).unwrap()))
    }
}

pub fn create_history_menu() -> ReedlineMenu {
    let menu = ListMenu::default().with_name(HISTORY_MENU_NAME);
    ReedlineMenu::HistoryMenu(Box::new(menu))
}
