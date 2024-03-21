use reedline::{FileBackedHistory, History, ListMenu, ReedlineMenu};
use reshell_runtime::conf::RuntimeConf;

use crate::print_warn;

pub static HISTORY_MENU_NAME: &str = "history_menu";

pub fn create_history(runtime_conf: &RuntimeConf) -> Box<dyn History> {
    let capacity = runtime_conf.history_capacity;

    if !runtime_conf.history_enabled {
        todo!("When #661 is merged");
    }

    let history = match dirs::home_dir() {
        Some(dir) => match FileBackedHistory::with_file(capacity, dir.join(".rsh_history")) {
            Ok(history) => history,
            Err(err) => {
                print_warn(&format!("Failed to use history file: {err}"));
                print_warn("History will not be saved for this session");
                FileBackedHistory::new(capacity)
            }
        },

        None => {
            print_warn("Failed to determine path to the home directory");
            print_warn("History will not be saved for this session");
            FileBackedHistory::new(capacity)
        }
    };

    Box::new(history)
}

pub fn create_history_menu() -> ReedlineMenu {
    let menu = ListMenu::default().with_name(HISTORY_MENU_NAME);
    ReedlineMenu::HistoryMenu(Box::new(menu))
}
