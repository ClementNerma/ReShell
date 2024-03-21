use std::path::PathBuf;

use crate::utils::lazy_cell::LazyCell;

pub static HOME_DIR: LazyCell<Option<PathBuf>> = LazyCell::new(dirs::home_dir);

pub static SHELL_DATA_DIR: LazyCell<Option<PathBuf>> =
    LazyCell::new(|| dirs::config_local_dir().map(|dir| dir.join("reshell")));

pub static INIT_SCRIPT_PATH: LazyCell<Option<PathBuf>> =
    LazyCell::new(|| SHELL_DATA_DIR.as_ref().map(|dir| dir.join("init.rsh")));

// pub static HISTORY_PATH: LazyCell<Option<PathBuf>> = LazyCell::new(|| {
//     SHELL_DATA_DIR
//         .as_ref()
//         .map(|dir| dir.join("history.rsh_history"))
// });
