use std::path::PathBuf;

use crate::utils::lazy_cell::LazyCell;

pub static HOME_DIR: LazyCell<Option<PathBuf>> = LazyCell::new(dirs::home_dir);

pub static SHELL_CONFIG_DIR: LazyCell<Option<PathBuf>> =
    LazyCell::new(|| dirs::config_dir().map(|dir| dir.join("reshell")));

pub static SHELL_LOCAL_DATA_DIR: LazyCell<Option<PathBuf>> =
    LazyCell::new(|| dirs::data_local_dir().map(|dir| dir.join("reshell")));

pub static INIT_SCRIPT_PATH: LazyCell<Option<PathBuf>> =
    LazyCell::new(|| SHELL_CONFIG_DIR.as_ref().map(|dir| dir.join("init.rsh")));

pub static HISTORY_PATH: LazyCell<Option<PathBuf>> = LazyCell::new(|| {
    SHELL_LOCAL_DATA_DIR
        .as_ref()
        .map(|dir| dir.join("history.db"))
});
