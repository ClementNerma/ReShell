use std::{path::PathBuf, sync::LazyLock};

pub static HOME_DIR: LazyLock<Option<PathBuf>> = LazyLock::new(dirs::home_dir);

pub static SHELL_CONFIG_DIR: LazyLock<Option<PathBuf>> =
    LazyLock::new(|| dirs::config_dir().map(|dir| dir.join("reshell")));

pub static SHELL_LOCAL_DATA_DIR: LazyLock<Option<PathBuf>> =
    LazyLock::new(|| dirs::data_local_dir().map(|dir| dir.join("reshell")));

pub static INIT_SCRIPT_PATH: LazyLock<Option<PathBuf>> =
    LazyLock::new(|| SHELL_CONFIG_DIR.as_ref().map(|dir| dir.join("init.rsh")));

pub static HISTORY_PATH: LazyLock<Option<PathBuf>> = LazyLock::new(|| {
    SHELL_LOCAL_DATA_DIR
        .as_ref()
        .map(|dir| dir.join("history.txt"))
});
