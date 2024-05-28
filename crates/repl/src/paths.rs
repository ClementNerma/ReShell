//!
//! Definition of commonly-used paths during runtime.
//!
//! Note that these paths do not change during the program's runtime, whatever happens.
//!

use std::{path::PathBuf, sync::LazyLock};

/// Path to the current user's home directory (e.g. `/home/$USER`)
pub static HOME_DIR: LazyLock<Option<PathBuf>> = LazyLock::new(dirs::home_dir);

/// Path to the REPL's config directory (e.g. `~/.config/reshell`)
pub static SHELL_CONFIG_DIR: LazyLock<Option<PathBuf>> =
    LazyLock::new(|| dirs::config_dir().map(|dir| dir.join("reshell")));

/// Path to the REPL's local data directory (e.g. `~/.local/share/reshell`)
pub static SHELL_LOCAL_DATA_DIR: LazyLock<Option<PathBuf>> =
    LazyLock::new(|| dirs::data_local_dir().map(|dir| dir.join("reshell")));

/// Path to the REPL's optional initialization script (e.g. `~/.config/reshell/init.rsh`)
pub static INIT_SCRIPT_PATH: LazyLock<Option<PathBuf>> =
    LazyLock::new(|| SHELL_CONFIG_DIR.as_ref().map(|dir| dir.join("init.rsh")));

/// Path to the REPL's history file (e.g. `~/.local/share/reshell/history.txt`)
pub static HISTORY_PATH: LazyLock<Option<PathBuf>> = LazyLock::new(|| {
    SHELL_LOCAL_DATA_DIR
        .as_ref()
        .map(|dir| dir.join("history.txt"))
});
