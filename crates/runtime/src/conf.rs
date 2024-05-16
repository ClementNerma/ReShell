use std::path::PathBuf;

/// Runtime configuration
#[derive(Debug)]
pub struct RuntimeConf {
    /// Maximum call recursion
    pub call_stack_limit: usize,

    /// History configuration
    pub history: HistoryConf,
}

impl Default for RuntimeConf {
    fn default() -> Self {
        Self {
            call_stack_limit: 10_000,
            history: HistoryConf::default(),
        }
    }
}

/// History configuration
#[derive(Debug, Clone)]
pub struct HistoryConf {
    /// Enable the history
    pub enabled: bool,

    /// Use a custom location instead of the builtin one
    pub custom_location: Option<PathBuf>,
}

impl Default for HistoryConf {
    fn default() -> Self {
        Self {
            enabled: true,
            custom_location: None,
        }
    }
}
