use std::path::PathBuf;

#[derive(Debug)]
pub struct RuntimeConf {
    pub call_stack_limit: usize,
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

#[derive(Debug, Clone)]
pub struct HistoryConf {
    pub enabled: bool,
    pub custom_location: Option<PathBuf>,
    // pub capacity: usize,
}

impl Default for HistoryConf {
    fn default() -> Self {
        Self {
            enabled: true,
            custom_location: None,
            // capacity: 100_000,
        }
    }
}
