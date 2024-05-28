use std::path::PathBuf;

#[derive(Debug)]
pub struct RuntimeConf {
    pub initial_home_dir: Option<PathBuf>,
    pub call_stack_limit: usize,
    pub history_path: Option<PathBuf>,
    pub history_enabled: bool,
    pub history_capacity: usize,
}

impl Default for RuntimeConf {
    fn default() -> Self {
        Self {
            initial_home_dir: None,
            call_stack_limit: 10_000,
            history_path: None,
            history_enabled: true,
            history_capacity: 1_000_000,
        }
    }
}
