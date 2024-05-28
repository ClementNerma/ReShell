use std::path::PathBuf;

#[derive(Debug)]
pub struct RuntimeConf {
    pub initial_home_dir: Option<PathBuf>,
    pub call_stack_limit: usize,
}

impl Default for RuntimeConf {
    fn default() -> Self {
        Self {
            initial_home_dir: None,
            call_stack_limit: 1000,
        }
    }
}
