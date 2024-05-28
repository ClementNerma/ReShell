use reedline::{FileBackedHistory, History};

pub fn create_history() -> Box<dyn History> {
    Box::new(FileBackedHistory::new(1000))
}
