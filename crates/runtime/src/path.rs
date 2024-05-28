use std::path::{Path, PathBuf};

pub fn find_in_path<P: AsRef<Path>>(
    dir: P,
    extensions: &ExecutablesExtension,
    path: &[P],
) -> Result<PathBuf, ()> {
    todo!()
}

pub enum ExecutablesExtension {
    None,
    Specific(Vec<String>),
}
