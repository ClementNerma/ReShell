use std::{
    fs::{DirEntry, ReadDir},
    path::PathBuf,
};

use anyhow::{Context, Result};

pub struct FsWalker {
    readers: Vec<(PathBuf, ReadDir)>,
    going_into_dir: Option<PathBuf>,
}

impl FsWalker {
    pub fn new(dir: PathBuf) -> Result<Self> {
        Ok(Self {
            readers: vec![],
            going_into_dir: Some(dir),
        })
    }

    pub fn skip_incoming_dir(&mut self) -> Result<PathBuf> {
        self.going_into_dir
            .take()
            .context("Not going into a directory")
    }
}

impl Iterator for FsWalker {
    type Item = Result<DirEntry>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(going_into_dir) = self.going_into_dir.take() {
                let reader = std::fs::read_dir(&going_into_dir).with_context(|| {
                    format!("Failed to read directory {}", going_into_dir.display())
                });

                match reader {
                    Err(err) => return Some(Err(err)),
                    Ok(reader) => {
                        self.readers.push((going_into_dir, reader));
                        continue;
                    }
                }
            }

            let (curr_dir, reader) = self.readers.last_mut()?;

            let Some(entry) = reader.next() else {
                self.readers.pop();
                continue;
            };

            let entry = entry.with_context(|| {
                format!("Failed to read item in directory {}", curr_dir.display())
            });

            if let Ok(entry) = entry.as_ref() {
                if entry.path().is_dir() {
                    self.going_into_dir = Some(entry.path());
                }
            }

            return Some(entry);
        }
    }
}
