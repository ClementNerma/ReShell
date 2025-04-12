use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use walkdir::WalkDir;

use crate::{Pattern, pattern::PatternMatchResult};

pub struct Walker {
    pattern: Pattern,
    strip_root_dir: PathBuf,
    dir_walker: walkdir::IntoIter,
}

impl Walker {
    pub fn new(pattern: Pattern, base_dir: &Path) -> Result<Self> {
        let strip_root_dir = if pattern.is_absolute() {
            // TODO: properly determine base dir from pattern
            PathBuf::new()
        } else {
            fs::canonicalize(base_dir)
                .with_context(|| format!("Failed to canonicalize path: {}", base_dir.display()))?
        };

        let walk_from = if pattern.is_absolute() {
            Path::new("/")
        } else {
            base_dir
        };

        Ok(Self {
            pattern,
            dir_walker: WalkDir::new(walk_from).min_depth(1).into_iter(),
            strip_root_dir,
        })
    }
}

impl Iterator for Walker {
    type Item = walkdir::Result<PathBuf>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry = match self.dir_walker.next()? {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err)),
            };

            let entry_path = entry.path().strip_prefix(&self.strip_root_dir).unwrap();

            match self.pattern.match_against(entry_path) {
                PatternMatchResult::PathNotAbsolute => unreachable!(),
                PatternMatchResult::Matched => return Some(Ok(entry_path.to_owned())),
                PatternMatchResult::NotMatched => {
                    if entry.file_type().is_dir() {
                        self.dir_walker.skip_current_dir();
                    }
                }
                PatternMatchResult::Starved => {}
            }
        }
    }
}
