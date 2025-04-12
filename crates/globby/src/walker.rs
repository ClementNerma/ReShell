use std::path::PathBuf;

use walkdir::WalkDir;

use crate::{Pattern, pattern::PatternMatchResult};

pub struct Walker {
    pattern: Pattern,
    base_dir: PathBuf,
    dir_walker: walkdir::IntoIter,
}

impl Walker {
    pub fn new(pattern: Pattern, base_dir: PathBuf) -> Self {
        Self {
            pattern,
            dir_walker: WalkDir::new(&base_dir).min_depth(1).into_iter(),
            base_dir,
        }
    }
}

impl Iterator for Walker {
    type Item = walkdir::Result<walkdir::DirEntry>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry = match self.dir_walker.next()? {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err)),
            };

            let entry_path = match self.pattern.is_absolute() {
                true => entry.path(),
                false => entry.path().strip_prefix(&self.base_dir).unwrap(),
            };

            match self.pattern.match_against(entry_path) {
                PatternMatchResult::PathNotAbsolute => unreachable!(),
                PatternMatchResult::Matched => return Some(Ok(entry)),
                PatternMatchResult::NotMatched => {
                    if entry_path.is_dir() {
                        self.dir_walker.skip_current_dir();
                    }
                }
                PatternMatchResult::Starved => {}
            }
        }
    }
}
