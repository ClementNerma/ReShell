use std::{
    fs::canonicalize,
    path::{MAIN_SEPARATOR_STR, Path, PathBuf},
};

use anyhow::{Context, Result};

use crate::{Pattern, fs_walker::FsWalker, parser::PatternType, pattern::PatternMatchResult};

pub struct Walker {
    pattern: Pattern,
    dir_walker: FsWalker,
    strip_dir: PathBuf,
}

impl Walker {
    pub fn new(pattern: Pattern, base_dir: &Path) -> Result<Self> {
        let walk_from = base_dir.join(pattern.common_root_dir());
        let walk_from = canonicalize(&walk_from).with_context(|| {
            format!(
                "Failed to canonicalize base directory {}",
                walk_from.display()
            )
        })?;

        Ok(Self {
            dir_walker: FsWalker::new(walk_from)?,

            strip_dir: if pattern.common_root_dir().is_absolute() {
                PathBuf::new()
            } else {
                let mut base_dir = base_dir;

                if let PatternType::RelativeToParent { depth } = pattern.pattern_type() {
                    for _ in 0..depth.into() {
                        let parent = base_dir.parent().context("TODO")?;
                        base_dir = parent;
                    }
                }

                base_dir.to_owned()
            },

            pattern,
        })
    }
}

impl Iterator for Walker {
    type Item = Result<PathBuf>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry = match self.dir_walker.next()? {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err)),
            };

            let entry_path = entry
                .path()
                .strip_prefix(&self.strip_dir)
                .unwrap()
                .to_owned();

            match self.pattern.match_against(&entry_path) {
                PatternMatchResult::PathNotAbsolute => unreachable!(),
                PatternMatchResult::Matched => {
                    return Some(Ok(match self.pattern.pattern_type() {
                        PatternType::RelativeToParent { depth } => {
                            // TODO: cache this string to avoid runtime formatting overhead
                            let prefix = format!("..{MAIN_SEPARATOR_STR}").repeat(depth.into());

                            Path::new(&prefix).join(entry_path)
                        }

                        _ => entry_path,
                    }));
                }
                PatternMatchResult::NotMatched => {
                    if entry.path().is_dir() {
                        self.dir_walker.skip_incoming_dir().unwrap();
                    }
                }
                PatternMatchResult::Starved => {}
            }
        }
    }
}
