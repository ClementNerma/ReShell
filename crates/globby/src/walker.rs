use std::{
    fs::canonicalize,
    path::{MAIN_SEPARATOR_STR, Path, PathBuf},
};

use anyhow::Result;

use crate::{Pattern, fs_walker::FsWalker, parser::PatternType, pattern::PatternMatchResult};

pub struct Walker {
    state: Option<WalkerState>,
}

struct WalkerState {
    pattern: Pattern,
    dir_walker: FsWalker,
    strip_dir: PathBuf,
}

impl Walker {
    pub fn new(pattern: Pattern, base_dir: &Path) -> Self {
        let walk_from = base_dir.join(pattern.common_root_dir());

        let Ok(walk_from) = canonicalize(&walk_from) else {
            return Self { state: None };
        };

        let strip_dir = if pattern.common_root_dir().is_absolute() {
            PathBuf::new()
        } else {
            let mut base_dir = base_dir;

            if let PatternType::RelativeToParent { depth } = pattern.pattern_type() {
                for _ in 0..depth.into() {
                    let Some(parent) = base_dir.parent() else {
                        return Self { state: None };
                    };

                    base_dir = parent;
                }
            }

            base_dir.to_owned()
        };

        Self {
            state: Some(WalkerState {
                pattern,
                dir_walker: FsWalker::new(walk_from),
                strip_dir,
            }),
        }
    }
}

impl Iterator for Walker {
    type Item = Result<PathBuf>;

    fn next(&mut self) -> Option<Self::Item> {
        let state = self.state.as_mut()?;

        loop {
            let entry = match state.dir_walker.next()? {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err)),
            };

            let entry_path = entry
                .path()
                .strip_prefix(&state.strip_dir)
                .unwrap()
                .to_owned();

            match state.pattern.match_against(&entry_path) {
                PatternMatchResult::PathNotAbsolute => unreachable!(),
                PatternMatchResult::Matched => {
                    return Some(Ok(match state.pattern.pattern_type() {
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
                        state.dir_walker.skip_incoming_dir().unwrap();
                    }
                }
                PatternMatchResult::Starved => {}
            }
        }
    }
}
