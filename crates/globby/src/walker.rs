use std::{
    fs::canonicalize,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use walkdir::WalkDir;

use crate::{Pattern, parser::PatternType, pattern::PatternMatchResult};

pub struct Walker {
    pattern: Pattern,
    dir_walker: walkdir::IntoIter,
    strip_dir: PathBuf,
}

impl Walker {
    pub fn new(pattern: Pattern, base_dir: &Path) -> Result<Self> {
        Ok(Self {
            dir_walker: WalkDir::new(base_dir.join(pattern.common_root_dir()))
                .min_depth(1)
                .into_iter(),
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
    type Item = walkdir::Result<PathBuf>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry = match self.dir_walker.next()? {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err)),
            };

            // TODO: don't unwrap
            let entry_path = canonicalize(entry.path()).unwrap();

            let entry_path = diff_paths(&entry_path, &self.strip_dir).unwrap();

            match self.pattern.match_against(&entry_path) {
                PatternMatchResult::PathNotAbsolute => unreachable!(),
                PatternMatchResult::Matched => return Some(Ok(entry_path)),
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

pub fn diff_paths(path: &Path, base: &Path) -> Option<PathBuf> {
    use std::path::Component;

    if path.is_absolute() != base.is_absolute() {
        if path.is_absolute() {
            Some(PathBuf::from(path))
        } else {
            None
        }
    } else {
        let mut ita = path.components();

        let mut itb = base.components();

        let mut comps: Vec<std::path::Component> = vec![];

        loop {
            match (ita.next(), itb.next()) {
                (None, None) => break,

                (Some(a), None) => {
                    comps.push(a);

                    comps.extend(ita.by_ref());

                    break;
                }

                (None, _) => comps.push(Component::ParentDir),

                (Some(a), Some(b)) if comps.is_empty() && a == b => (),

                (Some(a), Some(Component::CurDir)) => comps.push(a),

                (Some(_), Some(Component::ParentDir)) => return None,

                (Some(a), Some(_)) => {
                    comps.push(Component::ParentDir);

                    for _ in itb {
                        comps.push(Component::ParentDir);
                    }

                    comps.push(a);

                    comps.extend(ita.by_ref());

                    break;
                }
            }
        }

        Some(comps.iter().map(|c| c.as_os_str()).collect())
    }
}
