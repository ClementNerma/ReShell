#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

use std::{
    env::current_dir,
    path::{PathBuf, is_separator},
};

use anyhow::{Context, Result};
use globset::{GlobBuilder, GlobMatcher};
use walkdir::{DirEntry, WalkDir};

/// Recursive glob matcher, akin to Bash's
pub struct Globby {
    base_dir: PathBuf,
    matcher: GlobMatcher,
    walker: walkdir::IntoIter,
}

impl Globby {
    /// Create a new recursive glob matcher
    ///
    /// `pattern` is the glob pattern to use, e.g. `{a|b}.ex*`
    pub fn new(pattern: &str, dir: PathBuf, opts: GlobbyOptions) -> Result<Self> {
        let GlobbyOptions {
            case_insensitive,
            literal_separator,
            backslash_escape,
            empty_alternates,
        } = opts;

        let matcher = GlobBuilder::new(pattern)
            .case_insensitive(case_insensitive)
            .literal_separator(literal_separator)
            .backslash_escape(backslash_escape)
            .empty_alternates(empty_alternates)
            .build()
            .context("Invalid Glob pattern was provided")?
            .compile_matcher();

        let walker = WalkDir::new(&dir).min_depth(1).into_iter();

        Ok(Self {
            base_dir: dir,
            matcher,
            walker,
        })
    }

    /// Create a new recursive glob matcher for current directory
    pub fn in_current_dir(pattern: &str, opts: GlobbyOptions) -> Result<Self> {
        Self::new(
            pattern,
            current_dir().context("Failed to get current directory")?,
            opts,
        )
    }
}

impl Iterator for Globby {
    type Item = walkdir::Result<DirEntry>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry = match self.walker.next()? {
                Ok(entry) => entry,
                Err(err) => return Some(Err(err)),
            };

            let path = entry.path().strip_prefix(&self.base_dir).unwrap();

            if self.matcher.is_match(path) {
                return Some(Ok(entry));
            }
        }
    }
}

/// Options to configure how [`Globby`] behaves
pub struct GlobbyOptions {
    /// Enable case insensitivity (disabled by default)
    pub case_insensitive: bool,

    /// Require a literal `/` to match a path separator (enabled by default)
    pub literal_separator: bool,

    /// When enabled, a back slash (`\`) may be used to escape special characters in a glob pattern. Additionally, this will prevent `\` from being interpreted as a path separator on all platforms.
    ///
    /// This is enabled by default on platforms where `\` is not a path separator and disabled by default on platforms where `\` is a path separator.
    pub backslash_escape: bool,

    /// Accept an empty pattern in a list of alternates (disabled by default)
    ///
    /// For example, if this is set then the glob foo{,.txt} will match both foo and foo.txt.
    pub empty_alternates: bool,
}

impl Default for GlobbyOptions {
    fn default() -> Self {
        Self {
            case_insensitive: false,
            literal_separator: true,
            backslash_escape: !is_separator('\\'),
            empty_alternates: false,
        }
    }
}
