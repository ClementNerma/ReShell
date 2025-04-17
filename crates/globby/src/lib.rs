#![forbid(unsafe_code)]
#![forbid(unused_must_use)]
#![warn(unused_crate_dependencies)]

mod compiler;
mod fs_walker;
mod parser;
mod pattern;
mod walker;

use std::path::Path;

use anyhow::{Context, Result, anyhow};

pub use self::{
    pattern::{Pattern, PatternMatchResult, PatternOpts},
    walker::Walker,
};

pub fn glob(pattern: &str, dir: &Path, opts: PatternOpts) -> Result<Walker> {
    let pattern = Pattern::parse(pattern, opts)
        .map_err(|err| anyhow!("Failed to parse provided pattern: {err:?}"))?;

    Ok(Walker::new(pattern, dir))
}

pub fn glob_current_dir(pattern: &str, opts: PatternOpts) -> Result<Walker> {
    let current_dir =
        std::env::current_dir().context("Failed to get path of the current directory")?;

    glob(pattern, &current_dir, opts)
}
