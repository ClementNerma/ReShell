//!
//! Path globification module.
//!
//! Allows to create a glob pattern from a path input.
//!
//! Used for auto-completion of paths.
//!

use std::{
    borrow::Cow,
    path::{MAIN_SEPARATOR, MAIN_SEPARATOR_STR},
};

use reshell_prettify::{PrettyPrintable};
use reshell_runtime::{context::Context, values::RuntimeValue};

use crate::completer::UnescapedSegment;

#[derive(Debug)]
pub struct GlobPathOut {
    pub glob_pattern: String,
    pub starts_with: Option<GlobPathStartsWith>,
}

#[derive(Debug)]
pub enum GlobPathStartsWith {
    CurrentDir,
    HomeDirTilde(String),
    Variable { name: String, value: String },
}

pub fn globify_path(segments: &[UnescapedSegment], ctx: &Context) -> Result<GlobPathOut, String> {
    let mut starts_with = None;

    let path = segments
        .iter()
        .enumerate()
        .map(|(i, segment)| match &segment {
            UnescapedSegment::String(input) => {
                if i == 0 {
                    if let Some(stripped) = input
                        .strip_prefix("~/")
                        .or_else(|| input.strip_prefix("~\\"))
                    {
                        let home_dir = ctx
                            .home_dir()
                            .ok_or("Home directory is not set")?
                            .to_str()
                            .ok_or("Home directory contains invalid UTF-8 path")?;

                        starts_with = Some(GlobPathStartsWith::HomeDirTilde(home_dir.to_owned()));

                        return Ok(format!("{home_dir}{MAIN_SEPARATOR}{stripped}"));
                    }

                    if input.starts_with("./") || input.starts_with(".\\") {
                        starts_with = Some(GlobPathStartsWith::CurrentDir);
                    }
                }

                Ok(input.to_owned())
            }

            &UnescapedSegment::VariableName(var_name) => {
                let var = ctx
                    .visible_scopes_content()
                    .find_map(|scope| scope.vars.get(var_name))
                    .ok_or_else(|| format!("Variable '{var_name}' was not found"))?;

                let str = match &var.value.read_promise_no_write().value {
                    RuntimeValue::Int(int) => int.to_string(),
                    RuntimeValue::Float(float) => float.to_string(),
                    RuntimeValue::String(string) => string.clone(),

                    value => return Err(format!(
                        "Variable '{var_name}' does not have a stringifyable type ; found type {}",
                        value
                            .compute_type()
                            .display_inline()
                    )),
                };

                if i == 0 {
                    starts_with = Some(GlobPathStartsWith::Variable {
                        name: (*var_name).to_owned(),
                        value: str.clone(),
                    })
                }

                Ok(str)
            }
        })
        .collect::<Result<Vec<String>, String>>()?;

    Ok(GlobPathOut {
        glob_pattern: globify_finalized_path(&path.join(MAIN_SEPARATOR_STR)),
        starts_with,
    })
}

fn globify_finalized_path(path: &str) -> String {
    let mut path = path
        .split(['/', '\\'])
        .map(Cow::Borrowed)
        .collect::<Vec<_>>();

    let Some(last_part) = path.pop() else {
        return "*".to_owned();
    };

    path.push(if last_part.is_empty() {
        Cow::Borrowed("*")
    } else if last_part == "." {
        Cow::Borrowed(".*")
    } else if last_part.contains('*') {
        last_part
    } else {
        Cow::Owned(format!("*{last_part}*"))
    });

    path.join(MAIN_SEPARATOR_STR)
}
