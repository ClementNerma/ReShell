//!
//! Path globification module.
//!
//! Allows to create a glob pattern from a path input.
//!
//! Used for auto-completion of paths.
//!

use std::path::{MAIN_SEPARATOR, MAIN_SEPARATOR_STR};

use reshell_runtime::{context::Context, values::RuntimeValue};
use reshell_shared::pretty::{PrettyPrintOptions, PrettyPrintable};

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

    let glob_pattern = segments
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

                        return Ok(format!("{home_dir}{MAIN_SEPARATOR}{}", globify(stripped)));
                    }

                    if input.starts_with("./") || input.starts_with(".\\") {
                        starts_with = Some(GlobPathStartsWith::CurrentDir);
                    }
                }

                Ok(globify(input))
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
                            .display(ctx.type_alias_store(), PrettyPrintOptions::inline())
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

    let glob_pattern = glob_pattern.join(MAIN_SEPARATOR_STR);

    Ok(GlobPathOut {
        glob_pattern: if glob_pattern.is_empty() {
            "*".to_owned()
        } else {
            glob_pattern
        },
        starts_with,
    })
}

fn globify(input: &str) -> String {
    let mut globified_segments: Vec<String> = vec![];

    let segments = input.split(['/', '\\']).collect::<Vec<_>>();

    for (i, segment) in segments.iter().copied().enumerate() {
        if segment.is_empty() {
            if globified_segments.is_empty() {
                globified_segments.push(String::new());
            } else if i + 1 == segments.len() {
                globified_segments.push(String::from('*'))
            }
        } else if segment == "." && i + 1 == segments.len() {
            globified_segments.push(".*".to_owned());
        } else if segment == "." || segment == ".." || segment.contains(':') {
            globified_segments.push(segment.to_owned());
        } else {
            let mut globified = String::with_capacity(segment.len());

            if !segment.starts_with('*') {
                globified.push('*');
            }

            globified.push_str(segment);

            if !segment.ends_with('*') {
                globified.push('*');
            }

            globified_segments.push(globified);
        }
    }

    globified_segments.join(MAIN_SEPARATOR_STR)
}
