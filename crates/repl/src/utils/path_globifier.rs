use std::{
    path::{MAIN_SEPARATOR, MAIN_SEPARATOR_STR},
    sync::LazyLock,
};

use regex::Regex;
use reshell_runtime::{context::Context, values::RuntimeValue};

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

pub fn globify_path(input: &str, ctx: &Context) -> Result<GlobPathOut, String> {
    let segments = segmentize(input);

    let mut starts_with = None;

    let glob_pattern = segments
        .iter()
        .enumerate()
        .map(|(i, segment)| match segment {
            Segment::Raw(input) => {
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

            Segment::VarName(var_name) => {
                let var = ctx
                    .visible_scopes_content()
                    .find_map(|scope| scope.vars.get(*var_name))
                    .ok_or_else(|| format!("Variable '{var_name}' was not found"))?;

                let str = match &var.value.read_promise_no_write().value {
                    RuntimeValue::Int(int) => int.to_string(),
                    RuntimeValue::Float(float) => float.to_string(),
                    RuntimeValue::String(string) => string.clone(),

                    _ => {
                        return Err(format!(
                            "Variable '{var_name}' does not have a string-like type"
                        ))
                    }
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

    let glob_pattern = match glob_pattern.last() {
        Some(last) => {
            if last.is_empty() {
                format!("{}*", glob_pattern.join(MAIN_SEPARATOR_STR))
            } else {
                glob_pattern.join(MAIN_SEPARATOR_STR)
            }
        }

        None => "*".to_owned(),
    };

    Ok(GlobPathOut {
        glob_pattern,
        starts_with,
    })
}

enum Segment<'a> {
    Raw(&'a str),
    VarName(&'a str),
}

fn segmentize(input: &str) -> Vec<Segment> {
    let mut out = vec![];
    let mut last_offset = 0;

    for one_match in VAR_IN_PATH_REGEX.captures_iter(input) {
        let extract = one_match.get(0).unwrap();

        if extract.start() > last_offset {
            out.push(Segment::Raw(&input[last_offset..extract.start()]));
        }

        out.push(Segment::VarName(one_match.get(1).unwrap().as_str()));
        last_offset = extract.end();
    }

    if last_offset < input.len() {
        out.push(Segment::Raw(&input[last_offset..]));
    }

    out
}

fn globify(input: &str) -> String {
    let mut out = String::with_capacity(input.len());

    for (i, segment) in input.split(['/', '\\']).enumerate() {
        if segment.is_empty() {
            continue;
        }

        if i > 0 && !out.ends_with(MAIN_SEPARATOR) {
            out.push(MAIN_SEPARATOR);
        }

        if segment == "." || segment == ".." {
            out.push_str(segment);
            continue;
        }

        if !segment.starts_with('*') {
            out.push('*');
        }

        out.push_str(segment);

        if !segment.ends_with('*') {
            out.push('*');
        }
    }

    out
}

static VAR_IN_PATH_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("\\$([[:alpha:]_][[:alnum:]_]*)").unwrap());
