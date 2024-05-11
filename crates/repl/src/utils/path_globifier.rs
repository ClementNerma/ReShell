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

                    if input.starts_with("./") || input.starts_with(".\\\\") {
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

#[derive(Debug)]
enum Segment<'a> {
    Raw(&'a str),
    VarName(&'a str),
}

fn segmentize(input: &str) -> Vec<Segment> {
    let mut out = vec![];
    let mut last_offset = 0;

    for one_match in VAR_IN_PATH_REGEX.captures_iter(input) {
        let extract = one_match.get(0).unwrap();

        let mut backslashes = 0;

        for c in input[..extract.start()].chars().rev() {
            if c == '\\' {
                backslashes += 1;
            } else {
                break;
            }
        }

        out.push(if extract.start() > last_offset || backslashes % 2 != 0 {
            Segment::Raw(&input[last_offset..extract.end()])
        } else {
            Segment::VarName(one_match.get(1).unwrap().as_str())
        });

        last_offset = extract.end();
    }

    if last_offset < input.len() {
        out.push(Segment::Raw(&input[last_offset..]));
    }

    out
}

fn globify(input: &str) -> String {
    let mut globified_segments: Vec<String> = vec![];
    let mut curr_segment = String::new();
    let mut escaping = false;

    // TODO: last "." segment (not followed by a separator) should be globifyable (e.g. .gitignore, .env, etc.)
    let mut push_segment = |old_curr_segment: &mut String, is_last: bool| {
        let curr_segment = old_curr_segment.clone(); // TODO: no .clone()
        old_curr_segment.clear();

        // TODO: deal better with empty segments!
        if curr_segment.is_empty() {
            if globified_segments.is_empty() {
                globified_segments.push(String::new());
            } else if is_last {
                globified_segments.push(String::from('*'))
            }
        } else if curr_segment == "." || curr_segment == ".." || curr_segment.contains(':') {
            globified_segments.push(curr_segment);
        } else {
            // TODO: don't add stars if there is already one at the beginning or the end
            let mut segment = String::with_capacity(curr_segment.len());

            if !curr_segment.starts_with('*') {
                segment.push('*');
            }

            segment.push_str(&curr_segment);

            if !curr_segment.ends_with('*') {
                segment.push('*');
            }

            globified_segments.push(segment);
        }
    };

    for c in input.chars() {
        if (escaping && c == '\\') || (!escaping && c == '/') {
            escaping = false;
            push_segment(&mut curr_segment, false);
            continue;
        }

        if escaping {
            escaping = false;
            curr_segment.push(c);
        } else if c == '\\' {
            escaping = true;
        } else {
            curr_segment.push(c);
        }
    }

    push_segment(&mut curr_segment, true);

    globified_segments.join(MAIN_SEPARATOR_STR)
}

static VAR_IN_PATH_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("\\$([[:alpha:]_][[:alnum:]_]*)").unwrap());
