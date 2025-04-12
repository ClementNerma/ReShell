use std::{
    ffi::OsStr,
    os::unix::ffi::OsStrExt,
    path::{Path, PrefixComponent},
};

use anyhow::Result;
use parsy::ParsingError;

use crate::{
    compiler::{Component, compile_component},
    parser::{PATTERN_PARSER, RawPattern},
};

#[derive(Debug)]
pub struct Pattern {
    is_absolute: bool,
    components: Vec<Component>,
}

impl Pattern {
    // TODO: option for case insensitivity
    pub fn parse(input: &str) -> Result<Self, ParsingError> {
        let RawPattern {
            is_absolute,
            components,
        } = PATTERN_PARSER.parse_str(input).map(|parsed| parsed.data)?;

        Ok(Self {
            is_absolute,
            components: components.iter().map(compile_component).collect(),
        })
    }

    pub fn is_match(&self, path: &Path) -> bool {
        matches!(self.match_against(path), PatternMatchResult::Matched)
    }

    pub fn match_against(&self, path: &Path) -> PatternMatchResult {
        if self.is_absolute && !path.is_absolute() {
            return PatternMatchResult::PathNotAbsolute;
        }

        let (prefix, path_components) = simplify_path_components(path);

        if prefix.is_some() {
            todo!("TODO: handle prefixes in globber");
        }

        match_components(&self.components, &path_components)
    }

    pub fn is_absolute(&self) -> bool {
        self.is_absolute
    }
}

fn simplify_path_components(path: &Path) -> (Option<PrefixComponent>, Vec<&OsStr>) {
    let mut components_iter = path.components();

    let Some(first_component) = components_iter.next() else {
        // Cannot match against empty paths
        return (None, vec![]);
    };

    let mut normalized_components = vec![];

    let prefix = match first_component {
        std::path::Component::Prefix(prefix) => Some(prefix),

        std::path::Component::RootDir | std::path::Component::CurDir => None,

        std::path::Component::ParentDir => return (None, vec![]),

        std::path::Component::Normal(os_str) => {
            normalized_components.push(os_str);
            None
        }
    };

    for component in components_iter {
        match component {
            std::path::Component::Prefix(_) | std::path::Component::RootDir => unreachable!(),
            std::path::Component::CurDir => continue,
            std::path::Component::ParentDir => {
                normalized_components.pop();
            }
            std::path::Component::Normal(os_str) => normalized_components.push(os_str),
        }
    }

    (prefix, normalized_components)
}

fn match_components(components: &[Component], mut path: &[&OsStr]) -> PatternMatchResult {
    for i in 0..components.len() {
        match &components[i] {
            Component::Wildcard => {
                if path.is_empty() {
                    return if components[i + 1..]
                        .iter()
                        .any(|component| matches!(component, Component::Regex(_)))
                    {
                        PatternMatchResult::Starved
                    } else {
                        PatternMatchResult::Matched
                    };
                }

                let mut all_starved = true;

                for j in 0..path.len() {
                    match match_components(&components[i + 1..], &path[j..]) {
                        PatternMatchResult::PathNotAbsolute => unreachable!(),
                        PatternMatchResult::Matched => return PatternMatchResult::Matched,
                        PatternMatchResult::NotMatched => {
                            all_starved = false;
                        }
                        PatternMatchResult::Starved => {}
                    }
                }

                return if all_starved {
                    PatternMatchResult::Starved
                } else {
                    PatternMatchResult::NotMatched
                };
            }

            Component::Regex(regex) => {
                let Some(part) = path.first() else {
                    return PatternMatchResult::Starved;
                };

                path = &path[1..];

                if !regex.is_match(part.as_bytes()) {
                    return PatternMatchResult::NotMatched;
                }
            }
        }
    }

    if path.is_empty() {
        PatternMatchResult::Matched
    } else {
        PatternMatchResult::NotMatched
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PatternMatchResult {
    PathNotAbsolute,
    Matched,
    NotMatched,
    Starved,
}
