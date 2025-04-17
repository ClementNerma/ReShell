use std::{
    ffi::OsStr,
    os::unix::ffi::OsStrExt,
    path::{MAIN_SEPARATOR_STR, Path, PathBuf, PrefixComponent},
};

use anyhow::Result;
use parsy::ParsingError;

use crate::{
    compiler::{CaseSensitivity, Component, compile_component},
    parser::{PATTERN_PARSER, PatternType, RawComponent, RawPattern},
};

#[derive(Debug, Default, Clone, Copy)]
pub struct PatternOpts {
    pub case_insensitive: bool,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pattern_type: PatternType,
    common_root_dir: PathBuf,
    components: Vec<Component>,
}

impl Pattern {
    pub fn parse(input: &str, opts: PatternOpts) -> Result<Self, ParsingError> {
        let PatternOpts { case_insensitive } = opts;

        let RawPattern {
            pattern_type,
            components,
        } = PATTERN_PARSER.parse_str(input).map(|parsed| parsed.data)?;

        let mut common_root_dir_components = components
            .iter()
            .map_while(|component| match component {
                RawComponent::Literal(lit) => Some(lit.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        if common_root_dir_components.len() == components.len() {
            common_root_dir_components.pop();
        }

        let common_root_dir = common_root_dir_components.join(MAIN_SEPARATOR_STR);

        Ok(Self {
            pattern_type,
            common_root_dir: PathBuf::from(if matches!(pattern_type, PatternType::Absolute) {
                format!("/{common_root_dir}")
            } else if let PatternType::RelativeToParent { depth } = pattern_type {
                format!(
                    "{}{}{common_root_dir}",
                    format!("..{}", MAIN_SEPARATOR_STR).repeat(depth.into()),
                    if !common_root_dir.is_empty() {
                        MAIN_SEPARATOR_STR
                    } else {
                        ""
                    }
                )
            } else {
                common_root_dir
            }),
            components: components
                .iter()
                .map(|component| {
                    compile_component(
                        component,
                        if case_insensitive {
                            CaseSensitivity::Insensitive
                        } else {
                            CaseSensitivity::Sensitive
                        },
                    )
                })
                .collect(),
        })
    }

    pub fn is_match(&self, path: &Path) -> bool {
        matches!(self.match_against(path), PatternMatchResult::Matched)
    }

    pub fn match_against(&self, path: &Path) -> PatternMatchResult {
        if matches!(self.pattern_type, PatternType::Absolute) && !path.is_absolute() {
            return PatternMatchResult::PathNotAbsolute;
        }

        let (prefix, path_components) = simplify_path_components(path);

        if prefix.is_some() {
            todo!("TODO: handle prefixes in globber");
        }

        match_components(&self.components, &path_components)
    }

    pub fn common_root_dir(&self) -> &Path {
        &self.common_root_dir
    }

    pub fn pattern_type(&self) -> PatternType {
        self.pattern_type
    }
}

fn simplify_path_components(path: &Path) -> (Option<PrefixComponent>, Vec<&OsStr>) {
    use std::path::Component;

    let mut components_iter = path.components();

    let Some(first_component) = components_iter.next() else {
        // Cannot match against empty paths
        return (None, vec![]);
    };

    let mut normalized_components = vec![];

    let prefix = match first_component {
        Component::Prefix(prefix) => Some(prefix),
        Component::RootDir | Component::CurDir => None,

        Component::ParentDir => {
            normalized_components.push(OsStr::new(".."));
            None
        }

        Component::Normal(os_str) => {
            normalized_components.push(os_str);
            None
        }
    };

    for component in components_iter {
        match component {
            Component::Prefix(_) | Component::RootDir => unreachable!(),
            Component::CurDir => continue,
            Component::ParentDir => {
                normalized_components.push(OsStr::new(".."));
            }
            Component::Normal(os_str) => normalized_components.push(os_str),
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
