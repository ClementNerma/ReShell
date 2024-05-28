use std::{
    collections::HashMap,
    fmt,
    path::{Path, PathBuf},
};

use crate::compat::{TargetFamily, PATH_VAR_SEP, TARGET_FAMILY};

pub struct BinariesResolver {
    path_dirs: Vec<String>,
    entries: HashMap<String, PathBuf>,
}

impl BinariesResolver {
    pub fn new() -> Result<Self, PathParsingError> {
        Ok(Self {
            path_dirs: Self::parse_path_var()?,
            entries: HashMap::new(),
        })
    }

    pub fn empty() -> Self {
        Self {
            path_dirs: vec![],
            entries: HashMap::new(),
        }
    }

    fn parse_path_var() -> Result<Vec<String>, PathParsingError> {
        let path_dirs = std::env::var_os("PATH")
            .ok_or(PathParsingError::PathVariableNotSet)?
            .as_os_str()
            .to_str()
            .ok_or(PathParsingError::PathContainsInvalidUtf8)?
            .split(PATH_VAR_SEP)
            .map(str::to_owned)
            .collect();

        Ok(path_dirs)
    }

    pub fn refresh(&mut self) -> Result<(), PathParsingError> {
        self.path_dirs = Self::parse_path_var()?;
        self.clear();
        Ok(())
    }

    pub fn clear(&mut self) {
        self.entries = HashMap::new();
    }

    pub fn path_dirs(&self) -> &Vec<String> {
        &self.path_dirs
    }

    pub fn entries(&self) -> &HashMap<String, PathBuf> {
        &self.entries
    }

    pub fn resolve_binary_path(&mut self, name: &str) -> Result<PathBuf, String> {
        if name.contains('/') || name.contains('\\') {
            let path = Path::new(name);

            return if !path.is_file() {
                Err(format!("file at path '{name}' does not exist"))
            } else {
                Ok(path.to_owned())
            };
        };

        match self.entries.get(name) {
            Some(path) => {
                if !path.exists() {
                    self.entries.remove(name);
                    return self.resolve_binary_path(name);
                }

                Ok(path.to_owned())
            }

            None => {
                let resolved = self
                    .path_dirs
                    .iter()
                    .find_map(|dir| {
                        let dir = Path::new(dir);

                        match TARGET_FAMILY {
                            TargetFamily::Windows => {
                                let path = dir.join(name);

                                if path.is_file() {
                                    return Some(path);
                                }

                                [
                                    dir.join(name),
                                    dir.join(format!("{name}.exe")),
                                    dir.join(format!("{name}.cmd")),
                                    dir.join(format!("{name}.bat")),
                                ]
                                .into_iter()
                                .find(|path| path.is_file())
                            }

                            TargetFamily::Unix => {
                                let path = dir.join(name);

                                let mt = path.metadata().ok()?;

                                if !mt.is_file() {
                                    return None;
                                }

                                #[cfg(target_family = "unix")]
                                {
                                    use std::os::unix::fs::PermissionsExt;

                                    // Ensure exec permissions are present
                                    if mt.permissions().mode() & 0o111 != 0 {
                                        Some(path)
                                    } else {
                                        None
                                    }
                                }

                                #[cfg(not(target_family = "unix"))]
                                unreachable!()
                            }
                        }
                    })
                    .ok_or_else(|| format!("command '{name}' was not found"))?;

                self.entries.insert(name.to_owned(), resolved.clone());

                Ok(resolved)
            }
        }
    }
}

pub enum PathParsingError {
    PathVariableNotSet,
    PathContainsInvalidUtf8,
}

impl fmt::Display for PathParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathParsingError::PathVariableNotSet => write!(f, "PATH variable is not set"),
            PathParsingError::PathContainsInvalidUtf8 => {
                write!(f, "PATH variable contains invalid UTF-8 characters")
            }
        }
    }
}
