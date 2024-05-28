//!
//! Binaries resolution module
//!
//! This exposes a binaries resolver that can parse a platform-specific PATH environment variable,
//! and look for a provided command name.
//!
//! It will check permissions and perform implicit extension for Windows executables
//! (allowing to not manually writing `.exe`, `.cmd` or `.bat`).
//!

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
    /// Create a new binaries resolver.
    ///
    /// Will try to read and parse the `PATH` environment variable
    pub fn new() -> Result<Self, PathParsingError> {
        Ok(Self {
            path_dirs: Self::parse_path_var()?,
            entries: HashMap::new(),
        })
    }

    /// Create a new binaries resolver without a PATH
    pub fn empty() -> Self {
        Self {
            path_dirs: vec![],
            entries: HashMap::new(),
        }
    }

    /// Parse the PATH environment variable
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

    /// Refresh the resolver ; required when the PATH variable changes
    pub fn refresh(&mut self) -> Result<(), PathParsingError> {
        self.path_dirs = Self::parse_path_var()?;
        self.clear();
        Ok(())
    }

    /// Clear the resolver (remove all entries)
    pub fn clear(&mut self) {
        self.entries = HashMap::new();
    }

    pub fn path_dirs(&self) -> &Vec<String> {
        &self.path_dirs
    }

    pub fn entries(&self) -> &HashMap<String, PathBuf> {
        &self.entries
    }

    /// Find the executable file matching a specific command name
    ///
    /// The name argument can also be a relative or absolute path.
    ///
    /// Will fail if:
    /// * The command is not found
    /// * A path is provided and the file does not exist
    /// * The detected binary is not marked as executable
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
                    .find_map(|dir| find_exe_in_dir(name, Path::new(dir)).transpose())
                    .ok_or_else(|| format!("command '{name}' was not found"))??;

                self.entries.insert(name.to_owned(), resolved.clone());

                Ok(resolved)
            }
        }
    }
}

fn find_exe_in_dir(name: &str, dir: &Path) -> Result<Option<PathBuf>, String> {
    match TARGET_FAMILY {
        TargetFamily::Windows => {
            let path = dir.join(name);

            Ok(if path.is_file() {
                Some(path)
            } else {
                [
                    dir.join(name),
                    dir.join(format!("{name}.exe")),
                    dir.join(format!("{name}.cmd")),
                    dir.join(format!("{name}.bat")),
                ]
                .into_iter()
                .find(|path| path.is_file())
            })
        }

        TargetFamily::Unix => {
            let path = dir.join(name);

            let Ok(mt) = path.metadata() else {
                return Ok(None);
            };

            if !mt.is_file() {
                return Ok(None);
            }

            #[cfg(target_family = "unix")]
            {
                use std::os::unix::fs::PermissionsExt;

                // Ensure exec permissions are present
                if mt.permissions().mode() & 0o111 != 0 {
                    Ok(Some(path))
                } else {
                    Err(format!(
                        "file '{}' is not marked as executable",
                        path.display()
                    ))
                }
            }

            #[cfg(not(target_family = "unix"))]
            unreachable!()
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
