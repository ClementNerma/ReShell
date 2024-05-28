//!
//! Compatibility definition module
//!
//! Allows to detect which platform we're running on
//!

/// System target family
#[derive(Clone, Copy)]
pub enum TargetFamily {
    #[allow(dead_code)]
    Windows,

    #[allow(dead_code)]
    Unix,
}

#[cfg(target_family = "windows")]
pub static TARGET_FAMILY: TargetFamily = TargetFamily::Windows;

#[cfg(target_family = "unix")]
pub static TARGET_FAMILY: TargetFamily = TargetFamily::Unix;

#[cfg(not(any(target_family = "windows", target_family = "unix")))]
compile_error!("Only 'Windows' and 'Unix' families are supported!");

/// Single-character `PATH` environment variable separator (platform-specific)
pub static PATH_VAR_SEP: char = match TARGET_FAMILY {
    TargetFamily::Windows => ';',
    TargetFamily::Unix => ':',
};
