use std::{borrow::Cow, io::ErrorKind};

use reedline::{
    Prompt as RlPrompt, PromptEditMode, PromptHistorySearch, PromptHistorySearchStatus,
};
use reshell_builtins::repl::prompt::PromptRendering;

/// Custom prompt implementation for [`reedline`].
///
/// Can be customized programatically using the builtin `$generatePrompt` shell variable.
pub struct Prompt {
    rendering: PromptRendering,
}

impl Prompt {
    pub fn new(rendering: PromptRendering) -> Self {
        Self { rendering }
    }
}

impl RlPrompt for Prompt {
    fn render_prompt_left(&'_ self) -> Cow<'_, str> {
        if let Some(str) = &self.rendering.prompt_left {
            return str.into();
        }

        match std::env::current_dir() {
            Ok(path) => Cow::Owned(path.to_string_lossy().to_string()),

            Err(err) => Cow::Borrowed(match err.kind() {
                ErrorKind::NotFound => "<current directory does not exist>",
                ErrorKind::PermissionDenied => "<no permission to access current directory>",
                _ => "<current directory unavailable>",
            }),
        }
    }

    fn render_prompt_right(&'_ self) -> Cow<'_, str> {
        if let Some(str) = &self.rendering.prompt_right {
            return str.into();
        }

        "".into()
    }

    fn render_prompt_indicator(&'_ self, prompt_mode: PromptEditMode) -> Cow<'_, str> {
        assert!(matches!(prompt_mode, PromptEditMode::Custom(_)));

        if let Some(str) = &self.rendering.prompt_indicator {
            return str.into();
        }

        "〉".into()
    }

    fn render_prompt_multiline_indicator(&'_ self) -> Cow<'_, str> {
        if let Some(str) = &self.rendering.prompt_multiline_indicator {
            return str.into();
        }

        "::: ".into()
    }

    fn render_prompt_history_search_indicator(
        &'_ self,
        history_search: PromptHistorySearch,
    ) -> Cow<'_, str> {
        let prefix = match history_search.status {
            PromptHistorySearchStatus::Passing => "",
            PromptHistorySearchStatus::Failing => "failing ",
        };

        Cow::Owned(format!(
            "({}reverse-search: {}) ",
            prefix, history_search.term
        ))
    }
}
