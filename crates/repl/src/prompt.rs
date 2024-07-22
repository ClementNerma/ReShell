use std::borrow::Cow;

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
    fn render_prompt_left(&self) -> Cow<str> {
        if let Some(str) = &self.rendering.prompt_left {
            return str.into();
        }

        std::env::current_dir()
            .unwrap()
            .to_string_lossy()
            .to_string()
            .into()
    }

    fn render_prompt_right(&self) -> Cow<str> {
        if let Some(str) = &self.rendering.prompt_right {
            return str.into();
        }

        "".into()
    }

    fn render_prompt_indicator(&self, prompt_mode: PromptEditMode) -> Cow<str> {
        assert!(matches!(prompt_mode, PromptEditMode::Custom(_)));

        if let Some(str) = &self.rendering.prompt_indicator {
            return str.into();
        }

        "〉".into()
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        if let Some(str) = &self.rendering.prompt_multiline_indicator {
            return str.into();
        }

        "::: ".into()
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<str> {
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
