use reedline::{default_emacs_keybindings, KeyCode, KeyModifiers, Keybindings, ReedlineEvent};

use crate::completer::COMPLETION_MENU_NAME;

pub fn create_keybindings() -> Keybindings {
    let mut keybindings = default_emacs_keybindings();

    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        reedline::ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu(COMPLETION_MENU_NAME.to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );

    keybindings
}
