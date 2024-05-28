use reedline::{
    EditCommand, EditMode as RlEditMode, Emacs, Event, KeyCode, KeyModifiers, PromptEditMode,
    ReedlineEvent,
};

use crate::completer::COMPLETION_MENU_NAME;

pub fn create_edit_mode() -> Box<dyn RlEditMode> {
    Box::new(EditMode {
        default: Emacs::default(),
    })
}

pub struct EditMode {
    default: Emacs,
}

impl RlEditMode for EditMode {
    fn parse_event(&mut self, event: Event) -> ReedlineEvent {
        match event {
            Event::Mouse(_) => self.default.parse_event(event),
            Event::Resize(_, _) => self.default.parse_event(event),
            Event::Key(key) => match (key.code, key.modifiers) {
                (KeyCode::Left, KeyModifiers::NONE) => ReedlineEvent::Left,
                (KeyCode::Right, KeyModifiers::NONE) => ReedlineEvent::Right,
                (KeyCode::Up, KeyModifiers::NONE) => ReedlineEvent::Up,
                (KeyCode::Down, KeyModifiers::NONE) => ReedlineEvent::Down,

                (KeyCode::Left, KeyModifiers::CONTROL) => {
                    ReedlineEvent::Edit(vec![EditCommand::MoveWordLeft])
                }

                (KeyCode::Right, KeyModifiers::CONTROL) => {
                    ReedlineEvent::Edit(vec![EditCommand::MoveWordRight])
                }

                (KeyCode::Home, KeyModifiers::NONE) => {
                    ReedlineEvent::Edit(vec![EditCommand::MoveToStart])
                }

                (KeyCode::End, KeyModifiers::NONE) => {
                    ReedlineEvent::Edit(vec![EditCommand::MoveToEnd])
                }

                (KeyCode::Backspace, KeyModifiers::NONE) => {
                    ReedlineEvent::Edit(vec![EditCommand::Backspace])
                }

                (KeyCode::Backspace, KeyModifiers::CONTROL) => {
                    ReedlineEvent::Edit(vec![EditCommand::BackspaceWord])
                }

                (KeyCode::Delete, KeyModifiers::NONE) => {
                    ReedlineEvent::Edit(vec![EditCommand::Delete])
                }

                (KeyCode::Delete, KeyModifiers::CONTROL) => {
                    ReedlineEvent::Edit(vec![EditCommand::DeleteWord])
                }

                (KeyCode::Tab, KeyModifiers::NONE) => ReedlineEvent::UntilFound(vec![
                    ReedlineEvent::Menu(COMPLETION_MENU_NAME.to_string()),
                    ReedlineEvent::MenuNext,
                ]),

                (KeyCode::Tab, KeyModifiers::SHIFT) => ReedlineEvent::UntilFound(vec![
                    ReedlineEvent::Menu(COMPLETION_MENU_NAME.to_string()),
                    ReedlineEvent::MenuPrevious,
                ]),

                (KeyCode::Enter, KeyModifiers::SHIFT) => {
                    ReedlineEvent::Edit(vec![EditCommand::InsertNewline])
                }

                (KeyCode::Enter, KeyModifiers::NONE) => ReedlineEvent::Enter,

                (KeyCode::Char(c), modifier) => match (c, modifier) {
                    ('c', KeyModifiers::CONTROL) => ReedlineEvent::CtrlC,
                    ('d', KeyModifiers::CONTROL) => ReedlineEvent::CtrlD,
                    ('l', KeyModifiers::CONTROL) => ReedlineEvent::ClearScreen,
                    ('u', KeyModifiers::CONTROL) => {
                        // TODO: find a better command
                        ReedlineEvent::Edit(vec![EditCommand::CutCurrentLine])
                    }
                    ('z', KeyModifiers::CONTROL) => ReedlineEvent::Edit(vec![EditCommand::Undo]),
                    ('y', KeyModifiers::CONTROL) => ReedlineEvent::Edit(vec![EditCommand::Redo]),

                    ('"', KeyModifiers::NONE) => ReedlineEvent::Edit(vec![
                        EditCommand::InsertString("\"\"".to_string()),
                        EditCommand::MoveLeft,
                    ]),

                    ('(', KeyModifiers::NONE) => ReedlineEvent::Edit(vec![
                        EditCommand::InsertString("()".to_string()),
                        EditCommand::MoveLeft,
                    ]),

                    ('[', KeyModifiers::NONE) => ReedlineEvent::Edit(vec![
                        EditCommand::InsertString("[]".to_string()),
                        EditCommand::MoveLeft,
                    ]),

                    ('{', KeyModifiers::NONE) => ReedlineEvent::Edit(vec![
                        EditCommand::InsertString("{}".to_string()),
                        EditCommand::MoveLeft,
                    ]),

                    (_, KeyModifiers::NONE) => {
                        ReedlineEvent::Edit(vec![EditCommand::InsertChar(c)])
                    }

                    (_, KeyModifiers::SHIFT) => {
                        ReedlineEvent::Edit(vec![EditCommand::InsertString(
                            c.to_uppercase().to_string(),
                        )])
                    }

                    _ => ReedlineEvent::None,
                },

                _ => ReedlineEvent::None,
            },
        }
    }

    fn edit_mode(&self) -> PromptEditMode {
        PromptEditMode::Custom("reshell".to_string())
    }
}