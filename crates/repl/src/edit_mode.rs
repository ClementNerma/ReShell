use crossterm::event::Event;
use reedline::{
    EditCommand, EditMode as RlEditMode, KeyCode, KeyModifiers, PromptEditMode, ReedlineEvent,
    ReedlineRawEvent,
};

use crate::{completer::COMPLETION_MENU_NAME, history::HISTORY_MENU_NAME};

pub fn create_edit_mode() -> Box<dyn RlEditMode> {
    Box::new(EditMode {})
}

pub struct EditMode {}

impl RlEditMode for EditMode {
    fn parse_event(&mut self, event: ReedlineRawEvent) -> ReedlineEvent {
        match event.into() {
            Event::FocusGained => ReedlineEvent::None, // TODO: update when reedline gets correct focus support

            Event::FocusLost => ReedlineEvent::None, // TODO: update when reedline gets correct focus support

            Event::Paste(body) => ReedlineEvent::Edit(vec![EditCommand::InsertString(
                body.replace("\r\n", "\n").replace('\r', "\n"),
            )]),

            Event::Mouse(_) => ReedlineEvent::Mouse, // TODO: update when reedline gets correct mouse support

            Event::Resize(cols, rows) => ReedlineEvent::Resize(cols, rows),

            Event::Key(key) => match (key.code, key.modifiers) {
                (KeyCode::Up, KeyModifiers::NONE) => {
                    ReedlineEvent::UntilFound(vec![ReedlineEvent::MenuUp, ReedlineEvent::Up])
                }

                (KeyCode::Down, KeyModifiers::NONE) => {
                    ReedlineEvent::UntilFound(vec![ReedlineEvent::MenuDown, ReedlineEvent::Down])
                }

                (KeyCode::Left, KeyModifiers::NONE) => {
                    ReedlineEvent::UntilFound(vec![ReedlineEvent::MenuLeft, ReedlineEvent::Left])
                }

                (KeyCode::Right, KeyModifiers::NONE) => {
                    ReedlineEvent::UntilFound(vec![
                        ReedlineEvent::HistoryHintComplete,
                        ReedlineEvent::MenuRight, // ?
                        ReedlineEvent::Right,
                    ])
                }

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
                        ReedlineEvent::Edit(vec![EditCommand::CutCurrentLine])
                    }

                    ('z', KeyModifiers::CONTROL) => ReedlineEvent::Edit(vec![EditCommand::Undo]),
                    ('y', KeyModifiers::CONTROL) => ReedlineEvent::Edit(vec![EditCommand::Redo]),
                    ('r', KeyModifiers::CONTROL) => ReedlineEvent::UntilFound(vec![
                        ReedlineEvent::Menu(HISTORY_MENU_NAME.to_string()),
                        ReedlineEvent::MenuNext,
                    ]),

                    (_, _) => ReedlineEvent::Edit(vec![EditCommand::InsertChar(c)]),
                },

                _ => ReedlineEvent::None,
            },
        }
    }

    fn edit_mode(&self) -> PromptEditMode {
        PromptEditMode::Custom("reshell".to_string())
    }
}
