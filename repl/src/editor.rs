use reedline::{EditMode, Emacs};

use crate::keybindings::create_keybindings;

pub fn create_edit_mode() -> Box<dyn EditMode> {
    Box::new(Emacs::new(create_keybindings()))
}
