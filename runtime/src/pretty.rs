#[derive(Clone, Copy)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
}

pub trait PrettyPrintable {
    fn generate(&self) -> PrintablePiece;
}

pub struct Colored(String, Option<Color>);

impl Colored {
    fn len_chars(&self) -> usize {
        self.0.chars().count()
    }
}

pub enum PrintablePiece {
    Atomic(Colored),
    Suite {
        begin: Colored,
        items: Vec<PrintablePiece>,
        sep: Colored,
        end: Colored,
    },
}

pub struct PrettyPrintOptions {
    pub pretty: bool,
    pub tab_size: usize,
    pub colors: bool,
    pub max_line_size: usize,
}

impl PrintablePiece {
    fn len_chars(&self) -> usize {
        match self {
            PrintablePiece::Atomic(atom) => atom.len_chars(),

            PrintablePiece::Suite {
                begin,
                items,
                sep,
                end,
            } => {
                begin.len_chars()
                    + items.iter().map(Self::len_chars).sum::<usize>()
                    + if items.is_empty() {
                        0
                    } else {
                        sep.len_chars() * (items.len() - 1)
                    }
                    + end.len_chars()
            }
        }
    }

    fn fits_in_line(&self, max_line_size: usize) -> bool {
        self.len_chars() <= max_line_size
    }

    pub fn render(&self, w: &impl Fn(&str, Option<Color>)) {
        match self {
            PrintablePiece::Atomic(atom) => w(&atom.0, atom.1),
            PrintablePiece::Suite {
                begin,
                items,
                sep,
                end,
            } => {
                // if self fits inline => render everything inline
                // otherwise => render every item separately, and use indentation
            }
        }
    }
}
