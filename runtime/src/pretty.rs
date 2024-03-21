use colored::{Color, Colorize};

use crate::context::Context;

pub trait PrettyPrintable {
    fn generate_pretty_data(&self, ctx: &Context) -> PrintablePiece;

    fn render_uncolored(&self, ctx: &Context, opts: PrettyPrintOptions) -> String {
        let mut out = String::new();

        self.generate_pretty_data(ctx)
            .render(opts, |Colored(ref string, _)| {
                out.push_str(string);
            });

        out
    }

    fn render_colored(&self, ctx: &Context, opts: PrettyPrintOptions) -> String {
        let mut out = String::new();

        self.generate_pretty_data(ctx)
            .render(opts, |Colored(ref string, color)| match color {
                Some(color) => {
                    out.push_str(&format!("{}", string.color(*color)));
                }
                None => out.push_str(string),
            });

        out
    }
}

pub struct Colored(pub String, pub Option<Color>);

impl Colored {
    pub fn with_color(content: impl Into<String>, color: Color) -> Self {
        Self(content.into(), Some(color))
    }

    pub fn colorless(content: impl Into<String>) -> Self {
        Self(content.into(), None)
    }

    pub fn empty() -> Self {
        Self(String::new(), None)
    }

    fn len_chars(&self) -> usize {
        self.0.chars().count()
    }
}

pub enum PrintablePiece {
    Atomic(Colored),
    Suite(Vec<Colored>),
    List {
        begin: Colored,
        items: Vec<PrintablePiece>,
        sep: Colored,
        end: Colored,
        suffix: Option<Box<PrintablePiece>>,
    },
    Join(Vec<PrintablePiece>),
}

impl PrintablePiece {
    pub fn colored_atomic(content: impl Into<String>, color: Color) -> Self {
        Self::Atomic(Colored(content.into(), Some(color)))
    }
}

#[derive(Clone, Copy)]
pub struct PrettyPrintOptions {
    pub pretty: bool,
    pub tab_size: usize,
    // pub colors: bool,
    pub max_line_size: usize,
    pub line_prefix_size: usize,
}

impl PrettyPrintOptions {
    pub fn inline() -> Self {
        Self {
            pretty: false,
            tab_size: 0,
            max_line_size: 0,
            line_prefix_size: 0,
        }
    }
}

impl PrintablePiece {
    fn len_chars(&self) -> usize {
        match self {
            PrintablePiece::Atomic(atom) => atom.len_chars(),

            PrintablePiece::Suite(items) => {
                items.iter().map(|item| item.len_chars()).sum::<usize>()
            }

            PrintablePiece::List {
                begin,
                items,
                sep,
                end,
                suffix,
            } => {
                begin.len_chars()
                    + items.iter().map(Self::len_chars).sum::<usize>()
                    + if items.is_empty() {
                        0
                    } else {
                        (sep.len_chars() + 1/* space */) * (items.len() - 1)
                    }
                    + end.len_chars()
                    + match suffix {
                        Some(suffix) => suffix.len_chars(),
                        None => 0,
                    }
            }

            PrintablePiece::Join(pieces) => pieces.iter().map(PrintablePiece::len_chars).sum(),
        }
    }

    fn fits_in_line(&self, max_line_size: usize, prefix_size: usize) -> bool {
        self.len_chars() + prefix_size <= max_line_size
    }

    pub fn render(&self, opts: PrettyPrintOptions, mut w: impl FnMut(&Colored)) {
        self.render_inner(opts, &mut w, 0);
    }

    fn render_inner(
        &self,
        opts: PrettyPrintOptions,
        w: &mut impl FnMut(&Colored),
        current_ident: usize,
    ) {
        let PrettyPrintOptions {
            pretty,
            tab_size,
            // colors,
            max_line_size,
            line_prefix_size,
        } = opts;

        match self {
            PrintablePiece::Atomic(atom) => w(atom),

            PrintablePiece::Suite(items) => {
                for item in items {
                    w(item);
                }
            }

            PrintablePiece::List {
                begin,
                items,
                sep,
                end,
                suffix,
            } => {
                if !pretty || self.fits_in_line(max_line_size, current_ident + line_prefix_size) {
                    let space = Colored(" ".to_string(), None);

                    w(begin);

                    for (i, item) in items.iter().enumerate() {
                        item.render_inner(opts, w, current_ident);

                        if i < items.len() - 1 {
                            w(sep);
                            w(&space);
                        }
                    }

                    w(end);
                } else {
                    w(begin);

                    let spacing =
                        Colored(format!("\n{}", " ".repeat(current_ident + tab_size)), None);

                    for (i, item) in items.iter().enumerate() {
                        w(&spacing);

                        item.render_inner(opts, w, current_ident + tab_size);

                        if i < items.len() - 1 {
                            w(sep);
                        }
                    }

                    w(&Colored(format!("\n{}", " ".repeat(current_ident)), None));

                    w(end);

                    if let Some(suffix) = suffix {
                        suffix.render_inner(opts, w, current_ident);
                    }
                }
            }

            PrintablePiece::Join(pieces) => {
                for piece in pieces {
                    piece.render_inner(opts, w, current_ident);
                }
            }
        }
    }
}
