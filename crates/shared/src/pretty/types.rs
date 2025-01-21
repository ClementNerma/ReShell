use std::fmt::Display;

use colored::{Color, ColoredString, Colorize};

/// Trait enabling pretty-printing for custom types
///
/// It will allow to generate configurable displayable data
pub trait PrettyPrintable {
    /// Data required for pretty-printing
    type Context: ?Sized;

    /// Generate pretty-printing data for later processing
    fn generate_pretty_data(&self, ctx: &Self::Context) -> PrettyPrintablePiece;

    /// Obtain a [`Display`] type from this value
    fn display<'p, 'c>(
        &'p self,
        ctx: &'c Self::Context,
        opts: PrettyPrintOptions,
    ) -> PrettyPrintableDisplay<'p, 'c, Self>
    where
        Self: Sized,
    {
        PrettyPrintableDisplay {
            source: self,
            ctx,
            opts,
            no_colors: false,
        }
    }
}

/// Pretty-printable with options
pub struct PrettyPrintableDisplay<'p, 'c, P: PrettyPrintable> {
    pub source: &'p P,
    pub ctx: &'c P::Context,
    pub opts: PrettyPrintOptions,
    pub no_colors: bool,
}

impl<P: PrettyPrintable> PrettyPrintableDisplay<'_, '_, P> {
    pub fn no_colors(mut self) -> Self {
        self.no_colors = true;
        self
    }
}

impl<P: PrettyPrintable> Display for PrettyPrintableDisplay<'_, '_, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.source
            .generate_pretty_data(self.ctx)
            .render(self.opts, |styled| {
                if self.no_colors {
                    write!(f, "{}", styled.inner().input).unwrap()
                } else {
                    write!(f, "{}", styled.inner()).unwrap()
                }
            });

        Ok(())
    }
}

/// Colored string
#[derive(Clone)]
pub struct Styled(ColoredString);

impl Styled {
    pub fn inner(&self) -> &ColoredString {
        &self.0
    }

    pub fn colored(content: impl AsRef<str>, color: Color) -> Self {
        Self(content.as_ref().color(color))
    }

    pub fn colorless(content: impl AsRef<str>) -> Self {
        Self(ColoredString::from(content.as_ref()))
    }

    pub fn empty() -> Self {
        Self(ColoredString::from(""))
    }

    fn len_chars(&self) -> usize {
        self.0.chars().count()
    }
}

impl From<ColoredString> for Styled {
    fn from(value: ColoredString) -> Self {
        Self(value)
    }
}

/// Pretty-printable piece
#[derive(Clone)]
pub enum PrettyPrintablePiece {
    /// Atom: a simple string with a single color
    Atomic(Styled),

    /// Suite: a chain of atoms
    Suite(Vec<Styled>),

    /// List: a list with a beginning and end pieces, and a value separator
    /// Will be printed differently depending on the configuration
    List {
        begin: Vec<Styled>,
        items: Vec<PrettyPrintablePiece>,
        sep: Vec<Styled>,
        end: Vec<Styled>,
        suffix: Option<Box<PrettyPrintablePiece>>,
    },

    /// Join: a chain of pretty-printable pieces
    Join(Vec<PrettyPrintablePiece>),
}

impl PrettyPrintablePiece {
    /// Create a colored atom
    pub fn colored_atomic(content: impl AsRef<str>, color: Color) -> Self {
        Self::Atomic(Styled::colored(content, color))
    }
}

/// Options for pretty-printing
#[derive(Clone, Copy)]
pub struct PrettyPrintOptions {
    /// Display in a pretty manner.
    ///
    /// Will add spacing and newlines to improve readability.
    pub pretty: bool,

    /// How many spaces to represent a tab with
    pub tab_size: usize,

    /// Ideal maximum line size
    ///
    /// Some lines may be larger than this limit, consider it a "best-effort"
    pub max_line_size: usize,

    pub line_prefix_size: usize,
}

impl PrettyPrintOptions {
    /// Render in a single line, without most readibility spaces
    pub fn inline() -> Self {
        Self {
            pretty: false,
            tab_size: 0,
            max_line_size: 0,
            line_prefix_size: 0,
        }
    }

    /// Render on multiple lines and add spaces if it can improve readability
    pub fn multiline() -> Self {
        Self {
            pretty: true,
            tab_size: 4,
            max_line_size: 80,
            line_prefix_size: 0,
        }
    }
}

impl PrettyPrintablePiece {
    /// Compute how much characters will be displayed when rendering this piece on a single line
    ///
    /// Used to determine if this piece should be rendered on multiple lines (if this option is enabled)
    fn display_chars_count(&self) -> usize {
        match self {
            PrettyPrintablePiece::Atomic(atom) => atom.len_chars(),

            PrettyPrintablePiece::Suite(items) => {
                items.iter().map(|item| item.len_chars()).sum::<usize>()
            }

            PrettyPrintablePiece::List {
                begin,
                items,
                sep,
                end,
                suffix,
            } => {
                let begin_len = begin.iter().map(|s| s.len_chars()).sum::<usize>();
                let sep_len = sep.iter().map(|s| s.len_chars()).sum::<usize>();
                let end_len = end.iter().map(|s| s.len_chars()).sum::<usize>();

                begin_len
                    + items.iter().map(Self::display_chars_count).sum::<usize>()
                    + if items.is_empty() {
                        0
                    } else {
                        (sep_len + 1/* space */) * (items.len() - 1)
                    }
                    + end_len
                    + match suffix {
                        Some(suffix) => suffix.display_chars_count(),
                        None => 0,
                    }
            }

            PrettyPrintablePiece::Join(pieces) => pieces
                .iter()
                .map(PrettyPrintablePiece::display_chars_count)
                .sum(),
        }
    }

    fn fits_in_line(&self, max_line_size: usize, prefix_size: usize) -> bool {
        self.display_chars_count() + prefix_size <= max_line_size
    }

    /// Render this piece using a processing function
    ///
    /// Avoids unnecessary heap allocations (some will still happen)
    pub fn render(&self, opts: PrettyPrintOptions, mut w: impl FnMut(&Styled)) {
        self.render_inner(opts, &mut w, 0);
    }

    fn render_inner(
        &self,
        opts: PrettyPrintOptions,
        w: &mut impl FnMut(&Styled),
        current_ident: usize,
    ) {
        let PrettyPrintOptions {
            pretty,
            tab_size,
            max_line_size,
            line_prefix_size,
        } = opts;

        match self {
            PrettyPrintablePiece::Atomic(atom) => w(atom),

            PrettyPrintablePiece::Suite(items) => {
                for item in items {
                    w(item);
                }
            }

            PrettyPrintablePiece::List {
                begin,
                items,
                sep,
                end,
                suffix,
            } => {
                if !pretty || self.fits_in_line(max_line_size, current_ident + line_prefix_size) {
                    let space = Styled::colorless(" ");

                    for styled in begin {
                        w(styled);
                    }

                    for (i, item) in items.iter().enumerate() {
                        item.render_inner(opts, w, current_ident);

                        if i < items.len() - 1 {
                            for styled in sep {
                                w(styled);
                            }

                            w(&space);
                        }
                    }

                    for styled in end {
                        w(styled);
                    }
                } else {
                    for styled in begin {
                        w(styled);
                    }
                    let spacing =
                        Styled::colorless(format!("\n{}", " ".repeat(current_ident + tab_size)));

                    for (i, item) in items.iter().enumerate() {
                        w(&spacing);

                        item.render_inner(opts, w, current_ident + tab_size);

                        if i < items.len() - 1 {
                            for styled in sep {
                                w(styled);
                            }
                        }
                    }

                    w(&Styled::colorless(format!(
                        "\n{}",
                        " ".repeat(current_ident)
                    )));

                    for styled in end {
                        w(styled);
                    }
                }

                if let Some(suffix) = suffix {
                    suffix.render_inner(opts, w, current_ident);
                }
            }

            PrettyPrintablePiece::Join(pieces) => {
                for piece in pieces {
                    piece.render_inner(opts, w, current_ident);
                }
            }
        }
    }
}

impl PrettyPrintable for PrettyPrintablePiece {
    type Context = ();

    fn generate_pretty_data(&self, _: &Self::Context) -> PrettyPrintablePiece {
        self.clone()
    }
}
