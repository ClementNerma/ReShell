use nu_ansi_term::{Color, Style};
use parsy::CodeRange;
use reedline::StyledText;

pub struct HighlightList<'a> {
    source: &'a str,
    rendered: StyledText,
    prev: usize,
}

impl<'a> HighlightList<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            rendered: StyledText::new(),
            prev: 0,
        }
    }

    pub fn into_rendered(mut self) -> StyledText {
        self.push_untreated_bit(self.source.as_bytes().len());
        self.rendered
    }
}

impl<'a> HighlightList<'a> {
    fn push(&mut self, at: CodeRange, style: Style) {
        assert!(at.start.offset >= self.prev);

        let start = at.start.offset;
        let end = start + at.len;

        self.push_untreated_bit(start);

        self.rendered
            .push((style, self.source[start..end].to_string()));

        self.prev = start + at.len;
    }

    fn push_untreated_bit(&mut self, start: usize) {
        if start > self.prev {
            self.rendered
                .push((Style::default(), self.source[self.prev..start].to_string()));
        }
    }

    pub fn keyword(&mut self, at: CodeRange) {
        self.push(at, Color::Cyan.into());
    }

    // pub fn special_symbol(&mut self, at: CodeRange) {
    //     self.push(at, Color::Cyan.into());
    // }

    pub fn comment(&mut self, comment: CodeRange) {
        self.push(comment, Color::DarkGray.into());
    }

    pub fn var_name(&mut self, at: CodeRange) {
        self.push(at, Color::LightPurple.into());
    }

    pub fn env_var_name(&mut self, at: CodeRange) {
        self.push(at, Style::new().fg(Color::Blue).bold());
    }

    pub fn fn_name(&mut self, at: CodeRange) {
        self.push(at, Style::new().fg(Color::LightPurple).bold());
    }

    pub fn prop_name(&mut self, at: CodeRange) {
        self.push(at, Color::LightPurple.into());
    }

    pub fn flag(&mut self, at: CodeRange) {
        self.push(at, Style::new().fg(Color::LightMagenta).bold());
    }

    pub fn type_name(&mut self, at: CodeRange) {
        self.push(at, Style::new().fg(Color::LightYellow).bold());
    }

    pub fn bool(&mut self, at: CodeRange) {
        self.push(at, Color::Yellow.into());
    }

    pub fn int(&mut self, at: CodeRange) {
        self.push(at, Color::Green.into());
    }

    pub fn float(&mut self, at: CodeRange) {
        self.push(at, Color::Green.into());
    }

    pub fn string(&mut self, at: CodeRange) {
        self.push(at, Color::LightGreen.into());
    }

    pub fn escaped_char(&mut self, at: CodeRange) {
        self.push(at, Color::Cyan.into())
    }

    pub fn raw_str(&mut self, _: CodeRange) {
        // do nothing?
    }

    pub fn path(&mut self, at: CodeRange) {
        self.push(at, Color::Blue.into());
    }
}
