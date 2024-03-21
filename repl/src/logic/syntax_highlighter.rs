use nu_ansi_term::Style;
use reedline::StyledText;
use regex::Regex;

pub struct SyntaxHighlighter<'a> {
    input: &'a str,
    highlighted: Vec<Highlighted>,
}

impl<'a> SyntaxHighlighter<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            highlighted: vec![],
        }
    }

    pub fn regex(&mut self, regex: Regex, styles: &[Style]) {
        let mut items = vec![];

        for cap in regex.captures_iter(self.input) {
            assert_eq!(
                cap.len() - 1,
                styles.len(),
                "Number of captures do not match number of styles"
            );

            for (i, style) in styles.iter().enumerate() {
                let cap = cap.get(i + 1).unwrap();

                if self.highlighted.iter().any(|item| {
                    cap.start() < item.start + item.len && item.start < cap.start() + cap.len()
                }) {
                    continue;
                }

                items.push(Highlighted {
                    start: cap.start(),
                    len: cap.len(),
                    style: *style,
                })
            }
        }

        self.highlighted.extend(items);
    }

    pub fn range(&mut self, start: usize, len: usize, style: Style) {
        assert!(self.input[start..start + len].len() == len);

        let overlapping = self
            .highlighted
            .iter()
            .find(|h| h.start >= start && h.start + h.len <= start + len);

        if let Some(overlapping) = overlapping {
            panic!(
                "Found overlapping range:\n> Tried range {:?}\n> Found existing {overlapping:?}",
                Highlighted { start, len, style }
            );
        }

        self.highlighted.push(Highlighted { start, len, style });
    }

    pub fn finalize(mut self, blank_style: Style) -> StyledText {
        self.highlighted.sort_by_key(|item| item.start);

        let mut out = StyledText::new();

        let mut last_pos = 0;

        for Highlighted { start, len, style } in self.highlighted {
            if start > last_pos {
                out.push((blank_style, self.input[last_pos..start].to_string()));
            }

            out.push((style, self.input[start..start + len].to_string()));

            last_pos = start + len;
        }

        if self.input.len() > last_pos {
            out.push((blank_style, self.input[last_pos..].to_string()));
        }

        out
    }
}

#[derive(Debug)]
struct Highlighted {
    pub start: usize,
    pub len: usize,
    pub style: Style,
}
