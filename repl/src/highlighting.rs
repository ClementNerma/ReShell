use nu_ansi_term::Style;
use reedline::StyledText;
use regex::Regex;

pub struct SyntaxHighlighter<'a> {
    text: &'a str,
    items: Vec<Highlighted>,
}

impl<'a> SyntaxHighlighter<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text,
            items: vec![],
        }
    }

    pub fn highlight(&mut self, regex: Regex, styles: &[Style]) {
        let mut items = vec![];

        for cap in regex.captures_iter(self.text) {
            assert_eq!(cap.len() - 1, styles.len());

            for (i, style) in styles.iter().enumerate() {
                let cap = cap.get(i + 1).unwrap();

                if self.items.iter().any(|item| {
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

        self.items.extend(items);
    }

    // pub fn highlight_with(&mut self, regex: Regex, stylize: impl Fn(&str) -> StyledText) {
    //     let mut items = vec![];

    //     for cap in regex.captures_iter(self.text) {
    //         assert_eq!(cap.len(), 1);

    //         let cap = cap.get(0).unwrap();

    //         if self.items.iter().any(|item| {
    //             cap.start() < item.start + item.len - 1 && item.start < cap.start() + cap.len() - 1
    //         }) {
    //             continue;
    //         }

    //         items.push(Highlighted {
    //             start: cap.start(),
    //             len: cap.len(),
    //             render_as: Stylized::Rendered(stylize(cap.as_str())),
    //         })
    //     }

    //     self.items.extend(items);
    // }

    pub fn finalize(mut self, blank_style: Style) -> StyledText {
        self.items.sort_by_key(|item| item.start);

        let mut out = StyledText::new();

        let mut last_pos = 0;

        for Highlighted { start, len, style } in self.items {
            if start > last_pos {
                out.push((blank_style, self.text[last_pos..start].to_string()));
            }

            out.push((style, self.text[start..start + len].to_string()));

            // match render_as {
            //     Stylized::Style(style) => {
            //         out.push((style, self.text[start..start + len].to_string()))
            //     }

            //     Stylized::Rendered(stylized) => {
            //         assert_eq!(stylized.raw_string(), &self.text[start..start + len]);

            //         for piece in stylized.buffer {
            //             out.push(piece);
            //         }
            //     }
            // }

            last_pos = start + len;
        }

        if self.text.len() > last_pos {
            out.push((blank_style, self.text[last_pos..].to_string()));
        }

        out
    }
}

struct Highlighted {
    start: usize,
    len: usize,
    // render_as: Stylized,
    style: Style,
}

// enum Stylized {
//     Style(Style),
//     Rendered(StyledText),
// }
