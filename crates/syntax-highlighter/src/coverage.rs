//!
//! Input coverage computation
//!
//! This module provides a way to track which parts of an input (e.g. string) has been covered.
//! It allows listing uncovered parts of the input as well as fetching the next uncovered segment.
//!
//! It is notably used for syntax highlighting.

#[derive(Debug)]
pub struct InputCoverage {
    offset: usize,
    len: usize,
    covered: Vec<InputRange>,
}

impl InputCoverage {
    pub fn new(len: usize, offset: usize) -> Self {
        Self {
            len,
            offset,
            covered: vec![],
        }
    }

    pub fn mark_as_covered(&mut self, mut from: usize, len: usize) {
        assert!(from >= self.offset);

        from -= self.offset;

        let range = InputRange { from, len };

        if self.covered.is_empty() {
            self.covered.push(range);
            return;
        }

        match self
            .covered
            .iter()
            .rposition(|covered| covered.from + covered.len <= from)
        {
            Some(index) => self.covered.insert(index + 1, range),
            None => self.covered.insert(0, range),
        }
    }

    pub fn next_uncovered(&self) -> Option<InputRange> {
        self.next_uncovered_inner()
            .map(|InputRange { from, len }| InputRange {
                from: from + self.offset,
                len,
            })
    }

    fn next_uncovered_inner(&self) -> Option<InputRange> {
        if self.len == 0 {
            return None;
        }

        if self.covered.is_empty() {
            return Some(InputRange {
                from: 0,
                len: self.len,
            });
        }

        if let Some(first) = self.covered.first() {
            if first.from > 0 {
                return Some(InputRange {
                    from: 0,
                    len: first.from,
                });
            }
        }

        self.covered.iter().enumerate().find_map(|(i, covered)| {
            let limit = match self.covered.get(i + 1) {
                None => self.len,
                Some(next_covered) => next_covered.from,
            };

            let len = limit - (covered.from + covered.len);

            if len == 0 {
                None
            } else {
                Some(InputRange {
                    from: covered.from + covered.len,
                    len,
                })
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InputRange {
    pub from: usize,
    pub len: usize,
}
