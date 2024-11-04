#[derive(Clone)]
struct SourceReader<'a> {
    tokens: &'a Vec<Token>,
    tokens_offset: usize,
}

impl<'a> SourceReader<'a> {
    fn new(ts: &'a Vec<Token>) -> Self {
        Self {
            tokens: ts,
            tokens_offset: 0,
        }
    }

    fn next(&self) -> (Self, Option<Token>) {
        if self.tokens_offset >= self.tokens.len() {
            return (self.clone(), None);
        }
        let read_continue = Self {
            tokens: self.tokens,
            tokens_offset: self.tokens_offset + 1,
        };
        let token = self.tokens[self.tokens_offset].clone();
        return (read_continue, Some(token));
    }

    fn offset(&self) -> u32 {
        return self.tokens_offset as u32;
    }
}

trait Parser<T> {}
