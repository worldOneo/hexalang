use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum TokenValue {
    Identifier,
    String,
    Number,
    InlineComment,
    BraceOpen,
    BraceClose,
    ParenOpen,
    ParenClose,
    GT,
    LT,
    EQ,
    GTEQ,
    LTEQ,
    EQEQ,
    SquareOpen,
    SquareClose,
    Protocol,
    Process,
    Fn,
    If,
    For,
    Var,
    Val,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Pipe,
    Hat,
    Ampersand,
    Dot,
    Colon,
    Not,
    ShiftL,
    ShiftR,
    Whitespace,
}
#[derive(Clone, Debug)]
pub struct Token {
    start: u32,
    value: TokenValue,
}

impl Token {
    pub fn value(&self) -> TokenValue {
        return self.value.clone();
    }

    pub fn offset(&self) -> u32 {
        return self.start;
    }
}

#[derive(Clone, Debug)]
struct SourceReader<'a> {
    file: Rc<String>,
    offset: usize,
    data: &'a Vec<char>,
}

impl<'a> SourceReader<'a> {
    pub fn new(data: &'a Vec<char>, file_name: Rc<String>) -> Self {
        return Self {
            file: file_name,
            offset: 0,
            data: data,
        };
    }

    pub fn next_char(&self) -> (Self, Option<char>) {
        if self.offset >= self.data.len() {
            return (self.clone(), None);
        }
        let file_offset = self.offset + 1;
        let char = self.data[file_offset - 1];
        return (
            Self {
                offset: file_offset,
                file: self.file.clone(),
                data: self.data,
            },
            Some(char),
        );
    }

    pub fn emit_token<'b>(&self, value: TokenValue) -> Token {
        return Token {
            start: self.offset as u32,
            value,
        };
    }

    pub fn sequence(&self, end_exclusive: &Self) -> &'a [char] {
        return &self.data[self.offset..end_exclusive.offset];
    }

    pub fn set_offset(&self, offset: u32) -> Self {
        return Self {
            offset: offset as usize,
            data: self.data,
            file: self.file.clone(),
        };
    }
}

fn lex_exact<'a>(
    ot: SourceReader<'a>,
    exact: &str,
    token: TokenValue,
) -> (SourceReader<'a>, Option<Token>) {
    let mut t = ot.clone();
    let mut chariter = exact.chars();
    while let Some(required) = chariter.next() {
        if let (nt, Some(c)) = t.next_char() {
            t = nt;
            if c != required {
                return (ot, None);
            }
        } else {
            return (ot, None);
        }
    }

    return (t, Some(ot.emit_token(token)));
}

const FIRST_ID_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
const ID_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";

fn lex_identifier_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t;
    if let (nt, Some(char)) = ot.next_char() {
        if FIRST_ID_CHAR.contains(char) {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }

    while let (nt, Some(char)) = t.next_char() {
        if ID_CHAR.contains(char) {
            t = nt;
        } else {
            break;
        }
    }
    return (t.clone(), Some(ot.sequence(&t)));
}

fn lex_identifier<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let (t, v) = lex_identifier_value(ot.clone());
    if let Some(_) = v {
        let token = ot.emit_token(TokenValue::Identifier);
        return (t, Some(token));
    }
    return (ot, None);
}

fn lex_string_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t;
    if let (nt, Some(char)) = ot.next_char() {
        if char == '"' {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }

    let start = t.clone();
    let mut terminated = false;

    while let (nt, Some(char)) = t.next_char() {
        t = nt;
        if char == '"' {
            terminated = true;
            break;
        }
        if char == '\\' {
            if let (nt, Some('"')) = t.next_char() {
                t = nt;
            }
        }
    }
    if !terminated {
        return (ot, None);
    }
    return (t.clone(), Some(start.sequence(&t)));
}

fn lex_string<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let (t, v) = lex_string_value(ot.clone());
    if let Some(_) = v {
        return (t, Some(ot.emit_token(TokenValue::String)));
    }
    return (ot, None);
}

const DIGITS: &str = "0123456789";
const NUMBER_CHARS: &str = "0123456789boxabcdef._";

fn lex_number_string_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t = ot.clone();
    if let (nt, Some(n)) = t.next_char() {
        if DIGITS.contains(n) {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    while let (nt, Some(n)) = t.next_char() {
        if NUMBER_CHARS.contains(n) {
            t = nt;
        } else {
            break;
        }
    }
    return (t.clone(), Some(ot.sequence(&t)));
}

fn lex_number<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let (t, v) = lex_number_string_value(ot.clone());
    if let Some(_) = v {
        let token_value = TokenValue::Number;
        return (t, Some(ot.emit_token(token_value)));
    }
    return (ot, None);
}

fn lex_inline_comment_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t = ot.clone();
    if let (nt, Some(n)) = t.next_char() {
        if n == '/' {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    if let (nt, Some(n)) = t.next_char() {
        if n == '/' {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    let start = t.clone();
    while let (nt, Some(n)) = t.next_char() {
        if n != '\n' {
            t = nt;
        } else {
            break;
        }
    }
    return (t.clone(), Some(start.sequence(&t)));
}

fn lex_inline_comment<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let (t, v) = lex_inline_comment_value(ot.clone());
    if let Some(_) = v {
        let token_value = TokenValue::InlineComment;
        return (t, Some(ot.emit_token(token_value)));
    }
    return (ot, None);
}

const WHITESPACE: &str = " \r\n\t";

fn lex_whitespace_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t = ot.clone();
    if let (nt, Some(n)) = t.next_char() {
        if WHITESPACE.contains(n) {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    while let (nt, Some(n)) = t.next_char() {
        if WHITESPACE.contains(n) {
            t = nt;
        } else {
            break;
        }
    }
    return (t.clone(), Some(ot.sequence(&t)));
}

fn lex_whitespace<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let (t, v) = lex_whitespace_value(ot.clone());
    if let Some(_) = v {
        let token_value = TokenValue::Whitespace;
        return (t, Some(ot.emit_token(token_value)));
    }
    return (ot, None);
}

type LexFn<'a> = dyn Fn(SourceReader<'a>) -> (SourceReader<'a>, Option<Token>);

fn any_of<'a, const N: usize>(
    reader: SourceReader<'a>,
    values: [&LexFn<'a>; N],
) -> (SourceReader<'a>, Option<Token>) {
    let data = values
        .iter()
        .map(|f| f(reader.clone()))
        .filter(|(_, v)| v.is_some())
        .next();
    if let Some((reader, Some(v))) = data {
        return (reader, Some(v));
    }
    return (reader, None);
}

struct TokenIterator<'a> {
    source: SourceReader<'a>,
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let (source, t) = any_of(
            self.source.clone(),
            [
                &lex_inline_comment,
                &|r| lex_exact(r, "fn", TokenValue::Fn),
                &|r| lex_exact(r, "process", TokenValue::Process),
                &|r| lex_exact(r, "protocol", TokenValue::Protocol),
                &|r| lex_exact(r, "if", TokenValue::If),
                &|r| lex_exact(r, "for", TokenValue::For),
                &|r| lex_exact(r, "val", TokenValue::Val),
                &|r| lex_exact(r, "var", TokenValue::Var),
                &|r| lex_exact(r, "{", TokenValue::BraceOpen),
                &|r| lex_exact(r, "}", TokenValue::BraceClose),
                &|r| lex_exact(r, "[", TokenValue::SquareOpen),
                &|r| lex_exact(r, "]", TokenValue::SquareClose),
                &|r| lex_exact(r, "(", TokenValue::ParenOpen),
                &|r| lex_exact(r, ")", TokenValue::ParenClose),
                &|r| lex_exact(r, "<=", TokenValue::LTEQ),
                &|r| lex_exact(r, "<<", TokenValue::ShiftL),
                &|r| lex_exact(r, "<", TokenValue::LT),
                &|r| lex_exact(r, ">=", TokenValue::GTEQ),
                &|r| lex_exact(r, ">>", TokenValue::ShiftR),
                &|r| lex_exact(r, ">", TokenValue::GT),
                &|r| lex_exact(r, "==", TokenValue::EQEQ),
                &|r| lex_exact(r, "=", TokenValue::EQ),
                &|r| lex_exact(r, "|>", TokenValue::Pipe),
                &|r| lex_exact(r, "%", TokenValue::Mod),
                &|r| lex_exact(r, "*", TokenValue::Mul),
                &|r| lex_exact(r, "/", TokenValue::Div),
                &|r| lex_exact(r, "+", TokenValue::Plus),
                &|r| lex_exact(r, "-", TokenValue::Minus),
                &|r| lex_exact(r, "^", TokenValue::Hat),
                &|r| lex_exact(r, "&", TokenValue::Ampersand),
                &|r| lex_exact(r, ".", TokenValue::Dot),
                &|r| lex_exact(r, ":", TokenValue::Colon),
                &|r| lex_exact(r, "!", TokenValue::Not),
                &lex_identifier,
                &lex_string,
                &lex_number,
                &lex_whitespace,
            ],
        );
        self.source = source;
        return t;
    }
}

pub fn tokenize(input: String) -> Vec<Token> {
    let chars = input.chars().collect();
    let source = SourceReader::new(&chars, Rc::new(String::from("shell")));
    return TokenIterator { source }.collect();
}
