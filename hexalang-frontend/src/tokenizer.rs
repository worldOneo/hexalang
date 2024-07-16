use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum TokenValue {
    Identifier(Rc<String>),
    String(Rc<String>),
    Number(Rc<String>),
    InlineComment(Rc<String>),
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
}

#[derive(Clone, Debug)]
pub struct TokenLocation {
    line: usize,
    line_offset: usize,
    file_offset: usize,
}

#[derive(Clone, Debug)]
pub struct Token {
    file: Rc<String>,
    start: TokenLocation,
    stop: TokenLocation,
    value: TokenValue,
}

#[derive(Clone, Debug)]
struct SourceReader<'a> {
    file: Rc<String>,
    file_offset: usize,
    line_offset: usize,
    line: usize,
    data: &'a Vec<char>,
}

impl<'a> SourceReader<'a> {
    pub fn new(data: &'a Vec<char>, file_name: Rc<String>) -> Self {
        return Self {
            file: file_name,
            file_offset: 0,
            line_offset: 0,
            line: 0,
            data: data,
        };
    }

    pub fn next_char(&self) -> (Self, Option<char>) {
        if self.file_offset >= self.data.len() {
            return (self.clone(), None);
        }
        let file_offset = self.file_offset + 1;
        let mut line_offset = self.line_offset + 1;
        let mut line = self.line;
        let char = self.data[file_offset - 1];
        if char == '\n' {
            line_offset = 0;
            line += 1;
        }
        return (
            Self {
                file_offset: file_offset,
                line: line,
                line_offset: line_offset,
                file: self.file.clone(),
                data: self.data,
            },
            Some(char),
        );
    }

    fn to_location(&self) -> TokenLocation {
        return TokenLocation {
            line: self.line,
            file_offset: self.file_offset,
            line_offset: self.line_offset,
        };
    }

    pub fn emit_token<'b>(&self, start: SourceReader<'b>, value: TokenValue) -> Token {
        return Token {
            file: start.file.clone(),
            start: start.to_location(),
            stop: self.to_location(),
            value,
        };
    }
}

fn lex_exact<'a>(
    ot: SourceReader<'a>,
    exact: &str,
    token: TokenValue,
) -> (SourceReader<'a>, Option<Token>) {
    let mut t = ot.clone();
    let mut prev = ot.clone();
    let mut chariter = exact.chars();
    while let Some(required) = chariter.next() {
        if let (nt, Some(c)) = t.next_char() {
            prev = t.clone();
            t = nt;
            if c != required {
                return (ot, None);
            }
        }
    }

    return (t, Some(prev.emit_token(ot, token)));
}

const FIRST_ID_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
const ID_CHAR: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";

fn lex_identifier<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let mut t = ot.clone();
    let mut prev = t.clone();
    let mut id = String::new();
    if let (nt, Some(char)) = ot.next_char() {
        if FIRST_ID_CHAR.contains(char) {
            id.push(char);
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }

    while let (nt, Some(char)) = t.next_char() {
        if ID_CHAR.contains(char) {
            prev = t.clone();
            id.push(char);
            t = nt;
        } else {
            break;
        }
    }
    let token = prev.emit_token(ot, TokenValue::Identifier(Rc::new(id)));
    return (t, Some(token));
}

fn lex_string<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let mut t = ot.clone();
    let mut prev = t.clone();
    if let (nt, Some(char)) = ot.next_char() {
        if char == '"' {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }

    let mut str_content = String::new();

    while let (nt, Some(char)) = t.next_char() {
        prev = t.clone();
        t = nt;
        if char == '"' {
            break;
        }
        str_content.push(char);
        if char == '\\' {
            if let (nt, Some('"')) = t.next_char() {
                prev = t.clone();
                str_content.push('"');
                t = nt;
            }
        }
    }
    let token = prev.emit_token(ot, TokenValue::String(Rc::new(str_content)));
    return (t, Some(token));
}

const DIGITS: &str = "0123456789";
const NUMBER_CHARS: &str = "0123456789boxabcdef._";

fn lex_number<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let mut t = ot.clone();
    let mut prev = t.clone();
    let mut content = String::new();
    if let (nt, Some(n)) = t.next_char() {
        if DIGITS.contains(n) {
            content.push(n);
            t = nt;
        } else {
            return (ot, None);
        }
    }
    while let (nt, Some(n)) = t.next_char() {
        if NUMBER_CHARS.contains(n) {
            prev = t.clone();
            content.push(n);
            t = nt;
        } else {
            break;
        }
    }
    let token_value = TokenValue::Number(Rc::new(content));
    return (t, Some(prev.emit_token(ot, token_value)));
}

fn lex_inline_comment<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
    let mut t = ot.clone();
    let mut prev = t.clone();
    if let (nt, Some(n)) = t.next_char() {
        if n == '/' {
            t = nt;
        } else {
            return (ot, None);
        }
    }
    if let (nt, Some(n)) = t.next_char() {
        if n == '/' {
            prev = t.clone();
            t = nt;
        } else {
            return (ot, None);
        }
    }
    let mut content = String::new();
    while let (nt, Some(n)) = t.next_char() {
        if n != '\n' {
            prev = t.clone();
            content.push(n);
            t = nt;
        } else {
            break;
        }
    }
    let token_value = TokenValue::InlineComment(Rc::new(content));
    return (t, Some(prev.emit_token(ot, token_value)));
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

pub fn tokenize(input: String) -> Vec<Token> {
    let out = vec![];
    let chars = input.chars().peekable();
    let first_token = any_of(
        SourceReader::new(&chars.collect(), Rc::new(String::from("shell"))),
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
        ],
    )
    .1;
    dbg!(first_token);
    out
}
