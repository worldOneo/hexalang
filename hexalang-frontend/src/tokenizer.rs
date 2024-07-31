use std::{collections::VecDeque, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
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
    And,
    Land,
    Or,
    Lor,
    Dot,
    Colon,
    Not,
    ShiftL,
    ShiftR,
    Whitespace,
    // Phantom
    PhantomBraceClose,
    PhantomSquareClose,
    PhantomParenClose,
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

pub struct SourceFile {
    file: Rc<String>,
}

#[derive(Clone, Debug)]
pub struct SourceReader<'a> {
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

    fn next(&self) -> (Self, Option<char>) {
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

    fn emit_token<'b>(&self, value: TokenValue) -> Token {
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

    fn source_file(&self) -> SourceFile {
        return SourceFile {
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
        if let (nt, Some(c)) = t.next() {
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

pub fn lex_identifier_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t;
    if let (nt, Some(char)) = ot.next() {
        if FIRST_ID_CHAR.contains(char) {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }

    while let (nt, Some(char)) = t.next() {
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

pub fn lex_string_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t;
    if let (nt, Some(char)) = ot.next() {
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

    while let (nt, Some(char)) = t.next() {
        t = nt;
        if char == '"' {
            terminated = true;
            break;
        }
        if char == '\\' {
            if let (nt, Some('"')) = t.next() {
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

pub fn lex_number_string_value<'a>(ot: SourceReader<'a>) -> (SourceReader<'a>, Option<&'a [char]>) {
    let mut t = ot.clone();
    if let (nt, Some(n)) = t.next() {
        if DIGITS.contains(n) {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    while let (nt, Some(n)) = t.next() {
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
    if let (nt, Some(n)) = t.next() {
        if n == '/' {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    if let (nt, Some(n)) = t.next() {
        if n == '/' {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    let start = t.clone();
    while let (nt, Some(n)) = t.next() {
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
    if let (nt, Some(n)) = t.next() {
        if WHITESPACE.contains(n) {
            t = nt;
        } else {
            return (ot, None);
        }
    } else {
        return (ot, None);
    }
    while let (nt, Some(n)) = t.next() {
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

#[derive(PartialEq)]
enum OpenClose {
    Brace,
    Paren,
    Square,
}

struct TokenIterator<'a> {
    source: SourceReader<'a>,
    open_close_stack: Vec<OpenClose>,
    to_emit: VecDeque<Token>,
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if let Some(pop) = self.to_emit.pop_front() {
            return Some(pop);
        }
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
                &|r| lex_exact(r, "&&", TokenValue::Land),
                &|r| lex_exact(r, "&", TokenValue::And),
                &|r| lex_exact(r, "||", TokenValue::Lor),
                &|r| lex_exact(r, "|", TokenValue::Or),
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

        // emitting phantom closes to maybe maybe correctly patch the AST
        {
            let closing_value = |v: OpenClose| match v {
                OpenClose::Brace => TokenValue::PhantomBraceClose,
                OpenClose::Square => TokenValue::PhantomSquareClose,
                OpenClose::Paren => TokenValue::PhantomParenClose,
            };
            if let Some(t) = t.clone() {
                match t.value() {
                    TokenValue::BraceClose => {
                        while let Some(close) = self.open_close_stack.pop() {
                            if close == OpenClose::Brace {
                                break;
                            }
                            self.to_emit.push_back(Token {
                                start: t.offset(),
                                value: closing_value(close),
                            });
                        }
                    }
                    TokenValue::ParenClose => {
                        while let Some(close) = self.open_close_stack.pop() {
                            if close == OpenClose::Paren {
                                break;
                            }
                            self.to_emit.push_back(Token {
                                start: t.offset(),
                                value: closing_value(close),
                            });
                        }
                    }
                    TokenValue::SquareClose => {
                        while let Some(close) = self.open_close_stack.pop() {
                            if close == OpenClose::Square {
                                break;
                            }
                            self.to_emit.push_back(Token {
                                start: t.offset(),
                                value: closing_value(close),
                            });
                        }
                    }
                    TokenValue::SquareOpen => self.open_close_stack.push(OpenClose::Square),
                    TokenValue::BraceOpen => self.open_close_stack.push(OpenClose::Brace),
                    TokenValue::ParenOpen => self.open_close_stack.push(OpenClose::Paren),
                    _ => {}
                }
            }

            if let Some(e) = self.to_emit.pop_front() {
                if let Some(t) = t.clone() {
                    self.to_emit.push_back(t);
                }
                return Some(e);
            }
        }

        return t;
    }
}

pub fn tokenize<'a>(source: SourceReader<'a>) -> Vec<Token> {
    return TokenIterator {
        source,
        to_emit: VecDeque::new(),
        open_close_stack: vec![],
    }
    .collect();
}
