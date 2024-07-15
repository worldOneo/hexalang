use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum TokenValue {
    Identifier(Rc<String>),
    String(Rc<String>),
    Number(Rc<String>),
    InlineComment(Rc<String>),
    BlockComment(Rc<String>),
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

    pub fn nextChar(&self) -> (Self, Option<char>) {
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

fn parse_exact<'a>(
    ot: SourceReader<'a>,
    exact: &str,
    token: TokenValue,
) -> (SourceReader<'a>, Option<Token>) {
    let mut t = ot.clone();
    let mut read = String::new();
    while read.len() < exact.len() {
        if let (nt, Some(c)) = t.nextChar() {
            t = nt;
            read.push(c);
        } else {
            break;
        }
    }

    if read.as_str() == exact {
        return (t.nextChar().0, Some(t.emit_token(ot, token)));
    }
    return (t, None);
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
    dbg!(
        any_of(
            SourceReader::new(&chars.collect(), Rc::new(String::from("shell"))),
            [
                &|r| parse_exact(r, "fn", TokenValue::Fn),
                &|r| parse_exact(r, "process", TokenValue::Process),
                &|r| parse_exact(r, "protocol", TokenValue::Protocol),
                &|r| parse_exact(r, "if", TokenValue::If),
                &|r| parse_exact(r, "for", TokenValue::For),
                &|r| parse_exact(r, "val", TokenValue::Val),
                &|r| parse_exact(r, "var", TokenValue::Var),
                &|r| parse_exact(r, "{", TokenValue::BraceOpen),
                &|r| parse_exact(r, "}", TokenValue::BraceClose),
                &|r| parse_exact(r, "[", TokenValue::SquareOpen),
                &|r| parse_exact(r, "]", TokenValue::SquareClose),
                &|r| parse_exact(r, "(", TokenValue::ParenOpen),
                &|r| parse_exact(r, ")", TokenValue::ParenClose),
                &|r| parse_exact(r, "<=", TokenValue::LTEQ),
                &|r| parse_exact(r, "<<", TokenValue::ShiftL),
                &|r| parse_exact(r, "<", TokenValue::LT),
                &|r| parse_exact(r, ">=", TokenValue::GTEQ),
                &|r| parse_exact(r, ">>", TokenValue::ShiftR),
                &|r| parse_exact(r, ">", TokenValue::GT),
                &|r| parse_exact(r, "==", TokenValue::EQEQ),
                &|r| parse_exact(r, "=", TokenValue::EQ),
                &|r| parse_exact(r, "|>", TokenValue::Pipe),
                &|r| parse_exact(r, "%", TokenValue::Mod),
                &|r| parse_exact(r, "*", TokenValue::Mul),
                &|r| parse_exact(r, "/", TokenValue::Div),
                &|r| parse_exact(r, "+", TokenValue::Plus),
                &|r| parse_exact(r, "-", TokenValue::Minus),
                &|r| parse_exact(r, "^", TokenValue::Hat),
                &|r| parse_exact(r, "&", TokenValue::Ampersand),
                &|r| parse_exact(r, ".", TokenValue::Dot),
                &|r| parse_exact(r, ":", TokenValue::Colon),
                &|r| parse_exact(r, "!", TokenValue::Not),
            ],
        )
        .1
    );
    out
}
