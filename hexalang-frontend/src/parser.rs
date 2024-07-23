use crate::tokenizer::{self, Token, TokenValue};

const NULL: u32 = 4294967295;

#[derive(Debug, Clone)]
enum FunctionalNodeType {
    Fn,
    Assign,
    ValAssign,
    VarAssign,
    Type,
    If,
    For,
    Process,
    BiOp,
    Pipe,
    Int,
    Float,
    String,
    Block,
}

#[derive(Clone)]
struct FunctionalNode {
    primary_token: u32,
    data1: u32,
    data2: u32,
    additional_data: u16,
    node_type: FunctionalNodeType,
}

#[derive(Clone)]
enum TypeNodeType {
    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Array,
    Struct,
}

#[derive(Clone)]
struct TypeNode {
    primary_token: u32,
    data: u32,
    additional_data: u16,
    node_type: TypeNodeType,
}

#[derive(Clone)]
struct FunctionSignature {
    parameters: Vec<(u32, TypeNode)>, // (identifieridx, TypeNode)
    return_type: TypeNode,
}

#[derive(Clone)]
struct Identifier {
    sourcefile: u32,
    offset: u32,
}

#[derive(Clone)]
struct TypedIdentifier {
    sourcefile: u32,
    offset: u32,
    type_node: TypeNode,
}

#[derive(Clone)]
struct Assign {
    identifier: u32,
    value: FunctionalNode,
}

#[derive(Clone)]
enum MessageLevel {
    Debug,
    Info,
    Warning,
    Error,
}

#[derive(Clone)]
enum MessageType {
    TypeExpected,
    ValueExpected,
    BraceExpected,
    ParenExpected,
    SquareExpected,
    EQExpected,
    Literal,
}

#[derive(Clone)]
enum MessageContext {
    Type(TypeNodeType),
    Functional(FunctionalNodeType),
}

#[derive(Clone)]
struct Message {
    token: u32,
    level: MessageLevel,
    message: MessageType,
    context: MessageContext,
}

struct BumpStorage<T>
where
    T: Clone,
{
    data: Vec<T>,
}

impl<T> Default for BumpStorage<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

impl<T> BumpStorage<T>
where
    T: Clone,
{
    fn allocate(&mut self, t: T) -> u32 {
        self.data.push(t);
        return (self.data.len() - 1) as u32;
    }

    fn receive(&mut self, ptr: u32) -> T {
        return self.data[ptr as usize].clone();
    }
}

struct Tree<'a> {
    identifiers: BumpStorage<Identifier>,
    signatures: BumpStorage<FunctionSignature>,
    typed_identifier: BumpStorage<TypedIdentifier>,
    assign: BumpStorage<Assign>,
    integers: BumpStorage<u64>,
    float: BumpStorage<f64>,
    block: BumpStorage<Vec<FunctionalNode>>,

    type_nodes: BumpStorage<TypeNode>,
    functional_nodes: BumpStorage<FunctionalNode>,

    messages: Vec<Message>,
    used: bool,

    source: tokenizer::SourceReader<'a>,
    tokens: Vec<tokenizer::Token>,
}

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

    fn next(&mut self) -> (Self, Option<Token>) {
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

    fn expect(&mut self, value: TokenValue) -> (Self, Option<Token>) {
        if let (n, Some(t)) = self.next() {
            if t.value() == value {
                return (n, Some(t));
            }
        }
        return (self.clone(), None);
    }

    fn offset(&self) -> u32 {
        return self.tokens_offset as u32;
    }
}

impl<'source> Tree<'source> {
    pub fn new(source: tokenizer::SourceReader<'source>, tokens: Vec<tokenizer::Token>) -> Self {
        Self {
            tokens,
            source,
            identifiers: BumpStorage::default(),
            signatures: BumpStorage::default(),
            typed_identifier: BumpStorage::default(),
            assign: BumpStorage::default(),
            integers: BumpStorage::default(),
            float: BumpStorage::default(),
            block: BumpStorage::default(),
            type_nodes: BumpStorage::default(),
            functional_nodes: BumpStorage::default(),
            messages: vec![],
            used: false,
        }
    }

    pub fn parse(&mut self) {
        assert!(!self.used, "tried to parse into a used tree");
        self.used = true;
    }

    fn to_integer(chars: &[char]) -> Option<u64> {
        let mut value: u64 = 0;
        for c in chars {
            value *= 10;
            if !c.is_numeric() {
                return None;
            }
            let digit = (*c as u8) - b'0';
            value += digit as u64;
        }
        return Some(value);
    }

    fn parse_number<'a>(
        &mut self,
        mut source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, value) = source.next();
        if let Some(value) = value {
            if TokenValue::Number != value.value() {
                return (nsource, None);
            }
            let sequence_source = self.source.set_offset(value.offset());
            let (_, number) = tokenizer::lex_number_string_value(sequence_source);
            let data = number.expect("number token cant lex number again");
            if let Some(integer_value) = Self::to_integer(data) {
                return (
                    nsource,
                    Some(FunctionalNode {
                        primary_token: source.offset(),
                        data1: self.integers.allocate(integer_value),
                        data2: NULL,
                        additional_data: 0,
                        node_type: FunctionalNodeType::Int,
                    }),
                );
            }
            self.emit_message(
                MessageLevel::Error,
                MessageType::Literal,
                MessageContext::Functional(FunctionalNodeType::Int),
                source.offset(),
            );
        }
        return (nsource, None);
    }

    fn parse_expression<'a>(
        &mut self,
        mut source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        if let (nsource, Some(t)) = source.next() {
            match t.value() {
                TokenValue::String => (
                    nsource,
                    Some(FunctionalNode {
                        primary_token: source.offset(),
                        data1: NULL,
                        data2: NULL,
                        additional_data: 0,
                        node_type: FunctionalNodeType::String,
                    }),
                ),
                TokenValue::Number => self.parse_number(source),
                _ => (source, None),
            }
        } else {
            return (source, None);
        }
    }

    fn parse_type<'a>(&mut self, source: SourceReader<'a>) -> (SourceReader<'a>, Option<TypeNode>) {
        todo!();
    }

    fn parse_init<'a>(
        &mut self,
        mut source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let primary = source.offset();
        let (mut source, t) = source.next();
        if let None = t {
            return (source, None);
        }
        let t = t.unwrap();
        let var = t.value() == TokenValue::Var;
        let val = t.value() == TokenValue::Val;
        if !var && !val {
            return (source, None);
        }

        let mut fnode = FunctionalNode {
            primary_token: primary,
            data1: NULL,
            data2: NULL,
            additional_data: 0,
            node_type: if var {
                FunctionalNodeType::VarAssign
            } else {
                FunctionalNodeType::ValAssign
            },
        };

        let (mut source, value) = source.expect(TokenValue::Colon);
        if value.is_some() {
            let (source, _type) = self.parse_type(source.clone());
            if let Some(tnode) = _type {
                fnode.data1 = self.type_nodes.allocate(tnode);
            } else {
                self.emit_message(
                    MessageLevel::Error,
                    MessageType::TypeExpected,
                    MessageContext::Functional(fnode.node_type.clone()),
                    source.offset(),
                );
            }
        }
        let (mut source, value) = source.expect(TokenValue::EQ);
        if !value.is_some() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::EQExpected,
                MessageContext::Functional(fnode.node_type.clone()),
                source.offset(),
            );
        }
        let (nsource, expression) = self.parse_expression(source);
        if let Some(expression) = expression {
            fnode.data2 = self.functional_nodes.allocate(expression);
        } else {
            self.emit_message(
                MessageLevel::Error,
                MessageType::ValueExpected,
                MessageContext::Functional(fnode.node_type.clone()),
                nsource.offset(),
            );
        }
        source = nsource;

        return (source, Some(fnode));
    }

    fn emit_message(
        &mut self,
        level: MessageLevel,
        message: MessageType,
        context: MessageContext,
        token: u32,
    ) {
        self.messages.push(Message {
            token,
            level,
            message,
            context,
        });
    }
}
