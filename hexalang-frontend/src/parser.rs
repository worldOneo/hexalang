use crate::tokenizer::{self, Token, TokenValue};

pub const NULL: u32 = 4294967295;

#[derive(Debug, Clone)]
pub enum FunctionalNodeType {
    Fn,
    Assign,
    ValAssign,
    VarAssign,
    Type,
    If,
    For,
    Process,
    BiOp,
    UnOp,
    Pipe,
    Int,
    Float,
    String,
    Block,
    MemberAccess,
}

#[derive(Clone)]
pub enum UnOp {
    Minus = 0,
    Not = 1,
    Plus = 2,
}

#[derive(Clone, Debug)]
pub enum BiOp {
    Plus = 0,
    Minus = 1,
    Mul = 2,
    Div = 3,
    Mod = 4,
    Rsh = 5,
    Lsh = 6,
    And = 7,
    Xor = 9,
    Or = 10,
    Lor = 11,
    Land = 12,
    Eq = 13,
    Gte = 14,
    Gt = 15,
    Lte = 16,
    Lt = 17,
}

impl From<u16> for BiOp {
    fn from(value: u16) -> Self {
        match value {
            0 => BiOp::Plus,
            1 => BiOp::Minus,
            2 => BiOp::Mul,
            3 => BiOp::Div,
            4 => BiOp::Mod,
            5 => BiOp::Rsh,
            6 => BiOp::Lsh,
            7 => BiOp::And,
            9 => BiOp::Xor,
            10 => BiOp::Or,
            11 => BiOp::Lor,
            12 => BiOp::Land,
            13 => BiOp::Eq,
            14 => BiOp::Gte,
            15 => BiOp::Gt,
            16 => BiOp::Lte,
            17 => BiOp::Lt,
            _ => BiOp::Lt,
        }
    }
}

#[derive(Clone)]
pub struct FunctionalNode {
    pub primary_token: u32,
    pub data1: u32,
    pub data2: u32,
    pub additional_data: u16,
    pub node_type: FunctionalNodeType,
}

#[derive(Clone)]
pub enum TypeNodeType {
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
pub struct FunctionSignature {
    pub primary_token: u32,
    pub parameters: Vec<(u32, TypeNode)>, // (identifieridx, TypeNode)
    pub return_type: TypeNode,
}

#[derive(Clone)]
pub struct Identifier {
    pub sourcefile: u32,
    pub offset: u32,
}

#[derive(Clone)]
pub struct TypedIdentifier {
    pub sourcefile: u32,
    pub offset: u32,
    pub type_node: TypeNode,
}

#[derive(Clone)]
pub struct Assign {
    pub identifier: u32,
    pub value: FunctionalNode,
}

#[derive(Clone)]
pub enum MessageLevel {
    Debug,
    Info,
    Warning,
    Error,
}

#[derive(Clone)]
pub enum MessageType {
    TypeExpected,
    ValueExpected,
    BraceExpected,
    ParenExpected,
    SquareExpected,
    EQExpected,
    Literal,
    IdentifierExpected,
}

#[derive(Clone)]
pub enum MessageContext {
    Type(TypeNodeType),
    Functional(FunctionalNodeType),
}

#[derive(Clone)]
pub struct Message {
    pub token: u32,
    pub level: MessageLevel,
    pub message: MessageType,
    pub context: MessageContext,
}

pub struct BumpStorage<T>
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
    pub fn allocate(&mut self, t: T) -> u32 {
        self.data.push(t);
        return (self.data.len() - 1) as u32;
    }

    pub fn receive(&mut self, ptr: u32) -> T {
        return self.data[ptr as usize].clone();
    }
}

pub struct Tree<'a> {
    pub identifiers: BumpStorage<Identifier>,
    pub signatures: BumpStorage<FunctionSignature>,
    pub typed_identifier: BumpStorage<TypedIdentifier>,
    pub assign: BumpStorage<Assign>,
    pub integers: BumpStorage<u64>,
    pub float: BumpStorage<f64>,
    pub block: BumpStorage<Vec<FunctionalNode>>,

    pub type_nodes: BumpStorage<TypeNode>,
    pub functional_nodes: BumpStorage<FunctionalNode>,

    pub messages: Vec<Message>,
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

    pub fn parse(&mut self) -> Vec<FunctionalNode> {
        assert!(!self.used, "tried to parse into a used tree");
        self.used = true;
        let mut top_level = vec![];
        let tokens = self.tokens.clone();
        let mut source = SourceReader::new(&tokens);
        while let (nsource, Some(s)) = self.parse_statement(source) {
            top_level.push(s);
            source = nsource;
        }
        top_level
    }

    fn next_token<'a>(mut source: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
        while let (nsource, Some(t)) = source.next() {
            if t.value() != TokenValue::Whitespace && t.value() != TokenValue::InlineComment {
                return (nsource, Some(t));
            }
            source = nsource;
        }
        return (source, None);
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
        source: SourceReader<'a>,
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
            return (
                nsource,
                Some(FunctionalNode {
                    primary_token: source.offset(),
                    data1: NULL,
                    data2: NULL,
                    additional_data: 0,
                    node_type: FunctionalNodeType::Int,
                }),
            );
        }
        return (nsource, None);
    }

    fn binary_operator_weight(op: BiOp) -> u32 {
        match op {
            BiOp::Land | BiOp::Lor => 5,
            BiOp::Eq | BiOp::Gt | BiOp::Gte | BiOp::Lt | BiOp::Lte => 10,
            BiOp::Plus | BiOp::Minus => 15,
            BiOp::Div | BiOp::Mod | BiOp::Mul => 20,
            BiOp::And | BiOp::Or | BiOp::Xor | BiOp::Lsh | BiOp::Rsh => 25,
        }
    }

    fn token_to_binary_operator(token: TokenValue) -> Option<BiOp> {
        match token {
            TokenValue::Hat => Some(BiOp::Xor),
            TokenValue::And => Some(BiOp::And),
            TokenValue::Land => Some(BiOp::Land),
            TokenValue::Or => Some(BiOp::Or),
            TokenValue::Lor => Some(BiOp::Lor),
            TokenValue::Plus => Some(BiOp::Plus),
            TokenValue::Minus => Some(BiOp::Minus),
            TokenValue::Mul => Some(BiOp::Mul),
            TokenValue::Div => Some(BiOp::Div),
            TokenValue::Mod => Some(BiOp::Mod),
            TokenValue::EQEQ => Some(BiOp::Eq),
            TokenValue::GTEQ => Some(BiOp::Gte),
            TokenValue::LTEQ => Some(BiOp::Lte),
            TokenValue::GT => Some(BiOp::Gt),
            TokenValue::LT => Some(BiOp::Lt),
            TokenValue::ShiftL => Some(BiOp::Lsh),
            TokenValue::ShiftR => Some(BiOp::Rsh),
            _ => None,
        }
    }

    fn parse_dot_access<'a>(
        &mut self,
        lhs: FunctionalNode,
        weight: u32,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, t) = source.next();
        if let Some(t) = t {
            if let TokenValue::Identifier = t.value() {
                let lhs = FunctionalNode {
                    primary_token: source.offset(),
                    data1: self.functional_nodes.allocate(lhs),
                    data2: source.offset(),
                    additional_data: 0,
                    node_type: FunctionalNodeType::MemberAccess,
                };
                return self.parse_extension_max_weight(lhs, weight, nsource);
            }
        }
        self.emit_message(
            MessageLevel::Error,
            MessageType::IdentifierExpected,
            MessageContext::Functional(FunctionalNodeType::MemberAccess),
            source.offset(),
        );
        return (source, None);
    }

    fn parse_extension_max_weight<'a>(
        &mut self,
        lhs: FunctionalNode,
        weight: u32,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (tokensource, t) = source.next();
        if let None = t {
            return (source, None);
        }

        let t = t.unwrap();
        if let Some(op) = Self::token_to_binary_operator(t.value()) {
            let next_weight = Self::binary_operator_weight(op.clone());
            if next_weight <= weight {
                return (source, None);
            }
            let (nsource, rhs) = self.parse_expression(next_weight, tokensource.clone());
            let rhs = if let Some(rhs) = rhs {
                self.functional_nodes.allocate(rhs)
            } else {
                self.emit_message(
                    MessageLevel::Error,
                    MessageType::ValueExpected,
                    MessageContext::Functional(FunctionalNodeType::BiOp),
                    tokensource.offset(),
                );
                NULL
            };
            return (
                nsource,
                Some(FunctionalNode {
                    primary_token: tokensource.offset(),
                    data1: self.functional_nodes.allocate(lhs),
                    data2: rhs,
                    additional_data: op as u16,
                    node_type: FunctionalNodeType::BiOp,
                }),
            );
        }

        return match t.value() {
            TokenValue::Dot => self.parse_dot_access(lhs, weight, tokensource),
            // TokenValue::ParenOpen => {}
            _ => (source, None),
        };
    }

    fn parse_literal<'a>(
        &mut self,
        source: SourceReader<'a>,
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

    fn parse_unary_prefixed<'a>(
        &mut self,
        weight: u32,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, t) = source.next();
        if let Some(t) = t {
            let unop = match t.value() {
                TokenValue::Minus => Some(UnOp::Minus),
                TokenValue::Plus => Some(UnOp::Plus),
                TokenValue::Not => Some(UnOp::Not),
                _ => None,
            };
            if unop.is_none() {
                return (source, None);
            }
            let op = unop.unwrap();
            if let (nsource, Some(expr)) = self.parse_expression(weight, nsource.clone()) {
                return (
                    nsource,
                    Some(FunctionalNode {
                        data1: op as u32,
                        data2: self.functional_nodes.allocate(expr),
                        additional_data: 0,
                        primary_token: source.offset(),
                        node_type: FunctionalNodeType::UnOp,
                    }),
                );
            } else {
                self.emit_message(
                    MessageLevel::Error,
                    MessageType::ValueExpected,
                    MessageContext::Functional(FunctionalNodeType::UnOp),
                    nsource.offset(),
                );
                return (
                    nsource,
                    Some(FunctionalNode {
                        data1: NULL,
                        data2: NULL,
                        additional_data: 0,
                        primary_token: source.offset(),
                        node_type: FunctionalNodeType::UnOp,
                    }),
                );
            }
        }
        return (source, None);
    }

    fn parse_expression<'a>(
        &mut self,
        weight: u32,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let maybe_expr =
            if let (nsource, Some(v)) = self.parse_unary_prefixed(weight, source.clone()) {
                (nsource, Some(v))
            } else if let (nsource, Some(v)) = self.parse_literal(source.clone()) {
                (nsource, Some(v))
            } else {
                (source, None)
            };

        if let (nsource, Some(v)) = maybe_expr.clone() {
            if let (mut nsource, Some(mut v)) = self.parse_extension_max_weight(v, weight, nsource)
            {
                while let (nnsource, Some(nv)) =
                    self.parse_extension_max_weight(v.clone(), weight, nsource.clone())
                {
                    nsource = nnsource;
                    v = nv;
                }
                return (nsource, Some(v));
            }
        }
        return maybe_expr;
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
        let (nsource, expression) = self.parse_expression(0, source);
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

    fn parse_statement<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, statement) = self.parse_init(source.clone());
        if let Some(_) = statement {
            return (nsource, statement);
        }

        let (nsource, expression) = self.parse_expression(0, source.clone());
        if let Some(_) = expression {
            return (nsource, expression);
        }
        return (source, None);
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
