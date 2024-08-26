use crate::{
    bump,
    tokenizer::{self, Token, TokenValue},
};

pub const NULL: u32 = 4294967295;

#[derive(Debug, Clone)]
pub enum FunctionalNodeType {
    Fn,
    Assign,
    ValAssign,
    VarAssign,
    Type,
    Identifier,
    If,
    Else,
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

#[derive(Debug, Clone)]
pub enum TypeNodeType {
    Alias,
    Array,
    Struct,
}

#[derive(Clone)]
pub struct TypeNode {
    pub primary_token: u32,
    pub data1: u32,
    pub data2: u32,
    pub additional_data: u16,
    pub node_type: TypeNodeType,
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub primary_token: u32,
    pub parameters: Vec<TypedIdentifier>,
    pub return_type: u32,
}

#[derive(Clone)]
pub struct Identifier {
    pub primary_token: u32,
}

#[derive(Clone)]
pub struct TypedIdentifier {
    pub primary_token: u32,
    pub type_node: u32,
}

#[derive(Clone)]
pub struct Assign {
    pub identifier: u32,
    pub value: FunctionalNode,
}

#[derive(Debug, Clone)]
pub enum MessageLevel {
    Debug,
    Info,
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub enum MessageType {
    TypeExpected,
    ValueExpected,
    StatementExpected,
    BraceExpected,
    ParenExpected,
    SquareExpected,
    CommaExpected,
    EQExpected,
    Literal,
    IdentifierExpected,
}

#[derive(Debug, Clone)]
pub enum MessageContext {
    Type(TypeNodeType),
    Functional(FunctionalNodeType),
}

#[derive(Debug, Clone)]
pub struct Message {
    pub token: u32,
    pub level: MessageLevel,
    pub message: MessageType,
    pub context: MessageContext,
}

pub struct Tree<'a> {
    pub identifiers: bump::Storage<Identifier>,
    pub signatures: bump::Storage<FunctionSignature>,
    pub typed_identifier: bump::Storage<TypedIdentifier>,
    pub assign: bump::Storage<Assign>,
    pub integers: bump::Storage<u64>,
    pub float: bump::Storage<f64>,
    pub block: bump::Storage<Vec<FunctionalNode>>,

    pub type_nodes: bump::Storage<TypeNode>,
    pub functional_nodes: bump::Storage<FunctionalNode>,

    pub messages: Vec<Message>,
    used: bool,

    pub source: tokenizer::SourceReader<'a>,
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

    fn offset(&self) -> u32 {
        return self.tokens_offset as u32;
    }
}

impl<'source> Tree<'source> {
    pub fn new(source: tokenizer::SourceReader<'source>, tokens: Vec<tokenizer::Token>) -> Self {
        Self {
            tokens,
            source,
            identifiers: bump::Storage::default(),
            signatures: bump::Storage::default(),
            typed_identifier: bump::Storage::default(),
            assign: bump::Storage::default(),
            integers: bump::Storage::default(),
            float: bump::Storage::default(),
            block: bump::Storage::default(),
            type_nodes: bump::Storage::default(),
            functional_nodes: bump::Storage::default(),
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

    fn next<'a>(mut source: SourceReader<'a>) -> (SourceReader<'a>, Option<Token>) {
        while let (nsource, Some(t)) = source.next() {
            if t.value() != TokenValue::Whitespace && t.value() != TokenValue::InlineComment {
                return (nsource, Some(t));
            }
            source = nsource;
        }
        return (source, None);
    }

    fn expect<'a>(source: SourceReader<'a>, v: TokenValue) -> (SourceReader<'a>, Option<Token>) {
        if let (nsource, Some(t)) = Self::next(source.clone()) {
            if t.value() == v {
                return (nsource, Some(t));
            }
        }
        return (source, None);
    }

    fn expect_any<'a, const N: usize>(
        source: SourceReader<'a>,
        v: [TokenValue; N],
    ) -> (SourceReader<'a>, Option<Token>) {
        if let (nsource, Some(t)) = Self::next(source.clone()) {
            if v.contains(&t.value()) {
                return (nsource, Some(t));
            }
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
        let (nsource, value) = Self::next(source.clone());
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
        let (nsource, t) = Self::next(source.clone());
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
        let (tokensource, t) = Self::next(source.clone());
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
        if let (nsource, Some(t)) = Self::next(source.clone()) {
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
        let (nsource, t) = Self::next(source.clone());
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
        let maybe_expr = if let (nsource, Some(v)) =
            self.parse_unary_prefixed(weight, source.clone())
        {
            (nsource, Some(v))
        } else if let (nsource, Some(v)) = self.parse_literal(source.clone()) {
            (nsource, Some(v))
        } else if let (nsource, Some(v)) = self.parse_fn(source.clone()) {
            (nsource, Some(v))
        } else if let (nsource, Some(_)) = Self::expect(source.clone(), TokenValue::Identifier) {
            (
                nsource,
                Some(FunctionalNode {
                    primary_token: source.offset(),
                    data1: self.identifiers.allocate(Identifier {
                        primary_token: source.offset(),
                    }),
                    data2: NULL,
                    additional_data: 0,
                    node_type: FunctionalNodeType::Identifier,
                }),
            )
        } else {
            (source, None)
        };

        if let (nsource, Some(v)) = maybe_expr.clone() {
            let extensioned = self.parse_extension_max_weight(v, weight, nsource);
            if let (mut nsource, Some(mut v)) = extensioned {
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

    fn parse_array_type<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<TypeNode>) {
        let (nsource, sqopen) = Self::expect(source.clone(), TokenValue::SquareOpen);
        if sqopen.is_none() {
            return (source, None);
        }
        let (sizedsource, expr) = self.parse_expression(0, nsource.clone());
        let nsource = if expr.is_some() { sizedsource } else { nsource };
        let data = if let Some(expr) = expr {
            self.functional_nodes.allocate(expr)
        } else {
            NULL
        };
        let (closedsource, t) = Self::expect_any(
            nsource.clone(),
            [TokenValue::SquareClose, TokenValue::PhantomSquareClose],
        );
        if t.is_none() || t.unwrap().value() == TokenValue::PhantomSquareClose {
            self.emit_message(
                MessageLevel::Error,
                MessageType::SquareExpected,
                MessageContext::Type(TypeNodeType::Array),
                nsource.offset(),
            )
        }
        let (tsource, t) = self.parse_type(closedsource.clone());
        if t.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::TypeExpected,
                MessageContext::Type(TypeNodeType::Array),
                closedsource.offset(),
            )
        }
        return (
            tsource,
            Some(TypeNode {
                primary_token: source.offset(),
                data1: data,
                data2: self.type_nodes.allocate_or(t, NULL),
                additional_data: 0,
                node_type: TypeNodeType::Array,
            }),
        );
    }

    fn parse_type<'a>(&mut self, source: SourceReader<'a>) -> (SourceReader<'a>, Option<TypeNode>) {
        if let (arrsource, Some(arrtype)) = self.parse_array_type(source.clone()) {
            return (arrsource, Some(arrtype));
        }
        if let (nsource, Some(_)) = Self::expect(source.clone(), TokenValue::Identifier) {
            (
                nsource,
                Some(TypeNode {
                    primary_token: source.offset(),
                    data1: NULL,
                    data2: NULL,
                    additional_data: 0,
                    node_type: TypeNodeType::Alias,
                }),
            )
        } else {
            (source, None)
        }
    }

    fn parse_init<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let primary = source.offset();
        let (source, t) = Self::next(source);
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

        let mut typed_identifier = TypedIdentifier {
            primary_token: source.offset(),
            type_node: NULL,
        };

        let (source, identifier) = Self::expect(source, TokenValue::Identifier);
        if identifier.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::IdentifierExpected,
                MessageContext::Functional(fnode.node_type.clone()),
                source.offset(),
            );
        }

        let (source, colon) = Self::expect(source, TokenValue::Colon);
        if colon.is_some() {
            let (source, _type) = self.parse_type(source.clone());
            if let Some(tnode) = _type {
                typed_identifier.type_node = self.type_nodes.allocate(tnode);
            } else {
                self.emit_message(
                    MessageLevel::Error,
                    MessageType::TypeExpected,
                    MessageContext::Functional(fnode.node_type.clone()),
                    source.offset(),
                );
            }
        }

        fnode.data1 = self.typed_identifier.allocate(typed_identifier);

        let (mut source, eq) = Self::expect(source, TokenValue::EQ);
        if !eq.is_some() {
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
        let (nsource, statement) = self.parse_assign(source.clone());
        if let Some(_) = statement {
            return (nsource, statement);
        }
        let (nsource, statement) = self.parse_block(source.clone());
        if let Some(_) = statement {
            return (nsource, statement);
        }
        let (nsource, statement) = self.parse_if(source.clone());
        if let Some(_) = statement {
            return (nsource, statement);
        }
        let (nsource, statement) = self.parse_for(source.clone());
        if let Some(_) = statement {
            return (nsource, statement);
        }

        let (nsource, expression) = self.parse_expression(0, source.clone());
        if let Some(_) = expression {
            return (nsource, expression);
        }
        return (source, None);
    }

    fn parse_block<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let primary = source.offset();

        if let (mut nsource, Some(_)) = Self::expect(source.clone(), TokenValue::BraceOpen) {
            let mut block = vec![];
            while let (nnsource, Some(t)) = self.parse_statement(nsource.clone()) {
                block.push(t);
                nsource = nnsource;
            }

            let brclose = Self::expect_any(
                nsource.clone(),
                [TokenValue::BraceClose, TokenValue::PhantomBraceClose],
            );

            let ret = if let (nnsource, Some(t)) = brclose {
                nsource = nnsource;
                if t.value() == TokenValue::PhantomBraceClose {
                    self.emit_message(
                        MessageLevel::Error,
                        MessageType::BraceExpected,
                        MessageContext::Functional(FunctionalNodeType::Block),
                        primary,
                    );
                }
                Some(FunctionalNode {
                    primary_token: primary,
                    data1: self.block.allocate(block),
                    data2: NULL,
                    additional_data: 0,
                    node_type: FunctionalNodeType::Block,
                })
            } else {
                None
            };
            return (nsource, ret);
        }
        return (source, None);
    }

    fn parse_if<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, t) = Self::expect(source.clone(), TokenValue::If);
        if t.is_none() {
            return (source, None);
        }
        let (nsource, condition) = self.parse_expression(0, nsource);
        if condition.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::ValueExpected,
                MessageContext::Functional(FunctionalNodeType::If),
                source.offset(),
            );
        }
        let (nsource, statement) = self.parse_statement(nsource.clone());
        if statement.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::StatementExpected,
                MessageContext::Functional(FunctionalNodeType::If),
                source.offset(),
            );
        }
        let if_node = FunctionalNode {
            primary_token: source.offset(),
            data1: self.functional_nodes.allocate_or(condition, NULL),
            data2: self.functional_nodes.allocate_or(statement, NULL),
            additional_data: 0,
            node_type: FunctionalNodeType::If,
        };
        let (elsesource, elsetoken) = Self::expect(nsource.clone(), TokenValue::Else);
        if let Some(_) = elsetoken {
            let (stmtsource, elsestatement) = self.parse_statement(elsesource.clone());
            if elsestatement.is_none() {
                self.emit_message(
                    MessageLevel::Error,
                    MessageType::StatementExpected,
                    MessageContext::Functional(FunctionalNodeType::Else),
                    elsesource.offset(),
                );
            } else {
                let else_node = FunctionalNode {
                    primary_token: nsource.offset(),
                    data1: self.functional_nodes.allocate(if_node),
                    data2: self.functional_nodes.allocate_or(elsestatement, NULL),
                    additional_data: 0,
                    node_type: FunctionalNodeType::Else,
                };
                return (stmtsource, Some(else_node));
            }
            return (elsesource, Some(if_node));
        }
        return (nsource, Some(if_node));
    }

    fn parse_fn<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, fntoken) = Self::expect(source.clone(), TokenValue::Fn);
        if fntoken.is_none() {
            return (source, None);
        }
        let (mut psource, popen) = Self::expect(nsource.clone(), TokenValue::ParenOpen);
        if popen.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::ParenExpected,
                MessageContext::Functional(FunctionalNodeType::Fn),
                nsource.offset(),
            );
            psource = nsource;
        }
        let mut args = vec![];
        while let (_, None) = Self::expect_any(
            psource.clone(),
            [TokenValue::ParenClose, TokenValue::PhantomParenClose],
        ) {
            let mut typed_id = TypedIdentifier {
                primary_token: psource.offset(),
                type_node: NULL,
            };
            let (mut idsource, id) = Self::expect(psource.clone(), TokenValue::Identifier);
            if id.is_none() {
                self.emit_message(
                    MessageLevel::Error,
                    MessageType::IdentifierExpected,
                    MessageContext::Functional(FunctionalNodeType::Fn),
                    psource.offset(),
                );
                idsource = psource.clone();
            }

            let (mut csource, colon) = Self::expect(idsource.clone(), TokenValue::Colon);
            if colon.is_some() {
                let (tsource, tnode) = self.parse_type(csource.clone());
                if let Some(t) = tnode {
                    println!("Appedingin typenode of type: {:?}", t.node_type);
                    typed_id.type_node = self.type_nodes.allocate(t);
                    csource = tsource;
                } else {
                    self.emit_message(
                        MessageLevel::Error,
                        MessageType::TypeExpected,
                        MessageContext::Functional(FunctionalNodeType::Fn),
                        csource.offset(),
                    );
                }
            }

            let (commasource, comma) = Self::next(csource.clone());
            args.push(typed_id);
            if let Some(terminator) = comma {
                if TokenValue::Comma == terminator.value() {
                    csource = commasource;
                } else if TokenValue::ParenClose != terminator.value() {
                    self.emit_message(
                        MessageLevel::Error,
                        MessageType::CommaExpected,
                        MessageContext::Functional(FunctionalNodeType::Fn),
                        csource.offset(),
                    );
                    break;
                }
            }
            psource = csource;
        }

        let (psource, _) = Self::next(psource);

        let mut signature = FunctionSignature {
            primary_token: source.offset(),
            parameters: args,
            return_type: NULL,
        };
        let (mut csource, colon) = Self::expect(psource, TokenValue::Colon);
        if colon.is_some() {
            let (tsource, type_node) = self.parse_type(csource.clone());
            if let Some(node) = type_node {
                signature.return_type = self.type_nodes.allocate(node);
                csource = tsource;
            } else {
                self.emit_message(
                    MessageLevel::Error,
                    MessageType::TypeExpected,
                    MessageContext::Functional(FunctionalNodeType::Fn),
                    csource.offset(),
                );
            }
        }
        let (stmtsource, stmtnode) = self.parse_statement(csource);
        let node = FunctionalNode {
            primary_token: source.offset(),
            data1: self.signatures.allocate(signature),
            data2: self.functional_nodes.allocate_or(stmtnode, NULL),
            additional_data: 0,
            node_type: FunctionalNodeType::Fn,
        };
        if node.data2 == NULL {
            self.emit_message(
                MessageLevel::Error,
                MessageType::StatementExpected,
                MessageContext::Functional(FunctionalNodeType::Fn),
                source.offset(),
            );
        }
        return (stmtsource, Some(node));
    }

    fn parse_assign<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, id) = Self::expect(source.clone(), TokenValue::Identifier);
        if id.is_none() {
            return (source, None);
        }
        let (nsource, eq) = Self::expect(nsource.clone(), TokenValue::EQ);
        if eq.is_none() {
            return (nsource, None);
        }
        let (nsource, value) = self.parse_expression(0, nsource.clone());
        if value.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::ValueExpected,
                MessageContext::Functional(FunctionalNodeType::Assign),
                nsource.offset(),
            );
        }
        let fnode = FunctionalNode {
            primary_token: source.offset(),
            data1: self.functional_nodes.allocate_or(value, NULL),
            data2: NULL,
            additional_data: 0,
            node_type: FunctionalNodeType::Assign,
        };
        return (nsource, Some(fnode));
    }

    fn parse_for<'a>(
        &mut self,
        source: SourceReader<'a>,
    ) -> (SourceReader<'a>, Option<FunctionalNode>) {
        let (nsource, id) = Self::expect(source.clone(), TokenValue::For);
        if id.is_none() {
            return (source, None);
        }
        let mut fnode = FunctionalNode {
            primary_token: source.offset(),
            data1: NULL,
            data2: NULL,
            additional_data: 0,
            node_type: FunctionalNodeType::For,
        };
        let (nsource, condition) = self.parse_statement(nsource);
        if condition.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::StatementExpected,
                MessageContext::Functional(FunctionalNodeType::For),
                nsource.offset(),
            );
            return (nsource, Some(fnode));
        }
        fnode.data1 = self.functional_nodes.allocate_or(condition, NULL);
        let (nsource, statement) = self.parse_statement(nsource);
        if statement.is_none() {
            self.emit_message(
                MessageLevel::Error,
                MessageType::StatementExpected,
                MessageContext::Functional(FunctionalNodeType::For),
                nsource.offset(),
            );
            return (nsource, Some(fnode));
        }
        fnode.data2 = self.functional_nodes.allocate_or(statement, NULL);
        return (nsource, Some(fnode));
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
