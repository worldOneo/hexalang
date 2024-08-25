use std::collections::HashMap;

use crate::{bump, parser, tokenizer};

#[derive(Debug, Clone)]
pub enum IRTypeNodeType {
    Uint64,
    Uint32,
    Uint16,
    Uint8,
    Int64,
    Int32,
    Int16,
    Int8,
    ComptimeInt,
    Float64,
    Float32,
    ComptimeFloat,
    Bool,
    Array,
    Struct,
    Unkown,
}

#[derive(Debug, Clone)]
pub struct IRTypeNode {
    primary_token: u32,
    node_type: IRTypeNodeType,
    data1: u32,
    data2: u32,
}

#[derive(Debug, Clone)]
pub enum IRNodeType {
    BiOp,
    Assign,
    If,
    For,
    ConstLoadU64,
}

#[derive(Debug, Clone)]
pub struct IRNode {
    primary_token: u32,
    node_type: IRNodeType,
    data1: u32,
    data2: u32,
    data3: u64,
}

struct Function {
    block: u32,
}

struct Scope {
    renames: HashMap<String, u32>,
    vars: HashMap<u32, IRTypeNode>,
}

struct Compiler {
    blocks: bump::Storage<Vec<IRNode>>,
    scopes: Vec<Scope>,
    code_scopes: Vec<Vec<IRNode>>,
    output: Vec<IRNode>,
    counter: u32,
    functions: Function,
}

struct IRResult {
    expression_type: Option<IRTypeNode>,
    register: u32,
}

impl Compiler {
    pub fn run(&mut self, code: Vec<parser::FunctionalNode>, tree: &parser::Tree) {
        for inst in &code {
            self.run_line(inst, tree)
        }
    }

    fn type_to_ir(&mut self, node: &parser::TypeNode, tree: &parser::Tree) -> IRTypeNode {
        todo!()
    }

    fn nonetype(&mut self, primary_token: u32) -> IRTypeNode {
        IRTypeNode {
            primary_token,
            node_type: IRTypeNodeType::Unkown,
            data1: 0,
            data2: 0,
        }
    }

    fn run_line(&mut self, line: &parser::FunctionalNode, tree: &parser::Tree) {
        match &line.node_type {
            parser::FunctionalNodeType::Fn => todo!(),
            parser::FunctionalNodeType::Assign => todo!(),
            parser::FunctionalNodeType::ValAssign | parser::FunctionalNodeType::VarAssign => {
                let typed_id = tree.typed_identifier.receive(line.data1);
                let type_node = if typed_id.type_node != parser::NULL {
                    self.type_to_ir(&tree.type_nodes.receive(typed_id.type_node), tree)
                } else {
                    self.nonetype(typed_id.primary_token)
                };
                let result =
                    self.eval(&type_node, &tree.functional_nodes.receive(line.data2), tree);
                let source = tree.source.set_offset(typed_id.primary_token);
                let (_, name) = tokenizer::lex_identifier_value(source);
                let name = name.unwrap().iter().collect();
                let register = self.create_rename(&name);
                self.create_var(register, type_node);
                self.emit_node(IRNode {
                    primary_token: line.primary_token,
                    node_type: IRNodeType::Assign,
                    data1: register,
                    data2: result.register,
                    data3: 0,
                });
            }
            parser::FunctionalNodeType::Type => todo!(),
            parser::FunctionalNodeType::If => todo!(),
            parser::FunctionalNodeType::Else => todo!(),
            parser::FunctionalNodeType::For => todo!(),
            parser::FunctionalNodeType::Process => todo!(),
            parser::FunctionalNodeType::BiOp => todo!(),
            parser::FunctionalNodeType::UnOp => todo!(),
            parser::FunctionalNodeType::Pipe => todo!(),
            parser::FunctionalNodeType::Int => todo!(),
            parser::FunctionalNodeType::Float => todo!(),
            parser::FunctionalNodeType::String => todo!(),
            parser::FunctionalNodeType::Block => todo!(),
            parser::FunctionalNodeType::MemberAccess => todo!(),
        }
    }

    fn eval(
        &mut self,
        expected_type: &IRTypeNode,
        expression: &parser::FunctionalNode,
        tree: &parser::Tree,
    ) -> IRResult {
        match &expression.node_type {
            parser::FunctionalNodeType::Fn => todo!(),
            parser::FunctionalNodeType::BiOp => todo!(),
            parser::FunctionalNodeType::UnOp => todo!(),
            parser::FunctionalNodeType::Pipe => todo!(),
            parser::FunctionalNodeType::Int => {
                let intval = tree.integers.receive(expression.data1);
                let register = self.new_register();
                self.emit_node(IRNode {
                    primary_token: expression.primary_token,
                    node_type: IRNodeType::ConstLoadU64,
                    data1: register,
                    data2: 0,
                    data3: intval,
                });
                return IRResult {
                    expression_type: Some(IRTypeNode {
                        primary_token: expression.primary_token,
                        node_type: IRTypeNodeType::ComptimeInt,
                        data1: 0,
                        data2: 0,
                    }),
                    register,
                };
            }
            parser::FunctionalNodeType::Float => todo!(),
            parser::FunctionalNodeType::String => todo!(),
            parser::FunctionalNodeType::Block => todo!(),
            parser::FunctionalNodeType::MemberAccess => todo!(),
            _ => unreachable!(),
        }
    }

    fn new_register(&mut self) -> u32 {
        self.counter += 1;
        return self.counter;
    }

    fn emit_node(&mut self, node: IRNode) {
        self.code_scopes.last_mut().unwrap().push(node);
    }

    fn current_scope<'a>(&'a mut self) -> &'a mut Scope {
        return self.scopes.last_mut().unwrap();
    }

    fn create_rename(&mut self, name: &String) -> u32 {
        let register = self.new_register();
        self.current_scope().renames.insert(name.clone(), register);
        return register;
    }

    fn create_var(&mut self, register: u32, ty: IRTypeNode) {
        self.current_scope().vars.insert(register, ty);
    }
}
