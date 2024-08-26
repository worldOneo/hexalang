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
        match node.node_type {
            parser::TypeNodeType::Alias => todo!(),
            parser::TypeNodeType::Array => todo!(),
            parser::TypeNodeType::Struct => todo!(),
        }
    }

    fn no_type(&mut self, primary_token: u32) -> IRTypeNode {
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
                    self.no_type(typed_id.primary_token)
                };
                let result =
                    self.eval(&type_node, &tree.functional_nodes.receive(line.data2), tree);
                let source = tree.source.set_offset(typed_id.primary_token);
                let (_, name) = tokenizer::lex_identifier_value(source);
                let name = name.unwrap().iter().collect();
                let register = self.create_rename(&name);
                self.create_var(register, type_node);
                self.assign_var(line.primary_token, register, result.register);
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
            _ => unreachable!(),
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
            parser::FunctionalNodeType::BiOp => {
                let lhs = tree.functional_nodes.receive(expression.data1);
                let rhs = tree.functional_nodes.receive(expression.data2);
                let lhs_result = self.eval(expected_type, &lhs, &tree);
                let rhs_register = self.eval(expected_type, &rhs, &tree);
                let register = self.new_register();
                self.emit_node(IRNode {
                    primary_token: expression.primary_token,
                    node_type: IRNodeType::BiOp,
                    data1: lhs_result.register,
                    data2: rhs_register.register,
                    data3: expression.additional_data as u64,
                });
                self.implicit_register_cast(lhs_result.register, rhs_register.register);
                return IRResult {
                    expression_type: Some(expected_type.clone()),
                    register,
                };
            }
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
            parser::FunctionalNodeType::Identifier => {
                let source = tree.source.set_offset(expression.primary_token);
                let (_, data) = tokenizer::lex_identifier_value(source);
                let name: String = data.unwrap().iter().collect();
                let reg = self.current_scope().renames.get(&name).cloned();
                if let Some(reg) = reg {
                    return IRResult {
                        expression_type: self.current_scope().vars.get(&reg).cloned(),
                        register: reg,
                    };
                }
                unreachable!()
            }
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

    fn type_check(&mut self, a: IRTypeNode, b: IRTypeNode) -> bool {
        todo!();
    }

    fn implicit_register_cast(&mut self, a: u32, b: u32) -> bool {
        let lhst = self.current_scope().vars.get(&a).unwrap().clone();
        let rhst = self.current_scope().vars.get(&b).unwrap().clone();
        return self.type_check(lhst, rhst);
    }

    fn assign_var(&mut self, primary_token: u32, lhs: u32, rhs: u32) {
        self.emit_node(IRNode {
            primary_token,
            node_type: IRNodeType::Assign,
            data1: lhs,
            data2: rhs,
            data3: 0,
        });
        self.implicit_register_cast(lhs, rhs);
    }
}
