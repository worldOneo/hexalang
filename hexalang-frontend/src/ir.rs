use std::collections::HashMap;

use crate::{bump, parser, tokenizer};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeNodeType {
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
    Enum,
    Tuple,
    Unkown,
    Alias,
    Type,
    Function,
}

#[derive(Debug, Clone)]
pub struct TypeNode {
    primary_token: u32,
    node_type: TypeNodeType,
    data1: u32,
    data2: u32,
}

#[derive(Debug, Clone)]
pub enum IRNodeType {
    BiOp,
    Init,
    Assign,
    If,
    For,
    ConstLoadU64,
    LoadMember,
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
    vars: HashMap<u32, TypeNode>,
    type_vars: HashMap<String, TypeNode>,
}

#[derive(Clone)]
struct FunctionType {
    args: Vec<TypeNode>,
    ret: TypeNode,
}

struct Compiler {
    type_nodes: bump::Storage<TypeNode>,
    blocks: bump::Storage<Vec<IRNode>>,
    scopes: Vec<Scope>,
    code_scopes: Vec<Vec<IRNode>>,
    output: Vec<IRNode>,
    counter: u32,
    functions: Function,
    function_signature_types: bump::Storage<FunctionType>,
}

struct TypedRegister {
    expression_type: Option<TypeNode>,
    register: u32,
}

impl Compiler {
    pub fn run(&mut self, code: Vec<parser::FunctionalNode>, tree: &parser::Tree) {
        self.unordered_scope_scan(&code, tree);
        for inst in &code {
            self.run_line(inst, tree)
        }
    }

    fn type_to_ir(&mut self, node: &parser::TypeNode, tree: &parser::Tree) -> TypeNode {
        match node.node_type {
            parser::TypeNodeType::Alias => todo!(),
            parser::TypeNodeType::Array => todo!(),
            parser::TypeNodeType::Struct => todo!(),
        }
    }

    fn no_type(&mut self, primary_token: u32) -> TypeNode {
        TypeNode {
            primary_token,
            node_type: TypeNodeType::Unkown,
            data1: 0,
            data2: 0,
        }
    }

    fn lex_identifier(&mut self, token: u32, tree: &parser::Tree) -> String {
        let source = tree.source.set_offset(token);
        let (_, data) = tokenizer::lex_identifier_value(source);
        data.expect("Identifier expected").iter().collect()
    }

    fn infere_unordered_type(
        &mut self,
        statement: &parser::FunctionalNode,
        tree: &parser::Tree,
    ) -> TypeNode {
        match &statement.node_type {
            parser::FunctionalNodeType::Fn => {
                let signature = tree.signatures.receive(statement.data1);
                let ret = if signature.return_type == parser::NULL {
                    self.type_to_ir(&tree.type_nodes.receive(signature.return_type), tree)
                } else {
                    self.no_type(signature.primary_token)
                };
                let args = signature.parameters;
                let mut targs = vec![];
                for arg in args {
                    targs.push(self.type_to_ir(&tree.type_nodes.receive(arg.type_node), tree));
                }
                TypeNode {
                    primary_token: statement.primary_token,
                    node_type: TypeNodeType::Function,
                    data1: self
                        .function_signature_types
                        .allocate(FunctionType { args: targs, ret }),
                    data2: 0,
                }
            }
            parser::FunctionalNodeType::Type => {
                let translated_type =
                    self.type_to_ir(&tree.type_nodes.receive(statement.data1), tree);
                TypeNode {
                    primary_token: statement.primary_token,
                    data1: self.type_nodes.allocate(translated_type),
                    data2: 0,
                    node_type: TypeNodeType::Type,
                }
            }
            parser::FunctionalNodeType::Int => TypeNode {
                primary_token: statement.primary_token,
                data1: 0,
                data2: 0,
                node_type: TypeNodeType::ComptimeInt,
            },
            parser::FunctionalNodeType::Float => TypeNode {
                primary_token: statement.primary_token,
                data1: 0,
                data2: 0,
                node_type: TypeNodeType::ComptimeFloat,
            },
            parser::FunctionalNodeType::Identifier => TypeNode {
                primary_token: statement.primary_token,
                data1: 0,
                data2: 0,
                node_type: TypeNodeType::Alias,
            },
            _ => unreachable!(),
        }
    }

    fn unordered_scope_scan(&mut self, code: &Vec<parser::FunctionalNode>, tree: &parser::Tree) {
        let mut regs = vec![];
        for statement in code {
            match &statement.node_type {
                parser::FunctionalNodeType::ValAssign => {
                    let lhs = tree.typed_identifier.receive(statement.data1);
                    let rhs = tree.functional_nodes.receive(statement.data2);
                    let ty = self.infere_unordered_type(&rhs, tree);
                    let name = self.lex_identifier(lhs.primary_token, tree);
                    let reg = self.create_rename(&name);
                    regs.push(reg);
                    self.create_var(lhs.primary_token, reg, ty);
                }
                _ => panic!("Only val assign allowed in global scope"),
            }
        }

        for (statement, reg) in code.iter().zip(regs.iter()) {
            let lhs = tree.typed_identifier.receive(statement.data1);
            let rhs = tree.functional_nodes.receive(statement.data2);
            let nt = &self.no_type(lhs.primary_token);
            let r = self.eval(nt, &rhs, tree);
            self.emit_node(IRNode {
                primary_token: lhs.primary_token,
                node_type: IRNodeType::Assign,
                data1: *reg,
                data2: r.register,
                data3: 0,
            })
        }
    }

    fn run_line(&mut self, line: &parser::FunctionalNode, tree: &parser::Tree) {
        match &line.node_type {
            parser::FunctionalNodeType::Assign => {
                let name = self.lex_identifier(line.primary_token, tree);
                let reg = self.get_rename(&name);
                if let Some(reg) = reg {
                    let ty = self.get_type(reg).unwrap();
                    let rhs = self.eval(&ty, &tree.functional_nodes.receive(line.data1), tree);
                    self.implicit_register_cast(reg, rhs.register);
                    self.emit_node(IRNode {
                        primary_token: line.primary_token,
                        node_type: IRNodeType::Assign,
                        data1: reg,
                        data2: rhs.register,
                        data3: 0,
                    });
                }
            }
            parser::FunctionalNodeType::ValAssign | parser::FunctionalNodeType::VarAssign => {
                let typed_id = tree.typed_identifier.receive(line.data1);
                let type_node = if typed_id.type_node != parser::NULL {
                    self.type_to_ir(&tree.type_nodes.receive(typed_id.type_node), tree)
                } else {
                    self.no_type(typed_id.primary_token)
                };
                let result =
                    self.eval(&type_node, &tree.functional_nodes.receive(line.data2), tree);
                let name = self.lex_identifier(typed_id.primary_token, tree);
                let register = self.create_rename(&name);
                self.create_var(typed_id.primary_token, register, type_node);
                self.assign_var(line.primary_token, register, result.register);
            }
            parser::FunctionalNodeType::Type => todo!(),
            parser::FunctionalNodeType::If => todo!(),
            parser::FunctionalNodeType::Else => todo!(),
            parser::FunctionalNodeType::For => todo!(),
            parser::FunctionalNodeType::Process => todo!(),
            parser::FunctionalNodeType::Block => todo!(),
            parser::FunctionalNodeType::MemberAccess => todo!(),
            _ => unreachable!(),
        }
    }

    fn eval(
        &mut self,
        expected_type: &TypeNode,
        expression: &parser::FunctionalNode,
        tree: &parser::Tree,
    ) -> TypedRegister {
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
                return TypedRegister {
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
                return TypedRegister {
                    expression_type: Some(TypeNode {
                        primary_token: expression.primary_token,
                        node_type: TypeNodeType::ComptimeInt,
                        data1: 0,
                        data2: 0,
                    }),
                    register,
                };
            }
            parser::FunctionalNodeType::Float => todo!(),
            parser::FunctionalNodeType::String => todo!(),
            parser::FunctionalNodeType::Block => todo!(),
            parser::FunctionalNodeType::MemberAccess => {
                let lhs = tree.functional_nodes.receive(expression.data1);
                let notype = self.no_type(expression.primary_token);
                let result = self.eval(&notype, &lhs, tree);
                let rhs = tree.functional_nodes.receive(expression.data2);
                if result.expression_type.is_none() {
                    unreachable!()
                }
                self.compile_member_access(expected_type, result, &rhs, tree)
            }
            parser::FunctionalNodeType::Identifier => {
                let name = self.lex_identifier(expression.primary_token, tree);
                let reg = self.current_scope().renames.get(&name).cloned();
                if let Some(reg) = reg {
                    return TypedRegister {
                        expression_type: self.current_scope().vars.get(&reg).cloned(),
                        register: reg,
                    };
                }
                unreachable!()
            }
            _ => unreachable!(),
        }
    }

    fn compile_member_access(
        &mut self,
        expected_type: &TypeNode,
        lhs: TypedRegister,
        rhs: &parser::FunctionalNode,
        tree: &parser::Tree,
    ) -> TypedRegister {
        let lhs_type = lhs.expression_type.unwrap();

        match rhs.node_type {
            parser::FunctionalNodeType::Identifier => {
                if lhs_type.node_type != TypeNodeType::Struct {
                    panic!("Not struct accessed")
                }
                let ty = tree.struct_types.receive(lhs_type.data1);
                let member = self.lex_identifier(rhs.primary_token, tree);
                let entry = ty.fields.get(&member);
                if entry.is_none() {
                    panic!("No such member: {}", member)
                }
                let member_reg = self.new_register();
                let ty = entry.cloned().unwrap();
                let ty = self.type_to_ir(&tree.type_nodes.receive(ty), tree);
                self.create_var(rhs.primary_token, member_reg, ty.clone());
                self.emit_node(IRNode {
                    primary_token: rhs.primary_token,
                    node_type: IRNodeType::LoadMember,
                    data1: member_reg,
                    data2: lhs.register,
                    data3: 0,
                });
                TypedRegister {
                    expression_type: Some(ty),
                    register: member_reg,
                }
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

    fn get_rename(&mut self, name: &String) -> Option<u32> {
        for scope in self.scopes.iter().rev() {
            if let Some(reg) = scope.renames.get(name) {
                return Some(*reg);
            }
        }
        return None;
    }

    fn get_type(&mut self, reg: u32) -> Option<TypeNode> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.vars.get(&reg) {
                return Some(t.clone());
            }
        }
        return None;
    }

    fn create_var(&mut self, primary_token: u32, register: u32, ty: TypeNode) {
        let tyidx = self.type_nodes.allocate(ty.clone());
        self.emit_node(IRNode {
            primary_token,
            node_type: IRNodeType::Init,
            data1: register,
            data2: tyidx,
            data3: 0,
        });
        self.current_scope().vars.insert(register, ty);
    }

    fn type_check(&mut self, a: TypeNode, b: TypeNode) -> bool {
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
