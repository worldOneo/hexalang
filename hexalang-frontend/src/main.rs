#![feature(type_alias_impl_trait)]

use std::rc::Rc;

mod bump;
mod codegen;
mod ir;
mod parser;
mod parser2;
mod tokenizer;

fn main() {
    let src = "if 1 // this is a one btw\n+1*2==1*2+2 var a = 1 else 1+1 val test = fn(a: []int, b) { a + b a = 1 for a == 1 a = a(1, 2) + 1 }".chars().collect();
    let source = tokenizer::SourceReader::new(&src, Rc::new("shell".into()));
    let tokens = tokenizer::tokenize(source.clone());
    let mut tree = parser::Tree::new(source.clone(), tokens);
    let blk = tree.parse();
    println!("{:?}", tree.messages);
    println!("{:?}", blk[0].node_type);
    println!("{:?}", blk[1].node_type);
    println!(
        "{:?}",
        tree.functional_nodes.receive(blk[1].data2).node_type
    );
    let firstarg = tree
        .signatures
        .receive(tree.functional_nodes.receive(blk[1].data2).data1)
        .parameters[0]
        .type_node;
    println!("{:?}", tree.type_nodes.receive(firstarg).node_type);
}
