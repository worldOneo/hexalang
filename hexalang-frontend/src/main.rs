use std::rc::Rc;

mod tokenizer;
mod parser;

fn main() {
    let src = "if 1 // this is a one btw\n+1*2==1*2+2 var a = 1 else 1+1 val test = fn(a: int, b) { a + b a = 1 for a == 1 a = a + 1 }".chars().collect();
    let source = tokenizer::SourceReader::new(&src, Rc::new("shell".into()));
    let tokens = tokenizer::tokenize(source.clone());
    let mut tree = parser::Tree::new(source.clone(), tokens);
    let blk = tree.parse();
    println!("{:?}", tree.messages);
    println!("{:?}", blk[0].node_type);
    println!("{:?}", tree.block.receive(0)[1].node_type);
}