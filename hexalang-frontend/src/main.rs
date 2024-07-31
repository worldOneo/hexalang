use std::rc::Rc;

mod tokenizer;
mod parser;

fn main() {
    let src = "1+1*2==1*2+2".chars().collect();
    let source = tokenizer::SourceReader::new(&src, Rc::new("shell".into()));
    dbg!(tokenizer::tokenize(source.clone()));
    let mut tree = parser::Tree::new(source.clone(), tokenizer::tokenize(source));
    let blk = tree.parse();
    println!("{:?}", parser::BiOp::from(tree.functional_nodes.receive(blk[0].data1).additional_data));

}