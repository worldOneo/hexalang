mod tokenizer;
mod parser;

fn main() {
    dbg!(tokenizer::tokenize("fn test(a: [{i) { if a { for b { \"Test\"\n// this changes everything\n } } }".into()));
}