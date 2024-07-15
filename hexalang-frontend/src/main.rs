mod tokenizer;

fn main() {
    tokenizer::tokenize("if".into());
    tokenizer::tokenize("for".into());
    tokenizer::tokenize("fn".into());
    tokenizer::tokenize("process".into());
}
