mod tokenizer;

fn main() {
    tokenizer::tokenize("if".into());
    tokenizer::tokenize("for".into());
    tokenizer::tokenize("fn".into());
    tokenizer::tokenize("process".into());
    tokenizer::tokenize("bob1".into());
    tokenizer::tokenize("\"\\\"This is a test\"".into());
    tokenizer::tokenize("1234.5678".into());
    tokenizer::tokenize("// this is a test comment".into());
    tokenizer::tokenize(" ".into());
}
