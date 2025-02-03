use logos::Logos;
use lang::parser::{parser};
use lang::lexer::Token;
use chumsky::Parser;
use inkwell::context::Context;
use lang::compiler::Compiler;

fn main() {
    let source = r#"
fn say_hello() -> void {
    printf("Hello, World!\n");
}

fn main() -> void {
    say_hello();
    say_hello();
    say_hello();
    say_hello();
}
"#;


    println!("Source: {}", source);

    let mut tokens = Vec::new();

    for (token, span) in Token::lexer(source).spanned() {
        match token {
            Ok(token) => {
                tokens.push(token);
            }
            Err(e) => {
                panic!("{:#?} {:#?}", e, span);
            }
        }
    }

    println!("Tokens: {:#?}", tokens);

    let res = match parser().parse(&*tokens) {
        Ok(res) => res,
        Err(e) => {
            panic!("{:#?}", e);
        }
    };

    let ctx = Context::create();
    let mut compiler = Compiler::new(&ctx);

    compiler.compile(res).unwrap();
}
