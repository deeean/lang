use logos::Logos;
use lang::parser::{parser};
use lang::lexer::Token;
use chumsky::Parser;
use inkwell::context::Context;
use lang::compiler::Compiler;

fn main() {
    let source = r#"
fn fibonacci(n: i32) -> i32 {
    if (n <= 1) {
        return n;
    }

    return fibonacci(n - 1) + fibonacci(n - 2);
}

fn main() -> i32 {
    printf("%d\n", fibonacci(10));
    return 0;
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
