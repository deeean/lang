use logos::Logos;
use lang::ast::{parser};
use lang::lexer::Token;
use chumsky::Parser;
use inkwell::context::Context;
use lang::compiler::Compiler;

fn main() {
    let source = r#"
fn main() -> i32 {
    var i: i32 = 0;

    while (i < 10) {
        i = i + 1;

        printf("%d\n", i);
    }

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
                panic!("{:#?}", e);
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
