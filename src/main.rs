use logos::Logos;
use lang::parser::{parser};
use lang::lexer::Token;
use chumsky::Parser;
use inkwell::context::Context;
use lang::compiler::Compiler;
use lang::type_inference::TypeInference;

fn main() {
    let source = r#"
fn main() -> i32 {
    var a = 1 == 1;

    if a {
        printf("Hello, World!");
    } else {
        printf("Hello");
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
                panic!("{:#?} {:#?}", e, span);
            }
        }
    }

    println!("Tokens: {:#?}", tokens);

    let mut res = match parser().parse(&*tokens) {
        Ok(res) => res,
        Err(e) => {
            panic!("{:#?}", e);
        }
    };

    println!("Parsed: {:#?}", res);

    let mut type_inference = TypeInference::new();
    type_inference.type_inference_program(&mut res).unwrap();

    println!("Type inferenced: {:#?}", res);

    let ctx = Context::create();
    let mut compiler = Compiler::new(&ctx);

    compiler.compile(res).unwrap();
}
