use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use ape::builtin::builtin;
use ape::env::Env;
use ape::evaluator::Evaluator;
use ape::lexer::Lexer;
use ape::parser::Parser;

fn main() {
  let matches = clap::Command::new("ape")
    .bin_name("ape")
    .version("0.1.0")
    .args(&[
      clap::Arg::with_name("file")
        .help("Run Ape program")
        .index(1),
    ])
    .get_matches();

  if let Some(file) = matches.value_of("file") {
    let f = File::open(file).expect("File not found");
    let mut reader = std::io::BufReader::new(f);
    let mut buffer = String::new();
    reader.read_to_string(&mut buffer).expect("Failed to read file");

    let mut evaluator = Evaluator::new(Rc::new(RefCell::new(Env::from(builtin()))));
    let tokens = Lexer::new(buffer.as_str()).lex();
    let program = Parser::new(tokens).parse();

    println!("---------------------------------------------");
    println!("{:#?}", program);
    let result = evaluator.eval(program);
    if let Some(result) = result {
      if Evaluator::is_error(&result) {
        println!("Error: {}", result);
      }
    }

  } else {
    todo!();
  }
}
