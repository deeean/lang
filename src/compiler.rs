use std::collections::HashMap;
use std::rc::Rc;
use crate::ast::{BinaryOp, Expression, Program, Statement};
use crate::object::Object;
use crate::opcode::OpCode;

#[derive(Debug)]
pub struct Compiler {

}

impl Compiler {
  pub fn new() -> Self {
    Self {

    }
  }

  fn add_constant(&mut self, obj: Object) -> usize {
    0
  }

  fn compile_expression(&mut self, expression: &Expression) {
    match expression {
      Expression::Number(n) => {

      },
      Expression::Binary(left, operator, right) => {

      },
      _ => {}
    }
  }

  fn compile_statement(&mut self, statement: &Statement) {
    match statement {
      Statement::Expression(expression) => {

      },
      Statement::Let(name, expression) => {

      },
      _ => {}
    }
  }

  fn compile_program(&mut self, program: Program) {
    for statement in &program {
      self.compile_statement(statement);
    }
  }

  pub fn compile(&mut self, program: Program) {
    self.compile_program(program);
  }
}

#[cfg(test)]
mod tests {
  use crate::compiler::Compiler;
  use crate::lexer::Lexer;
  use crate::parser::Parser;

  #[test]
  fn compiler() {
    let program = Parser::new(Lexer::new(r#"

    let i = 1000;

    i += 100;
    i /= 100;

    "#).lex()).parse().unwrap();
    let mut compiler = Compiler::new();
    compiler.compile(program);
  }
}