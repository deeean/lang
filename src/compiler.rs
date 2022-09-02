use std::collections::HashMap;
use std::rc::Rc;
use crate::ast::{BinaryOp, Expression, Program, Statement};
use crate::object::Object;
use crate::opcode::OpCode;
use byteorder::{ByteOrder, BigEndian, WriteBytesExt, ReadBytesExt};

fn make_instruction(opcode: OpCode, operands: &Vec<usize>) -> Vec<u8> {
  let mut instruction = Vec::new();
  let widths = opcode.operand_widths();
  instruction.push(opcode as u8);

  for (o, width) in operands.into_iter().zip(widths) {
    match width {
      2 => {
        instruction.write_u16::<BigEndian>(*o as u16).unwrap()
      },
      1 => {
        instruction.write_u8(*o as u8).unwrap()
      },
      _ => panic!("unsupported operand width {}", width),
    };
  }

  instruction
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolScope {
  Global,
  Local,
  Builtin,
  Free
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
  pub name: String,
  pub scope: SymbolScope,
  pub index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTable {
  pub outer: Option<Box<SymbolTable>>,
  pub symbols: HashMap<String, Rc<Symbol>>,
  pub num_definitions: usize,
  pub free_symbols: Vec<Rc<Symbol>>,
}

impl SymbolTable {
  pub fn new() -> SymbolTable {
    SymbolTable {
      outer: None,
      symbols: HashMap::new(),
      num_definitions: 0,
      free_symbols: Vec::new(),
    }
  }

  fn define(&mut self, name: &String) -> Rc<Symbol> {
    let scope = if self.outer.is_some() {
      SymbolScope::Local
    } else {
      SymbolScope::Global
    };

    let symbol = Rc::new(Symbol {
      name: name.clone(),
      scope,
      index: self.num_definitions,
    });

    self.symbols.insert(name.clone(), Rc::clone(&symbol));
    self.num_definitions += 1;

    symbol
  }
}

#[derive(Debug)]
pub struct Bytecode {
  pub instructions: Vec<u8>,
  pub constants: Vec<Rc<Object>>,
}

#[derive(Debug, Clone)]
struct Emitted {
  pub opcode: OpCode,
  pub position: usize,
}

#[derive(Debug)]
struct Scope {
  instructions: Vec<u8>,
  last: Option<Emitted>,
  prev: Option<Emitted>,
}

#[derive(Debug)]
pub struct Compiler {
  pub constants: Vec<Rc<Object>>,
  pub symbol_table: SymbolTable,

  scopes: Vec<Scope>,
  scope_depth: usize,
}

impl Compiler {
  pub fn new() -> Self {
    let mut symbol_table = SymbolTable::new();

    Self {
      constants: vec![],
      symbol_table,
      scopes: vec![
        Scope {
          instructions: vec![],
          last: None,
          prev: None,
        }
      ],
      scope_depth: 0,
    }
  }

  fn emit(&mut self, opcode: OpCode, operands: &Vec<usize>) -> usize {
    let mut instruction = make_instruction(opcode, operands);
    let pos = self.add_instruction(&mut instruction);
    self.set_last_instruction(opcode, pos);
    pos
  }

  fn set_last_instruction(&mut self, opcode: OpCode, position: usize) {
    match &self.scopes[self.scope_depth].last {
      Some(instruction) => self.scopes[self.scope_depth].prev = Some(instruction.clone()),
      _ => (),
    }

    self.scopes[self.scope_depth].last = Some(Emitted{opcode, position});
  }

  fn add_instruction(&mut self, instruction: &Vec<u8>) -> usize {
    let pos = self.scopes[self.scope_depth].instructions.len();
    self.scopes[self.scope_depth].instructions.extend_from_slice(instruction);
    pos
  }

  fn add_constant(&mut self, obj: Object) -> usize {
    self.constants.push(Rc::new(obj));
    self.constants.len() - 1
  }

  fn compile_expression(&mut self, expression: &Expression) {
    match expression {
      Expression::Number(n) => {
        let number = Object::Number(*n);
        let operand = vec![self.add_constant(number)];
        self.emit(OpCode::Constant, &operand);
      },
      Expression::Binary(left, operator, right) => {
        self.compile_expression(left);
        self.compile_expression(right);

        match operator.clone() {
          BinaryOp::Add => self.emit(OpCode::Add, &vec![]),
          BinaryOp::Sub => self.emit(OpCode::Sub, &vec![]),
          BinaryOp::Mul => self.emit(OpCode::Mul, &vec![]),
          BinaryOp::Div => self.emit(OpCode::Div, &vec![]),
          BinaryOp::Mod => self.emit(OpCode::Mod, &vec![]),
          BinaryOp::Equal => self.emit(OpCode::Equal, &vec![]),
          BinaryOp::NotEqual => self.emit(OpCode::NotEqual, &vec![]),
          BinaryOp::LessThan => self.emit(OpCode::LessThan, &vec![]),
          BinaryOp::LessThanOrEqual => self.emit(OpCode::LessThanOrEqual, &vec![]),
          BinaryOp::GreaterThan => self.emit(OpCode::GreaterThan, &vec![]),
          BinaryOp::GreaterThanOrEqual => self.emit(OpCode::GreaterThanOrEqual, &vec![]),
          _ => panic!("unsupported binary operator {:?}", operator),
        };
      },
      _ => {}
    }
  }

  fn compile_statement(&mut self, statement: &Statement) {
    match statement {
      Statement::Expression(expression) => {
        self.compile_expression(expression);
        self.emit(OpCode::Pop, &vec![]);
      },
      Statement::Let(name, expression) => {
        let symbol = self.symbol_table.define(name);
        self.compile_expression(expression);

        match &symbol.scope {
          SymbolScope::Global => self.emit(OpCode::SetGlobal, &vec![symbol.index]),
          SymbolScope::Local => self.emit(OpCode::SetLocal, &vec![symbol.index]),
          _ => panic!("unsupported symbol scope {:?}", symbol.scope),
        };
      },
      _ => {}
    }
  }

  fn compile_program(&mut self, program: Program) {
    for statement in &program {
      self.compile_statement(statement);
    }
  }


  fn bytecode(&mut self) -> Bytecode {
    Bytecode {
      instructions: self.scopes[self.scope_depth].instructions.clone(),
      constants: self.constants.clone(),
    }
  }


  pub fn compile(&mut self, program: Program) -> Bytecode {
    self.compile_program(program);
    self.bytecode()
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
    let bytecode = compiler.compile(program);
    println!("{:#?}", bytecode);
  }
}