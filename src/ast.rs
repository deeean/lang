#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
  Positive,
  Negative,
  Not,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  Equal,
  NotEqual,
  GreaterThan,
  LessThan,
  GreaterThanOrEqual,
  LessThanOrEqual,
  And,
  Or,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignOp {
  Assign,
  AddAssign,
  SubtractAssign,
  MultiplyAssign,
  DivideAssign,
  ModuloAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
  Identifier(String),
  Null,
  Number(f64),
  String(String),
  Boolean(bool),

  Unary(UnaryOp, Box<Expression>),
  Binary(Box<Expression>, BinaryOp, Box<Expression>),

  Assign(String, AssignOp, Box<Expression>),

  Call(Box<Expression>, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Let(String, Expression),
  For(
    Option<Program>,
    Option<Expression>,
    Option<Program>,
    Program,
  ),
  Expression(Expression),
}

pub type Program = Vec<Statement>;