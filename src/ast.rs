#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
  Positive,
  Negative,
  Not,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Equal,
  NotEqual,
  GreaterThan,
  LessThan,
  GreaterThanOrEqual,
  LessThanOrEqual,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AssignOperator {
  Assign,
  AddAssign,
  SubtractAssign,
  MultiplyAssign,
  DivideAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
  Identifier(String),
  Null,
  Number(f64),
  String(String),
  Boolean(bool),

  Unary(UnaryOperator, Box<Expression>),
  Binary(Box<Expression>, BinaryOperator, Box<Expression>),
  Call(Box<Expression>, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Let(String, Expression),
  Assign(String, AssignOperator, Expression),
  Expression(Expression),
}