use std::fmt;
use std::num::ParseFloatError;
use crate::ast::{AssignOperator, BinaryOperator, Expression, Statement, UnaryOperator};
use crate::token::{TokenKind, Token};

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
  UnexpectedToken,
}

impl fmt::Display for ParseErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match *self {
      ParseErrorKind::UnexpectedToken => write!(f, "Unexpected token"),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ParseError {
  pub kind: ParseErrorKind,
  pub message: String,
}

impl ParseError {
  pub fn new(kind: ParseErrorKind, message: String) -> Self {
    Self { kind, message }
  }
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.kind, self.message)
  }
}

pub type ParseErrors = Vec<ParseError>;

pub struct Parser {
  tokens: Vec<Token>,
  errors: ParseErrors,
  curr: usize,
  next: usize,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self { tokens, errors: vec![], curr: 0, next: 1 }
  }

  fn parse_number(slice: String) -> Result<f64, ParseFloatError> {
    let reg = regex::Regex::new("_").unwrap();
    return reg.replace_all(slice.as_str(), "").parse::<f64>()
  }

  fn is_ended(&self) -> bool {
    self.curr >= self.tokens.len()
  }

  fn peek(&self) -> Token {
    if self.is_ended() {
      self.tokens[self.tokens.len() - 1].clone()
    } else {
      self.tokens[self.curr].clone()
    }
  }

  fn next_peek(&self) -> Token {
    if self.next >= self.tokens.len() {
      self.tokens[self.tokens.len() - 1].clone()
    } else {
      self.tokens[self.next].clone()
    }
  }

  fn advance(&mut self) -> Token {
    if self.is_ended() {
      return self.tokens[self.tokens.len() - 1].clone()
    }

    let token = self.peek();
    self.curr = self.next;
    self.next += 1;
    token
  }

  fn expect_next_token_kind(&mut self, kind: TokenKind) -> bool {
    if self.next_peek().kind == kind {
      self.advance();
      true
    } else {
      self.error_next_token_kind(kind);
      false
    }
  }

  fn error_next_token_kind(&mut self, kind: TokenKind) {
    self.errors.push(ParseError::new(
      ParseErrorKind::UnexpectedToken,
      format!("Expected next token kind to be {:?}, got {:?} instead", kind, self.next_peek().kind),
    ));
  }

  fn parse_identifier(&mut self) -> Option<String> {
    match self.peek().kind {
      TokenKind::Identifier => Some(self.peek().slice),
      _ => None,
    }
  }

  fn parse_identifier_expression(&mut self) -> Option<Expression> {
    match self.parse_identifier() {
      Some(identifier) => Some(Expression::Identifier(identifier)),
      None => None,
    }
  }

  fn parse_number_expression(&mut self) -> Option<Expression> {
    match self.peek().kind {
      TokenKind::Number => {
        match Parser::parse_number(self.peek().slice) {
          Ok(number) => Some(Expression::Number(number)),
          Err(err) => {
            self.errors.push(ParseError::new(
              ParseErrorKind::UnexpectedToken,
              format!("{}", err),
            ));
            None
          }
        }
      },
      _ => None,
    }
  }

  fn parse_string_expression(&mut self) -> Option<Expression> {
    match self.peek().kind {
      TokenKind::String => {
        Some(Expression::String(self.peek().slice))
      },
      _ => None,
    }
  }

  fn parse_boolean_expression(&mut self) -> Option<Expression> {
    match self.peek().kind {
      TokenKind::Boolean => Some(Expression::Boolean(self.peek().slice == "true")),
      _ => None,
    }
  }

  fn parse_null_expression(&mut self) -> Option<Expression> {
    match self.peek().kind {
      TokenKind::Null => Some(Expression::Null),
      _ => None,
    }
  }

  fn parse_let_statement(&mut self) -> Option<Statement> {
    match self.next_peek().kind {
      TokenKind::Identifier => {
        self.advance();
      },
      _ => return None,
    };

    let name = match self.parse_identifier() {
      Some(name) => name,
      None => return None,
    };

    if !self.expect_next_token_kind(TokenKind::Equal) {
      return None;
    }

    self.advance();

    let expr = match self.parse_expression(Precedence::None) {
      Some(expr) => expr,
      None => return None,
    };

    if self.next_peek().kind == TokenKind::Semicolon {
      self.advance();
    }

    Some(Statement::Let(name, expr))
  }

  fn parse_binary_operation_expression(&mut self, left: Option<Expression>) -> Option<Expression> {
    let left = match left {
      Some(left) => left,
      None => return None,
    };

    let binary_operator = match self.peek().kind {
      TokenKind::Plus => BinaryOperator::Add,
      TokenKind::Minus => BinaryOperator::Subtract,
      TokenKind::Star => BinaryOperator::Multiply,
      TokenKind::Slash => BinaryOperator::Divide,
      TokenKind::EqualEqual => BinaryOperator::Equal,
      TokenKind::BangEqual => BinaryOperator::NotEqual,
      TokenKind::Less => BinaryOperator::LessThan,
      TokenKind::LessEqual => BinaryOperator::LessThanOrEqual,
      TokenKind::Greater => BinaryOperator::GreaterThan,
      TokenKind::GreaterEqual => BinaryOperator::GreaterThanOrEqual,
      _ => return None,
    };

    let precedence = Precedence::from(self.peek());

    self.advance();

    match self.parse_expression(precedence) {
      Some(right) => {
        Some(Expression::Binary(Box::new(left), binary_operator, Box::new(right)))
      },
      None => None,
    }
  }

  fn parse_unary_operation_expression(&mut self) -> Option<Expression> {
    let unary_operator = match self.peek().kind {
      TokenKind::Plus => UnaryOperator::Positive,
      TokenKind::Minus => UnaryOperator::Negative,
      TokenKind::Bang => UnaryOperator::Not,
      _ => return None,
    };

    self.advance();

    match self.parse_expression(Precedence::Unary) {
      Some(right) => {
        Some(Expression::Unary(unary_operator, Box::new(right)))
      },
      None => None,
    }
  }

  fn parse_grouped_expression(&mut self) -> Option<Expression> {
    self.advance();

    let expr = match self.parse_expression(Precedence::None) {
      Some(expr) => expr,
      None => return None,
    };

    if !self.expect_next_token_kind(TokenKind::RightParen) {
      return None;
    }

    Some(expr)
  }

  fn parse_expression_items(&mut self, end_token_kind: TokenKind) -> Option<Vec<Expression>> {
    let mut items = Vec::new();

    if self.next_peek().kind == end_token_kind {
      self.advance();
      return Some(items);
    }

    self.advance();

    match self.parse_expression(Precedence::None) {
      Some(expr) => {
        items.push(expr);
      },
      None => return None,
    };

    while self.next_peek().kind == TokenKind::Comma {
      self.advance();
      self.advance();

      match self.parse_expression(Precedence::None) {
        Some(expr) => {
          items.push(expr);
        },
        None => return None,
      };
    }

    if !self.expect_next_token_kind(end_token_kind) {
      return None;
    }

    Some(items)
  }

  fn parse_call_expression(&mut self, callee: Option<Expression>) -> Option<Expression> {
    let callee = match callee {
      Some(callee) => callee,
      None => return None,
    };

    let arguments = match self.parse_expression_items(TokenKind::RightParen) {
      Some(arguments) => arguments,
      None => return None,
    };

    Some(Expression::Call(Box::new(callee), arguments))
  }

  fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
    let mut left = match self.peek().kind {
      TokenKind::Null => self.parse_null_expression(),
      TokenKind::Identifier => self.parse_identifier_expression(),
      TokenKind::Number => self.parse_number_expression(),
      TokenKind::String => self.parse_string_expression(),
      TokenKind::Boolean => self.parse_boolean_expression(),
      TokenKind::Bang | TokenKind::Plus | TokenKind::Minus => self.parse_unary_operation_expression(),
      TokenKind::LeftParen => self.parse_grouped_expression(),
      _ => return None,
    };

    while self.next_peek().kind != TokenKind::Semicolon && precedence < Precedence::from(self.next_peek()) {
      match self.next_peek().kind {
        TokenKind::Plus |
        TokenKind::Minus |
        TokenKind::Slash |
        TokenKind::Star |
        TokenKind::EqualEqual |
        TokenKind::BangEqual |
        TokenKind::Less |
        TokenKind::LessEqual |
        TokenKind::Greater |
        TokenKind::GreaterEqual => {
          self.advance();
          left = self.parse_binary_operation_expression(left);
        },
        TokenKind::LeftParen => {
          self.advance();
          left = self.parse_call_expression(left);
        },
        _ => return left,
      }
    }

    left
  }

  fn parse_expression_statement(&mut self) -> Option<Statement> {
    match self.parse_expression(Precedence::None) {
      Some(expr) => {
        if self.next_peek().kind == TokenKind::Semicolon {
          self.advance();
        }

        Some(Statement::Expression(expr))
      },
      None => None,
    }
  }

  fn parse_assignment_statement(&mut self) -> Option<Statement> {
    let name = match self.parse_identifier() {
      Some(name) => name,
      None => return None,
    };

    self.advance();

    let assign_operator = match self.peek().kind {
      TokenKind::Equal => AssignOperator::Assign,
      TokenKind::PlusEqual => AssignOperator::AddAssign,
      TokenKind::MinusEqual => AssignOperator::SubtractAssign,
      TokenKind::StarEqual => AssignOperator::MultiplyAssign,
      TokenKind::SlashEqual => AssignOperator::DivideAssign,
      _ => return None,
    };

    self.advance();

    let right = match self.parse_expression(Precedence::None) {
      Some(expr) => expr,
      None => return None,
    };

    Some(Statement::Assign(name, assign_operator, right))
  }

  fn parse_statement(&mut self) -> Option<Statement> {
    match self.peek().kind {
      TokenKind::Let => self.parse_let_statement(),
      TokenKind::Identifier => {
        match self.next_peek().kind {
          TokenKind::Equal |
          TokenKind::PlusEqual |
          TokenKind::MinusEqual |
          TokenKind::StarEqual |
          TokenKind::SlashEqual => {
            self.parse_assignment_statement()
          },
          _ => self.parse_expression_statement(),
        }
      }
      _ => self.parse_expression_statement(),
    }
  }

  pub fn parse(&mut self) -> Vec<Statement> {
    let mut statements = Vec::new();
    while self.peek().kind != TokenKind::Eof {
      match self.parse_statement() {
        Some(stmt) => {
          statements.push(stmt);
        },
        None => {}
      }

      self.advance();
    }

    statements
  }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
  None,
  Equality, // ==, !=
  Comparison, // >, >=, <, <=
  Term, // +, -
  Factor, // *, /
  Unary, // !x, -x
  Call,
}

impl Precedence {
  fn from(token: Token) -> Precedence {
    match token.kind {
      TokenKind::EqualEqual | TokenKind::BangEqual => Precedence::Equality,
      TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => Precedence::Comparison,
      TokenKind::Plus | TokenKind::Minus => Precedence::Term,
      TokenKind::Star | TokenKind::Slash => Precedence::Factor,
      TokenKind::LeftParen => Precedence::Call,
      _ => Precedence::None,
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::ast::{BinaryOperator, Expression, Statement, UnaryOperator};
  use crate::lexer::Lexer;
  use crate::parser::Parser;

  #[test]
  fn parse() {
    let tests = vec![
      (
        "a + b + c",
        Statement::Expression(
          Expression::Binary(
            Box::new(Expression::Binary(
              Box::new(Expression::Identifier("a".to_string())),
              BinaryOperator::Add,
              Box::new(Expression::Identifier("b".to_string())),
            )),
            BinaryOperator::Add,
            Box::new(Expression::Identifier("c".to_string())),
          ),
        ),
      ),
      (
        "-a",
        Statement::Expression(
          Expression::Unary(
            UnaryOperator::Negative,
            Box::new(Expression::Identifier("a".to_string())),
          ),
        ),
      ),
      (
        "+a",
        Statement::Expression(
          Expression::Unary(
            UnaryOperator::Positive,
            Box::new(Expression::Identifier("a".to_string())),
          ),
        ),
      ),
      (
        "(10 + 20) / 30",
        Statement::Expression(
          Expression::Binary(
            Box::new(
              Expression::Binary(
                Box::new(Expression::Number(10.0)),
                BinaryOperator::Add,
                Box::new(Expression::Number(20.0)),
              ),
            ),
            BinaryOperator::Divide,
            Box::new(Expression::Number(30.0)),
          )
        ),
      )
    ];

    for (input, expect) in tests {
      let mut program = Parser::new(Lexer::new(input).lex()).parse();

      assert_eq!(vec![expect], program);
    }
  }
}