#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,

  Comma,
  Semicolon,

  Equal,
  EqualEqual,
  Bang,
  BangEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,

  Plus,
  PlusEqual,
  Minus,
  MinusEqual,
  Star,
  StarEqual,
  Slash,
  SlashEqual,

  Identifier,
  String,
  Number,
  Boolean,
  Null,

  Let,
  For,
  Break,
  Continue,

  Comment,
  Illegal,
  Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  pub kind: TokenKind,
  pub slice: String,
  pub span: Span,
}

impl Token {
  pub fn new(kind: TokenKind, slice: String, span: Span) -> Token {
    Token {
      kind,
      slice,
      span,
    }
  }
}