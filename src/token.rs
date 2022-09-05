#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
  Illegal,
  Comment,

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
  Percent,
  PercentEqual,

  Identifier,
  String,
  Number,
  Boolean,
  Null,

  Let,
  For,
  Break,
  Continue,
  Return,

  Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
  pub kind: TokenKind,
  pub slice: &'a str,
  pub span: Span,
}

impl <'a> Token<'a> {
  pub fn new(kind: TokenKind, slice: &'a str, span: Span) -> Token {
    Token {
      kind,
      slice,
      span,
    }
  }
}