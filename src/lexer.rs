use crate::token::{TokenKind, Span, Token};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
  input: &'a str,
  curr: usize,
  next: usize,
}

impl <'a> Lexer<'a> {
  pub fn new(input: &str) -> Lexer<'_> {
    Lexer {
      input,
      curr: 0,
      next: 1,
    }
  }

  fn is_ended(&self) -> bool {
    self.curr >= self.input.len()
  }

  fn advance(&mut self) -> u8 {
    if self.is_ended() {
      return 0;
    }

    let char = self.input.as_bytes()[self.curr];
    self.curr = self.next;
    self.next += 1;
    char
  }

  fn skip_whitespace(&mut self) {
    loop {
      match self.peek() {
        b' ' | b'\r' | b'\t' | b'\n' => {
          self.advance();
        },
        _ => break,
      }
    };
  }

  fn peek(&mut self) -> u8 {
    if self.is_ended() {
      0
    } else {
      self.input.as_bytes()[self.curr]
    }
  }

  fn next_peek(&mut self) -> u8 {
    if self.next >= self.input.len() {
      0
    } else {
      self.input.as_bytes()[self.next]
    }
  }

  fn next_token(&mut self) -> Token {
    self.skip_whitespace();

    let start = self.curr;

    let kind = match self.peek() {
      b'(' => TokenKind::LeftParen,
      b')' => TokenKind::RightParen,
      b'{' => TokenKind::LeftBrace,
      b'}' => TokenKind::RightBrace,
      b';' => TokenKind::Semicolon,
      b',' => TokenKind::Comma,
      b'!' => {
        match self.next_peek() {
          b'=' => {
            self.advance();
            TokenKind::BangEqual
          },
          _ => TokenKind::Bang,
        }
      }
      b'=' => {
        match self.next_peek() {
          b'=' => {
            self.advance();
            TokenKind::EqualEqual
          },
          _ => TokenKind::Equal,
        }
      }
      b'>' => {
        match self.next_peek() {
          b'=' => {
            self.advance();
            TokenKind::GreaterEqual
          },
          _ => TokenKind::Greater,
        }
      }
      b'<' => {
        match self.next_peek() {
          b'=' => {
            self.advance();
            TokenKind::LessEqual
          },
          _ => TokenKind::Less,
        }
      }
      b'+' => {
        match self.next_peek() {
          b'=' => {
            self.advance();
            TokenKind::PlusEqual
          },
          _ => TokenKind::Plus,
        }
      },
      b'-' => {
        match self.next_peek() {
          b'=' => {
            self.advance();
            TokenKind::MinusEqual
          },
          _ => TokenKind::Minus,
        }
      },
      b'*' => {
        match self.next_peek() {
          b'=' => {
            self.advance();
            TokenKind::StarEqual
          },
          _ => TokenKind::Star,
        }
      },
      b'/' => {
        match self.next_peek() {
          b'/' => {
            return self.comment();
          }
          b'=' => {
            self.advance();
            TokenKind::SlashEqual
          },
          _ => TokenKind::Slash,
        }
      },
      b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
        return self.identifier();
      }
      b'"' => {
        return self.string();
      }
      b'0'..=b'9' => {
        return self.number();
      }
      0 => return Token::new(TokenKind::Eof, "".to_owned(), Span(self.next, self.next)),
      _ => TokenKind::Illegal,
    };

    let slice = &self.input[start..self.next];
    let span = Span(start, self.next);

    self.advance();

    Token::new(kind, slice.to_owned(), span)
  }

  fn number(&mut self) -> Token {
    let start = self.curr;

    loop {
      match self.peek() {
        b'0'..=b'9' | b'_' => {
          self.advance();
        }
        _ => {
          break;
        }
      }
    }

    if self.peek() == b'.' {
      self.advance();

      loop {
        match self.peek() {
          b'0'..=b'9' | b'_' => {
            self.advance();
          }
          _ => {
            break;
          }
        }
      }
    }

    let slice = &self.input[start..self.curr];

    Token::new(TokenKind::Number, slice.to_owned(), Span(start, self.curr))
  }

  fn string(&mut self) -> Token {
    self.advance();

    let start = self.curr;

    loop {
      match self.peek() {
        b'"' | 0 => {
          let slice = &self.input[start..self.curr];
          self.advance();
          return Token::new(TokenKind::String, slice.to_owned(), Span(start, self.curr));
        }
        b'\\' => {
          if self.next_peek() == b'\"' {
            self.advance();
            self.advance();
          }
        }
        _ => {
          self.advance();
        }
      }
    }
  }

  fn comment(&mut self) -> Token {
    let start = self.curr;

    loop {
      match self.peek() {
        b'\n' | 0 => {
          self.advance();
          break;
        }
        _ => {
          self.advance();
        }
      }
    };

    Token::new(TokenKind::Comment, self.input[start..self.curr].to_owned(), Span(start, self.curr))
  }

  fn identifier(&mut self) -> Token {
    let start = self.curr;

    loop {
      match self.peek() {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9' => {
          self.advance();
        }
        _ => break,
      }
    };

    let slice = &self.input[start..self.curr];
    let kind = match slice {
      "let" => TokenKind::Let,
      "true" | "false" => TokenKind::Boolean,
      "null" => TokenKind::Null,
      "for" => TokenKind::For,
      "break" => TokenKind::Break,
      "continue" => TokenKind::Continue,
      _ => TokenKind::Identifier,
    };

    Token::new(kind, slice.to_owned(), Span(start, self.curr))
  }

  pub fn lex(&mut self) -> Vec<Token> {
    let mut tokens = Vec::new();

    loop {
      let token = self.next_token();
      let is_eof = token.kind == TokenKind::Eof;
      tokens.push(token);

      if is_eof {
        break;
      }
    }

    tokens
  }
}

#[cfg(test)]
mod tests {
  use crate::lexer::Lexer;
  use crate::token::TokenKind;

  #[test]
  fn lex() {
    assert_eq!(Lexer::new(r#"let x = 0.1 + 100_000_000;"#).lex().iter().map(|t| t.kind).collect::<Vec<_>>(), vec![
      TokenKind::Let,
      TokenKind::Identifier,
      TokenKind::Equal,
      TokenKind::Number,
      TokenKind::Plus,
      TokenKind::Number,
      TokenKind::Semicolon,
      TokenKind::Eof,
    ]);

    assert_eq!(Lexer::new(r#"let res = "Hello, world";"#).lex().iter().map(|t| t.kind).collect::<Vec<_>>(), vec![
      TokenKind::Let,
      TokenKind::Identifier,
      TokenKind::Equal,
      TokenKind::String,
      TokenKind::Semicolon,
      TokenKind::Eof,
    ]);
  }
}