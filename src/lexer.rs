use logos::Logos;

fn unescape_string(input: &str) -> String {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    chars.next();
    chars.next_back();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(escaped) = chars.next() {
                match escaped {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    _ => {
                        result.push('\\');
                        result.push(escaped);
                    }
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}


#[derive(Logos, Hash, Eq, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("=")]
    Equal,

    #[token("!")]
    Bang,

    #[token("%")]
    Percent,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token("<=")]
    LessEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("==")]
    EqualEqual,

    #[token("!=")]
    BangEqual,

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("->")]
    RightArrow,

    #[token("var")]
    Var,

    #[token("fn")]
    Fn,

    #[token("return")]
    Return,

    #[token("i32")]
    I32,

    #[token("i64")]
    I64,

    #[token("f64")]
    F64,

    #[token("void")]
    Void,

    #[token("boolean")]
    Boolean,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("string")]
    String,

    #[token("while")]
    While,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i32>().unwrap())]
    I32Literal(i32),

    #[regex(r"[0-9]+i64", |lex| lex.slice()[..lex.slice().len()-3].parse::<i64>().unwrap())]
    I64Literal(i64),

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().to_owned())]
    F64Literal(String),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| unescape_string(lex.slice()))]
    StringLiteral(String),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),

    #[end]
    Eof,
}
