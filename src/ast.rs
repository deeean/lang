#[derive(Debug)]
pub enum Statement {
    VariableDeclaration {
        name: String,
        kind: Kind,
        value: Expression,
    },
    Assignment {
        name: String,
        value: Expression,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<Param>,
        return_kind: Kind,
        body: Vec<Statement>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    If {
        condition: Expression,
        body: Vec<Statement>,
        else_ifs: Vec<(Expression, Vec<Statement>)>,
        else_body: Option<Vec<Statement>>,
    },
    Expression(Expression),
    Return(Expression),
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub kind: Kind,
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Literal(Literal),
    Variable(String),
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum Literal {
    I32(i32),
    I64(i64),
    String(String),
    Boolean(bool),
}

#[derive(Clone, Debug)]
pub enum Kind {
    I32,
    I64,
    String,
    Boolean,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Negate,
    Not,
}
