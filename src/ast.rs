pub type Program = Vec<Statement>;

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration {
        name: String,
        r#type: Option<Type>,
        value: Expression,
    },
    Assignment {
        name: String,
        value: Expression,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<Param>,
        return_type: Type,
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
    Break,
    Continue,
    Expression(Expression),
    Return(Expression),
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub r#type: Type,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Literal {
    Number(Option<Type>, String),
    String(String),
    Boolean(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    F32,
    F64,
    Void,
    String,
    Boolean,
    Unknown,
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
