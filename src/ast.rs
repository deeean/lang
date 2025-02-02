use chumsky::prelude::*;
use crate::lexer::Token;

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

pub fn parser() -> impl Parser<Token, Vec<Statement>, Error = Simple<Token>> {
    let statement = recursive(|statement| {
        let expression = recursive(|expression| {
            let primary = recursive(|_| {
                let literal = select! {
                    Token::True => Expression::Literal(Literal::Boolean(true)),
                    Token::False => Expression::Literal(Literal::Boolean(false)),
                    Token::I32Literal(value) => Expression::Literal(Literal::I32(value)),
                    Token::I64Literal(value) => Expression::Literal(Literal::I64(value)),
                    Token::StringLiteral(value) => Expression::Literal(Literal::String(value)),
                };

                let identifier = select! { Token::Identifier(name) => name };

                let function_call = identifier
                    .then(
                        expression.clone()
                            .separated_by(just(Token::Comma))
                            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                    )
                    .map(|(name, arguments)| Expression::FunctionCall { name, arguments });

                let parens = expression
                    .clone()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen));

                function_call
                    .or(literal)
                    .or(parens)
                    .or(identifier.map(Expression::Variable))
            });

            let unary = just(Token::Minus)
                .to(UnaryOperator::Negate)
                .or(just(Token::Bang).to(UnaryOperator::Not))
                .repeated()
                .then(primary)
                .foldr(|operator, expr| Expression::Unary {
                    operator,
                    operand: Box::new(expr),
                });

            let product = unary.clone()
                .then(
                    choice((
                        just(Token::Star).to(BinaryOperator::Multiply),
                        just(Token::Slash).to(BinaryOperator::Divide),
                    ))
                    .then(unary.clone())
                    .repeated(),
                )
                .foldl(
                    |left, (op, right)| Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                );

            let sum = product.clone()
                .then(
                    choice((
                        just(Token::Plus).to(BinaryOperator::Add),
                        just(Token::Minus).to(BinaryOperator::Subtract),
                    ))
                    .then(product.clone())
                    .repeated(),
                )
                .foldl(
                    |left, (op, right)| Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                );

            let comparison = sum.clone()
                .then(
                    choice((
                        just(Token::Less).to(BinaryOperator::Less),
                        just(Token::LessEqual).to(BinaryOperator::LessEqual),
                        just(Token::Greater).to(BinaryOperator::Greater),
                        just(Token::GreaterEqual).to(BinaryOperator::GreaterEqual),
                    ))
                    .then(sum.clone())
                    .repeated(),
                )
                .foldl(
                    |left, (op, right)| Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                );

            let equality = comparison.clone()
                .then(
                    choice((
                        just(Token::EqualEqual).to(BinaryOperator::Equal),
                        just(Token::BangEqual).to(BinaryOperator::NotEqual),
                    ))
                    .then(comparison.clone())
                    .repeated(),
                )
                .foldl(
                    |left, (op, right)| Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                );

            let logical_and = equality.clone()
                .then(
                    just(Token::And).to(BinaryOperator::And)
                        .then(equality)
                        .repeated(),
                )
                .foldl(
                    |left, (op, right)| Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                );

            let logical_or = logical_and.clone()
                .then(
                    just(Token::Or).to(BinaryOperator::Or)
                        .then(logical_and)
                        .repeated(),
                )
                .foldl(
                    |left, (op, right)| Expression::Binary {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                );

            logical_or
        });

        let variable_declaration = just(Token::Var)
            .ignore_then(select! { Token::Identifier(name) => name })
            .then_ignore(just(Token::Colon))
            .then(kind_parser())
            .then_ignore(just(Token::Equal))
            .then(expression.clone())
            .then_ignore(just(Token::Semicolon))
            .map(|((name, kind), value)| Statement::VariableDeclaration { name, kind, value });

        let function_parameter = select! { Token::Identifier(name) => name }
            .then_ignore(just(Token::Colon))
            .then(kind_parser())
            .map(|(name, kind)| Param { name, kind });

        let function_declaration = just(Token::Fn)
            .ignore_then(select! { Token::Identifier(name) => name })
            .then(
                function_parameter
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            )
            .then_ignore(just(Token::RightArrow))
            .then(kind_parser())
            .then(
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            )
            .map(|(((name, parameters), return_kind), body)| Statement::FunctionDeclaration { name, parameters, return_kind, body });

        let expr_statement = expression.clone()
            .then_ignore(just(Token::Semicolon))
            .map(Statement::Expression);

        let return_statement = just(Token::Return)
            .ignore_then(expression.clone())
            .then_ignore(just(Token::Semicolon))
            .map(Statement::Return);

        let assignment = select! { Token::Identifier(name) => name }
            .then_ignore(just(Token::Equal))
            .then(expression.clone())
            .then_ignore(just(Token::Semicolon))
            .map(|(name, value)| Statement::Assignment { name, value });

        let while_statement = just(Token::While)
            .ignore_then(
                expression
                    .clone()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            )
            .then(
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            )
            .map(|(condition, body)| Statement::While { condition, body });

        variable_declaration
            .or(function_declaration)
            .or(expr_statement)
            .or(return_statement)
            .or(assignment)
            .or(while_statement)
    });

    statement
        .repeated()
        .then_ignore(end())
}

fn kind_parser() -> impl Parser<Token, Kind, Error = Simple<Token>> {
    choice((
        just(Token::I32).to(Kind::I32),
        just(Token::I64).to(Kind::I64),
        just(Token::Boolean).to(Kind::Boolean),
        just(Token::String).to(Kind::String),
    ))
}
