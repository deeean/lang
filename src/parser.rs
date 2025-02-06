use chumsky::error::Simple;
use chumsky::{select, Parser};
use chumsky::prelude::{choice, end, just, recursive};
use crate::ast::{BinaryOperator, Expression, Type, Param, Statement, UnaryOperator, Literal};
use crate::lexer::Token;

pub fn parser() -> impl Parser<Token, Vec<Statement>, Error = Simple<Token>> {

    let statement = recursive(|statement| {
        let expression = recursive(|expression| {
            let primary = recursive(|_| {
                let literal = select! {
                    Token::True => Expression::Literal(Literal::Boolean(true)),
                    Token::False => Expression::Literal(Literal::Boolean(false)),
                    Token::NumberLiteral(value) => Expression::Literal(Literal::Number(None, value)),
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
                        just(Token::Percent).to(BinaryOperator::Modulo),
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
            .then(
                just(Token::Colon)
                    .ignore_then(type_parser())
                    .or_not()
            )
            .then_ignore(just(Token::Equal))
            .then(expression.clone())
            .then_ignore(just(Token::Semicolon))
            .map(|((name, r#type), value)| Statement::VariableDeclaration { name, r#type, value });

        let function_parameter = select! { Token::Identifier(name) => name }
            .then_ignore(just(Token::Colon))
            .then(type_parser())
            .map(|(name, kind)| Param { name, r#type: kind });

        let function_declaration = just(Token::Fn)
            .ignore_then(select! { Token::Identifier(name) => name })
            .then(
                function_parameter
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            )
            .then_ignore(just(Token::RightArrow))
            .then(type_parser())
            .then(
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            )
            .map(|(((name, parameters), return_kind), body)| Statement::FunctionDeclaration { name, parameters, return_type: return_kind, body });

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
            )
            .then(
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            )
            .map(|(condition, body)| Statement::While { condition, body });

        let else_if = just(Token::Else)
            .ignore_then(just(Token::If))
            .ignore_then(
                expression
                    .clone()
            )
            .then(
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            );

        let else_branch = just(Token::Else)
            .ignore_then(
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            )
            .or_not();

        let if_statement = just(Token::If)
            .ignore_then(
                expression
                    .clone()
            )
            .then(
                statement
                    .clone()
                    .repeated()
                    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
            )
            .then(else_if.repeated())
            .then(else_branch)
            .map(|(((if_condition, if_body), else_ifs), else_branch)| {
                Statement::If {
                    condition: if_condition,
                    body: if_body,
                    else_ifs,
                    else_body: else_branch,
                }
            });

        let break_statement = just(Token::Break)
            .then_ignore(just(Token::Semicolon))
            .map(|_| Statement::Break);

        let continue_statement = just(Token::Continue)
            .then_ignore(just(Token::Semicolon))
            .map(|_| Statement::Continue);

        variable_declaration
            .or(function_declaration)
            .or(if_statement)
            .or(expr_statement)
            .or(return_statement)
            .or(assignment)
            .or(while_statement)
            .or(break_statement)
            .or(continue_statement)
    });

    statement
        .repeated()
        .then_ignore(end())
}

fn type_parser() -> impl Parser<Token, Type, Error = Simple<Token>> {
    choice((
        just(Token::U8).to(Type::U8),
        just(Token::U16).to(Type::U16),
        just(Token::U32).to(Type::U32),
        just(Token::U64).to(Type::U64),
        just(Token::U128).to(Type::U128),
        just(Token::USize).to(Type::USize),
        just(Token::I8).to(Type::I8),
        just(Token::I16).to(Type::I16),
        just(Token::I32).to(Type::I32),
        just(Token::I64).to(Type::I64),
        just(Token::I128).to(Type::I128),
        just(Token::ISize).to(Type::ISize),
        just(Token::F32).to(Type::F32),
        just(Token::F64).to(Type::F64),
        just(Token::Void).to(Type::Void),
        just(Token::Boolean).to(Type::Boolean),
        just(Token::String).to(Type::String),
    ))
}
