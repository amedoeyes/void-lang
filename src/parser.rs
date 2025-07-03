use crate::{
    ast::{Expr, InfixOp, PrefixOp, Stmt},
    lexer::{Lexer, Token},
    span::Spanned,
};

#[derive(Debug)]
pub enum Error {
    UnexpectedToken(String, Spanned<Token>),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    token: Spanned<Token>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        Parser { lexer, token }
    }

    fn parse(&mut self) -> Result<Vec<Spanned<Stmt>>> {
        let mut ast = vec![];

        while self.token.value != Token::Eof {
            match self.token.value {
                Token::Let => {
                    let span = self.token.span;

                    self.advance();
                    let Token::Identifier(id) = self.token.value.clone() else {
                        return Err(Error::UnexpectedToken(
                            "Identifier".to_string(),
                            self.token.clone(),
                        ));
                    };

                    self.advance();
                    self.expect(Token::Equal)?;

                    let expr = self.parse_expr(0)?;
                    self.expect(Token::Semicolon)?;

                    ast.push(Spanned::new(
                        Stmt::Let { name: id, expr },
                        span.merge(&self.token.span),
                    ));
                }

                _ => {
                    let expr = self.parse_expr(0)?;
                    let span = expr.span;
                    ast.push(Spanned::new(Stmt::Expr(expr), span));
                }
            }
        }

        Ok(ast)
    }

    fn advance(&mut self) {
        self.token = self.lexer.next_token();
    }

    fn expect(&mut self, token: Token) -> Result<()> {
        if self.token.value != token {
            return Err(Error::UnexpectedToken(
                token.to_string(),
                self.token.clone(),
            ));
        }
        self.advance();
        Ok(())
    }

    fn parse_atom(&mut self) -> Result<Spanned<Expr>> {
        match &self.token.value {
            Token::Boolean(bool) => {
                let val = match bool.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                let span = self.token.span;
                self.advance();
                Ok(Spanned::new(Expr::Boolean(val), span))
            }

            Token::Integer(int) => {
                let val = int.parse::<i64>().unwrap();
                let span = self.token.span;
                self.advance();
                Ok(Spanned::new(Expr::Integer(val), span))
            }

            Token::Identifier(id) => {
                let id = id.clone();
                let span = self.token.span;
                self.advance();
                Ok(Spanned::new(Expr::Identifier(id.clone()), span))
            }

            Token::If => {
                let span = self.token.span;
                self.advance();

                let cond = self.parse_expr(0)?;
                self.expect(Token::Then)?;
                let then = self.parse_expr(0)?;
                self.expect(Token::Else)?;
                let alt = self.parse_expr(0)?;

                let span = span.merge(&alt.span);
                Ok(Spanned::new(
                    Expr::Condition {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        alt: Box::new(alt),
                    },
                    span,
                ))
            }

            Token::ParenLeft => {
                let span = self.token.span;
                self.advance();
                if self.token.value == Token::ParenRight {
                    let span = span.merge(&self.token.span);
                    self.advance();
                    Ok(Spanned::new(Expr::Unit, span))
                } else {
                    let expr = self.parse_expr(0)?;
                    let span = span.merge(&self.token.span);
                    self.expect(Token::ParenRight)?;
                    Ok(Spanned::new(expr.value, span))
                }
            }

            _ => Err(Error::UnexpectedToken(
                "expression".to_string(),
                self.token.clone(),
            )),
        }
    }

    fn parse_prefix(&mut self) -> Result<Spanned<Expr>> {
        if let Some(op) = PrefixOp::from_token(&self.token.value) {
            match op {
                PrefixOp::Neg => {
                    let span = self.token.span;
                    self.advance();

                    let (_, r_bp) = op.precedence();
                    let rhs = self.parse_expr(r_bp)?;

                    let span = span.merge(&rhs.span);
                    Ok(Spanned::new(
                        Expr::Prefix {
                            op,
                            rhs: Box::new(rhs),
                        },
                        span,
                    ))
                }
            }
        } else {
            unreachable!()
        }
    }

    fn parse_infix(&mut self, lhs: &Spanned<Expr>, min_bp: u8) -> Result<Spanned<Expr>> {
        let mut lhs = lhs.clone();

        while let Some(op) = InfixOp::from_token(&self.token.value) {
            let (l_bp, r_bp) = op.precedence();
            if l_bp < min_bp {
                break;
            }
            self.advance();
            let rhs = self.parse_expr(r_bp)?;
            let span = lhs.span.merge(&rhs.span);
            lhs = Spanned::new(
                Expr::Infix {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                },
                span,
            )
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Spanned<Expr>> {
        let mut expr = if matches!(self.token.value, Token::Hyphen) {
            self.parse_prefix()?
        } else {
            self.parse_atom()?
        };

        if self.token.value == Token::HyphenGreaterThan {
            if let Expr::Identifier(id) = expr.value {
                let span = expr.span;
                self.advance();

                let body = self.parse_expr(0)?;

                let span = span.merge(&body.span);
                expr = Spanned::new(
                    Expr::Lambda {
                        param: Spanned::new(id.clone(), expr.span),
                        body: Box::new(body),
                    },
                    span,
                );
            } else {
                return Err(Error::UnexpectedToken(
                    "identifier".to_string(),
                    self.token.clone(),
                ));
            }
        }

        while matches!(
            self.token.value,
            Token::Boolean(_)
                | Token::Identifier(_)
                | Token::If
                | Token::Integer(_)
                | Token::ParenLeft
        ) {
            let arg = self.parse_atom()?;
            let span = expr.span.merge(&arg.span);
            expr = Spanned::new(
                Expr::Application {
                    func: Box::new(expr),
                    arg: Box::new(arg),
                },
                span,
            );
        }

        if matches!(
            self.token.value,
            Token::EqualEqual
                | Token::Hyphen
                | Token::Percent
                | Token::Plus
                | Token::Slash
                | Token::Star
        ) {
            expr = self.parse_infix(&expr, min_bp)?
        }

        Ok(expr)
    }
}

pub fn parse(input: &str) -> Result<Vec<Spanned<Stmt>>> {
    let mut parser = Parser::new(input);
    parser.parse()
}
