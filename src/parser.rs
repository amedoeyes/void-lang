use core::num;

use crate::{
    ast::{Expr, InfixOp, PrefixOp, Stmt},
    lexer::{Lexer, Token},
    position::{Span, Spanned},
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    ParseInt(#[from] num::ParseIntError),

    #[error("{0}: expected '{1}' but got '{2}'")]
    UnexpectedToken(Span, String, Token),
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
        let mut prog = vec![];

        while self.token.value != Token::Eof {
            match self.token.value {
                Token::Let => {
                    let mut span = self.token.span;
                    self.advance();
                    let Token::Identifier(id) = self.token.value.clone() else {
                        return Err(Error::UnexpectedToken(
                            self.token.span,
                            "Identifier".to_string(),
                            self.token.value.clone(),
                        ));
                    };
                    self.advance();
                    self.expect(Token::Equal)?;
                    let expr = self.parse_expr(0)?;
                    self.expect(Token::Semicolon)?;
                    span.merge(&self.token.span);
                    prog.push(Spanned {
                        value: Stmt::Let { name: id, expr },
                        span,
                    });
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    let span = expr.span;
                    prog.push(Spanned {
                        value: Stmt::Expr(expr),
                        span,
                    });
                }
            }
        }
        Ok(prog)
    }

    fn advance(&mut self) {
        self.token = self.lexer.next_token();
    }

    fn expect(&mut self, token: Token) -> Result<()> {
        if self.token.value != token {
            return Err(Error::UnexpectedToken(
                self.token.span,
                token.to_string(),
                self.token.value.clone(),
            ));
        }
        self.advance();
        Ok(())
    }

    fn expect_optional(&mut self, token: Token) -> bool {
        let val = self.token.value == token;
        if val {
            self.advance();
        }
        val
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
                Ok(Spanned {
                    value: Expr::Boolean(val),
                    span,
                })
            }

            Token::Integer(int) => {
                let val = int.parse::<i64>()?;
                let span = self.token.span;
                self.advance();
                Ok(Spanned {
                    value: Expr::Integer(val),
                    span,
                })
            }

            Token::Identifier(id) => {
                let id = id.clone();
                let span = self.token.span;
                self.advance();
                Ok(Spanned {
                    value: Expr::Identifier(id),
                    span,
                })
            }

            Token::If => {
                let mut span = self.token.span;
                self.advance();
                let cond = self.parse_expr(0)?;
                self.expect(Token::Then)?;
                let then = self.parse_expr(0)?;
                self.expect(Token::Else)?;
                let alt = self.parse_expr(0)?;
                span.merge(&alt.span);
                Ok(Spanned {
                    value: Expr::Condition {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        alt: Box::new(alt),
                    },
                    span,
                })
            }

            Token::ParenLeft => {
                let mut span = self.token.span;
                self.advance();
                span.merge(&self.token.span);
                if self.expect_optional(Token::ParenRight) {
                    Ok(Spanned {
                        value: Expr::Unit,
                        span,
                    })
                } else {
                    let expr = self.parse_expr(0)?;
                    self.expect(Token::ParenRight)?;
                    span.merge(&expr.span);
                    Ok(Spanned {
                        value: expr.value,
                        span,
                    })
                }
            }

            _ => Err(Error::UnexpectedToken(
                self.token.span,
                "expression".to_string(),
                self.token.value.clone(),
            )),
        }
    }

    fn parse_prefix(&mut self) -> Result<Spanned<Expr>> {
        let mut expr = Spanned {
            value: Expr::Integer(0),
            span: self.token.span,
        };

        while let Some(op) = PrefixOp::from_token(&self.token.value) {
            match op {
                PrefixOp::Neg => {
                    let mut span = self.token.span;
                    self.advance();
                    let (_, r_bp) = op.precedence();
                    let rhs = self.parse_expr(r_bp)?;
                    span.merge(&rhs.span);
                    expr = Spanned {
                        value: Expr::Prefix {
                            op,
                            rhs: Box::new(rhs),
                        },
                        span,
                    };
                }
            };
        }

        Ok(expr)
    }

    fn parse_infix(&mut self, lhs: &Spanned<Expr>, min_bp: u8) -> Result<Spanned<Expr>> {
        let mut lhs = lhs.clone();

        while let Some(op) = InfixOp::from_token(&self.token.value) {
            let (l_bp, r_bp) = op.precedence();
            if l_bp < min_bp {
                break;
            }

            self.advance();

            let mut span = lhs.span;
            let rhs = self.parse_expr(r_bp)?;
            span.merge(&rhs.span);

            lhs = Spanned {
                value: Expr::Infix {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                },
                span,
            };
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Spanned<Expr>> {
        let mut expr = if matches!(self.token.value, Token::Hyphen) {
            self.parse_prefix()?
        } else {
            self.parse_atom()?
        };

        if self.expect_optional(Token::HyphenGreaterThan) {
            if let Expr::Identifier(id) = expr.value {
                let body = self.parse_expr(0)?;
                let span = *expr.span.merge(&body.span);
                expr = Spanned {
                    value: Expr::Lambda {
                        param: id.clone(),
                        body: Box::new(body),
                    },
                    span,
                };
            } else {
                return Err(Error::UnexpectedToken(
                    self.token.span,
                    "identifier".to_string(),
                    self.token.value.clone(),
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
            let span = *expr.span.merge(&arg.span);
            expr = Spanned {
                value: Expr::Application {
                    func: Box::new(expr),
                    arg: Box::new(arg),
                },
                span,
            };
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
