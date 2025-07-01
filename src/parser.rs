use core::num;

use crate::{
    ast::{Expr, InfixOp, PrefixOp},
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

    pub fn parse(&mut self) -> Result<Spanned<Expr>> {
        self.parse_expr(0)
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
                Ok(Spanned::<Expr> {
                    value: Expr::Boolean(val),
                    span,
                })
            }

            Token::Integer(int) => {
                let val = int.parse::<i64>()?;
                let span = self.token.span;
                self.advance();
                Ok(Spanned::<Expr> {
                    value: Expr::Integer(val),
                    span,
                })
            }

            Token::Identifier(id) => {
                let id = id.clone();
                let span = self.token.span;
                self.advance();
                Ok(Spanned::<Expr> {
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
                Ok(Spanned::<Expr> {
                    value: Expr::Condition {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        alt: Box::new(alt),
                    },
                    span,
                })
            }

            Token::ParenLeft => {
                self.advance();
                let expr = self.parse_expr(0)?;
                self.expect(Token::ParenRight)?;
                Ok(expr)
            }

            _ => Err(Error::UnexpectedToken(
                self.token.span,
                "expression".to_string(),
                self.token.value.clone(),
            )),
        }
    }

    fn parse_prefix(&mut self) -> Result<Spanned<Expr>> {
        if let Some(op) = PrefixOp::from_token(&self.token.value) {
            match op {
                PrefixOp::Neg => {
                    let mut span = self.token.span;
                    self.advance();
                    let (_, r_bp) = op.precedence();
                    let rhs = self.parse_expr(r_bp)?;
                    span.merge(&rhs.span);
                    Ok(Spanned::<Expr> {
                        value: Expr::Prefix {
                            op,
                            rhs: Box::new(rhs),
                        },
                        span,
                    })
                }
            }
        } else {
            Err(Error::UnexpectedToken(
                self.token.span,
                "prefix operator".to_string(),
                self.token.value.clone(),
            ))
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

            let mut span = lhs.span;
            let rhs = self.parse_expr(r_bp)?;
            span.merge(&rhs.span);

            lhs = Spanned::<Expr> {
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
        let mut expr = if self.is_prefix() {
            self.parse_prefix()?
        } else {
            let mut expr = self.parse_atom()?;
            while self.can_apply() {
                let arg = self.parse_atom()?;
                let mut span = expr.span;
                span.merge(&arg.span);
                expr = Spanned::<Expr> {
                    value: Expr::Application {
                        func: Box::new(expr),
                        arg: Box::new(arg),
                    },
                    span,
                };
            }
            expr
        };

        if self.is_infix() {
            expr = self.parse_infix(&expr, min_bp)?
        }

        Ok(expr)
    }

    fn is_prefix(&self) -> bool {
        matches!(self.token.value, Token::Hyphen)
    }

    fn is_infix(&self) -> bool {
        matches!(
            self.token.value,
            Token::Plus
                | Token::Hyphen
                | Token::Star
                | Token::Slash
                | Token::EqualEqual
                | Token::HyphenGreaterThan
        )
    }

    fn can_apply(&self) -> bool {
        matches!(
            self.token.value,
            Token::Boolean(_)
                | Token::Integer(_)
                | Token::Identifier(_)
                | Token::ParenLeft
                | Token::If
        )
    }
}
