use core::num;

use crate::{
    expr::{Expr, InfixOp, PrefixOp, SpannedExpr},
    lexer::{Lexer, Span, Token, TokenKind},
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    ParseInt(#[from] num::ParseIntError),

    #[error("{0}: expected '{1}' but got '{2}'")]
    UnexpectedToken(Span, String, TokenKind),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    lookahead_token: Option<Token>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        Parser {
            lexer,
            current_token: token,
            lookahead_token: None,
        }
    }

    pub fn parse(&mut self) -> Result<SpannedExpr> {
        self.parse_expr(0)
    }

    fn advance(&mut self) {
        if let Some(token) = self.lookahead_token.take() {
            self.current_token = token;
        } else {
            self.current_token = self.lexer.next_token();
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.current_token.kind != kind {
            return Err(Error::UnexpectedToken(
                self.current_token.span,
                kind.to_string(),
                self.current_token.kind.clone(),
            ));
        }
        self.advance();
        Ok(())
    }

    fn peek(&mut self) -> &Token {
        if self.lookahead_token.is_none() {
            self.lookahead_token = Some(self.lexer.next_token());
        }
        self.lookahead_token.as_ref().unwrap()
    }

    fn parse_atom(&mut self) -> Result<SpannedExpr> {
        match &self.current_token.kind {
            TokenKind::Boolean(bool) => {
                let val = match bool.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                let span = self.current_token.span;
                self.advance();
                Ok(SpannedExpr {
                    value: Expr::Boolean(val),
                    span,
                })
            }

            TokenKind::Integer(int) => {
                let val = int.parse::<i64>()?;
                let span = self.current_token.span;
                self.advance();
                Ok(SpannedExpr {
                    value: Expr::Integer(val),
                    span,
                })
            }

            TokenKind::Identifier(id) => {
                let id = id.clone();
                let span = self.current_token.span;
                self.advance();
                Ok(SpannedExpr {
                    value: Expr::Identifier(id),
                    span,
                })
            }

            TokenKind::If => {
                let mut span = self.current_token.span;
                self.advance();
                let cond = self.parse_expr(0)?;
                self.expect(TokenKind::Then)?;
                let then = self.parse_expr(0)?;
                self.expect(TokenKind::Else)?;
                let alt = self.parse_expr(0)?;
                span.merge(&alt.span);
                Ok(SpannedExpr {
                    value: Expr::Condition {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        alt: Box::new(alt),
                    },
                    span,
                })
            }

            TokenKind::ParenLeft => {
                self.advance();
                let expr = self.parse_expr(0)?;
                self.expect(TokenKind::ParenRight)?;
                Ok(expr)
            }

            _ => Err(Error::UnexpectedToken(
                self.current_token.span,
                "expression".to_string(),
                self.current_token.kind.clone(),
            )),
        }
    }

    fn parse_prefix(&mut self) -> Result<SpannedExpr> {
        if let Some(op) = PrefixOp::from_token(&self.current_token.kind) {
            match op {
                PrefixOp::Neg => {
                    let mut span = self.current_token.span;
                    self.advance();
                    let (_, r_bp) = op.precedence();
                    let rhs = self.parse_expr(r_bp)?;
                    span.merge(&rhs.span);
                    Ok(SpannedExpr {
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
                self.current_token.span,
                "prefix operator".to_string(),
                self.current_token.kind.clone(),
            ))
        }
    }

    fn parse_infix(&mut self, lhs: &SpannedExpr, min_bp: u8) -> Result<SpannedExpr> {
        let mut lhs = lhs.clone();

        while let Some(op) = InfixOp::from_token(&self.current_token.kind) {
            let (l_bp, r_bp) = op.precedence();
            if l_bp < min_bp {
                break;
            }

            self.advance();

            let mut span = lhs.span;
            let rhs = self.parse_expr(r_bp)?;
            span.merge(&rhs.span);

            lhs = SpannedExpr {
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

    fn parse_expr(&mut self, min_bp: u8) -> Result<SpannedExpr> {
        let mut expr = if self.is_prefix() {
            self.parse_prefix()?
        } else {
            let mut expr = self.parse_atom()?;
            while self.can_apply() {
                let arg = self.parse_atom()?;
                let mut span = expr.span;
                span.merge(&arg.span);
                expr = SpannedExpr {
                    value: Expr::App {
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
        matches!(self.current_token.kind, TokenKind::Hyphen)
    }

    fn is_infix(&self) -> bool {
        matches!(
            self.current_token.kind,
            TokenKind::Plus
                | TokenKind::Hyphen
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::HyphenGreaterThan
        )
    }

    fn can_apply(&self) -> bool {
        matches!(
            self.current_token.kind,
            TokenKind::Boolean(_)
                | TokenKind::Integer(_)
                | TokenKind::Identifier(_)
                | TokenKind::ParenLeft
                | TokenKind::If
        )
    }
}
