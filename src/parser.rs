use crate::{
    error::{Error, Result},
    lexer::{Lexer, Token, TokenKind},
};

#[derive(Debug)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl InfixOp {
    fn from_token(kind: &TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(InfixOp::Add),
            TokenKind::Hyphen => Some(InfixOp::Sub),
            TokenKind::Star => Some(InfixOp::Mul),
            TokenKind::Slash => Some(InfixOp::Div),
            _ => None,
        }
    }

    fn precedence(&self) -> (u8, u8) {
        match self {
            InfixOp::Add | InfixOp::Sub => (1, 2),
            InfixOp::Mul | InfixOp::Div => (3, 4),
        }
    }
}

#[derive(Debug)]
pub enum PrefixOp {
    Neg,
}

impl PrefixOp {
    fn from_token(kind: &TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Hyphen => Some(PrefixOp::Neg),
            _ => None,
        }
    }

    fn precedence(&self) -> (u8, u8) {
        match self {
            PrefixOp::Neg => (0, 5),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Identifier(String),
    Condition {
        cond: Box<Expr>,
        then: Box<Expr>,
        alt: Box<Expr>,
    },
    Infix {
        lhs: Box<Expr>,
        op: InfixOp,
        rhs: Box<Expr>,
    },
    Prefix {
        op: PrefixOp,
        rhs: Box<Expr>,
    },
    Fun {
        param: Box<Expr>,
        body: Box<Expr>,
    },
    App {
        func: Box<Expr>,
        arg: Box<Expr>,
    },
}

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

    pub fn parse(&mut self) -> Result<Expr> {
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
                kind,
                self.current_token.kind.clone(),
                self.current_token.span,
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

    fn parse_atom(&mut self) -> Result<Expr> {
        match &self.current_token.kind {
            TokenKind::Integer(int) => {
                let value = int.parse::<i64>()?;
                self.advance();
                Ok(Expr::Integer(value))
            }

            TokenKind::Identifier(id) => {
                let id = id.clone();
                self.advance();
                if self.current_token.kind == TokenKind::HyphenGreaterThan {
                    self.advance();
                    let expr = self.parse_expr(0)?;
                    Ok(Expr::Fun {
                        param: Box::new(Expr::Identifier(id)),
                        body: Box::new(expr),
                    })
                } else {
                    Ok(Expr::Identifier(id))
                }
            }

            TokenKind::If => {
                self.advance();
                let r#if = self.parse_expr(0)?;
                self.expect(TokenKind::Then)?;
                let then = self.parse_expr(0)?;
                self.expect(TokenKind::Else)?;
                let r#else = self.parse_expr(0)?;
                Ok(Expr::Condition {
                    cond: Box::new(r#if),
                    then: Box::new(then),
                    alt: Box::new(r#else),
                })
            }

            TokenKind::ParenLeft => {
                self.advance();
                let expr = self.parse_expr(0)?;
                self.expect(TokenKind::ParenRight)?;
                Ok(expr)
            }

            _ => todo!("{:?}", self.current_token.kind),
        }
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        if let Some(op) = PrefixOp::from_token(&self.current_token.kind) {
            match &self.current_token.kind {
                TokenKind::Hyphen => {
                    self.advance();
                    let (_, r_bp) = op.precedence();
                    let rhs = self.parse_expr(r_bp)?;
                    Ok(Expr::Prefix {
                        op,
                        rhs: Box::new(rhs),
                    })
                }

                _ => todo!("{:?}", self.current_token.kind),
            }
        } else {
            todo!("{:?}", self.current_token.kind)
        }
    }

    fn parse_infix(&mut self, lhs: Expr, min_bp: u8) -> Result<Expr> {
        let mut lhs = lhs;

        while let Some(op) = InfixOp::from_token(&self.current_token.kind) {
            let (l_bp, r_bp) = op.precedence();
            if l_bp < min_bp {
                break;
            }

            self.advance();

            let rhs = self.parse_expr(r_bp)?;

            lhs = Expr::Infix {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr> {
        let mut lhs = if self.is_prefix() {
            self.parse_prefix()?
        } else {
            let mut expr = self.parse_atom()?;
            while self.can_apply() {
                let arg = self.parse_atom()?;
                expr = Expr::App {
                    func: Box::new(expr),
                    arg: Box::new(arg),
                };
            }
            expr
        };

        if self.is_infix() {
            lhs = self.parse_infix(lhs, min_bp)?
        }

        Ok(lhs)
    }

    fn is_prefix(&self) -> bool {
        matches!(self.current_token.kind, TokenKind::Hyphen)
    }

    fn is_infix(&self) -> bool {
        matches!(
            self.current_token.kind,
            TokenKind::Plus | TokenKind::Hyphen | TokenKind::Star | TokenKind::Slash
        )
    }

    fn can_apply(&self) -> bool {
        matches!(
            self.current_token.kind,
            TokenKind::Identifier(_) | TokenKind::Integer(_) | TokenKind::ParenLeft | TokenKind::If
        )
    }
}
