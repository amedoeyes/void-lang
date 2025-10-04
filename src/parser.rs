use crate::{
    context::{Context, Node, NodeId},
    error::SyntaxError,
    expr::{Expr, InfixOp, PrefixOp},
    lexer::{Lexer, Token},
    span::Span,
};

type Result<T> = std::result::Result<T, SyntaxError>;

#[derive(Debug)]
pub struct Parser<'a> {
    context: &'a mut Context,
    lexer: Lexer,
    token: Token,
    span: Span,
}

impl<'a> Parser<'a> {
    pub fn new(context: &'a mut Context, input: &str) -> Self {
        Parser {
            context,
            lexer: Lexer::new(input),
            token: Token::Eof,
            span: Span::default(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<NodeId>> {
        let mut nodes = Vec::new();

        self.advance()?;

        while self.token != Token::Eof {
            match self.token {
                Token::Let => {
                    let span = self.span;

                    self.advance()?;
                    let name = if let Token::Identifier(id) = &self.token {
                        id.clone()
                    } else {
                        return Err(SyntaxError::UnexpectedToken(
                            "Identifier".to_string(),
                            (self.token, self.span),
                        ));
                    };

                    self.advance()?;
                    self.expect(Token::Equal)?;

                    let expr = self.parse_expr(0.0)?;
                    self.expect(Token::Semicolon)?;

                    let bind = self.context.add_bind(&name, expr);
                    self.context
                        .set_span(bind, span.merge(*self.context.get_span(expr)));

                    nodes.push(bind);
                }

                _ => {
                    nodes.push(self.parse_expr(0.0)?);
                }
            }
        }

        Ok(nodes)
    }

    fn advance(&mut self) -> Result<()> {
        let (token, span) = self.lexer.next_token()?;
        self.token = token;
        self.span = span;
        Ok(())
    }

    fn expect(&mut self, token: Token) -> Result<()> {
        if self.token != token {
            return Err(SyntaxError::UnexpectedToken(
                token.to_string(),
                (self.token.clone(), self.span),
            ));
        }
        self.advance()?;
        Ok(())
    }

    fn parse_atom(&mut self) -> Result<NodeId> {
        match &self.token {
            Token::Boolean(bool) => {
                let expr = self.context.add_expr(Expr::Boolean(match bool.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                }));
                self.context.set_span(expr, self.span);
                self.advance()?;
                Ok(expr)
            }

            Token::Integer(int) => {
                let expr = self
                    .context
                    .add_expr(Expr::Integer(int.parse::<i64>().unwrap()));
                self.context.set_span(expr, self.span);
                self.advance()?;
                Ok(expr)
            }

            Token::Char(char) => {
                let expr = self.context.add_expr(Expr::Char(*char));
                self.context.set_span(expr, self.span);
                self.advance()?;
                Ok(expr)
            }

            Token::String(str) => {
                let span = self.span;
                let mut list = self.context.add_expr(Expr::Nil);
                for char in str.chars().rev() {
                    let expr = self.context.add_expr(Expr::Char(char));
                    list = self.context.add_expr(Expr::Cons {
                        head: expr,
                        tail: list,
                    });
                }
                self.context.set_span(list, span);
                self.advance()?;
                Ok(list)
            }

            Token::BracketLeft => {
                let span = self.span;

                self.advance()?;

                if self.token == Token::BracketRight {
                    let list = self.context.add_expr(Expr::Nil);
                    self.context.set_span(list, span.merge(self.span));
                    self.advance()?;

                    Ok(list)
                } else {
                    let mut elems = Vec::new();

                    loop {
                        let elem = self.parse_expr(0.0)?;
                        elems.push(elem);

                        if self.token == Token::Comma {
                            self.advance()?;
                            continue;
                        } else {
                            break;
                        }
                    }

                    let span = span.merge(self.span);
                    self.expect(Token::BracketRight)?;

                    let mut list = self.context.add_expr(Expr::Nil);
                    for elem in elems.into_iter().rev() {
                        list = self.context.add_expr(Expr::Cons {
                            head: elem,
                            tail: list,
                        });
                    }
                    self.context.set_span(list, span);

                    Ok(list)
                }
            }

            Token::Identifier(id) => {
                let expr = self.context.add_expr(Expr::Identifier(id.clone()));
                self.context.set_span(expr, self.span);
                self.advance()?;
                Ok(expr)
            }

            Token::If => {
                let span = self.span;
                self.advance()?;
                let cond = self.parse_expr(0.0)?;
                self.expect(Token::Then)?;
                let then = self.parse_expr(0.0)?;
                self.expect(Token::Else)?;
                let alt = self.parse_expr(0.0)?;
                let expr = self.context.add_expr(Expr::Condition { cond, then, alt });
                self.context
                    .set_span(expr, span.merge(*self.context.get_span(alt)));
                Ok(expr)
            }

            Token::ParenLeft => {
                let span = self.span;
                self.advance()?;
                if self.token == Token::ParenRight {
                    let expr = self.context.add_expr(Expr::Unit);
                    self.context.set_span(expr, span.merge(self.span));
                    self.advance()?;
                    Ok(expr)
                } else {
                    let expr = self.parse_expr(0.0)?;
                    self.context.set_span(expr, span.merge(self.span));
                    self.expect(Token::ParenRight)?;
                    Ok(expr)
                }
            }

            _ => Err(SyntaxError::UnexpectedToken(
                "expression".to_string(),
                (self.token.clone(), self.span),
            )),
        }
    }

    fn parse_prefix(&mut self) -> Result<NodeId> {
        if let Some(op) = PrefixOp::from_token(&self.token) {
            match op {
                PrefixOp::Neg | PrefixOp::Not => {
                    let span = self.span;
                    self.advance()?;
                    let (_, r_bp) = op.precedence();
                    let rhs = self.parse_expr(r_bp)?;
                    let expr = self.context.add_expr(Expr::Prefix { op, rhs });
                    self.context
                        .set_span(expr, span.merge(*self.context.get_span(rhs)));
                    Ok(expr)
                }
            }
        } else {
            unreachable!()
        }
    }

    fn parse_infix(&mut self, lhs: NodeId, min_bp: f32) -> Result<NodeId> {
        let mut lhs = lhs;

        while let Some(op) = InfixOp::from_token(&self.token) {
            let (l_bp, r_bp) = op.precedence();
            if l_bp < min_bp {
                break;
            }
            self.advance()?;
            let rhs = self.parse_expr(r_bp)?;
            let expr = self.context.add_expr(Expr::Infix { lhs, op, rhs });
            self.context.set_span(
                expr,
                self.context
                    .get_span(lhs)
                    .merge(*self.context.get_span(rhs)),
            );
            lhs = expr;
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self, min_bp: f32) -> Result<NodeId> {
        let mut expr = if matches!(self.token, Token::Hyphen | Token::Bang) {
            self.parse_prefix()?
        } else {
            self.parse_atom()?
        };

        if self.token == Token::HyphenGreaterThan {
            if let Node::Expr(Expr::Identifier(_)) = self.context.get_node(expr) {
                self.advance()?;

                let param = expr;
                let body = self.parse_expr(0.0)?;

                expr = self.context.add_expr(Expr::Lambda { param, body });
                self.context.set_span(
                    expr,
                    self.context
                        .get_span(param)
                        .merge(*self.context.get_span(body)),
                );
            } else {
                return Err(SyntaxError::UnexpectedToken(
                    "identifier".to_string(),
                    (self.token.clone(), self.span),
                ));
            }
        }

        while matches!(
            self.token,
            Token::Boolean(_)
                | Token::BracketLeft
                | Token::Char(_)
                | Token::Identifier(_)
                | Token::If
                | Token::Integer(_)
                | Token::ParenLeft
                | Token::String(_)
        ) {
            let arg = self.parse_atom()?;
            let func = expr;
            expr = self.context.add_expr(Expr::Application { func, arg });
            self.context.set_span(
                expr,
                self.context
                    .get_span(func)
                    .merge(*self.context.get_span(arg)),
            );
        }

        if matches!(
            self.token,
            Token::AmpersandAmpersand
                | Token::BangEqual
                | Token::Colon
                | Token::EqualEqual
                | Token::GreaterThan
                | Token::GreaterThanEqual
                | Token::Hyphen
                | Token::LessThan
                | Token::LessThanEqual
                | Token::Percent
                | Token::PipePipe
                | Token::Plus
                | Token::PlusPlus
                | Token::Slash
                | Token::Star
        ) {
            expr = self.parse_infix(expr, min_bp)?
        }

        Ok(expr)
    }
}

pub fn parse(ctx: &mut Context, input: &str) -> Result<Vec<NodeId>> {
    Parser::new(ctx, input).parse()
}
