use crate::{
    context::{Context, Node, NodeId},
    error::SyntaxError,
    expr::Expr,
    lexer::{Lexer, Token},
    span::Span,
    ty,
};

type Result<T> = std::result::Result<T, SyntaxError>;

#[derive(Debug)]
pub struct Parser<'a> {
    context: &'a mut Context,
    lexer: Lexer,
    token: Token,
    span: Span,
    peek: Option<(Token, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(context: &'a mut Context, input: &str) -> Self {
        Parser {
            context,
            lexer: Lexer::new(input),
            token: Token::Eof,
            span: Span::default(),
            peek: None,
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

                    let name = match &self.token {
                        Token::Identifier(id) => {
                            let id = id.clone();
                            self.advance()?;
                            id
                        }
                        Token::ParenLeft => {
                            self.advance()?;
                            let op = if let Token::Operator(op) = &self.token {
                                let op = op.clone();
                                self.advance()?;
                                op
                            } else {
                                return Err(SyntaxError::UnexpectedToken(
                                    "perator".to_string(),
                                    (self.token, self.span),
                                ));
                            };
                            self.expect(Token::ParenRight)?;
                            op
                        }
                        _ => {
                            return Err(SyntaxError::UnexpectedToken(
                                "Identifier or operator".to_string(),
                                (self.token, self.span),
                            ));
                        }
                    };

                    self.expect(Token::Equal)?;

                    let expr = self.parse_expr(0, false)?;
                    self.expect(Token::Semicolon)?;

                    let bind = self.context.add_bind(&name, expr);

                    self.context
                        .set_span(bind, span.merge(self.context.get_span(expr)));

                    nodes.push(bind);
                }

                _ => {
                    nodes.push(self.parse_expr(0, false)?);
                }
            }
        }

        Ok(nodes)
    }

    fn advance(&mut self) -> Result<()> {
        if let Some((token, span)) = self.peek.take() {
            self.token = token;
            self.span = span;
        } else {
            let (token, span) = self.lexer.next_token()?;
            self.token = token;
            self.span = span;
        }
        Ok(())
    }

    fn peek(&mut self) -> Result<&Token> {
        if self.peek.is_none() {
            self.peek = Some(self.lexer.next_token()?)
        }
        Ok(&self.peek.as_ref().unwrap().0)
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
                self.context.set_type(list, ty!([Char]));
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
                        let elem = self.parse_expr(0, false)?;
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
                let cond = self.parse_expr(0, false)?;
                self.expect(Token::Then)?;
                let then = self.parse_expr(0, false)?;
                self.expect(Token::Else)?;
                let alt = self.parse_expr(0, false)?;
                let expr = self.context.add_expr(Expr::Condition { cond, then, alt });
                self.context
                    .set_span(expr, span.merge(self.context.get_span(alt)));
                Ok(expr)
            }
            Token::ParenLeft => {
                let span = self.span;
                self.advance()?;
                match self.token.clone() {
                    Token::ParenRight => {
                        let expr = self.context.add_expr(Expr::Unit);
                        self.context.set_span(expr, span.merge(self.span));
                        self.advance()?;
                        Ok(expr)
                    }
                    Token::Operator(op) => {
                        self.advance()?;
                        match &self.token {
                            Token::ParenRight => {
                                self.advance()?;

                                let lhs = self.context.add_expr(Expr::Identifier("a".into()));
                                let rhs = self.context.add_expr(Expr::Identifier("b".into()));
                                let infix = self.context.add_expr(Expr::Infix { lhs, op, rhs });
                                let inner = self.context.add_expr(Expr::Lambda {
                                    param: "b".into(),
                                    body: infix,
                                });
                                let expr = self.context.add_expr(Expr::Lambda {
                                    param: "a".into(),
                                    body: inner,
                                });

                                self.context.set_span(expr, span.merge(self.span));
                                let span = self.context.get_span(expr);

                                self.context.set_span(lhs, span);
                                self.context.set_span(rhs, span);
                                self.context.set_span(infix, span);
                                self.context.set_span(inner, span);

                                Ok(expr)
                            }
                            _ => {
                                let lhs = self.context.add_expr(Expr::Identifier("a".into()));
                                let rhs = self.parse_expr(0, false)?;
                                let infix = self.context.add_expr(Expr::Infix { lhs, op, rhs });
                                let expr = self.context.add_expr(Expr::Lambda {
                                    param: "a".into(),
                                    body: infix,
                                });

                                self.context.set_span(expr, span.merge(self.span));
                                let span = self.context.get_span(expr);

                                self.context.set_span(lhs, span);
                                self.context.set_span(rhs, span);
                                self.context.set_span(infix, span);

                                self.expect(Token::ParenRight)?;
                                Ok(expr)
                            }
                        }
                    }
                    _ => {
                        let expr = self.parse_expr(0, true)?;
                        match self.token.clone() {
                            Token::ParenRight => {
                                self.context.set_span(expr, span.merge(self.span));
                                self.advance()?;
                                Ok(expr)
                            }
                            Token::Operator(op) => {
                                if *self.peek()? == Token::ParenRight {
                                    self.advance()?;

                                    let rhs = self.context.add_expr(Expr::Identifier("b".into()));
                                    let infix =
                                        self.context.add_expr(Expr::Infix { lhs: expr, op, rhs });
                                    let expr = self.context.add_expr(Expr::Lambda {
                                        param: "b".into(),
                                        body: infix,
                                    });

                                    self.context.set_span(expr, span.merge(self.span));
                                    let span = self.context.get_span(expr);

                                    self.context.set_span(rhs, span);
                                    self.context.set_span(infix, span);

                                    self.expect(Token::ParenRight)?;
                                    Ok(expr)
                                } else {
                                    let expr = self.parse_infix(expr, 0)?;
                                    self.context.set_span(expr, span.merge(self.span));
                                    self.expect(Token::ParenRight)?;
                                    Ok(expr)
                                }
                            }
                            _ => Err(SyntaxError::UnexpectedToken(
                                Token::ParenRight.to_string(),
                                (self.token.clone(), self.span),
                            )),
                        }
                    }
                }
            }
            _ => Err(SyntaxError::UnexpectedToken(
                "expression".to_string(),
                (self.token.clone(), self.span),
            )),
        }
    }

    fn parse_infix(&mut self, mut lhs: NodeId, min_bp: i32) -> Result<NodeId> {
        while let Token::Operator(op) = self.token.clone() {
            let (l_bp, r_bp) = self
                .context
                .get_operator(&op)
                .cloned()
                .unwrap_or_default()
                .binding_power();
            if l_bp < min_bp {
                break;
            }
            self.advance()?;
            let rhs = self.parse_expr(r_bp, false)?;
            let expr = self.context.add_expr(Expr::Infix { lhs, op, rhs });
            self.context.set_span(
                expr,
                self.context.get_span(lhs).merge(self.context.get_span(rhs)),
            );
            lhs = expr;
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self, min_bp: i32, skip_infix: bool) -> Result<NodeId> {
        let mut expr = self.parse_atom()?;

        if self.token == Token::HyphenGreaterThan {
            if let Node::Expr(Expr::Identifier(param)) = self.context.get_node(expr).clone() {
                self.advance()?;

                let body = self.parse_expr(0, false)?;

                expr = self.context.add_expr(Expr::Lambda { param, body });
                self.context.set_span(
                    expr,
                    self.context
                        .get_span(expr)
                        .merge(self.context.get_span(body)),
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
                    .merge(self.context.get_span(arg)),
            );
        }

        if !skip_infix && matches!(self.token, Token::Operator(_)) {
            expr = self.parse_infix(expr, min_bp)?
        }

        Ok(expr)
    }
}

pub fn parse(ctx: &mut Context, input: &str) -> Result<Vec<NodeId>> {
    Parser::new(ctx, input).parse()
}
