use core::fmt;
use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
};

use crate::{
    context::{Associativity, Context, NodeId},
    expr::Expr,
    lexer::{self, Lexer, Token},
    span::Span,
    ty,
};

#[derive(Debug)]
pub enum Error {
    Lexer(lexer::Error),
    UnexpectedToken(String, (Token, Span)),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Lexer(error) => error.fmt(f),
            Error::UnexpectedToken(expected, (token, _)) => {
                write!(f, "expected '{}' but got '{}'", expected, *token,)
            }
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Parser<'a> {
    context: &'a mut Context,
    lexer: Lexer,
    lookahead: VecDeque<(Token, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(context: &'a mut Context, input: &str) -> Self {
        Parser {
            context,
            lexer: Lexer::new(input),
            lookahead: VecDeque::new(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<NodeId>> {
        let mut nodes = Vec::new();
        while let (token, _) = self.peek(0)?.clone()
            && token != Token::Eof
        {
            match token {
                Token::Op => self.parse_operator_decl()?,
                Token::Let => nodes.push(self.parse_bind_decl()?),
                Token::Import => nodes.push(self.parse_import_decl()?),
                _ => nodes.push(self.parse_expr(0)?),
            }
        }
        Ok(nodes)
    }

    fn parse_bind_decl(&mut self) -> Result<NodeId> {
        let (_, let_span) = self.expect(Token::Let)?;
        let name = match self.advance()? {
            (Token::Identifier(id), _) => id,
            (Token::ParenLeft, _) => match self.advance()? {
                (Token::Operator(op), _) => {
                    self.expect(Token::ParenRight)?;
                    op
                }
                other => {
                    return Err(Error::UnexpectedToken("operator".to_string(), other));
                }
            },
            other => {
                return Err(Error::UnexpectedToken(
                    "Identifier or operator".to_string(),
                    other,
                ));
            }
        };
        self.expect(Token::Equal)?;
        let expr = self.parse_expr(0)?;
        let expr_span = self.context.get_span(expr);
        self.expect(Token::Semicolon)?;
        let bind = self.context.add_bind(&name, expr);
        self.context.set_span(bind, let_span.merge(expr_span));
        Ok(bind)
    }

    fn parse_operator_decl(&mut self) -> Result<()> {
        self.expect(Token::Op)?;
        let op = match self.advance()? {
            (Token::Operator(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".to_string(), other));
            }
        };
        let assoc = match self.advance()? {
            (Token::Left, _) => Associativity::Left,
            (Token::Right, _) => Associativity::Right,
            (Token::None, _) => Associativity::None,
            other => {
                return Err(Error::UnexpectedToken(
                    "left, right or none".to_string(),
                    other,
                ));
            }
        };
        let prec = match self.advance()? {
            (Token::Integer(prec), _) => prec.parse().unwrap(),
            other => {
                return Err(Error::UnexpectedToken("integer".to_string(), other));
            }
        };
        self.expect(Token::Semicolon)?;
        self.context.add_operator(&op, prec, assoc);
        Ok(())
    }

    fn parse_import_decl(&mut self) -> Result<NodeId> {
        let (_, start_span) = self.expect(Token::Import)?;

        let mut module = Vec::new();
        loop {
            let part = match self.advance()? {
                (Token::Identifier(id), _) => id,
                other => {
                    return Err(Error::UnexpectedToken("Identifier".to_string(), other));
                }
            };
            module.push(part);
            match self.peek(0)? {
                (Token::Operator(val), _) => {
                    if val == "." {
                        self.advance()?;
                        continue;
                    } else {
                        return Err(Error::UnexpectedToken(".".to_string(), self.peek(0)?));
                    }
                }
                _ => break,
            }
        }
        let end_span = self.expect(Token::Semicolon)?.1;

        let import = self.context.add_import(&module);
        self.context.set_span(import, start_span.merge(end_span));
        Ok(import)
    }

    fn parse_expr(&mut self, min_bp: i32) -> Result<NodeId> {
        let mut expr = self.parse_primary_expr()?;
        expr = self.parse_application(expr)?;
        expr = self.parse_infix(expr, min_bp)?;
        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> Result<NodeId> {
        match self.peek(0)?.0 {
            Token::Boolean(_) => self.parse_bool_lit(),
            Token::Integer(_) => self.parse_integer_lit(),
            Token::Char(_) => self.parse_char_lit(),
            Token::String(_) => self.parse_string_lit(),
            Token::BracketLeft => self.parse_list_lit(),
            Token::Identifier(_) => match self.peek(1)? {
                (Token::HyphenGreaterThan, _) => self.parse_lambda_expr(),
                _ => self.parse_identifier(),
            },
            Token::If => self.parse_cond_expr(),
            Token::ParenLeft => match self.peek(1)?.0 {
                Token::ParenRight => self.parse_unit_lit(),
                Token::Operator(_) => match self.peek(2)?.0 {
                    Token::ParenRight => self.parse_operator_section(),
                    _ => self.parse_operator_right_section(),
                },
                _ => {
                    let mut depth = 1;
                    let mut pos = 1;
                    let mut last_paren_pos = 0;
                    while depth > 0 {
                        match self.peek(pos)?.0 {
                            Token::ParenLeft => depth += 1,
                            Token::ParenRight => {
                                depth -= 1;
                                last_paren_pos = pos;
                            }
                            Token::Eof => {
                                return Err(Error::UnexpectedToken(
                                    ")".into(),
                                    self.peek(last_paren_pos + 1)?,
                                ));
                            }
                            _ => {}
                        }
                        pos += 1;
                    }

                    match self.peek(pos - 2)?.0 {
                        Token::Operator(_) => self.parse_operator_left_section(),
                        _ => self.parse_paren_expr(),
                    }
                }
            },
            _ => Err(Error::UnexpectedToken(
                "expression".to_string(),
                self.peek(0)?,
            )),
        }
    }

    fn parse_operator_section(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::ParenLeft)?.1;
        let op = match self.advance()? {
            (Token::Operator(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".into(), other));
            }
        };
        let end_span = self.expect(Token::ParenRight)?.1;
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
        let span = start_span.merge(end_span);
        self.context.set_span(expr, span);
        self.context.set_span(lhs, span);
        self.context.set_span(rhs, span);
        self.context.set_span(infix, span);
        self.context.set_span(inner, span);
        Ok(expr)
    }

    fn parse_operator_left_section(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::ParenLeft)?.1;
        let lhs = self.parse_expr(0)?;
        let op = match self.advance()? {
            (Token::Operator(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".into(), other));
            }
        };
        let end_span = self.expect(Token::ParenRight)?.1;
        let rhs = self.context.add_expr(Expr::Identifier("b".into()));
        let infix = self.context.add_expr(Expr::Infix { lhs, op, rhs });
        let expr = self.context.add_expr(Expr::Lambda {
            param: "b".into(),
            body: infix,
        });
        let span = start_span.merge(end_span);
        self.context.set_span(expr, span);
        self.context.set_span(rhs, span);
        self.context.set_span(infix, span);
        Ok(expr)
    }

    fn parse_operator_right_section(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::ParenLeft)?.1;
        let op = match self.advance()? {
            (Token::Operator(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".into(), other));
            }
        };
        let rhs = self.parse_expr(0)?;
        let end_span = self.expect(Token::ParenRight)?.1;
        let lhs = self.context.add_expr(Expr::Identifier("a".into()));
        let infix = self.context.add_expr(Expr::Infix { lhs, op, rhs });
        let expr = self.context.add_expr(Expr::Lambda {
            param: "a".into(),
            body: infix,
        });
        let span = start_span.merge(end_span);
        self.context.set_span(expr, span);
        self.context.set_span(lhs, span);
        self.context.set_span(rhs, span);
        self.context.set_span(infix, span);
        Ok(expr)
    }

    fn parse_paren_expr(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::ParenLeft)?.1;
        let expr = self.parse_expr(0)?;
        let end_span = self.expect(Token::ParenRight)?.1;
        self.context.set_span(expr, start_span.merge(end_span));
        Ok(expr)
    }

    fn parse_unit_lit(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::ParenLeft)?.1;
        let end_span = self.expect(Token::ParenRight)?.1;
        let expr = self.context.add_expr(Expr::Unit);
        self.context.set_span(expr, start_span.merge(end_span));
        Ok(expr)
    }

    fn parse_cond_expr(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::If)?.1;
        let cond = self.parse_expr(0)?;
        self.expect(Token::Then)?;
        let then = self.parse_expr(0)?;
        self.expect(Token::Else)?;
        let alt = self.parse_expr(0)?;
        let end_span = self.context.get_span(alt);
        let expr = self.context.add_expr(Expr::Condition { cond, then, alt });
        self.context.set_span(expr, start_span.merge(end_span));
        Ok(expr)
    }

    fn parse_identifier(&mut self) -> std::result::Result<NodeId, Error> {
        match self.advance()? {
            (Token::Identifier(id), span) => {
                let expr = self.context.add_expr(Expr::Identifier(id));
                self.context.set_span(expr, span);
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("identifier".into(), other)),
        }
    }

    fn parse_lambda_expr(&mut self) -> std::result::Result<NodeId, Error> {
        match self.advance()? {
            (Token::Identifier(param), param_span) => {
                self.expect(Token::HyphenGreaterThan)?;
                let body = self.parse_expr(0)?;
                let body_span = self.context.get_span(body);
                let expr = self.context.add_expr(Expr::Lambda { param, body });
                self.context.set_span(expr, param_span.merge(body_span));
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("identifier".into(), other)),
        }
    }

    fn parse_list_lit(&mut self) -> std::result::Result<NodeId, Error> {
        match self.advance()? {
            (Token::BracketLeft, start_span) => match self.peek(0)? {
                (Token::BracketRight, end_span) => {
                    self.advance()?;
                    let list = self.context.add_expr(Expr::Nil);
                    self.context.set_span(list, start_span.merge(end_span));
                    Ok(list)
                }
                _ => {
                    let mut elems = Vec::new();
                    loop {
                        elems.push(self.parse_expr(0)?);
                        match self.peek(0)? {
                            (Token::Comma, _) => {
                                self.advance()?;
                                continue;
                            }
                            _ => break,
                        }
                    }
                    let (_, end_span) = self.expect(Token::BracketRight)?;
                    let mut list = self.context.add_expr(Expr::Nil);
                    for elem in elems.into_iter().rev() {
                        list = self.context.add_expr(Expr::Cons {
                            head: elem,
                            tail: list,
                        });
                    }
                    self.context.set_span(list, start_span.merge(end_span));
                    Ok(list)
                }
            },
            other => Err(Error::UnexpectedToken(
                Token::BracketLeft.to_string(),
                other,
            )),
        }
    }

    fn parse_string_lit(&mut self) -> std::result::Result<NodeId, Error> {
        match self.advance()? {
            (Token::String(str), span) => {
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
                Ok(list)
            }
            other => Err(Error::UnexpectedToken("string".into(), other)),
        }
    }

    fn parse_char_lit(&mut self) -> std::result::Result<NodeId, Error> {
        match self.advance()? {
            (Token::Char(char), span) => {
                let expr = self.context.add_expr(Expr::Char(char));
                self.context.set_span(expr, span);
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("char".into(), other)),
        }
    }

    fn parse_integer_lit(&mut self) -> Result<NodeId> {
        match self.advance()? {
            (Token::Integer(int), span) => {
                let expr = self
                    .context
                    .add_expr(Expr::Integer(int.parse::<i64>().unwrap()));
                self.context.set_span(expr, span);
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("integer".into(), other)),
        }
    }

    fn parse_bool_lit(&mut self) -> Result<NodeId> {
        match self.advance()? {
            (Token::Boolean(bool), span) => {
                let expr = self.context.add_expr(Expr::Boolean(match bool.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                }));
                self.context.set_span(expr, span);
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("boolean".into(), other)),
        }
    }

    fn parse_infix(&mut self, mut lhs: NodeId, min_bp: i32) -> Result<NodeId> {
        while let (Token::Operator(op), _) = self.peek(0)?
            && self.peek(1)?.0 != Token::ParenRight
        {
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
            let rhs = self.parse_expr(r_bp)?;
            let expr = self.context.add_expr(Expr::Infix { lhs, op, rhs });
            self.context.set_span(
                expr,
                self.context.get_span(lhs).merge(self.context.get_span(rhs)),
            );
            lhs = expr;
        }
        Ok(lhs)
    }

    fn parse_application(&mut self, mut func: NodeId) -> Result<NodeId> {
        while let Ok(arg) = self.parse_primary_expr() {
            let start_span = self.context.get_span(func);
            let end_span = self.context.get_span(arg);
            func = self.context.add_expr(Expr::Application { func, arg });
            self.context.set_span(func, start_span.merge(end_span));
        }
        Ok(func)
    }

    fn peek(&mut self, n: usize) -> Result<(Token, Span)> {
        while self.lookahead.len() <= n {
            self.lookahead
                .push_back(self.lexer.next_token().map_err(Error::Lexer)?);
        }
        Ok(self.lookahead[n].clone())
    }

    fn advance(&mut self) -> Result<(Token, Span)> {
        if let Some(peek) = self.lookahead.pop_front() {
            Ok(peek)
        } else {
            self.lexer.next_token().map_err(Error::Lexer)
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(Token, Span)> {
        match self.advance()? {
            (token, span) if token == expected => Ok((token, span)),
            other => Err(Error::UnexpectedToken(expected.to_string(), other)),
        }
    }
}

pub fn parse(ctx: &mut Context, input: &str) -> Result<Vec<NodeId>> {
    Parser::new(ctx, input).parse()
}
