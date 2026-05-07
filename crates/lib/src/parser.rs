use core::fmt;
use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
};

use crate::{
    context::{Associativity, Context, Node, NodeId},
    expr::{Expr, TypeExpr},
    lexer::{self, Delimiter, Keyword, Lexer, Literal, Token},
    span::Span,
    ty,
    type_system::Type,
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
    lexer: Lexer<'a>,
    lookahead: VecDeque<(Token, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(context: &'a mut Context, input: &'a str) -> Self {
        Parser {
            context,
            lexer: Lexer::new(input),
            lookahead: VecDeque::new(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<NodeId>> {
        let mut nodes = Vec::new();
        while let (token, span) = self.peek(0)?.clone()
            && token != Token::Eof
        {
            match token {
                Token::Keyword(Keyword::Op) => self.parse_operator_decl()?,
                Token::Keyword(Keyword::Type) => nodes.push(self.parse_type_decl()?),
                Token::Keyword(Keyword::Let) => nodes.push(self.parse_bind_decl()?),
                Token::Keyword(Keyword::Import) => nodes.push(self.parse_import_decl()?),
                _ => {
                    return Err(Error::UnexpectedToken(
                        "let|import|op".into(),
                        (token, span),
                    ));
                }
            }
        }
        Ok(nodes)
    }

    fn parse_type_decl(&mut self) -> Result<NodeId> {
        let (_, start_span) = self.expect(Token::Keyword(Keyword::Type))?;
        let name = match self.advance()? {
            (Token::Type(ty), _) => ty,
            other => {
                return Err(Error::UnexpectedToken("type".to_string(), other));
            }
        };

        let mut params = Vec::new();

        while let (Token::Identifier(id), _) = self.peek(0)? {
            params.push(id);
            self.advance()?;
        }

        self.expect(Token::Symbol("=".into()))?;

        let mut consts = Vec::new();

        loop {
            let cons = self.parse_type_expr()?;
            consts.push(cons);

            match self.peek(0)? {
                (Token::Symbol(val), _) if val == "|" => {
                    self.advance()?;
                    continue;
                }
                _ => break,
            }
        }

        let (_, end_span) = self.expect(Token::Delimiter(Delimiter::Semicolon))?;

        let ty = self.context.add_type(name, params, consts);
        Ok(ty)
    }

    fn parse_bind_decl(&mut self) -> Result<NodeId> {
        let (_, let_span) = self.expect(Token::Keyword(Keyword::Let))?;
        let name = match self.advance()? {
            (Token::Identifier(id), _) => id,
            (Token::Delimiter(Delimiter::ParenLeft), _) => match self.advance()? {
                (Token::Symbol(op), _) => {
                    self.expect(Token::Delimiter(Delimiter::ParenRight))?;
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
        self.expect(Token::Symbol("=".into()))?;
        let expr = self.parse_expr(0)?;
        let expr_span = self.context.get_span(expr);
        self.expect(Token::Delimiter(Delimiter::Semicolon))?;
        let bind = self.context.add_bind(&name, expr);
        self.context.set_span(bind, let_span.merge(expr_span));
        Ok(bind)
    }

    fn parse_operator_decl(&mut self) -> Result<()> {
        self.expect(Token::Keyword(Keyword::Op))?;
        let op = match self.advance()? {
            (Token::Symbol(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".to_string(), other));
            }
        };
        let assoc = match self.advance()? {
            (Token::Keyword(Keyword::Left), _) => Associativity::Left,
            (Token::Keyword(Keyword::Right), _) => Associativity::Right,
            (Token::Keyword(Keyword::None), _) => Associativity::None,
            other => {
                return Err(Error::UnexpectedToken(
                    "left, right or none".to_string(),
                    other,
                ));
            }
        };
        let prec = match self.advance()? {
            (Token::Literal(Literal::Integer(prec)), _) => prec.parse().unwrap(),
            other => {
                return Err(Error::UnexpectedToken("integer".to_string(), other));
            }
        };
        self.expect(Token::Delimiter(Delimiter::Semicolon))?;
        self.context.add_operator(&op, prec, assoc);
        Ok(())
    }

    fn parse_import_decl(&mut self) -> Result<NodeId> {
        let (_, start_span) = self.expect(Token::Keyword(Keyword::Import))?;

        let mut module = Vec::new();
        loop {
            let part = match self.advance()? {
                (Token::Identifier(id), _) => id,
                other => {
                    return Err(Error::UnexpectedToken("identifier".to_string(), other));
                }
            };
            module.push(part);
            match self.peek(0)? {
                (Token::Symbol(val), _) => {
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
        let end_span = self.expect(Token::Delimiter(Delimiter::Semicolon))?.1;

        let import = self.context.add_import(&module);
        self.context.set_span(import, start_span.merge(end_span));
        Ok(import)
    }

    fn parse_type_expr(&mut self) -> Result<NodeId> {
        let expr = self.parse_primary_type_expr()?;
        if let Some(TypeExpr::Constructor(..)) = self.context.get_type_expr(expr) {
            let mut temp = Vec::new();
            while let Ok(arg) = self.parse_primary_type_expr() {
                temp.push(arg);
            }
            if let Some(TypeExpr::Constructor(_, args)) = self.context.get_type_expr_mut(expr) {
                args.extend(temp)
            }
        }
        Ok(expr)
    }

    fn parse_primary_type_expr(&mut self) -> Result<NodeId> {
        match self.peek(0)?.0 {
            Token::Type(_) => match self.advance()? {
                (Token::Type(ty), span) => {
                    let expr = self
                        .context
                        .add_type_expr(TypeExpr::Constructor(ty, Vec::new()));
                    self.context.set_span(expr, span);
                    Ok(expr)
                }
                other => Err(Error::UnexpectedToken("type".into(), other)),
            },
            Token::Identifier(_) => match self.advance()? {
                (Token::Identifier(id), span) => {
                    let expr = self.context.add_type_expr(TypeExpr::Identifier(id));
                    self.context.set_span(expr, span);
                    Ok(expr)
                }
                other => Err(Error::UnexpectedToken("identifier".into(), other)),
            },
            Token::Delimiter(Delimiter::ParenLeft) => match self.peek(1)?.0 {
                Token::Delimiter(Delimiter::ParenRight) => {
                    let start_span = self.expect(Token::Delimiter(Delimiter::ParenLeft))?.1;
                    let end_span = self.expect(Token::Delimiter(Delimiter::ParenRight))?.1;
                    let expr = self.context.add_type_expr(TypeExpr::Unit);
                    self.context.set_span(expr, start_span.merge(end_span));
                    Ok(expr)
                }
                _ => {
                    let start_span = self.expect(Token::Delimiter(Delimiter::ParenLeft))?.1;
                    let expr = self.parse_type_expr()?;
                    let end_span = self.expect(Token::Delimiter(Delimiter::ParenRight))?.1;
                    self.context.set_span(expr, start_span.merge(end_span));
                    Ok(expr)
                }
            },
            _ => Err(Error::UnexpectedToken(
                "type expression".to_string(),
                self.peek(0)?,
            )),
        }
    }

    fn parse_expr(&mut self, min_bp: i32) -> Result<NodeId> {
        let mut expr = self.parse_primary_expr()?;
        expr = self.parse_application(expr)?;
        expr = self.parse_infix(expr, min_bp)?;
        Ok(expr)
    }

    fn parse_primary_expr(&mut self) -> Result<NodeId> {
        match self.peek(0)?.0 {
            Token::Literal(Literal::Bool(_)) => self.parse_bool_lit(),
            Token::Literal(Literal::Integer(_)) => self.parse_integer_lit(),
            Token::Literal(Literal::Char(_)) => self.parse_char_lit(),
            Token::Literal(Literal::String(_)) => self.parse_string_lit(),
            Token::Delimiter(Delimiter::BracketLeft) => self.parse_list_lit(),
            Token::Type(_) => self.parse_type(),
            Token::Identifier(_) => match self.peek(1)? {
                (Token::Symbol(s), _) if s == "->" => self.parse_lambda_expr(),
                _ => self.parse_identifier(),
            },
            Token::Keyword(Keyword::If) => self.parse_cond_expr(),
            Token::Delimiter(Delimiter::ParenLeft) => match self.peek(1)?.0 {
                Token::Delimiter(Delimiter::ParenRight) => self.parse_unit_lit(),
                Token::Symbol(_) => match self.peek(2)?.0 {
                    Token::Delimiter(Delimiter::ParenRight) => self.parse_operator_section(),
                    _ => self.parse_operator_right_section(),
                },
                _ => {
                    let mut depth = 1;
                    let mut pos = 1;
                    let mut last_paren_pos = 0;
                    while depth > 0 {
                        match self.peek(pos)?.0 {
                            Token::Delimiter(Delimiter::ParenLeft) => depth += 1,
                            Token::Delimiter(Delimiter::ParenRight) => {
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
                        Token::Symbol(_) => self.parse_operator_left_section(),
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
        let start_span = self.expect(Token::Delimiter(Delimiter::ParenLeft))?.1;
        let op = match self.advance()? {
            (Token::Symbol(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".into(), other));
            }
        };
        let end_span = self.expect(Token::Delimiter(Delimiter::ParenRight))?.1;

        let lhs = self.context.add_expr(Expr::Identifier("a".into()));
        let rhs = self.context.add_expr(Expr::Identifier("b".into()));

        let op = self.context.add_expr(Expr::Identifier(op));
        let app1 = self
            .context
            .add_expr(Expr::Application { func: op, arg: lhs });
        let app2 = self.context.add_expr(Expr::Application {
            func: app1,
            arg: rhs,
        });

        let inner = self.context.add_expr(Expr::Lambda {
            param: "b".into(),
            body: app2,
        });
        let expr = self.context.add_expr(Expr::Lambda {
            param: "a".into(),
            body: inner,
        });

        let span = start_span.merge(end_span);
        self.context.set_span(expr, span);
        self.context.set_span(lhs, span);
        self.context.set_span(rhs, span);
        self.context.set_span(op, span);
        self.context.set_span(app1, span);
        self.context.set_span(app2, span);
        self.context.set_span(inner, span);

        Ok(expr)
    }

    fn parse_operator_left_section(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::Delimiter(Delimiter::ParenLeft))?.1;
        let lhs = self.parse_expr(0)?;
        let op = match self.advance()? {
            (Token::Symbol(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".into(), other));
            }
        };
        let end_span = self.expect(Token::Delimiter(Delimiter::ParenRight))?.1;

        let rhs = self.context.add_expr(Expr::Identifier("b".into()));

        let op = self.context.add_expr(Expr::Identifier(op));
        let app1 = self
            .context
            .add_expr(Expr::Application { func: op, arg: lhs });
        let app2 = self.context.add_expr(Expr::Application {
            func: app1,
            arg: rhs,
        });

        let expr = self.context.add_expr(Expr::Lambda {
            param: "b".into(),
            body: app2,
        });

        let span = start_span.merge(end_span);
        self.context.set_span(expr, span);
        self.context.set_span(rhs, span);
        self.context.set_span(op, span);
        self.context.set_span(app1, span);
        self.context.set_span(app2, span);

        Ok(expr)
    }

    fn parse_operator_right_section(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::Delimiter(Delimiter::ParenLeft))?.1;
        let op = match self.advance()? {
            (Token::Symbol(op), _) => op,
            other => {
                return Err(Error::UnexpectedToken("operator".into(), other));
            }
        };
        let rhs = self.parse_expr(0)?;
        let end_span = self.expect(Token::Delimiter(Delimiter::ParenRight))?.1;

        let lhs = self.context.add_expr(Expr::Identifier("a".into()));

        let op = self.context.add_expr(Expr::Identifier(op));
        let app1 = self
            .context
            .add_expr(Expr::Application { func: op, arg: lhs });
        let app2 = self.context.add_expr(Expr::Application {
            func: app1,
            arg: rhs,
        });

        let expr = self.context.add_expr(Expr::Lambda {
            param: "a".into(),
            body: app2,
        });

        let span = start_span.merge(end_span);
        self.context.set_span(expr, span);
        self.context.set_span(lhs, span);
        self.context.set_span(rhs, span);
        self.context.set_span(op, span);
        self.context.set_span(app1, span);
        self.context.set_span(app2, span);

        Ok(expr)
    }

    fn parse_paren_expr(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::Delimiter(Delimiter::ParenLeft))?.1;
        let expr = self.parse_expr(0)?;
        let end_span = self.expect(Token::Delimiter(Delimiter::ParenRight))?.1;
        self.context.set_span(expr, start_span.merge(end_span));
        Ok(expr)
    }

    fn parse_unit_lit(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::Delimiter(Delimiter::ParenLeft))?.1;
        let end_span = self.expect(Token::Delimiter(Delimiter::ParenRight))?.1;
        let expr = self.context.add_expr(Expr::Unit);
        self.context.set_span(expr, start_span.merge(end_span));
        Ok(expr)
    }

    fn parse_cond_expr(&mut self) -> std::result::Result<NodeId, Error> {
        let start_span = self.expect(Token::Keyword(Keyword::If))?.1;
        let cond = self.parse_expr(0)?;
        self.expect(Token::Keyword(Keyword::Then))?;
        let then = self.parse_expr(0)?;
        self.expect(Token::Keyword(Keyword::Else))?;
        let alt = self.parse_expr(0)?;
        let end_span = self.context.get_span(alt);
        let expr = self.context.add_expr(Expr::Condition { cond, then, alt });
        self.context.set_span(expr, start_span.merge(end_span));
        Ok(expr)
    }

    fn parse_type(&mut self) -> std::result::Result<NodeId, Error> {
        match self.advance()? {
            (Token::Type(ty), span) => {
                let expr = self.context.add_expr(Expr::Constructor(ty));
                self.context.set_span(expr, span);
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("identifier".into(), other)),
        }
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
                self.expect(Token::Symbol("->".into()))?;
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
            (Token::Delimiter(Delimiter::BracketLeft), start_span) => match self.peek(0)? {
                (Token::Delimiter(Delimiter::BracketRight), end_span) => {
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
                            (Token::Delimiter(Delimiter::Comma), _) => {
                                self.advance()?;
                                continue;
                            }
                            _ => break,
                        }
                    }
                    let (_, end_span) = self.expect(Token::Delimiter(Delimiter::BracketRight))?;
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
                Token::Delimiter(Delimiter::BracketLeft).to_string(),
                other,
            )),
        }
    }

    fn parse_string_lit(&mut self) -> std::result::Result<NodeId, Error> {
        match self.advance()? {
            (Token::Literal(Literal::String(str)), span) => {
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
            (Token::Literal(Literal::Char(char)), span) => {
                let expr = self.context.add_expr(Expr::Char(char));
                self.context.set_span(expr, span);
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("char".into(), other)),
        }
    }

    fn parse_integer_lit(&mut self) -> Result<NodeId> {
        match self.advance()? {
            (Token::Literal(Literal::Integer(int)), span) => {
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
            (Token::Literal(Literal::Bool(bool)), span) => {
                let expr = self.context.add_expr(Expr::Boolean(bool));
                self.context.set_span(expr, span);
                Ok(expr)
            }
            other => Err(Error::UnexpectedToken("true or false".into(), other)),
        }
    }

    fn parse_infix(&mut self, mut lhs: NodeId, min_bp: i32) -> Result<NodeId> {
        while let (Token::Symbol(op), _) = self.peek(0)?
            && self.peek(1)?.0 != Token::Delimiter(Delimiter::ParenRight)
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

            let id = self.context.add_expr(Expr::Identifier(op));
            let app = self
                .context
                .add_expr(Expr::Application { func: id, arg: lhs });
            let expr = self.context.add_expr(Expr::Application {
                func: app,
                arg: rhs,
            });

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
