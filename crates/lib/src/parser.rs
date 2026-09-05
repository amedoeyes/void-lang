use core::fmt;
use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
};

use fxhash::FxHashMap;

use crate::{
    ast::{
        arena::NodeArena,
        expr::Expr,
        node::{Node, NodeKind},
        pattern::Pattern,
        type_expr::TypeExpr,
    },
    lexer::{self, Delimiter, Keyword, Lexer, Literal, Token},
    span::Span,
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
                write!(f, "expected '{}' but got '{}'", expected, *token)
            }
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Copy)]
pub struct Operator {
    pub precedence: i32,
    pub associativity: Associativity,
}

impl Default for Operator {
    fn default() -> Self {
        Self {
            precedence: 9,
            associativity: Associativity::default(),
        }
    }
}

impl Operator {
    pub fn new(precedence: i32, associativity: Associativity) -> Self {
        Self {
            precedence,
            associativity,
        }
    }

    pub fn binding_power(&self) -> (i32, i32) {
        match self.associativity {
            Associativity::Left => (self.precedence, self.precedence + 1),
            Associativity::Right => (self.precedence + 1, self.precedence),
            Associativity::None => (self.precedence, self.precedence),
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub enum Associativity {
    #[default]
    Left,
    Right,
    None,
}

#[derive(Debug)]
pub struct Parser<'a> {
    nodes: &'a mut NodeArena,
    lexer: Lexer<'a>,
    lookahead: VecDeque<(Token, Span)>,
    operators: FxHashMap<String, Operator>,
}

impl<'a> Parser<'a> {
    pub fn new(nodes: &'a mut NodeArena, input: &'a str) -> Self {
        Parser {
            nodes,
            lexer: Lexer::new(input),
            lookahead: VecDeque::new(),
            operators: FxHashMap::default(),
        }
    }

    pub fn parse(mut self) -> Result<()> {
        let mut nodes = Vec::new();
        while let (token, span) = self.peek(0)?.clone()
            && token != Token::Eof
        {
            match token {
                Token::Keyword(Keyword::Op) => {
                    self.parse_decl_operator()?;
                    continue;
                }
                _ => {}
            }

            let (decl, _) = match token {
                Token::Keyword(Keyword::Type) => self.parse_decl_type(),
                Token::Keyword(Keyword::Primitive) => self.parse_decl_primitive(),
                Token::Keyword(Keyword::Let) => self.parse_decl_bind(),
                _ => Err(Error::UnexpectedToken(
                    "top-level declaration".into(),
                    (token, span),
                )),
            }?;

            nodes.push(decl);
        }
        self.nodes.alloc(NodeKind::Module(nodes));
        Ok(())
    }

    fn parse_decl_type(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Keyword(Keyword::Type))?;
        let (name, _) = self.expect_type()?;
        self.expect_token(Token::Symbol("=".into()))?;
        let (params, _) = self.peek(0).and_then(|(token, _)| match token {
            Token::Symbol(sym) if sym == "<" => self.parse_delimited_list(
                Token::Symbol("<".into()),
                Token::Symbol(">".into()),
                Token::Delimiter(Delimiter::Comma),
                |p| p.expect_identifier().map(|(id, _)| id),
            ),
            _ => Ok((Vec::new(), Span::DUMMY)),
        })?;
        let (consts, _) = self.advance().and_then(|(token, span)| match token {
            Token::Keyword(Keyword::Enum) => self.parse_delimited_list(
                Token::Delimiter(Delimiter::BraceLeft),
                Token::Delimiter(Delimiter::BraceRight),
                Token::Delimiter(Delimiter::Comma),
                |p| {
                    p.advance().and_then(|(token, span)| match token {
                        Token::Type(ty) => {
                            let mut args = Vec::new();
                            while let Ok((arg, _)) = p.parse_type_expr() {
                                args.push(arg);
                            }
                            Ok((ty, args))
                        }
                        _ => Err(Error::UnexpectedToken("variant".to_string(), (token, span))),
                    })
                },
            ),
            _ => return Err(Error::UnexpectedToken("enum".to_string(), (token, span))),
        })?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::Semicolon))?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::Type(name, params, consts),
            start_span.merge(end_span),
        ))
    }

    fn parse_decl_primitive(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Keyword(Keyword::Primitive))?;
        let (name, _) = self.expect_identifier()?;
        self.expect_token(Token::Symbol(":".into()))?;
        let (type_expr, _) = self.parse_type_expr()?;
        self.expect_token(Token::Symbol("=".into()))?;
        let link_name = self.advance().and_then(|(token, span)| match token {
            Token::Literal(Literal::String(name)) => Ok(name),
            _ => Err(Error::UnexpectedToken("link symbol".into(), (token, span))),
        })?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::Semicolon))?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::Primitive(name, type_expr, link_name),
            start_span.merge(end_span),
        ))
    }

    fn parse_decl_bind(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Keyword(Keyword::Let))?;
        let name = self.advance().and_then(|(token, span)| match token {
            Token::Identifier(id) => Ok(id),
            Token::Delimiter(Delimiter::ParenLeft) => {
                self.advance().and_then(|(token, span)| match token {
                    Token::Symbol(op) => {
                        self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
                        Ok(op)
                    }
                    _ => Err(Error::UnexpectedToken(
                        "operator".to_string(),
                        (token, span),
                    )),
                })
            }
            _ => Err(Error::UnexpectedToken(
                "Identifier or operator".to_string(),
                (token, span),
            )),
        })?;
        let type_expr = self.peek(0).and_then(|(token, _)| match token {
            Token::Symbol(s) if s == ":" => self
                .advance()
                .and_then(|_| self.parse_type_expr())
                .map(|(t, _)| Some(t)),
            _ => Ok(None),
        })?;
        self.expect_token(Token::Symbol("=".into()))?;
        let (expr, _) = self.parse_expr(0)?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::Semicolon))?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::Bind(name, type_expr, expr),
            start_span.merge(end_span),
        ))
    }

    fn parse_decl_operator(&mut self) -> Result<()> {
        self.expect_token(Token::Keyword(Keyword::Op))?;
        let (op, _) = self.expect_operator()?;
        let assoc = self.advance().and_then(|(token, span)| match token {
            Token::Keyword(Keyword::Left) => Ok(Associativity::Left),
            Token::Keyword(Keyword::Right) => Ok(Associativity::Right),
            Token::Keyword(Keyword::None) => Ok(Associativity::None),
            _ => Err(Error::UnexpectedToken(
                "left, right or none".to_string(),
                (token, span),
            )),
        })?;
        let prec = self.advance().and_then(|(token, span)| match token {
            Token::Literal(Literal::Integer(prec)) => Ok(prec.parse().unwrap()),
            _ => Err(Error::UnexpectedToken("integer".to_string(), (token, span))),
        })?;
        self.expect_token(Token::Delimiter(Delimiter::Semicolon))?;
        self.operators.insert(op.into(), Operator::new(prec, assoc));
        Ok(())
    }

    fn parse_type_expr(&mut self) -> Result<(Node, Span)> {
        self.peek(0)
            .and_then(|(token, span)| match token {
                Token::Type(..) => self.parse_type_expr_constructor(),
                Token::Identifier(..) => self.parse_type_expr_identifier(),
                Token::Delimiter(Delimiter::ParenLeft) => {
                    self.peek(1).and_then(|(token, _)| match token {
                        Token::Delimiter(Delimiter::ParenRight) => self.parse_type_expr_unit(),
                        _ => self.parse_type_expr_paren(),
                    })
                }
                Token::Symbol(s) if s == "<" => self.parse_type_expr_forall(),
                _ => Err(Error::UnexpectedToken(
                    "type expression".to_string(),
                    (token, span),
                )),
            })
            .and_then(|type_expr| {
                self.peek(0).and_then(|(token, _)| match token {
                    Token::Symbol(s) if s == "->" => self.parse_type_expr_lambda(type_expr),
                    _ => Ok(type_expr),
                })
            })
    }

    fn parse_type_expr_constructor(&mut self) -> Result<(Node, Span)> {
        self.advance().and_then(|(token, span)| match token {
            Token::Type(ty) => {
                let (args, end_span) = self.peek(0).and_then(|(token, _)| match token {
                    Token::Symbol(s) if s == "<" => self.parse_delimited_list(
                        Token::Symbol("<".into()),
                        Token::Symbol(">".into()),
                        Token::Delimiter(Delimiter::Comma),
                        |p| p.parse_type_expr().map(|(t, _)| t),
                    ),
                    _ => Ok((Vec::new(), span)),
                })?;
                Ok(self.nodes.alloc_with_span(
                    NodeKind::TypeExpr(TypeExpr::Constructor(ty, args)),
                    span.merge(end_span),
                ))
            }
            _ => Err(Error::UnexpectedToken("type".into(), (token, span))),
        })
    }

    fn parse_type_expr_identifier(&mut self) -> Result<(Node, Span)> {
        self.expect_identifier().map(|(id, span)| {
            self.nodes
                .alloc_with_span(NodeKind::TypeExpr(TypeExpr::Identifier(id)), span)
        })
    }

    fn parse_type_expr_unit(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::ParenLeft))?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::TypeExpr(TypeExpr::Unit),
            start_span.merge(end_span),
        ))
    }

    fn parse_type_expr_lambda(&mut self, (lhs, span): (Node, Span)) -> Result<(Node, Span)> {
        self.expect_token(Token::Symbol("->".into()))?;
        let (rhs, end_span) = self.parse_type_expr()?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::TypeExpr(TypeExpr::Lambda(lhs, rhs)),
            span.merge(end_span),
        ))
    }

    fn parse_type_expr_forall(&mut self) -> Result<(Node, Span)> {
        let (params, start_span) = self.parse_delimited_list(
            Token::Symbol("<".into()),
            Token::Symbol(">".into()),
            Token::Delimiter(Delimiter::Comma),
            |p| p.expect_identifier().map(|(id, _)| id),
        )?;
        let (body, end_span) = self.parse_type_expr()?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::TypeExpr(TypeExpr::Forall(params, body)),
            start_span.merge(end_span),
        ))
    }

    fn parse_type_expr_paren(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::ParenLeft))?;
        let (expr, _) = self.parse_type_expr()?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
        let span = start_span.merge(end_span);
        self.nodes.set_span(expr, span);
        Ok((expr, span))
    }

    fn parse_expr(&mut self, min_bp: i32) -> Result<(Node, Span)> {
        self.parse_expr_primary().and_then(|expr| {
            self.parse_expr_application(expr)
                .and_then(|expr| self.parse_expr_infix(expr, min_bp))
        })
    }

    fn parse_expr_primary(&mut self) -> Result<(Node, Span)> {
        self.peek(0).and_then(|(token, span)| match token {
            Token::Literal(Literal::Integer(..)) => self.parse_expr_integer_lit(),
            Token::Literal(Literal::Char(..)) => self.parse_expr_char_lit(),
            Token::Type(..) => self.parse_expr_constructor(),
            Token::Identifier(..) => self.peek(1).and_then(|(token, _)| match token {
                Token::Symbol(s) if s == "->" => self.parse_expr_lambda(),
                _ => self.parse_expr_identifier(),
            }),
            Token::Keyword(Keyword::Match) => self.parse_expr_match(),
            Token::Keyword(Keyword::If) => self.parse_expr_if(),
            Token::Delimiter(Delimiter::BraceLeft) => self.parse_expr_block(),
            Token::Delimiter(Delimiter::ParenLeft) => {
                self.peek(1).and_then(|(token, _)| match token {
                    Token::Delimiter(Delimiter::ParenRight) => self.parse_expr_unit_lit(),
                    Token::Symbol(..) => self.peek(2).and_then(|(token, _)| match token {
                        Token::Delimiter(Delimiter::ParenRight) => self.parse_expr_op_section(),
                        _ => self.parse_expr_right_op_section(),
                    }),
                    _ => {
                        let mut depth = 1;
                        let mut pos = 1;
                        while depth > 0 {
                            match self.peek(pos)?.0 {
                                Token::Delimiter(Delimiter::ParenLeft) => depth += 1,
                                Token::Delimiter(Delimiter::ParenRight) => depth -= 1,
                                Token::Eof => break,
                                _ => {}
                            }
                            pos += 1;
                        }
                        self.peek(pos - 2).and_then(|(token, _)| match token {
                            Token::Symbol(..) => self.parse_expr_left_op_section(),
                            _ => self.parse_expr_paren(),
                        })
                    }
                })
            }
            _ => Err(Error::UnexpectedToken(
                "expression".to_string(),
                (token, span),
            )),
        })
    }

    fn parse_expr_unit_lit(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::ParenLeft))?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
        Ok(self
            .nodes
            .alloc_with_span(NodeKind::Expr(Expr::Unit), start_span.merge(end_span)))
    }

    fn parse_expr_char_lit(&mut self) -> Result<(Node, Span)> {
        self.advance().and_then(|(token, span)| match token {
            Token::Literal(Literal::Char(char)) => Ok(self
                .nodes
                .alloc_with_span(NodeKind::Expr(Expr::Char(char)), span)),
            _ => Err(Error::UnexpectedToken("char".into(), (token, span))),
        })
    }

    fn parse_expr_integer_lit(&mut self) -> Result<(Node, Span)> {
        self.advance().and_then(|(token, span)| match token {
            Token::Literal(Literal::Integer(int)) => Ok(self
                .nodes
                .alloc_with_span(NodeKind::Expr(Expr::Integer(int.parse().unwrap())), span)),
            _ => Err(Error::UnexpectedToken("integer".into(), (token, span))),
        })
    }

    fn parse_expr_constructor(&mut self) -> Result<(Node, Span)> {
        self.expect_type().map(|(ty, span)| {
            self.nodes
                .alloc_with_span(NodeKind::Expr(Expr::Constructor(ty)), span)
        })
    }

    fn parse_expr_identifier(&mut self) -> Result<(Node, Span)> {
        self.expect_identifier().map(|(id, span)| {
            self.nodes
                .alloc_with_span(NodeKind::Expr(Expr::Identifier(id)), span)
        })
    }

    fn parse_expr_lambda(&mut self) -> Result<(Node, Span)> {
        self.advance().and_then(|(token, span)| match token {
            Token::Identifier(lhs) => {
                self.expect_token(Token::Symbol("->".into()))?;
                let (rhs, end_span) = self.parse_expr(0)?;
                Ok(self
                    .nodes
                    .alloc_with_span(NodeKind::Expr(Expr::Lambda(lhs, rhs)), span.merge(end_span)))
            }
            _ => Err(Error::UnexpectedToken("identifier".into(), (token, span))),
        })
    }

    fn parse_expr_match(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Keyword(Keyword::Match))?;
        let (scrutinee, _) = self.parse_expr(0)?;
        self.expect_token(Token::Keyword(Keyword::With))?;
        let (branches, end_span) = self.parse_delimited_list(
            Token::Delimiter(Delimiter::BraceLeft),
            Token::Delimiter(Delimiter::BraceRight),
            Token::Delimiter(Delimiter::Comma),
            |p| {
                let (pattern, _) = p.parse_pattern()?;
                p.expect_token(Token::Symbol("=>".into()))?;
                let (body, _) = p.parse_expr(0)?;
                Ok((pattern, body))
            },
        )?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::Expr(Expr::Match(scrutinee, branches)),
            start_span.merge(end_span),
        ))
    }

    fn parse_expr_if(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Keyword(Keyword::If))?;
        let (condition, _) = self.parse_expr(0)?;
        self.expect_token(Token::Keyword(Keyword::Then))?;
        let (true_body, _) = self.parse_expr(0)?;
        self.expect_token(Token::Keyword(Keyword::Else))?;
        let (false_body, end_span) = self.parse_expr(0)?;
        let span = start_span.merge(end_span);
        let (false_pat, _) = self.nodes.alloc_with_span(
            NodeKind::Pattern(Pattern::Constructor("False".into(), Vec::new())),
            span,
        );
        let (true_pat, _) = self.nodes.alloc_with_span(
            NodeKind::Pattern(Pattern::Constructor("True".into(), Vec::new())),
            span,
        );
        Ok(self.nodes.alloc_with_span(
            NodeKind::Expr(Expr::Match(
                condition,
                Vec::from([(false_pat, false_body), (true_pat, true_body)]),
            )),
            span,
        ))
    }

    fn parse_expr_block(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::BraceLeft))?;
        let mut nodes = Vec::new();
        loop {
            match self.peek(0)?.0 {
                Token::Delimiter(Delimiter::BraceRight) => break,
                Token::Keyword(Keyword::Let) => nodes.push(self.parse_decl_bind()?.0),
                _ => {
                    nodes.push(self.parse_expr(0)?.0);
                    break;
                }
            }
        }
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::BraceRight))?;
        Ok(self.nodes.alloc_with_span(
            NodeKind::Expr(Expr::Block(nodes)),
            start_span.merge(end_span),
        ))
    }

    fn parse_expr_op_section(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::ParenLeft))?;
        let (op, _) = self.expect_operator()?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
        let lhs = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Identifier("a".into())));
        let rhs = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Identifier("b".into())));
        let op = self.nodes.alloc(NodeKind::Expr(Expr::Identifier(op)));
        let app1 = self.nodes.alloc(NodeKind::Expr(Expr::Application(op, lhs)));
        let app2 = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Application(app1, rhs)));
        let inner = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Lambda("b".into(), app2)));
        Ok(self.nodes.alloc_with_span(
            NodeKind::Expr(Expr::Lambda("a".into(), inner)),
            start_span.merge(end_span),
        ))
    }

    fn parse_expr_left_op_section(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::ParenLeft))?;
        let (lhs, _) = self.parse_expr(0)?;
        let (op, _) = self.expect_operator()?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
        let rhs = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Identifier("b".into())));
        let op = self.nodes.alloc(NodeKind::Expr(Expr::Identifier(op)));
        let app1 = self.nodes.alloc(NodeKind::Expr(Expr::Application(op, lhs)));
        let app2 = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Application(app1, rhs)));
        Ok(self.nodes.alloc_with_span(
            NodeKind::Expr(Expr::Lambda("b".into(), app2)),
            start_span.merge(end_span),
        ))
    }

    fn parse_expr_right_op_section(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::ParenLeft))?;
        let (op, _) = self.expect_operator()?;
        let (rhs, _) = self.parse_expr(0)?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
        let lhs = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Identifier("a".into())));
        let op = self.nodes.alloc(NodeKind::Expr(Expr::Identifier(op)));
        let app1 = self.nodes.alloc(NodeKind::Expr(Expr::Application(op, lhs)));
        let app2 = self
            .nodes
            .alloc(NodeKind::Expr(Expr::Application(app1, rhs)));
        Ok(self.nodes.alloc_with_span(
            NodeKind::Expr(Expr::Lambda("a".into(), app2)),
            start_span.merge(end_span),
        ))
    }

    fn parse_expr_infix(&mut self, lhs: (Node, Span), min_bp: i32) -> Result<(Node, Span)> {
        let (mut lhs, mut lhs_span) = lhs;
        while let (Token::Symbol(op), op_span) = self.peek(0)?
            && self.peek(1)?.0 != Token::Delimiter(Delimiter::ParenRight)
        {
            let (l_bp, r_bp) = self
                .operators
                .get(&op)
                .copied()
                .unwrap_or_default()
                .binding_power();
            if l_bp < min_bp {
                break;
            }
            self.advance()?;
            let (rhs, rhs_span) = self.parse_expr(r_bp)?;
            let (id, _) = self
                .nodes
                .alloc_with_span(NodeKind::Expr(Expr::Identifier(op)), op_span);
            let (app, _) = self.nodes.alloc_with_span(
                NodeKind::Expr(Expr::Application(id, lhs)),
                op_span.merge(lhs_span),
            );
            (lhs, lhs_span) = self.nodes.alloc_with_span(
                NodeKind::Expr(Expr::Application(app, rhs)),
                lhs_span.merge(rhs_span),
            );
        }
        Ok((lhs, lhs_span))
    }

    fn parse_expr_application(&mut self, lhs: (Node, Span)) -> Result<(Node, Span)> {
        let (mut lhs, mut lhs_span) = lhs;
        while let Ok((rhs, rhs_span)) = self.parse_expr_primary() {
            (lhs, lhs_span) = self.nodes.alloc_with_span(
                NodeKind::Expr(Expr::Application(lhs, rhs)),
                lhs_span.merge(rhs_span),
            );
        }
        Ok((lhs, lhs_span))
    }

    fn parse_expr_paren(&mut self) -> Result<(Node, Span)> {
        let (_, start_span) = self.expect_token(Token::Delimiter(Delimiter::ParenLeft))?;
        let (expr, _) = self.parse_expr(0)?;
        let (_, end_span) = self.expect_token(Token::Delimiter(Delimiter::ParenRight))?;
        let span = start_span.merge(end_span);
        self.nodes.set_span(expr, span);
        Ok((expr, span))
    }

    fn parse_pattern(&mut self) -> Result<(Node, Span)> {
        self.peek(0).and_then(|(token, span)| match token {
            Token::Identifier(id) if id == "_" => self.parse_pattern_wildcard(),
            Token::Identifier(..) => self.parse_pattern_identifier(),
            Token::Type(..) => self.parse_pattern_constructor(),
            _ => Err(Error::UnexpectedToken("pattern".to_string(), (token, span))),
        })
    }

    fn parse_pattern_wildcard(&mut self) -> Result<(Node, Span)> {
        self.expect_token(Token::Identifier("_".into()))
            .map(|(_, span)| {
                self.nodes
                    .alloc_with_span(NodeKind::Pattern(Pattern::Wildcard), span)
            })
    }

    fn parse_pattern_identifier(&mut self) -> Result<(Node, Span)> {
        self.expect_identifier().map(|(id, span)| {
            self.nodes
                .alloc_with_span(NodeKind::Pattern(Pattern::Identifier(id)), span)
        })
    }

    fn parse_pattern_constructor(&mut self) -> Result<(Node, Span)> {
        let (ty, start_span) = self.expect_type()?;
        let mut args = Vec::new();
        let mut end_span = start_span;
        while let Ok((arg, arg_span)) = self.parse_pattern() {
            args.push(arg);
            end_span = arg_span;
        }
        Ok(self.nodes.alloc_with_span(
            NodeKind::Pattern(Pattern::Constructor(ty, args)),
            start_span.merge(end_span),
        ))
    }

    pub fn parse_delimited_list<T>(
        &mut self,
        open: Token,
        close: Token,
        sep: Token,
        mut parser: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(Vec<T>, Span)> {
        let (_, start_span) = self.expect_token(open)?;
        let mut res = Vec::new();
        if self.peek(0)?.0 != close {
            res.push(parser(self)?);
            while self.peek(0)?.0 == sep {
                self.advance()?;
                if self.peek(0)?.0 == close {
                    break;
                }
                res.push(parser(self)?);
            }
        }
        let (_, end_span) = self.expect_token(close)?;
        Ok((res, start_span.merge(end_span)))
    }

    fn peek(&mut self, n: usize) -> Result<(Token, Span)> {
        while self.lookahead.len() <= n {
            self.lookahead
                .push_back(self.lexer.next_token().map_err(Error::Lexer)?);
        }
        Ok(self.lookahead[n].clone())
    }

    fn advance(&mut self) -> Result<(Token, Span)> {
        self.lookahead.pop_front().map_or_else(
            || self.lexer.next_token().map_err(Error::Lexer),
            |peek| Ok(peek),
        )
    }

    fn expect_token(&mut self, expected: Token) -> Result<(Token, Span)> {
        self.advance().and_then(|(token, span)| {
            if token == expected {
                Ok((token, span))
            } else {
                Err(Error::UnexpectedToken(expected.to_string(), (token, span)))
            }
        })
    }

    fn expect_type(&mut self) -> Result<(String, Span)> {
        self.advance().and_then(|(token, span)| match token {
            Token::Type(ty) => Ok((ty, span)),
            _ => Err(Error::UnexpectedToken("type".to_string(), (token, span))),
        })
    }

    fn expect_identifier(&mut self) -> Result<(String, Span)> {
        self.advance().and_then(|(token, span)| match token {
            Token::Identifier(id) => Ok((id, span)),
            _ => Err(Error::UnexpectedToken(
                "identifier".to_string(),
                (token, span),
            )),
        })
    }

    fn expect_operator(&mut self) -> Result<(String, Span)> {
        self.advance().and_then(|(token, span)| match token {
            Token::Symbol(op) => Ok((op, span)),
            _ => Err(Error::UnexpectedToken("operator".into(), (token, span))),
        })
    }
}

pub fn parse(nodes: &mut NodeArena, input: &str) -> Result<()> {
    Parser::new(nodes, input).parse()
}
