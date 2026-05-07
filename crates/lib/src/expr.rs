use crate::context::NodeId;

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Boolean(bool),
    Char(char),
    Integer(i64),
    Constructor(String),
    Identifier(String),
    Condition {
        cond: NodeId,
        then: NodeId,
        alt: NodeId,
    },
    Lambda {
        param: String,
        body: NodeId,
    },
    Application {
        func: NodeId,
        arg: NodeId,
    },
    Nil,
    Cons {
        head: NodeId,
        tail: NodeId,
    },
    Infix {
        lhs: NodeId,
        op: String,
        rhs: NodeId,
    },
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Unit,
    Identifier(String),
    Constructor(String, Vec<NodeId>),
}
