use crate::context::NodeId;

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Boolean(bool),
    Char(char),
    Integer(i64),
    Identifier(String),
    Condition {
        cond: NodeId,
        then: NodeId,
        alt: NodeId,
    },
    Infix {
        lhs: NodeId,
        op: String,
        rhs: NodeId,
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
}
