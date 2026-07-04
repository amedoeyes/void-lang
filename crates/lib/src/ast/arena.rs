use crate::{
    ast::{
        expr::Expr,
        node::{Node, NodeKind},
        type_expr::TypeExpr,
    },
    span::Span,
    type_system::Type,
};

#[derive(Debug, Clone)]
pub struct NodeArena {
    kinds: Vec<NodeKind>,
    spans: Vec<Span>,
    types: Vec<Option<Type>>,
}

impl NodeArena {
    pub fn new() -> Self {
        Self {
            kinds: Vec::new(),
            spans: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn alloc(&mut self, kind: NodeKind) -> Node {
        let node = Node(self.kinds.len());
        self.kinds.push(kind);
        self.spans.push(Span::DUMMY);
        self.types.push(None);
        node
    }

    pub fn alloc_with_span(&mut self, kind: NodeKind, span: Span) -> (Node, Span) {
        let node = self.alloc(kind);
        self.spans[node.0] = span;
        (node, span)
    }

    pub fn get_type_expr(&self, node: Node) -> Option<&TypeExpr> {
        self.kinds.get(node.0).and_then(|n| {
            if let NodeKind::TypeExpr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn get_type_expr_mut(&mut self, node: Node) -> Option<&mut TypeExpr> {
        self.kinds.get_mut(node.0).and_then(|n| {
            if let NodeKind::TypeExpr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn get_expr(&self, node: Node) -> Option<&Expr> {
        self.kinds.get(node.0).and_then(|n| {
            if let NodeKind::Expr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn get_expr_mut(&mut self, node: Node) -> Option<&mut Expr> {
        self.kinds.get_mut(node.0).and_then(|n| {
            if let NodeKind::Expr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn nodes(&self) -> impl Iterator<Item = Node> {
        (0..self.kinds.len()).map(Node)
    }

    pub fn kinds(&self) -> &[NodeKind] {
        &self.kinds
    }

    pub fn spans(&self) -> &[Span] {
        &self.spans
    }

    pub fn types(&self) -> &[Option<Type>] {
        &self.types
    }

    pub fn kind(&self, node: Node) -> &NodeKind {
        &self.kinds[node.0]
    }

    pub fn kind_mut(&mut self, node: Node) -> &mut NodeKind {
        &mut self.kinds[node.0]
    }

    pub fn span(&self, node: Node) -> Span {
        self.spans[node.0]
    }

    pub fn span_mut(&mut self, node: Node) -> &mut Span {
        &mut self.spans[node.0]
    }

    pub fn ty(&self, node: Node) -> Option<&Type> {
        self.types[node.0].as_ref()
    }

    pub fn ty_mut(&mut self, node: Node) -> Option<&mut Type> {
        self.types[node.0].as_mut()
    }

    pub fn set_span(&mut self, node: Node, span: Span) {
        self.spans[node.0] = span;
    }

    pub fn set_ty(&mut self, node: Node, ty: Type) {
        self.types[node.0] = Some(ty);
    }
}
