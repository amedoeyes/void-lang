use crate::{
    ast::node::{Node, NodeKind},
    span::Span,
    type_system::{Type, TypeScheme},
};

#[derive(Debug, Clone)]
pub struct BuiltinNodes {
    pub wildcard: Node,
}

#[derive(Debug, Clone)]
pub struct NodeArena {
    kinds: Vec<NodeKind>,
    spans: Vec<Span>,
    types: Vec<Option<Type>>,
    schemes: Vec<Option<TypeScheme>>,
    pub builtins: BuiltinNodes,
}

impl NodeArena {
    pub fn new() -> Self {
        let mut nodes = Self {
            kinds: Vec::new(),
            spans: Vec::new(),
            types: Vec::new(),
            schemes: Vec::new(),
            builtins: BuiltinNodes { wildcard: Node(0) },
        };
        nodes.builtins.wildcard = nodes.alloc(NodeKind::Pattern(super::pattern::Pattern::Wildcard));
        nodes
    }

    pub fn alloc(&mut self, kind: NodeKind) -> Node {
        let node = Node(self.kinds.len());
        self.kinds.push(kind);
        self.spans.push(Span::DUMMY);
        self.types.push(None);
        self.schemes.push(None);
        node
    }

    pub fn alloc_with_span(&mut self, kind: NodeKind, span: Span) -> (Node, Span) {
        let node = self.alloc(kind);
        self.spans[node.0] = span;
        (node, span)
    }

    pub fn nodes(&self) -> Vec<Node> {
        (0..self.kinds.len()).map(Node).collect()
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

    pub fn schemes(&self) -> &[Option<TypeScheme>] {
        &self.schemes
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

    pub fn scheme(&self, node: Node) -> Option<&TypeScheme> {
        self.schemes[node.0].as_ref()
    }

    pub fn scheme_mut(&mut self, node: Node) -> Option<&mut TypeScheme> {
        self.schemes[node.0].as_mut()
    }

    pub fn set_span(&mut self, node: Node, span: Span) {
        self.spans[node.0] = span;
    }

    pub fn set_ty(&mut self, node: Node, ty: Type) {
        self.types[node.0] = Some(ty);
    }

    pub fn set_scheme(&mut self, node: Node, scheme: TypeScheme) {
        self.schemes[node.0] = Some(scheme);
    }
}
