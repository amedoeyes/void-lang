use std::{cell::RefCell, rc::Rc};

use fxhash::FxHashMap;

use crate::{
    context::Context,
    eval::{self, Value},
    span::Span,
    type_system::Type,
};

pub type BuiltinEvalFn = fn(&Context, Rc<RefCell<Value>>, Span) -> eval::Result<Rc<RefCell<Value>>>;

pub struct Builtin {
    pub ty: Type,
    pub eval: BuiltinEvalFn,
}

pub type Builtins = FxHashMap<String, Builtin>;

pub fn builtins() -> Builtins {
    let mut builtins = FxHashMap::default();

    let ty = Type::Var(0);
    builtins.insert(
        "print".into(),
        Builtin {
            ty: Type::Fun(Box::new(ty.clone()), Box::new(ty.clone())),
            eval: |ctx, arg, _| {
                println!("{}", arg.borrow().display(ctx));
                Ok(arg)
            },
        },
    );

    let ty = Type::Var(1);
    builtins.insert(
        "head".into(),
        Builtin {
            ty: Type::Fun(
                Box::new(Type::List(Box::new(ty.clone()))),
                Box::new(ty.clone()),
            ),
            eval: |ctx, arg, span| {
                let arg = arg.borrow();
                match &*arg {
                    Value::List(l) => Ok(Rc::new(RefCell::new(
                        l.head().ok_or(eval::Error::EmptyList(span))?.force(ctx)?,
                    ))),
                    _ => unreachable!(),
                }
            },
        },
    );

    let ty = Type::Var(2);
    builtins.insert(
        "tail".into(),
        Builtin {
            ty: Type::Fun(
                Box::new(Type::List(Box::new(ty.clone()))),
                Box::new(Type::List(Box::new(ty))),
            ),
            eval: |_, arg, span| {
                let arg = arg.borrow();
                match &*arg {
                    Value::List(l) => Ok(Rc::new(RefCell::new(Value::List(
                        l.tail().ok_or(eval::Error::EmptyList(span))?,
                    )))),
                    _ => unreachable!(),
                }
            },
        },
    );

    builtins
}
