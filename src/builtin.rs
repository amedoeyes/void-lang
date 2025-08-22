use std::collections::HashMap;

use crate::{
    context::Context,
    eval::{self, Value},
    span::Span,
    type_system::Type,
};

pub struct Builtin {
    pub ty: Type,
    pub eval: fn(&Context, &Value, Span) -> eval::Result<Value>,
}

pub type Builtins = HashMap<String, Builtin>;

pub fn builtins() -> Builtins {
    let mut builtins = HashMap::new();

    let ty = Type::Var(0);
    builtins.insert(
        "print".into(),
        Builtin {
            ty: Type::Fun(Box::new(ty.clone()), Box::new(ty.clone())),
            eval: |ctx, arg, _| {
                println!("{}", arg.display(ctx));
                Ok(arg.clone())
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
            eval: |_, arg, span| match arg {
                Value::List(l) => l.front().cloned().ok_or(eval::Error::EmptyList(span)),
                _ => unreachable!(),
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
            eval: |_, arg, _| match arg {
                Value::List(l) => {
                    let mut l2 = l.clone();
                    l2.pop_front();
                    Ok(Value::List(l2))
                }
                _ => unreachable!(),
            },
        },
    );

    builtins
}
