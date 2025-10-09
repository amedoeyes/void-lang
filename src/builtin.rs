use std::{cell::RefCell, rc::Rc};

use fxhash::FxHashMap;

use crate::{
    context::Context,
    eval::{self, Value},
    span::Span,
    type_system::Type,
};

pub type BuiltinEvalFn = fn(&Context, Rc<RefCell<Value>>, Span) -> eval::Result<Rc<RefCell<Value>>>;

#[derive(Debug, Clone)]
pub enum BuiltinKind {
    Function(BuiltinEvalFn),
    Value(Rc<RefCell<Value>>),
}

#[derive(Debug, Clone)]
pub struct Builtin {
    pub ty: Type,
    pub kind: BuiltinKind,
}

pub type Builtins = FxHashMap<String, Builtin>;

pub fn builtins() -> Builtins {
    let mut builtins = FxHashMap::default();

    builtins.insert(
        "args".into(),
        Builtin {
            ty: Type::List(Box::new(Type::List(Box::new(Type::Char)))),
            kind: BuiltinKind::Value({
                let args = std::env::args().skip(2).collect::<Vec<_>>();
                let mut arg_list = eval::List::Nil;

                for arg in args.iter().rev() {
                    let mut string_list = eval::List::Nil;
                    for ch in arg.chars().rev() {
                        string_list = string_list.push(Value::Char(ch));
                    }
                    arg_list = arg_list.push(Value::List(string_list));
                }

                Rc::new(RefCell::new(Value::List(arg_list)))
            }),
        },
    );

    builtins.insert(
        "read".into(),
        Builtin {
            ty: Type::Fun(
                Box::new(Type::List(Box::new(Type::Char))),
                Box::new(Type::List(Box::new(Type::Char))),
            ),
            kind: BuiltinKind::Function(|ctx, arg, span| {
                let arg = arg.borrow();
                let path = match &*arg {
                    Value::List(l) => {
                        let mut list = l.clone();
                        let mut chars = Vec::new();
                        while let Some(mut h) = list.head() {
                            match h.force(ctx).unwrap() {
                                Value::Char(ch) => chars.push(ch),
                                _ => unreachable!(),
                            }
                            list = list.tail().unwrap_or_default();
                        }
                        chars.into_iter().collect::<String>()
                    }
                    _ => unreachable!(),
                };

                let contents = if path == "-" {
                    use std::io::Read;
                    let mut input = String::new();
                    std::io::stdin().read_to_string(&mut input).map_err(|e| {
                        eval::Error::IOError(format!("Failed to read from stdin: {}", e), span)
                    })?;
                    input
                } else {
                    std::fs::read_to_string(&path).map_err(|e| {
                        eval::Error::IOError(format!("Failed to read file '{}': {}", path, e), span)
                    })?
                };

                let mut list = eval::List::default();
                for ch in contents.chars().rev() {
                    list = list.push(Value::Char(ch))
                }

                Ok(Rc::new(RefCell::new(Value::List(list))))
            }),
        },
    );

    builtins.insert(
        "print".into(),
        Builtin {
            ty: Type::Fun(Box::new(Type::Var(1)), Box::new(Type::Var(1))),
            kind: BuiltinKind::Function(|ctx, arg, _| {
                println!("{}", arg.borrow().display(ctx));
                Ok(arg)
            }),
        },
    );

    builtins.insert(
        "head".into(),
        Builtin {
            ty: Type::Fun(
                Box::new(Type::List(Box::new(Type::Var(2)))),
                Box::new(Type::Var(2)),
            ),
            kind: BuiltinKind::Function(|ctx, arg, span| {
                let arg = arg.borrow();
                match &*arg {
                    Value::List(l) => Ok(Rc::new(RefCell::new(
                        l.head().ok_or(eval::Error::EmptyList(span))?.force(ctx)?,
                    ))),
                    _ => unreachable!(),
                }
            }),
        },
    );

    builtins.insert(
        "tail".into(),
        Builtin {
            ty: Type::Fun(
                Box::new(Type::List(Box::new(Type::Var(3)))),
                Box::new(Type::List(Box::new(Type::Var(3)))),
            ),
            kind: BuiltinKind::Function(|_, arg, span| {
                let arg = arg.borrow();
                match &*arg {
                    Value::List(l) => Ok(Rc::new(RefCell::new(Value::List(
                        l.tail().ok_or(eval::Error::EmptyList(span))?,
                    )))),
                    _ => unreachable!(),
                }
            }),
        },
    );

    builtins
}
