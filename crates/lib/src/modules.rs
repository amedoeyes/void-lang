use std::{
    fs,
    path::{Path, PathBuf},
};

use fxhash::FxHashSet;
use include_dir::{Dir, include_dir};

use crate::{
    context::{Context, Node, NodeId},
    error::Error,
    parser,
};

type Result<T> = std::result::Result<T, Error>;

static STD_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/std");

pub fn resolve_imports(
    ctx: &mut Context,
    nodes: &[NodeId],
    parent: &Path,
    visited: &mut FxHashSet<PathBuf>,
    imported: &mut FxHashSet<PathBuf>,
) -> Result<Vec<NodeId>> {
    let mut new_nodes = Vec::new();

    for node in nodes {
        match ctx.get_node(*node) {
            Node::Import(module) => {
                let is_std = module.first().map_or_else(|| false, |part| part == "std")
                    || parent
                        .iter()
                        .next()
                        .map_or_else(|| false, |part| part == "std");

                let import_path = if is_std {
                    if module.len() == 1 && module.first().map(|s| s.as_str()) == Some("std") {
                        PathBuf::from("module.void")
                    } else {
                        PathBuf::from(module.join("/")).with_extension("void")
                    }
                } else {
                    let base_path = parent.join(module.join("/"));
                    if base_path.is_dir() && base_path.join("module.void").exists() {
                        base_path.join("module.void")
                    } else {
                        base_path.with_extension("void")
                    }
                };

                let parent_dir = if is_std {
                    import_path
                        .parent()
                        .map(|p| {
                            if p.as_os_str().is_empty() {
                                Path::new("std")
                            } else {
                                p
                            }
                        })
                        .unwrap_or(Path::new("std"))
                } else {
                    import_path
                        .parent()
                        .ok_or_else(|| Error::InvalidPath(import_path.clone()))?
                };

                if !imported.insert(import_path.clone()) {
                    continue;
                }

                if !visited.insert(import_path.clone()) {
                    return Err(Error::CircularImport(import_path));
                }

                let contents = if is_std {
                    let file_path = if import_path.iter().next() == Some("std".as_ref()) {
                        import_path.components().skip(1).collect::<PathBuf>()
                    } else {
                        import_path.clone()
                    };

                    STD_DIR
                        .get_file(&file_path)
                        .ok_or_else(|| Error::ModuleNotFound(module.clone()))?
                        .contents_utf8()
                        .unwrap()
                } else {
                    if !import_path.exists() {
                        return Err(Error::ModuleNotFound(module.clone()));
                    }
                    &fs::read_to_string(&import_path)?
                };

                let imported_nodes = match parser::parse(ctx, contents) {
                    Ok(nodes) => resolve_imports(ctx, &nodes, parent_dir, visited, imported)?,
                    Err(err) => {
                        return Err(Error::Syntax(
                            import_path,
                            contents.to_string(),
                            Box::new(err),
                        ));
                    }
                };

                visited.remove(&import_path);

                new_nodes.extend(
                    imported_nodes
                        .iter()
                        .filter(|n| matches!(ctx.get_node(**n), Node::Bind(_, _)))
                        .cloned(),
                );
            }
            _ => new_nodes.push(*node),
        };
    }

    Ok(new_nodes)
}
