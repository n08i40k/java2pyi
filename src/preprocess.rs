use std::{
    collections::HashMap,
    fs,
    path::Path,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    index_tree::{GlobalIndexTree, ImportedIndexTree, LocalIndexTree, PackageIndexTree},
    model::{Exclusions, Root},
    status,
};

/// Parse AST and keep parser-owned source storage alive through RootCell.
pub fn parse_java_ast<P: AsRef<Path>>(
    path: P,
) -> std::result::Result<Root, Box<java_ast_parser::ErrorCell<'static>>> {
    let data = fs::read_to_string(path).unwrap();
    java_ast_parser::parse_owned(data).map(Root::new)
}

#[derive(Debug)]
pub struct Scope {
    pub ast: Arc<Root>,
    pub local_index_tree: LocalIndexTree,
}

impl Scope {
    pub fn from_roots(roots: &[Arc<Root>], exclusions: &Exclusions) -> Box<[Self]> {
        status::update(&format!("Indexing Trees 0/{}", roots.len()));
        let package_index_trees = {
            let progress = AtomicUsize::new(0);
            let package_index_trees = roots
                .par_iter()
                .map(|root| {
                    let tree = PackageIndexTree::from_ast(root, exclusions);
                    let label = package_label(root.ast().package);
                    update_parallel_progress("Indexing Trees", &progress, roots.len(), label);
                    tree
                })
                .collect::<Vec<_>>();

            let mut groups: HashMap<String, PackageIndexTree> =
                HashMap::with_capacity(package_index_trees.len());

            for package_index_tree in package_index_trees {
                if let Some(target_index_tree) = groups.get_mut(package_index_tree.package()) {
                    target_index_tree.merge_with(&package_index_tree);
                } else {
                    groups.insert(package_index_tree.package().to_string(), package_index_tree);
                }
            }

            groups
        };

        let global_index_tree = Arc::new(GlobalIndexTree::from_iter(package_index_trees.values()));
        let mut shared_package_indices = HashMap::with_capacity(package_index_trees.len());
        for (package, index_tree) in &package_index_trees {
            shared_package_indices.insert(package.clone(), index_tree.shared_local_index());
        }

        status::update(&format!("Indexing Scopes 0/{}", roots.len()));
        let progress = AtomicUsize::new(0);
        roots
            .par_iter()
            .map(|root| {
                let ast = root.ast();
                let imported_index_tree = ImportedIndexTree::from_imports(
                    ast.imports.iter().copied(),
                    global_index_tree.clone(),
                );

                let shared_local_index = shared_package_indices.get(ast.package).unwrap().clone();

                let local_index_tree = LocalIndexTree::new(
                    global_index_tree.clone(),
                    imported_index_tree,
                    shared_local_index,
                );

                let label = package_label(ast.package);
                update_parallel_progress("Indexing Scopes", &progress, roots.len(), label);

                Scope {
                    ast: root.clone(),
                    local_index_tree,
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

pub fn preprocess_asts(roots: &[Arc<Root>], exclusions: &Exclusions) -> Box<[Scope]> {
    Scope::from_roots(roots, exclusions)
}

fn package_label(package: &str) -> &str {
    if package.is_empty() {
        "<root>"
    } else {
        package
    }
}

fn update_parallel_progress(stage: &str, progress: &AtomicUsize, total: usize, label: &str) {
    let completed = progress.fetch_add(1, Ordering::Relaxed) + 1;
    status::update(&format!("{} {}/{}: {}", stage, completed, total, label));
}
