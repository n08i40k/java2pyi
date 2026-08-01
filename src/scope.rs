use std::{
    collections::HashMap,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    exclude::Exclusions,
    index_tree::{GlobalIndexTree, PackageIndexTree},
    model::Root,
    status,
};

#[derive(Debug)]
pub struct Scope<'a> {
    pub root: &'a Root,
    pub index_tree: Arc<GlobalIndexTree<'a>>,
}

impl<'a> Scope<'a> {
    pub fn from_roots(roots: &'a [Arc<Root>], exclusions: &Exclusions<'a>) -> Box<[Self]> {
        status::update(&format!("Indexing Trees 0/{}", roots.len()));
        let package_index_trees = {
            let progress = AtomicUsize::new(0);
            let package_index_trees = roots
                .par_iter()
                .map(|root| {
                    let tree = PackageIndexTree::from_ir(root, exclusions);
                    let label = package_label(root.ir().package);
                    update_parallel_progress("Indexing Trees", &progress, roots.len(), label);
                    tree
                })
                .collect::<Vec<_>>();

            let mut groups: HashMap<String, PackageIndexTree<'a>> =
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

        status::update(&format!("Indexing Scopes 0/{}", roots.len()));
        let progress = AtomicUsize::new(0);
        roots
            .par_iter()
            .map(|root| {
                let label = package_label(root.ir().package);
                update_parallel_progress("Indexing Scopes", &progress, roots.len(), label);

                Scope {
                    root: root.as_ref(),
                    index_tree: global_index_tree.clone(),
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
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
