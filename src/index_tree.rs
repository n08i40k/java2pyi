use crate::ir;
use orx_tree::{Dyn, DynTree, NodeIdx, NodeRef};
use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::exclude::Exclusions;
use crate::model::{ClassRef, Root, type_idents};

#[derive(Debug, Clone)]
pub enum TreeNode<'a> {
    Root,
    Package(Arc<String>),
    Class(ClassRef<'a>),
}

impl<'a> TreeNode<'a> {
    pub fn ident(&self) -> Option<&'_ str> {
        match self {
            TreeNode::Root => None,
            TreeNode::Package(ident) => Some(ident.as_str()),
            TreeNode::Class(cell) => Some(cell.name),
        }
    }

    fn resolved(&self) -> Option<ClassRef<'a>> {
        match self {
            TreeNode::Class(cell) => Some(*cell),
            _ => None,
        }
    }
}

impl std::hash::Hash for TreeNode<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(ident) = self.ident() {
            ident.hash(state);
        } else {
            0.hash(state);
        }
    }
}

impl std::cmp::PartialEq for TreeNode<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self.ident(), other.ident()) {
            (None, None) => true,
            (Some(l), Some(r)) => l == r,
            _ => false,
        }
    }
}

impl std::cmp::Eq for TreeNode<'_> {}

impl From<&str> for TreeNode<'_> {
    fn from(value: &str) -> Self {
        Self::Package(Arc::from(value.to_string()))
    }
}

impl<'a> From<ClassRef<'a>> for TreeNode<'a> {
    fn from(value: ClassRef<'a>) -> Self {
        Self::Class(value)
    }
}

pub type IndexTree<'a> = DynTree<TreeNode<'a>>;

#[derive(Debug, Clone)]
pub struct PackageIndexTree<'a> {
    package: String,
    inner: IndexTree<'a>,
}

impl<'a> Deref for PackageIndexTree<'a> {
    type Target = DynTree<TreeNode<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for PackageIndexTree<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub fn merge_index_trees<'a>(
    target_tree: &mut DynTree<TreeNode<'a>>,
    target_node_idx: NodeIdx<Dyn<TreeNode<'a>>>,
    source_tree: &DynTree<TreeNode<'a>>,
    source_node_idx: NodeIdx<Dyn<TreeNode<'a>>>,
) {
    for source_child in source_tree.node(source_node_idx).children() {
        let same_target_idx =
            target_tree
                .node(target_node_idx)
                .children()
                .find_map(|target_child| {
                    if target_child.data() == source_child.data() {
                        Some(target_child.idx())
                    } else {
                        None
                    }
                });

        if let Some(same_target_idx) = same_target_idx {
            merge_index_trees(
                target_tree,
                same_target_idx,
                source_tree,
                source_child.idx(),
            );
        } else {
            target_tree
                .node_mut(target_node_idx)
                .push_child_tree(source_child.as_cloned_subtree());
        }
    }
}

impl<'a> PackageIndexTree<'a> {
    pub fn from_ir(root: &'a Root, exclusions: &Exclusions<'a>) -> Self {
        fn walk_class<'a>(
            tree: &mut DynTree<TreeNode<'a>>,
            parent_idx: NodeIdx<Dyn<TreeNode<'a>>>,
            self_cell: ClassRef<'a>,
            exclusions: &Exclusions<'a>,
        ) {
            if exclusions.contains(self_cell) {
                return;
            }

            let self_idx = {
                let mut parent_mut = tree.node_mut(parent_idx);
                parent_mut.push_child(TreeNode::from(self_cell))
            };

            for child_cell in self_cell.children.iter().map(ClassRef::new) {
                walk_class(tree, self_idx, child_cell, exclusions);
            }
        }

        let mut tree = DynTree::new(TreeNode::Root);
        let root_idx = tree.root().idx();
        let ir = root.ir();

        for class_cell in ir.classes.iter().map(ClassRef::new) {
            walk_class(&mut tree, root_idx, class_cell, exclusions);
        }

        Self {
            package: ir.package.to_string(),
            inner: tree,
        }
    }

    pub fn package(&self) -> &str {
        &self.package
    }

    pub fn merge_with(&mut self, other: &Self) {
        let self_idx = self.inner.root().idx();
        let other_idx = other.inner.root().idx();

        merge_index_trees(&mut self.inner, self_idx, &other.inner, other_idx);
    }
}

#[derive(Debug, Clone)]
pub struct GlobalIndexTree<'a>(IndexTree<'a>);

impl<'a> Deref for GlobalIndexTree<'a> {
    type Target = DynTree<TreeNode<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GlobalIndexTree<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a, 'p> FromIterator<&'p PackageIndexTree<'a>> for GlobalIndexTree<'a> {
    fn from_iter<IT: IntoIterator<Item = &'p PackageIndexTree<'a>>>(iter: IT) -> Self {
        let mut tree = IndexTree::new(TreeNode::Root);
        let root_idx = tree.root().idx();

        for package_index_tree in iter {
            let package_idx = {
                let mut current_idx = root_idx;

                for node in package_index_tree.package().split('.').map(TreeNode::from) {
                    let node_idx = tree.node(current_idx).children().find_map(|x| {
                        if x.data() == &node {
                            Some(x.idx())
                        } else {
                            None
                        }
                    });

                    current_idx =
                        node_idx.unwrap_or_else(|| tree.node_mut(current_idx).push_child(node));
                }

                current_idx
            };

            let mut package_mut = tree.node_mut(package_idx);

            for child in package_index_tree.root().children() {
                package_mut.push_child_tree(child.as_cloned_subtree());
            }
        }

        Self(tree)
    }
}

impl<'a> GlobalIndexTree<'a> {
    pub fn search_path<'q, I>(&self, query: I) -> Option<ClassRef<'a>>
    where
        I: IntoIterator<Item = &'q str>,
    {
        let mut current_idx = self.0.root().idx();

        for ident in query {
            let node_idx = find_child(&self.0, current_idx, ident)?;

            current_idx = node_idx;
        }

        self.0.node(current_idx).data().resolved()
    }

    pub fn search(&self, query: &ir::Type<'_>) -> Option<ClassRef<'a>> {
        self.search_path(type_idents(query)?)
    }
}

fn find_child<'a>(
    tree: &IndexTree<'a>,
    parent_idx: NodeIdx<Dyn<TreeNode<'a>>>,
    ident: &str,
) -> Option<NodeIdx<Dyn<TreeNode<'a>>>> {
    tree.node(parent_idx).children().find_map(|child| {
        if child.data().ident() == Some(ident) {
            Some(child.idx())
        } else {
            None
        }
    })
}
