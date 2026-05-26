use java_ast_parser::ast;
use orx_tree::{Bfs, Dyn, DynTree, NodeIdx, NodeRef};
use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    sync::Arc,
};

use crate::model::{
    ClassRef, EnumRef, Exclusions, InterfaceRef, ResolvedType, Root, TypeRef, qualified_type_idents,
};

#[derive(Debug, Clone)]
pub enum TreeNode {
    Root,
    Package(Arc<String>),
    Class(ClassRef),
    Enum(EnumRef),
    Interface(InterfaceRef),
}

impl ResolvedType {
    fn from_node(node: &TreeNode) -> Option<Self> {
        match node {
            TreeNode::Class(class_cell) => Some(Self::Class(*class_cell)),
            TreeNode::Interface(interface_cell) => Some(Self::Interface(*interface_cell)),
            _ => None,
        }
    }
}

impl TreeNode {
    pub fn ident(&self) -> Option<&'_ str> {
        match self {
            TreeNode::Root => None,
            TreeNode::Package(ident) => Some(ident.as_str()),
            TreeNode::Class(cell) => Some(cell.ident()),
            TreeNode::Enum(cell) => Some(cell.ident()),
            TreeNode::Interface(cell) => Some(cell.ident()),
        }
    }
}

impl std::hash::Hash for TreeNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if let Some(ident) = self.ident() {
            ident.hash(state);
        } else {
            0.hash(state);
        }
    }
}

impl std::cmp::PartialEq for TreeNode {
    fn eq(&self, other: &Self) -> bool {
        match (self.ident(), other.ident()) {
            (None, None) => true,
            (Some(l), Some(r)) => l == r,
            _ => false,
        }
    }
}

impl std::cmp::Eq for TreeNode {}

impl From<&str> for TreeNode {
    fn from(value: &str) -> Self {
        Self::Package(Arc::from(value.to_string()))
    }
}

impl From<ClassRef> for TreeNode {
    fn from(value: ClassRef) -> Self {
        Self::Class(value)
    }
}

impl From<InterfaceRef> for TreeNode {
    fn from(value: InterfaceRef) -> Self {
        Self::Interface(value)
    }
}

impl From<EnumRef> for TreeNode {
    fn from(value: EnumRef) -> Self {
        Self::Enum(value)
    }
}
pub type IndexTree = DynTree<TreeNode>;

#[derive(Debug, Clone)]
pub struct SharedLocalIndex {
    tree: Arc<IndexTree>,
    reverse_local: Arc<HashMap<ClassRef, NodeIdx<Dyn<TreeNode>>>>,
}

#[derive(Debug, Clone)]
pub struct PackageIndexTree {
    package: String,
    inner: IndexTree,
}

impl Deref for PackageIndexTree {
    type Target = DynTree<TreeNode>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for PackageIndexTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub fn merge_index_trees(
    target_tree: &mut DynTree<TreeNode>,
    target_node_idx: NodeIdx<Dyn<TreeNode>>,
    source_tree: &DynTree<TreeNode>,
    source_node_idx: NodeIdx<Dyn<TreeNode>>,
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

impl PackageIndexTree {
    pub fn from_ast(root: &Root, exclusions: &Exclusions) -> Self {
        fn walk_interface(
            tree: &mut DynTree<TreeNode>,
            parent_idx: NodeIdx<Dyn<TreeNode>>,
            self_cell: InterfaceRef,
            exclusions: &Exclusions,
        ) {
            if exclusions.contains(TypeRef::Interface(self_cell)) {
                return;
            }

            let self_idx = {
                let mut parent_mut = tree.node_mut(parent_idx);
                parent_mut.push_child(TreeNode::from(self_cell))
            };

            let self_ref = self_cell.borrow();

            for class_cell in self_ref.classes.iter().map(ClassRef::new) {
                walk_class(tree, self_idx, class_cell, exclusions);
            }

            for interface_cell in self_ref.interfaces.iter().map(InterfaceRef::new) {
                walk_interface(tree, self_idx, interface_cell, exclusions);
            }

            for enum_cell in self_ref.enums.iter().map(EnumRef::new) {
                walk_enum(tree, self_idx, enum_cell, exclusions);
            }
        }

        fn walk_class(
            tree: &mut DynTree<TreeNode>,
            parent_idx: NodeIdx<Dyn<TreeNode>>,
            self_cell: ClassRef,
            exclusions: &Exclusions,
        ) {
            if exclusions.contains(TypeRef::Class(self_cell)) {
                return;
            }

            let self_idx = {
                let mut parent_mut = tree.node_mut(parent_idx);
                parent_mut.push_child(TreeNode::from(self_cell))
            };

            let self_ref = self_cell.borrow();

            for class_cell in self_ref.classes.iter().map(ClassRef::new) {
                walk_class(tree, self_idx, class_cell, exclusions);
            }

            for interface_cell in self_ref.interfaces.iter().map(InterfaceRef::new) {
                walk_interface(tree, self_idx, interface_cell, exclusions);
            }

            for enum_cell in self_ref.enums.iter().map(EnumRef::new) {
                walk_enum(tree, self_idx, enum_cell, exclusions);
            }
        }

        fn walk_enum(
            tree: &mut DynTree<TreeNode>,
            parent_idx: NodeIdx<Dyn<TreeNode>>,
            self_cell: EnumRef,
            exclusions: &Exclusions,
        ) {
            if exclusions.contains(TypeRef::Enum(self_cell)) {
                return;
            }

            let self_idx = {
                let mut parent_mut = tree.node_mut(parent_idx);
                parent_mut.push_child(TreeNode::from(self_cell))
            };

            let self_ref = self_cell.borrow();

            for class_cell in self_ref.classes.iter().map(ClassRef::new) {
                walk_class(tree, self_idx, class_cell, exclusions);
            }

            for interface_cell in self_ref.interfaces.iter().map(InterfaceRef::new) {
                walk_interface(tree, self_idx, interface_cell, exclusions);
            }

            for enum_cell in self_ref.enums.iter().map(EnumRef::new) {
                walk_enum(tree, self_idx, enum_cell, exclusions);
            }
        }

        let mut tree = DynTree::new(TreeNode::Root);
        let root_idx = tree.root().idx();
        let ast = root.ast();

        for interface_cell in ast.interfaces.iter().map(InterfaceRef::new) {
            walk_interface(&mut tree, root_idx, interface_cell, exclusions);
        }

        for class_cell in ast.classes.iter().map(ClassRef::new) {
            walk_class(&mut tree, root_idx, class_cell, exclusions);
        }

        for enum_cell in ast.enums.iter().map(EnumRef::new) {
            walk_enum(&mut tree, root_idx, enum_cell, exclusions);
        }

        Self {
            package: ast.package.to_string(),
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

    pub fn shared_local_index(&self) -> SharedLocalIndex {
        let tree = Arc::new(self.inner.clone());
        let mut reverse_local = HashMap::new();

        for idx in tree.root().indices::<Bfs>() {
            let TreeNode::Class(class_cell) = tree.node(idx).data() else {
                continue;
            };

            reverse_local.insert(*class_cell, idx);
        }

        SharedLocalIndex {
            tree,
            reverse_local: Arc::new(reverse_local),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalIndexTree(IndexTree);

impl Deref for GlobalIndexTree {
    type Target = DynTree<TreeNode>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for GlobalIndexTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> FromIterator<&'a PackageIndexTree> for GlobalIndexTree {
    fn from_iter<IT: IntoIterator<Item = &'a PackageIndexTree>>(iter: IT) -> Self {
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

impl GlobalIndexTree {
    pub fn search_path<'a, I>(&self, query: I) -> Option<ResolvedType>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut current_idx = self.0.root().idx();

        for ident in query {
            let node_idx = find_child(&self.0, current_idx, ident)?;

            current_idx = node_idx;
        }

        ResolvedType::from_node(self.0.node(current_idx).data())
    }

    pub fn search(&self, query: &ast::QualifiedType<'_>) -> Option<ResolvedType> {
        let parts = qualified_type_idents(query)?;

        self.search_path(parts)
    }
}

#[derive(Debug, Clone)]
pub struct ImportedIndexTree {
    global: Arc<GlobalIndexTree>,
    imports: Box<[ImportPath]>,
}

#[derive(Debug, Clone)]
enum ImportPath {
    Wildcard { prefix: Box<[String]> },
    Exact { first: String, parts: Box<[String]> },
}

impl ImportedIndexTree {
    pub fn from_imports<'a, I>(import_iter: I, global_index_tree: Arc<GlobalIndexTree>) -> Self
    where
        I: IntoIterator<Item = &'a str>,
    {
        let iter = import_iter.into_iter();
        let (lower_bound, _) = iter.size_hint();
        let mut seen = HashSet::with_capacity(lower_bound);
        let mut imports = Vec::with_capacity(lower_bound);
        for import in iter {
            if !seen.insert(import) {
                continue;
            }

            if let Some(prefix) = import.strip_suffix(".*") {
                imports.push(ImportPath::Wildcard {
                    prefix: split_import(prefix),
                });
            } else {
                let parts = split_import(import);
                if let Some(first) = parts.last().cloned() {
                    imports.push(ImportPath::Exact { first, parts });
                }
            }
        }

        Self {
            global: global_index_tree,
            imports: imports.into_boxed_slice(),
        }
    }

    pub fn search(&self, query: &ast::QualifiedType<'_>) -> Option<ResolvedType> {
        let query_parts = qualified_type_idents(query)?;
        let first_query_part = query_parts.clone().next()?;

        for import in &self.imports {
            let resolved = match import {
                ImportPath::Wildcard { prefix } => self
                    .global
                    .search_path(prefix.iter().map(String::as_str).chain(query_parts.clone())),
                ImportPath::Exact { first, parts } => {
                    if first.as_str() != first_query_part {
                        continue;
                    }

                    self.global.search_path(
                        parts[..parts.len().saturating_sub(1)]
                            .iter()
                            .map(String::as_str)
                            .chain(query_parts.clone()),
                    )
                }
            };

            if resolved.is_some() {
                return resolved;
            }
        }

        None
    }
}

#[derive(Debug, Clone)]
pub struct LocalIndexTree {
    global: Arc<GlobalIndexTree>,
    imported: ImportedIndexTree,
    local: Arc<IndexTree>,
    reverse_local: Arc<HashMap<ClassRef, NodeIdx<Dyn<TreeNode>>>>,
}

impl LocalIndexTree {
    pub fn new(
        global: Arc<GlobalIndexTree>,
        imported: ImportedIndexTree,
        shared_local: SharedLocalIndex,
    ) -> Self {
        Self {
            global,
            imported,
            local: shared_local.tree,
            reverse_local: shared_local.reverse_local,
        }
    }

    pub fn search_global(&self, query: &ast::QualifiedType<'_>) -> Option<ResolvedType> {
        self.global.search(query)
    }

    pub fn search_imported(&self, query: &ast::QualifiedType<'_>) -> Option<ResolvedType> {
        self.imported.search(query)
    }

    pub fn search_local(
        &self,
        scope: Option<ClassRef>,
        query: &ast::QualifiedType<'_>,
    ) -> Option<ResolvedType> {
        let query_parts = qualified_type_idents(query)?;
        let mut current_scope = scope;

        loop {
            let root_idx = current_scope
                .and_then(|x| self.reverse_local.get(&x))
                .cloned()
                .unwrap_or_else(|| self.local.root().idx());
            let mut current_idx = root_idx;
            let mut matched = true;

            for ident in query_parts.clone() {
                let Some(node_idx) = find_child(&self.local, current_idx, ident) else {
                    matched = false;
                    break;
                };

                current_idx = node_idx;
            }

            if matched
                && let Some(resolved) = ResolvedType::from_node(self.local.node(current_idx).data())
            {
                return Some(resolved);
            }

            current_scope?;
            let parent_node = self.local.node(root_idx).parent()?;
            current_scope = if let TreeNode::Class(class_cell) = parent_node.data() {
                Some(*class_cell)
            } else {
                None
            };
        }
    }

    pub fn search(
        &self,
        scope: Option<ClassRef>,
        query: &ast::QualifiedType<'_>,
    ) -> Option<ResolvedType> {
        self.search_local(scope, query)
            .or_else(|| self.search_imported(query))
            .or_else(|| self.search_global(query))
    }
}

fn find_child(
    tree: &IndexTree,
    parent_idx: NodeIdx<Dyn<TreeNode>>,
    ident: &str,
) -> Option<NodeIdx<Dyn<TreeNode>>> {
    tree.node(parent_idx).children().find_map(|child| {
        if child.data().ident() == Some(ident) {
            Some(child.idx())
        } else {
            None
        }
    })
}

fn split_import(import: &str) -> Box<[String]> {
    import.split('.').map(str::to_string).collect()
}
