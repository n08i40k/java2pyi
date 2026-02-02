use std::{
    collections::HashMap,
    fs,
    ops::{Deref, DerefMut},
    path::Path,
    rc::Rc,
};

use itertools::Itertools;
use java_ast_parser::ast::{self, ClassCell, Modifiers, Root};
use topo_sort::{SortResults, TopoSort};

use crate::index_table::{
    GlobalIndexTable, ImportedIndexTable, IndexTable, LocalIndexTable, QualifiedIndexTable,
};

mod index_table;

/// Generate [imported index table][crate::index_table::ImportedIndexTable] from list of imports and [qualified index table][crate::index_table::QualifiedIndexTable].
fn generate_import_map<'a, I>(
    imports: &[impl AsRef<str>],
    qualified_index_tables: I,
) -> ImportedIndexTable
where
    I: IntoIterator<Item = &'a QualifiedIndexTable>,
{
    let mut imported_index_table = IndexTable::new();

    for qualified_index_table in qualified_index_tables {
        let s_keys = qualified_index_table
            .idt
            .keys()
            .map(|x| x.split('.').collect::<Box<[_]>>())
            .collect::<Box<[_]>>();

        for import in imports {
            let import = import.as_ref();
            let package = qualified_index_table.package.as_str();

            if !import.starts_with(package) {
                continue;
            }

            let Some((_, rem)) = import.split_at_checked(package.len() + 1) else {
                continue;
            };

            if rem == "*" {
                for (k, v) in &qualified_index_table.idt {
                    imported_index_table.insert(k.to_string(), v.clone());
                }
                break;
            }

            for s_key in &s_keys {
                let mut joined = rem.split('.').zip_longest(s_key.iter());

                let mut insert = false;
                let mut prev: Option<&str> = None;

                while let Some(v) = joined.next() {
                    insert = false;

                    match v {
                        itertools::EitherOrBoth::Both(import, ident) => {
                            if import == "*" {
                                let rem_idents = joined
                                    .filter_map(|v| match v {
                                        itertools::EitherOrBoth::Both(_, ident) => Some(ident),
                                        itertools::EitherOrBoth::Left(_) => None,
                                        itertools::EitherOrBoth::Right(ident) => Some(ident),
                                    })
                                    .join(".");

                                let key = if rem_idents.is_empty() {
                                    ident.to_string()
                                } else {
                                    format!("{}.{}", ident, rem_idents)
                                };

                                let class =
                                    qualified_index_table.idt.get(&s_key.join(".")).unwrap();

                                imported_index_table.insert(key, class.clone());

                                break;
                            }

                            if import != *ident {
                                break;
                            }

                            insert = true;
                            prev = Some(ident);
                        }
                        itertools::EitherOrBoth::Left(_) => break,
                        itertools::EitherOrBoth::Right(ident) => {
                            let Some(prev) = prev else {
                                break;
                            };

                            let rem_idents = joined
                                .filter_map(|v| match v {
                                    itertools::EitherOrBoth::Both(_, ident) => Some(ident),
                                    itertools::EitherOrBoth::Left(_) => None,
                                    itertools::EitherOrBoth::Right(ident) => Some(ident),
                                })
                                .join(".");

                            let scoped_key = format!("{}.{}", prev, ident);

                            let key = if rem_idents.is_empty() {
                                scoped_key
                            } else {
                                format!("{}.{}", scoped_key, rem_idents)
                            };

                            let class = qualified_index_table.idt.get(&s_key.join(".")).unwrap();

                            imported_index_table.insert(key, class.clone());

                            break;
                        }
                    }
                }

                if insert {
                    imported_index_table.insert(
                        s_key.last().unwrap().to_string(),
                        qualified_index_table
                            .idt
                            .get(&s_key.join("."))
                            .unwrap()
                            .clone(),
                    );
                }
            }
        }
    }

    ImportedIndexTable::from(imported_index_table)
}

/// Generate [QualifiedIndexTable<Class>][crate::index_table::QualifiedIndexTable] from [AST][java_ast_parser::ast::Root].
fn create_class_index_table(ast: &Root) -> QualifiedIndexTable {
    let mut idt = IndexTable::new();

    fn walk_class(idt: &mut IndexTable, pp: Option<&str>, class_rc: &ClassCell) {
        let class = class_rc.borrow();

        let cp = if let Some(pp) = pp {
            format!("{}.{}", pp, class.ident)
        } else {
            class.ident.to_string()
        };

        for class in &class.classes {
            walk_class(idt, Some(&cp), class);
        }

        idt.insert(cp, class_rc.clone());
    }

    for class in &ast.classes {
        walk_class(&mut idt, None, class);
    }

    QualifiedIndexTable {
        package: ast.package.to_string(),
        idt,
    }
}

/// Remove `private`, `protected` and `static` functions and variables from all classes in ast
/// recursively.
/// As we don't see them in Python (afaik).
fn remove_non_public_symbols(ast: &mut ast::Root) {
    fn walk_class(class: &mut java_ast_parser::ast::Class) {
        class.variables = class
            .variables
            .clone()
            .into_iter()
            .filter(|v| {
                v.modifiers.intersects(Modifiers::PUBLIC)
                    && !v.modifiers.intersects(Modifiers::STATIC | Modifiers::FINAL)
            })
            .collect();

        class.functions = class
            .functions
            .clone()
            .into_iter()
            .filter(|f| f.modifiers.intersects(Modifiers::PUBLIC))
            .collect();

        for class in class.classes.iter_mut() {
            walk_class(class.borrow_mut().deref_mut());
        }
    }

    for class in ast.classes.iter_mut() {
        walk_class(class.borrow_mut().deref_mut());
    }
}

/// Parse ast and convert it to owned ("disconnect" from string).
fn get_ast<P: AsRef<Path>>(path: P) -> Option<ast::Root> {
    let data = fs::read_to_string(path).unwrap();

    let mut ast = match java_ast_parser::parse(&data) {
        Ok(ast) => ast,
        Err(e) => {
            match &e {
                lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                    let line = data[..token.0].chars().filter(|&ch| ch == '\n').count() + 1;

                    let column = token.0 - data[..token.0].rfind("\n").unwrap_or(0);

                    let position = format!("line {}, column {}", line, column);
                    println!("{}\n{}", e, position);
                }
                _ => {
                    println!("{}", e);
                }
            };

            return None;
        }
    };

    remove_non_public_symbols(&mut ast);

    Some(ast)
}

/// ClassPtr -> Local Scope
/// # Examples:
/// ru.n08i40k.MyClass -> None
/// ru.n08i40k.MyClass.InnerClass -> Some("MyClass")
fn create_local_scope_map(ast: &ast::Root) -> HashMap<ClassCell, String> {
    let mut map: HashMap<ClassCell, String> = HashMap::new();

    fn walk_class(
        map: &mut HashMap<ClassCell, String>,
        scope: Option<&str>,
        class_cell: &ClassCell,
    ) {
        let class = class_cell.borrow();

        if let Some(scope) = &scope {
            map.insert(class_cell.clone(), scope.to_string());
        }

        let scope = if let Some(parent) = scope {
            format!("{}.{}", parent, &class.ident)
        } else {
            class.ident.to_string()
        };

        for class_cell in &class.classes {
            walk_class(map, Some(&scope), class_cell);
        }
    }

    for class_cell in &ast.classes {
        walk_class(&mut map, None, class_cell);
    }

    map
}

/// ChildPtr -> ParentPtr
fn infer_extends_map<'a, T: IntoIterator<Item = &'a (ClassCell, &'a LocalIndexTable)>>(
    scope_map: &HashMap<ClassCell, String>,
    classes: T,
) -> HashMap<ClassCell, ClassCell> {
    let mut map: HashMap<ClassCell, ClassCell> = HashMap::new();

    for (class_cell, local_index_table) in classes {
        let scope = scope_map.get(class_cell).map(|x| x.as_str());

        let extend_ident = if let Some(ast::Type { name: typename, .. }) =
            &class_cell.borrow().extends
            && let ast::TypeName::Ident(ident) = typename
        {
            Some(ident.clone())
        } else {
            None
        };

        if let Some(ident) = extend_ident
            && let Some(parent_cell) = local_index_table.search(scope, ident.as_str())
        {
            let mut class = class_cell.borrow_mut();

            class.extends = {
                let mut extends = class.extends.take().unwrap().clone();
                extends.name = ast::TypeName::ResolvedIdent(parent_cell.clone());

                Some(extends)
            };
        };

        for variable in &mut class_cell.borrow_mut().variables {
            if let ast::TypeName::Ident(ident) = &variable.r#type.name
                && let Some(type_cell) = local_index_table.search(scope, ident)
            {
                variable.r#type.name = ast::TypeName::ResolvedIdent(type_cell.clone());
            }
        }

        for function in &mut class_cell.borrow_mut().functions {
            if let ast::TypeName::Ident(ident) = &function.return_type.name
                && let Some(type_cell) = local_index_table.search(scope, ident)
            {
                function.return_type.name = ast::TypeName::ResolvedIdent(type_cell.clone());
            }
        }

        //map.insert(class_cell.clone(), parent_class.clone());
    }

    map
}

fn get_all_classes_recursively<'a, T: IntoIterator<Item = &'a Scope>>(
    scope_iter: T,
) -> Box<[(ClassCell, &'a LocalIndexTable)]> {
    let mut vec: Vec<(ClassCell, &'a LocalIndexTable)> = Vec::new();

    fn walk_class<'a>(
        vec: &mut Vec<(ClassCell, &'a LocalIndexTable)>,
        local_index_table: &'a LocalIndexTable,
        class: &ClassCell,
    ) {
        vec.push((class.clone(), local_index_table));

        for class in &class.borrow().classes {
            walk_class(vec, local_index_table, class);
        }
    }

    for scope in scope_iter {
        for class in &scope.ast.classes {
            walk_class(&mut vec, &scope.local_index_table, class);
        }
    }

    vec.into_boxed_slice()
}

// fn run_extends(ast: &mut ast::Root, local_index_table: &LocalIndexTable) {
//     fn exec_class(
//         scope: Option<&str>,
//         class: &mut ast::Class,
//         local_index_table: &LocalIndexTable,
//     ) {
//         let Some(ast::Type { name: typename, .. }) = &class.extends else {
//             return;
//         };
//
//         let ident = match typename {
//             ast::TypeName::Ident(ident) => ident,
//             ast::TypeName::QualifiedIdent(ident) => ident,
//             _ => return,
//         };
//
//         let Some(parent_class) = local_index_table.search(scope, ident) else {
//             return;
//         };
//     }
//
//     for class in ast.classes.iter_mut() {
//         // walk_class(class);
//     }
// }
//

fn extend<'a, T: IntoIterator<Item = &'a ClassCell>>(
    classes_iter: T,
    extends_map: &HashMap<ClassCell, ClassCell>,
) {
    for class_cell in classes_iter {
        let mut class = class_cell.borrow_mut();

        if class.extends.is_none() {
            continue;
        }

        let Some(parent_cell) = extends_map.get(class_cell) else {
            continue;
        };

        let parent = parent_cell.borrow();

        class.variables = {
            let mut variables = parent.variables.clone().into_vec();
            variables.extend_from_slice(&class.variables);
            variables.into_boxed_slice()
        };

        class.functions = {
            let mut functions = parent.functions.clone().into_vec();
            functions.extend_from_slice(&class.functions);
            functions.into_boxed_slice()
        };
    }
}
fn cleanup_unknown_extends<'a, T: IntoIterator<Item = &'a ClassCell>>(
    classes_iter: T,
    extends_map: &HashMap<ClassCell, ClassCell>,
) {
    for class_cell in classes_iter {
        let mut class = class_cell.borrow_mut();

        match class.extends {
            None => continue,
            Some(ast::Type {
                name: ast::TypeName::ResolvedIdent(_),
                ..
            }) => continue,
            _ => {}
        }

        class.extends = None;
    }
}
fn sort_and_filter_extendable_classes<'a, T: IntoIterator<Item = &'a ClassCell>>(
    classes_iter: T,
    classes_iter_len: usize,
) -> Option<Box<[ClassCell]>> {
    let mut topo_sort = TopoSort::with_capacity(classes_iter_len);

    for class_cell in classes_iter {
        if let Some(ast::Type {
            name: ast::TypeName::ResolvedIdent(type_cell),
            ..
        }) = &class_cell.borrow().extends
        {
            topo_sort.insert(class_cell.clone(), vec![type_cell.clone()]);
        }
    }

    let SortResults::Full(nodes) = topo_sort.into_vec_nodes() else {
        return None;
    };

    Some(nodes.into_boxed_slice())
}

struct Scope {
    pub ast: Rc<ast::Root>,
    pub local_index_table: LocalIndexTable,
}

impl Scope {
    pub fn from_slice(ast_slice: &[Rc<ast::Root>]) -> Box<[Self]> {
        let qualified_index_table_map = {
            let mut map: HashMap<*const ast::Root, QualifiedIndexTable> = HashMap::new();

            for ast in ast_slice.iter() {
                map.insert(
                    ast.deref() as *const ast::Root,
                    create_class_index_table(ast),
                );
            }

            map
        };

        let qualified_index_tables = qualified_index_table_map.values().collect::<Box<_>>();

        let global_index_table = Rc::new(GlobalIndexTable::from_iter(
            qualified_index_table_map.values(),
        ));

        ast_slice
            .iter()
            .map(|ast| {
                let imported_index_table = generate_import_map(
                    ast.imports
                        .iter()
                        .map(|x| x.as_str())
                        .collect::<Box<[_]>>()
                        .as_ref(),
                    qualified_index_table_map.values(),
                );

                let qualified_index_table = qualified_index_tables
                    .iter()
                    .filter_map(|v| {
                        if v.package != ast.package {
                            return None;
                        }

                        Some(v.idt.clone())
                    })
                    .reduce(|l, r| {
                        let mut n = HashMap::new();

                        n.extend(l);
                        n.extend(r);

                        n
                    })
                    .map(|v| QualifiedIndexTable {
                        package: ast.package.clone(),
                        idt: v,
                    })
                    .unwrap();

                let local_index_table = LocalIndexTable::new(
                    global_index_table.clone(),
                    imported_index_table,
                    qualified_index_table,
                );

                Scope {
                    ast: ast.clone(),
                    local_index_table,
                }
            })
            .collect::<Box<_>>()
    }
}

#[derive(Debug)]
pub enum Error {
    CircullarDependency,
}

fn preprocess_ast(ast_slice: &[Rc<ast::Root>], inherit_by_merge: bool) -> Result<(), Error> {
    let scopes = Scope::from_slice(ast_slice);

    let classes_with_index_table = get_all_classes_recursively(&scopes);

    let scoped_map = ast_slice
        .iter()
        .map(|x| create_local_scope_map(x.deref()))
        .reduce(|l, r| {
            let mut n = HashMap::new();

            n.extend(l);
            n.extend(r);

            n
        })
        .unwrap_or(HashMap::new());

    let extends_map = infer_extends_map(&scoped_map, &classes_with_index_table);

    let classes = classes_with_index_table
        .into_iter()
        .map(|(x, _)| x)
        .collect::<Box<_>>();

    cleanup_unknown_extends(&classes, &extends_map);

    if inherit_by_merge {
        let nodes = sort_and_filter_extendable_classes(&classes, classes.len())
            .ok_or(Error::CircullarDependency)?;

        extend(&nodes, &extends_map);
    }

    Ok(())
}

fn main() {
    let asts = [
        get_ast("java/example1.java").unwrap(),
        get_ast("java/example2.java").unwrap(),
    ]
    .into_iter()
    .map(Rc::new)
    .collect::<Box<_>>();

    preprocess_ast(&asts, true).unwrap();

    dbg!(asts);
}
