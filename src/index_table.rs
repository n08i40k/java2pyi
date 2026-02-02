use java_ast_parser::ast::ClassCell;
use std::{collections::HashMap, rc::Rc};

pub type IndexTable = HashMap<String, ClassCell>;

#[derive(Debug, Clone)]
pub struct QualifiedIndexTable {
    pub package: String,
    pub idt: IndexTable,
}

#[derive(Debug, Clone)]
pub struct GlobalIndexTable(IndexTable);

impl<'a> FromIterator<&'a QualifiedIndexTable> for GlobalIndexTable {
    fn from_iter<IT: IntoIterator<Item = &'a QualifiedIndexTable>>(iter: IT) -> Self {
        let mut idt = IndexTable::new();

        for i in iter {
            for (k, v) in &i.idt {
                idt.insert(format!("{}.{}", i.package, k), v.clone());
            }
        }

        Self(idt)
    }
}

impl GlobalIndexTable {
    pub fn search(&self, query: &str) -> Option<ClassCell> {
        self.0.get(query).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct ImportedIndexTable(IndexTable);

impl From<IndexTable> for ImportedIndexTable {
    fn from(value: IndexTable) -> Self {
        Self(value)
    }
}

impl ImportedIndexTable {
    pub fn search(&self, query: &str) -> Option<ClassCell> {
        self.0.get(query).cloned()
    }
}

#[derive(Debug, Clone)]
pub struct LocalIndexTable {
    global: Rc<GlobalIndexTable>,
    imported: ImportedIndexTable,
    local: QualifiedIndexTable,
}

impl LocalIndexTable {
    pub fn new(
        global: Rc<GlobalIndexTable>,
        imported: ImportedIndexTable,
        local: QualifiedIndexTable,
    ) -> Self {
        Self {
            global,
            imported,
            local,
        }
    }
    pub fn search_global(&self, query: &str) -> Option<ClassCell> {
        self.global.search(query)
    }

    pub fn search_imported(&self, query: &str) -> Option<ClassCell> {
        self.imported.search(query)
    }

    pub fn search_local(&self, scope: Option<&str>, query: &str) -> Option<ClassCell> {
        self.local
            .idt
            .keys()
            .find_map(|key| {
                if let Some(scope) = scope
                    && !key.starts_with(scope)
                {
                    return None;
                }

                let (_, rem) = key.split_at_checked(scope.map(|x| x.len() + 1).unwrap_or(0))?;

                // dbg!(&rem, &query);
                if rem == query {
                    self.local.idt.get(key).cloned()
                } else {
                    None
                }
            })
            .or_else(|| {
                let scope = scope?;

                if !scope.contains('.') {
                    // println!("------- LOWER -------");
                    return self.search_local(None, query);
                }

                // println!("------- LOWER -------");

                self.search_local(Some(scope.rsplit_once('.').unwrap().0), query)
            })
    }

    pub fn search(&self, scope: Option<&str>, query: &str) -> Option<ClassCell> {
        self.search_local(scope, query)
            .or_else(|| self.search_imported(query))
            .or_else(|| self.search_global(query))
    }
}
