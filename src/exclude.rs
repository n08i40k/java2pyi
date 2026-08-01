use std::{collections::HashSet, sync::Arc};

use crate::{
    ir,
    model::{ClassRef, Root},
};

#[derive(Debug, Default, Clone)]
pub struct Exclusions<'a> {
    excluded: HashSet<ClassRef<'a>>,
}

impl<'a> Exclusions<'a> {
    pub fn insert(&mut self, value: ClassRef<'a>) {
        self.excluded.insert(value);
    }

    pub fn contains(&self, value: ClassRef<'a>) -> bool {
        self.excluded.contains(&value)
    }
}

pub fn retain(
    roots: &mut Vec<Arc<Root>>,
    exclude_packages: &[String],
    exclude_identifiers: &HashSet<String>,
) {
    if exclude_packages.is_empty() && exclude_identifiers.is_empty() {
        return;
    }

    roots.retain(|root| {
        let ir = root.ir();

        !is_excluded_package(ir.package, exclude_packages)
            && ir
                .classes
                .iter()
                .any(|class| !exclude_identifiers.contains(&qualify(ir.package, class.name)))
    });
}

pub fn collect<'a>(
    roots: &'a [Arc<Root>],
    exclude_identifiers: &HashSet<String>,
) -> Exclusions<'a> {
    let mut exclusions = Exclusions::default();
    if exclude_identifiers.is_empty() {
        return exclusions;
    }

    for root in roots {
        let ir = root.ir();
        collect_into(ir.classes, ir.package, exclude_identifiers, &mut exclusions);
    }

    exclusions
}

fn is_excluded_package(package: &str, exclude_packages: &[String]) -> bool {
    exclude_packages.iter().any(|prefix| {
        package == prefix
            || package
                .strip_prefix(prefix)
                .is_some_and(|suffix| suffix.starts_with('.'))
    })
}

fn qualify(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{}.{}", prefix, name)
    }
}

fn collect_into<'a>(
    classes: &'a [ir::Class<'a>],
    prefix: &str,
    exclude_identifiers: &HashSet<String>,
    exclusions: &mut Exclusions<'a>,
) {
    for class in classes {
        let path = qualify(prefix, class.name);

        if exclude_identifiers.contains(&path) {
            exclusions.insert(ClassRef::new(class));
            continue;
        }

        collect_into(class.children, &path, exclude_identifiers, exclusions);
    }
}
