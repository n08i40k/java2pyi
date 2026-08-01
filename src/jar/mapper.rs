use std::borrow::Cow;

use bumpalo::Bump;
use jvm_class::ClassAccessFlags;

use crate::ir::{self, FromUnderlying};
use crate::jar::{ParsedClass, ParsedUnit};

pub fn map_root<'a>(bump: &'a Bump, unit: &ParsedUnit<'_>) -> anyhow::Result<ir::Root<'a>> {
    let class = map_class(bump, unit, unit.binary_name, &unit.main)?;

    Ok(ir::Root {
        package: ir::intern(bump, Cow::Borrowed(unit.package)),
        classes: bump.alloc_slice_fill_iter(std::iter::once(class)),
    })
}

pub fn map_class<'a>(
    bump: &'a Bump,
    unit: &ParsedUnit<'_>,
    binary_name: &str,
    class: &jvm_class::Class<'_>,
) -> anyhow::Result<ir::Class<'a>> {
    let mut mapped = ir::Class::from_underlying(bump, &class.constant_pool, class)?;

    let mut children = Vec::new();
    for child in unit.children_of(binary_name).collect::<Vec<_>>() {
        if skip_nested(child) {
            continue;
        }

        let mut mapped_child = map_class(bump, unit, child.binary_name, &child.class)?;

        if is_anonymous(child) {
            mapped_child.rename(bump, flat_name(bump, child.simple_name()));
            mapped_child.anonymous = Some(qualified_name(bump, child.binary_name));
        }

        children.push(mapped_child);
    }

    mapped.children = bump.alloc_slice_fill_iter(children);

    Ok(mapped)
}

fn skip_nested(child: &ParsedClass<'_>) -> bool {
    child
        .class
        .access_flags
        .contains(ClassAccessFlags::SYNTHETIC)
}

fn is_anonymous(child: &ParsedClass<'_>) -> bool {
    let simple_name = child.simple_name();
    let nested_name = simple_name.rsplit('$').next().unwrap_or(simple_name);

    nested_name.starts_with(|ch: char| ch.is_ascii_digit())
}

fn flat_name<'a>(bump: &'a Bump, simple_name: &str) -> &'a str {
    ir::intern(bump, Cow::Owned(simple_name.replace('$', "_")))
}

fn qualified_name<'a>(bump: &'a Bump, binary_name: &str) -> &'a str {
    ir::intern(bump, Cow::Owned(binary_name.replace('/', ".")))
}
