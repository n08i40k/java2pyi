use std::{
    hash::{Hash, Hasher},
    ops::Deref,
    ptr,
};

use bumpalo::Bump;

use crate::ir;

#[derive(Debug)]
pub struct Root {
    ir: ir::Root<'static>,
    #[allow(dead_code)]
    strings: Bump,
}

unsafe impl Sync for Root {}

impl Root {
    pub fn build<E>(
        build: impl for<'a> FnOnce(&'a Bump) -> Result<ir::Root<'a>, E>,
    ) -> Result<Self, E> {
        let strings = Bump::new();
        let ir = build(&strings)?;

        let ir = unsafe { std::mem::transmute::<ir::Root<'_>, ir::Root<'static>>(ir) };

        Ok(Self { ir, strings })
    }

    pub fn ir(&self) -> &ir::Root<'_> {
        &self.ir
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ClassRef<'a>(&'a ir::Class<'a>);

impl<'a> ClassRef<'a> {
    pub fn new(value: &'a ir::Class<'a>) -> Self {
        Self(value)
    }
}

impl<'a> Deref for ClassRef<'a> {
    type Target = ir::Class<'a>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl PartialEq for ClassRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl Eq for ClassRef<'_> {}

impl Hash for ClassRef<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::from_ref(self.0).hash(state);
    }
}

pub fn base_type<'a>(ty: &'a ir::Type<'a>) -> &'a ir::Type<'a> {
    match ty {
        ir::Type::Array(inner) => base_type(inner),
        _ => ty,
    }
}

pub fn array_depth(ty: &ir::Type<'_>) -> usize {
    match ty {
        ir::Type::Array(inner) => 1 + array_depth(inner),
        _ => 0,
    }
}

pub fn object_parts<'a>(
    ty: &'a ir::Type<'a>,
) -> Option<(&'a [&'a str], &'a [ir::SimpleObjectType<'a>])> {
    match base_type(ty) {
        ir::Type::Object(packages, types) => Some((packages, types)),
        _ => None,
    }
}

pub fn type_args<'a>(ty: &'a ir::Type<'a>) -> &'a [ir::Type<'a>] {
    object_parts(ty)
        .and_then(|(_, types)| types.last())
        .map(|last| last.type_args)
        .unwrap_or(&[])
}

pub fn type_idents<'a>(ty: &'a ir::Type<'a>) -> Option<impl Iterator<Item = &'a str> + Clone> {
    let (packages, types) = object_parts(ty)?;

    Some(
        packages
            .iter()
            .copied()
            .chain(types.iter().map(|part| part.ident)),
    )
}

pub fn primitive_python_type(ty: &ir::Type<'_>) -> Option<&'static str> {
    match base_type(ty) {
        ir::Type::Boolean => Some("bool"),
        ir::Type::Byte | ir::Type::Short | ir::Type::Int | ir::Type::Long => Some("int"),
        ir::Type::Char => Some("str"),
        ir::Type::Float | ir::Type::Double => Some("float"),
        ir::Type::Object(..)
        | ir::Type::Array(_)
        | ir::Type::ParameterRef(_)
        | ir::Type::ParameterUnbound
        | ir::Type::ParameterUpperBound(_)
        | ir::Type::ParameterLowerBound(_) => None,
    }
}
