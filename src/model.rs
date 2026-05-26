use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
    slice,
};

use java_ast_parser::{RootCell, ast};

#[derive(Debug)]
pub struct Root {
    cell: RootCell,
}

impl Root {
    pub fn new(cell: RootCell) -> Self {
        Self { cell }
    }

    pub fn ast(&self) -> &ast::Root<'_> {
        self.cell.root()
    }
}

macro_rules! ast_ref {
    ($name:ident, $target:ident) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name(usize);

        impl $name {
            pub fn new(value: &ast::$target<'_>) -> Self {
                Self(value as *const ast::$target<'_> as usize)
            }

            pub fn borrow(&self) -> &ast::$target<'static> {
                // SAFETY: references are created from parser nodes owned by RootCell values
                // retained for the whole processing pipeline. The parser AST is immutable.
                unsafe { &*(self.0 as *const ast::$target<'static>) }
            }

            pub fn ident(&self) -> &str {
                self.borrow().name
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl Eq for $name {}

        impl Hash for $name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        unsafe impl Send for $name {}
        unsafe impl Sync for $name {}
    };
}

ast_ref!(ClassRef, Class);
ast_ref!(InterfaceRef, Interface);
ast_ref!(EnumRef, Enum);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeRef {
    Class(ClassRef),
    Interface(InterfaceRef),
    Enum(EnumRef),
}

#[derive(Debug, Default, Clone)]
pub struct Exclusions {
    excluded: HashSet<TypeRef>,
}

impl Exclusions {
    pub fn insert(&mut self, value: TypeRef) {
        self.excluded.insert(value);
    }

    pub fn contains(&self, value: TypeRef) -> bool {
        self.excluded.contains(&value)
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedType {
    Class(ClassRef),
    Interface(InterfaceRef),
}

pub fn base_type<'a>(ty: &'a ast::Type<'a>) -> &'a ast::Type<'a> {
    match ty {
        ast::Type::Array(inner) => base_type(inner),
        _ => ty,
    }
}

pub fn array_depth(ty: &ast::Type<'_>) -> usize {
    match ty {
        ast::Type::Array(inner) => 1 + array_depth(inner),
        _ => 0,
    }
}

pub fn named_type<'a>(ty: &'a ast::Type<'a>) -> Option<(&'a str, &'a [ast::GenericImpl<'a>])> {
    match base_type(ty) {
        ast::Type::Named {
            name,
            generic_impls,
        } => Some((name, generic_impls)),
        _ => None,
    }
}

#[derive(Clone)]
pub struct QualifiedTypeIdents<'a> {
    parts: slice::Iter<'a, ast::Type<'a>>,
}

impl<'a> Iterator for QualifiedTypeIdents<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        self.parts.next().map(|part| {
            named_type(part)
                .map(|(name, _)| name)
                .expect("qualified type ident iterator is prevalidated")
        })
    }
}

pub fn qualified_type_idents<'a>(
    ty: &'a ast::QualifiedType<'a>,
) -> Option<QualifiedTypeIdents<'a>> {
    for part in ty {
        named_type(part)?;
    }

    Some(QualifiedTypeIdents { parts: ty.iter() })
}

pub fn primitive_python_type<'a>(ty: &'a ast::Type<'a>) -> Option<&'static str> {
    match base_type(ty) {
        ast::Type::Void => Some("None"),
        ast::Type::Boolean => Some("bool"),
        ast::Type::Byte => Some("int"),
        ast::Type::Char => Some("str"),
        ast::Type::Short | ast::Type::Integer | ast::Type::Long => Some("int"),
        ast::Type::Float | ast::Type::Double => Some("float"),
        ast::Type::Named { .. } | ast::Type::Array(_) => None,
    }
}
