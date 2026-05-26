use bitflags::bitflags;
use std::ops::{Deref, DerefMut};

bitflags! {
    #[derive(Debug, Clone, PartialEq)]
    pub struct Modifiers: u16 {
        const PUBLIC        = 1 << 0;
        const PROTECTED     = 1 << 1;
        const PRIVATE       = 1 << 2;

        const STATIC        = 1 << 3;
        const FINAL         = 1 << 4;
        const SEALED        = 1 << 5;
        const NON_SEALED    = 1 << 6;
        const ABSTRACT      = 1 << 7;

        const NATIVE        = 1 << 8;
        const SYNCHRONIZED  = 1 << 9;
        const TRANSIENT     = 1 << 10;
        const VOLATILE      = 1 << 11;
        const STRICTFP      = 1 << 12;

        const DEFAULT       = 1 << 13;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedType<'a>(Box<[Type<'a>]>);

impl<'a> QualifiedType<'a> {
    pub fn new(items: Box<[Type<'a>]>) -> Self {
        Self(items)
    }

    pub fn empty() -> Self {
        Self(Box::new([]))
    }

    pub fn into_inner(self) -> Box<[Type<'a>]> {
        self.0
    }
}

impl<'a> Default for QualifiedType<'a> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<'a> From<Box<[Type<'a>]>> for QualifiedType<'a> {
    fn from(items: Box<[Type<'a>]>) -> Self {
        Self::new(items)
    }
}

impl<'a, const N: usize> From<[Type<'a>; N]> for QualifiedType<'a> {
    fn from(items: [Type<'a>; N]) -> Self {
        Self::new(Box::new(items))
    }
}

impl<'a> Deref for QualifiedType<'a> {
    type Target = [Type<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for QualifiedType<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> IntoIterator for QualifiedType<'a> {
    type Item = Type<'a>;
    type IntoIter = std::vec::IntoIter<Type<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_vec().into_iter()
    }
}

impl<'a, 'b> IntoIterator for &'b QualifiedType<'a> {
    type Item = &'b Type<'a>;
    type IntoIter = std::slice::Iter<'b, Type<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, 'b> IntoIterator for &'b mut QualifiedType<'a> {
    type Item = &'b mut Type<'a>;
    type IntoIter = std::slice::IterMut<'b, Type<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericWildcardBoundary<'a> {
    None,
    Extends(QualifiedType<'a>),
    Super(QualifiedType<'a>),
}

impl<'a> std::fmt::Display for GenericWildcardBoundary<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericWildcardBoundary::None => write!(f, "?"),
            GenericWildcardBoundary::Extends(items) => write!(
                f,
                "? extends {}",
                items
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
            GenericWildcardBoundary::Super(items) => write!(
                f,
                "? super {}",
                items
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericImpl<'a> {
    Type(QualifiedType<'a>),
    Wildcard(GenericWildcardBoundary<'a>),
}

impl<'a> From<QualifiedType<'a>> for GenericImpl<'a> {
    fn from(ty: QualifiedType<'a>) -> Self {
        Self::Type(ty)
    }
}

impl<'a> From<GenericWildcardBoundary<'a>> for GenericImpl<'a> {
    fn from(wildcard: GenericWildcardBoundary<'a>) -> Self {
        Self::Wildcard(wildcard)
    }
}

impl<'a> std::fmt::Display for GenericImpl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericImpl::Type(items) => write!(
                f,
                "{}",
                items
                    .iter()
                    .map(|part| part.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            ),
            GenericImpl::Wildcard(wildcard_boundary) => wildcard_boundary.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Void,
    Boolean,
    Byte,
    Char,
    Short,
    Integer,
    Long,
    Float,
    Double,
    Named {
        name: &'a str,
        generic_impls: Box<[GenericImpl<'a>]>,
    },
    Array(Box<Type<'a>>),
}

impl<'a> Type<'a> {
    pub fn wrap_by_array(&mut self, depth: usize) {
        for _ in 0..depth {
            let inner = std::mem::replace(self, Self::Void);
            *self = Self::Array(Box::new(inner));
        }
    }
}

pub(crate) struct QualifiedTypeBuilder<'a> {
    ty: QualifiedType<'a>,
}

impl<'a> QualifiedTypeBuilder<'a> {
    pub(crate) fn new(ty: impl Into<QualifiedType<'a>>) -> Self {
        Self { ty: ty.into() }
    }

    pub(crate) fn with_array_depth(mut self, depth: usize) -> Self {
        if depth > 0 {
            self.ty
                .last_mut()
                .expect("qualified type must contain at least one type")
                .wrap_by_array(depth);
        }

        self
    }

    pub(crate) fn build(self) -> QualifiedType<'a> {
        self.ty
    }
}

impl<'a> std::fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Boolean => write!(f, "boolean"),
            Self::Byte => write!(f, "byte"),
            Self::Char => write!(f, "char"),
            Self::Short => write!(f, "short"),
            Self::Integer => write!(f, "integer"),
            Self::Long => write!(f, "long"),
            Self::Float => write!(f, "float"),
            Self::Double => write!(f, "double"),
            Self::Named {
                name,
                generic_impls,
            } => {
                if generic_impls.is_empty() {
                    write!(f, "{name}")
                } else {
                    write!(
                        f,
                        "{name}<{}>",
                        generic_impls
                            .iter()
                            .map(|part| part.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
            }
            Self::Array(inner_type) => write!(f, "{inner_type}[]"),
        }
    }
}

pub(crate) struct TypeBuilder<'a, const NAMED: bool> {
    ty: Type<'a>,
}

impl<'a, const NAMED: bool> TypeBuilder<'a, NAMED> {
    pub(crate) fn wrap_by_array(mut self, depth: usize) -> Self {
        for _ in 0..depth {
            self.ty = Type::Array(Box::new(self.ty));
        }

        self
    }

    pub(crate) fn build(self) -> Type<'a> {
        self.ty
    }
}

impl<'a> TypeBuilder<'a, false> {
    pub(crate) fn any(ty: Type<'a>) -> TypeBuilder<'a, false> {
        TypeBuilder { ty }
    }
}

impl<'a> TypeBuilder<'a, true> {
    pub(crate) fn named(name: &'a str) -> TypeBuilder<'a, true> {
        TypeBuilder {
            ty: Type::Named {
                name,
                generic_impls: Box::new([]),
            },
        }
    }

    pub(crate) fn with_generic_impls(mut self, generic_impls: Box<[GenericImpl<'a>]>) -> Self {
        match self.ty {
            Type::Named { name, .. } => {
                self.ty = Type::Named {
                    name,
                    generic_impls,
                }
            }
            _ => unreachable!(),
        }

        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'a> {
    pub modifiers: Modifiers,
    pub r#type: QualifiedType<'a>,
    pub name: &'a str,
}

pub(crate) struct FieldBuilder<'a> {
    modifiers: Modifiers,
    r#type: QualifiedType<'a>,
    name: &'a str,
}

impl<'a> FieldBuilder<'a> {
    pub(crate) fn single(name: &'a str) -> Self {
        Self {
            modifiers: Modifiers::empty(),
            r#type: QualifiedType::empty(),
            name,
        }
    }

    pub(crate) fn build(self) -> Field<'a> {
        Field {
            modifiers: self.modifiers,
            r#type: self.r#type,
            name: self.name,
        }
    }

    pub(crate) fn with_modifiers(mut self, modifiers: Modifiers) -> Self {
        self.modifiers.insert(modifiers);
        self
    }

    pub(crate) fn with_type(mut self, ty: QualifiedType<'a>) -> Self {
        self.r#type = ty;
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericDecl<'a> {
    pub name: &'a str,
    pub extends: Box<[QualifiedType<'a>]>,
}

impl<'a> std::fmt::Display for GenericDecl<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.extends.is_empty() {
            write!(f, "{}", self.name)
        } else {
            write!(
                f,
                "{} extends {}",
                self.name,
                self.extends
                    .iter()
                    .map(|extend| extend
                        .iter()
                        .map(|part| part.to_string())
                        .collect::<Vec<_>>()
                        .join("."))
                    .collect::<Vec<_>>()
                    .join(".")
            )
        }
    }
}

pub(crate) struct GenericDeclBuilder<'a> {
    name: &'a str,
    extends: Vec<QualifiedType<'a>>,
}

impl<'a> GenericDeclBuilder<'a> {
    pub(crate) fn new(name: &'a str) -> Self {
        Self {
            name,
            extends: Vec::new(),
        }
    }

    pub(crate) fn with_extends(
        mut self,
        extends: impl IntoIterator<Item = QualifiedType<'a>>,
    ) -> Self {
        self.extends.extend(extends);
        self
    }

    pub(crate) fn build(self) -> GenericDecl<'a> {
        GenericDecl {
            name: self.name,
            extends: self.extends.into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArgument<'a> {
    pub modifiers: Modifiers,
    pub r#type: QualifiedType<'a>,
    pub name: &'a str,
    pub vararg: bool,
}

pub(crate) struct FunctionArgumentBuilder<'a> {
    modifiers: Modifiers,
    r#type: QualifiedType<'a>,
    name: &'a str,
    vararg: bool,
}

impl<'a> FunctionArgumentBuilder<'a> {
    pub(crate) fn new(name: &'a str) -> Self {
        Self {
            modifiers: Modifiers::empty(),
            r#type: QualifiedType::empty(),
            name,
            vararg: false,
        }
    }

    pub(crate) fn with_modifiers(mut self, modifiers: Modifiers) -> Self {
        self.modifiers.insert(modifiers);
        self
    }

    pub(crate) fn with_type(mut self, ty: QualifiedType<'a>) -> Self {
        self.r#type = ty;
        self
    }

    pub(crate) fn with_vararg(mut self) -> Self {
        self.vararg = true;
        self
    }

    pub(crate) fn build(self) -> FunctionArgument<'a> {
        FunctionArgument {
            modifiers: self.modifiers,
            r#type: self.r#type,
            name: self.name,
            vararg: self.vararg,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a> {
    pub modifiers: Modifiers,
    pub generic_decls: Box<[GenericDecl<'a>]>,
    pub return_type: QualifiedType<'a>,
    pub name: &'a str,
    pub args: Box<[FunctionArgument<'a>]>,
}

pub(crate) struct FunctionBuilder<'a> {
    modifiers: Modifiers,
    generic_decls: Vec<GenericDecl<'a>>,
    return_type: QualifiedType<'a>,
    name: &'a str,
    args: Vec<FunctionArgument<'a>>,
}

impl<'a> FunctionBuilder<'a> {
    pub(crate) fn new(name: &'a str) -> Self {
        Self {
            modifiers: Modifiers::empty(),
            generic_decls: Vec::new(),
            return_type: QualifiedType::empty(),
            name,
            args: Vec::new(),
        }
    }

    pub(crate) fn with_modifiers(mut self, modifiers: Modifiers) -> Self {
        self.modifiers.insert(modifiers);
        self
    }

    pub(crate) fn with_generic_decls(
        mut self,
        generic_decls: impl IntoIterator<Item = GenericDecl<'a>>,
    ) -> Self {
        self.generic_decls.extend(generic_decls);
        self
    }

    pub(crate) fn with_return_type(mut self, ty: QualifiedType<'a>) -> Self {
        self.return_type = ty;
        self
    }

    pub(crate) fn with_args(
        mut self,
        args: impl IntoIterator<Item = FunctionArgument<'a>>,
    ) -> Self {
        self.args.extend(args);
        self
    }

    pub(crate) fn build(self) -> Function<'a> {
        Function {
            modifiers: self.modifiers,
            generic_decls: self.generic_decls.into_boxed_slice(),
            return_type: self.return_type,
            name: self.name,
            args: self.args.into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ClassEntry<'a> {
    Fields(Box<[Field<'a>]>),
    Function(Function<'a>),
    Class(Class<'a>),
    Enum(Enum<'a>),
    Interface(Interface<'a>),
    Skip,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class<'a> {
    pub modifiers: Modifiers,
    pub name: &'a str,
    pub generic_decls: Box<[GenericDecl<'a>]>,

    pub extend: Option<QualifiedType<'a>>,
    pub implements: Box<[QualifiedType<'a>]>,
    pub permits: Box<[QualifiedType<'a>]>,

    pub fields: Box<[Field<'a>]>,
    pub functions: Box<[Function<'a>]>,

    pub classes: Box<[Class<'a>]>,
    pub enums: Box<[Enum<'a>]>,
    pub interfaces: Box<[Interface<'a>]>,
}

pub(crate) struct ClassBuilder<'a, const RECORD: bool> {
    modifiers: Modifiers,
    name: &'a str,
    generic_decls: Vec<GenericDecl<'a>>,

    extend: Option<QualifiedType<'a>>,
    implements: Vec<QualifiedType<'a>>,
    permits: Vec<QualifiedType<'a>>,

    fields: Vec<Field<'a>>,
    functions: Vec<Function<'a>>,

    classes: Vec<Class<'a>>,
    enums: Vec<Enum<'a>>,
    interfaces: Vec<Interface<'a>>,
}

impl<'a> ClassBuilder<'a, true> {
    pub(crate) fn record(name: &'a str, fields: impl IntoIterator<Item = Field<'a>>) -> Self {
        let fields = fields.into_iter().collect::<Vec<_>>();

        let functions = fields
            .iter()
            .map(|field| {
                FunctionBuilder::new(field.name)
                    .with_modifiers(Modifiers::PUBLIC)
                    .with_return_type(field.r#type.clone())
                    .build()
            })
            .collect::<Vec<_>>();

        Self {
            modifiers: Modifiers::empty(),
            name,
            generic_decls: Vec::new(),
            extend: Some(QualifiedType::from([
                TypeBuilder::named("java").build(),
                TypeBuilder::named("lang").build(),
                TypeBuilder::named("Record").build(),
            ])),
            implements: Vec::new(),
            permits: Vec::new(),
            fields,
            functions,
            classes: Vec::new(),
            enums: Vec::new(),
            interfaces: Vec::new(),
        }
    }
}

impl<'a> ClassBuilder<'a, false> {
    pub(crate) fn class(name: &'a str) -> Self {
        Self {
            modifiers: Modifiers::empty(),
            name,
            generic_decls: Vec::new(),
            extend: None,
            implements: Vec::new(),
            permits: Vec::new(),
            fields: Vec::new(),
            functions: Vec::new(),
            classes: Vec::new(),
            enums: Vec::new(),
            interfaces: Vec::new(),
        }
    }

    pub(crate) fn with_extend(mut self, extend: QualifiedType<'a>) -> Self {
        let _ = self.extend.insert(extend);
        self
    }

    pub(crate) fn with_permits(
        mut self,
        permits: impl IntoIterator<Item = QualifiedType<'a>>,
    ) -> Self {
        self.permits.extend(permits);
        self
    }
}

impl<'a, const RECORD: bool> ClassBuilder<'a, RECORD> {
    pub(crate) fn with_modifiers(mut self, modifiers: Modifiers) -> Self {
        self.modifiers.insert(modifiers);
        self
    }

    pub(crate) fn with_generic_decls(
        mut self,
        generic_decls: impl IntoIterator<Item = GenericDecl<'a>>,
    ) -> Self {
        self.generic_decls.extend(generic_decls);
        self
    }

    pub(crate) fn with_implements(
        mut self,
        implements: impl IntoIterator<Item = QualifiedType<'a>>,
    ) -> Self {
        self.implements.extend(implements);
        self
    }

    pub(crate) fn with_entries(
        mut self,
        entries: impl IntoIterator<Item = ClassEntry<'a>>,
    ) -> Self {
        for entry in entries {
            match entry {
                ClassEntry::Fields(v) => self.fields.extend(v),
                ClassEntry::Function(f) => self.functions.push(f),
                ClassEntry::Class(c) => self.classes.push(c),
                ClassEntry::Enum(e) => self.enums.push(e),
                ClassEntry::Interface(i) => self.interfaces.push(i),
                ClassEntry::Skip => continue,
            }
        }

        self
    }

    pub(crate) fn build(self) -> Class<'a> {
        Class {
            modifiers: self.modifiers,
            name: self.name,
            generic_decls: self.generic_decls.into_boxed_slice(),
            extend: self.extend,
            implements: self.implements.into_boxed_slice(),
            permits: self.permits.into_boxed_slice(),
            fields: self.fields.into_boxed_slice(),
            functions: self.functions.into_boxed_slice(),
            classes: self.classes.into_boxed_slice(),
            enums: self.enums.into_boxed_slice(),
            interfaces: self.interfaces.into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum EnumEntry<'a> {
    Fields(Box<[Field<'a>]>),
    Function(Function<'a>),
    Class(Class<'a>),
    Enum(Enum<'a>),
    Interface(Interface<'a>),
    Skip,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum<'a> {
    pub modifiers: Modifiers,
    pub name: &'a str,
    pub generic_decls: Box<[GenericDecl<'a>]>,

    pub implements: Box<[QualifiedType<'a>]>,

    pub fields: Box<[Field<'a>]>,
    pub functions: Box<[Function<'a>]>,

    pub classes: Box<[Class<'a>]>,
    pub enums: Box<[Enum<'a>]>,
    pub interfaces: Box<[Interface<'a>]>,
}

pub(crate) struct EnumBuilder<'a> {
    modifiers: Modifiers,
    name: &'a str,
    generic_decls: Vec<GenericDecl<'a>>,

    implements: Vec<QualifiedType<'a>>,

    fields: Vec<Field<'a>>,
    functions: Vec<Function<'a>>,

    classes: Vec<Class<'a>>,
    enums: Vec<Enum<'a>>,
    interfaces: Vec<Interface<'a>>,
}

impl<'a> EnumBuilder<'a> {
    pub(crate) fn new(name: &'a str) -> Self {
        Self {
            modifiers: Modifiers::empty(),
            name,
            generic_decls: Vec::new(),
            implements: Vec::new(),
            fields: Vec::new(),
            functions: Vec::new(),
            classes: Vec::new(),
            enums: Vec::new(),
            interfaces: Vec::new(),
        }
    }

    pub(crate) fn with_modifiers(mut self, modifiers: Modifiers) -> Self {
        self.modifiers.insert(modifiers);
        self
    }

    pub(crate) fn with_generic_decls(
        mut self,
        generic_decls: impl IntoIterator<Item = GenericDecl<'a>>,
    ) -> Self {
        self.generic_decls.extend(generic_decls);
        self
    }

    pub(crate) fn with_implements(
        mut self,
        extends: impl IntoIterator<Item = QualifiedType<'a>>,
    ) -> Self {
        self.implements.extend(extends);
        self
    }

    pub(crate) fn with_entries(mut self, entries: impl IntoIterator<Item = EnumEntry<'a>>) -> Self {
        for entry in entries {
            match entry {
                EnumEntry::Fields(v) => self.fields.extend(v),
                EnumEntry::Function(f) => self.functions.push(f),
                EnumEntry::Class(c) => self.classes.push(c),
                EnumEntry::Enum(e) => self.enums.push(e),
                EnumEntry::Interface(i) => self.interfaces.push(i),
                EnumEntry::Skip => continue,
            }
        }

        self
    }

    pub(crate) fn build(self) -> Enum<'a> {
        Enum {
            modifiers: self.modifiers,
            name: self.name,
            generic_decls: self.generic_decls.into_boxed_slice(),
            implements: self.implements.into_boxed_slice(),
            fields: self.fields.into_boxed_slice(),
            functions: self.functions.into_boxed_slice(),
            classes: self.classes.into_boxed_slice(),
            enums: self.enums.into_boxed_slice(),
            interfaces: self.interfaces.into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum InterfaceEntry<'a> {
    Fields(Box<[Field<'a>]>),
    Function(Function<'a>),
    Class(Class<'a>),
    Enum(Enum<'a>),
    Interface(Interface<'a>),
    Skip,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface<'a> {
    pub modifiers: Modifiers,
    pub name: &'a str,
    pub generic_decls: Box<[GenericDecl<'a>]>,

    pub extends: Box<[QualifiedType<'a>]>,
    pub permits: Box<[QualifiedType<'a>]>,

    pub fields: Box<[Field<'a>]>,
    pub functions: Box<[Function<'a>]>,

    pub classes: Box<[Class<'a>]>,
    pub enums: Box<[Enum<'a>]>,
    pub interfaces: Box<[Interface<'a>]>,
}

pub(crate) struct InterfaceBuilder<'a> {
    modifiers: Modifiers,
    name: &'a str,
    generic_decls: Vec<GenericDecl<'a>>,

    extends: Vec<QualifiedType<'a>>,
    permits: Vec<QualifiedType<'a>>,

    fields: Vec<Field<'a>>,
    functions: Vec<Function<'a>>,

    classes: Vec<Class<'a>>,
    enums: Vec<Enum<'a>>,
    interfaces: Vec<Interface<'a>>,
}

impl<'a> InterfaceBuilder<'a> {
    pub(crate) fn new(name: &'a str) -> Self {
        Self {
            modifiers: Modifiers::empty(),
            name,
            generic_decls: Vec::new(),
            extends: Vec::new(),
            permits: Vec::new(),
            fields: Vec::new(),
            functions: Vec::new(),
            classes: Vec::new(),
            enums: Vec::new(),
            interfaces: Vec::new(),
        }
    }

    pub(crate) fn with_modifiers(mut self, modifiers: Modifiers) -> Self {
        self.modifiers.insert(modifiers);
        self
    }

    pub(crate) fn with_generic_decls(
        mut self,
        generic_decls: impl IntoIterator<Item = GenericDecl<'a>>,
    ) -> Self {
        self.generic_decls.extend(generic_decls);
        self
    }

    pub(crate) fn with_extends(
        mut self,
        extends: impl IntoIterator<Item = QualifiedType<'a>>,
    ) -> Self {
        self.extends.extend(extends);
        self
    }

    pub(crate) fn with_permits(
        mut self,
        permits: impl IntoIterator<Item = QualifiedType<'a>>,
    ) -> Self {
        self.permits.extend(permits);
        self
    }

    pub(crate) fn with_entries(
        mut self,
        entries: impl IntoIterator<Item = InterfaceEntry<'a>>,
    ) -> Self {
        for entry in entries {
            match entry {
                InterfaceEntry::Fields(v) => self.fields.extend(v),
                InterfaceEntry::Function(f) => self.functions.push(f),
                InterfaceEntry::Class(c) => self.classes.push(c),
                InterfaceEntry::Enum(e) => self.enums.push(e),
                InterfaceEntry::Interface(i) => self.interfaces.push(i),
                InterfaceEntry::Skip => continue,
            }
        }

        self
    }

    pub(crate) fn build(self) -> Interface<'a> {
        Interface {
            modifiers: self.modifiers,
            name: self.name,
            generic_decls: self.generic_decls.into_boxed_slice(),
            extends: self.extends.into_boxed_slice(),
            permits: self.permits.into_boxed_slice(),
            fields: self.fields.into_boxed_slice(),
            functions: self.functions.into_boxed_slice(),
            classes: self.classes.into_boxed_slice(),
            enums: self.enums.into_boxed_slice(),
            interfaces: self.interfaces.into_boxed_slice(),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum RootEntry<'a> {
    Class(Class<'a>),
    Enum(Enum<'a>),
    Interface(Interface<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Root<'a> {
    pub package: &'a str,
    pub imports: Box<[&'a str]>,
    pub classes: Box<[Class<'a>]>,
    pub enums: Box<[Enum<'a>]>,
    pub interfaces: Box<[Interface<'a>]>,
}

pub(crate) struct RootBuilder<'a> {
    package: &'a str,
    imports: Vec<&'a str>,
    classes: Vec<Class<'a>>,
    enums: Vec<Enum<'a>>,
    interfaces: Vec<Interface<'a>>,
}

impl<'a> RootBuilder<'a> {
    pub(crate) fn new(package: &'a str) -> Self {
        Self {
            package,
            imports: Vec::new(),
            classes: Vec::new(),
            enums: Vec::new(),
            interfaces: Vec::new(),
        }
    }

    pub(crate) fn with_imports(mut self, imports: impl IntoIterator<Item = &'a str>) -> Self {
        self.imports.extend(imports);
        self
    }

    pub(crate) fn with_entries(mut self, entries: impl IntoIterator<Item = RootEntry<'a>>) -> Self {
        for entry in entries {
            match entry {
                RootEntry::Class(c) => self.classes.push(c),
                RootEntry::Enum(e) => self.enums.push(e),
                RootEntry::Interface(i) => self.interfaces.push(i),
            }
        }

        self
    }

    pub(crate) fn build(self) -> Root<'a> {
        Root {
            package: self.package,
            imports: self.imports.into_boxed_slice(),
            classes: self.classes.into_boxed_slice(),
            enums: self.enums.into_boxed_slice(),
            interfaces: self.interfaces.into_boxed_slice(),
        }
    }
}

impl<'a> std::hash::Hash for Root<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self, state);
    }
}
