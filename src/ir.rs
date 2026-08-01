use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

use bitflags::bitflags;
use bumpalo::{Bump, collections::Vec as BumpVec};

use crate::sign;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct Modifiers: u16 {
        const PUBLIC        = 1 << 0;
        const PROTECTED     = 1 << 1;
        const PRIVATE       = 1 << 2;

        const STATIC        = 1 << 3;
        const FINAL         = 1 << 4;
        const ABSTRACT      = 1 << 5;

        const NATIVE        = 1 << 6;
        const SYNCHRONIZED  = 1 << 7;
        const TRANSIENT     = 1 << 8;
        const VOLATILE      = 1 << 9;
        const STRICTFP      = 1 << 10;
    }
}

impl Display for Modifiers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, (name, _)) in self.iter_names().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }

            match name {
                "PUBLIC" => write!(f, "public")?,
                "PROTECTED" => write!(f, "protected")?,
                "PRIVATE" => write!(f, "private")?,

                "STATIC" => write!(f, "static")?,
                "FINAL" => write!(f, "final")?,
                "ABSTRACT" => write!(f, "abstract")?,

                "NATIVE" => write!(f, "native")?,
                "SYNCHRONIZED" => write!(f, "synchronized")?,
                "TRANSIENT" => write!(f, "transient")?,
                "VOLATILE" => write!(f, "volatile")?,
                "STRICTFP" => write!(f, "strictfp")?,

                _ => unreachable!(),
            }
        }

        Ok(())
    }
}

impl From<jvm_class::ClassAccessFlags> for Modifiers {
    fn from(value: jvm_class::ClassAccessFlags) -> Self {
        let mut mods = Modifiers::empty();

        if value.contains(jvm_class::ClassAccessFlags::PUBLIC) {
            mods.insert(Self::PUBLIC);
        } else {
            mods.insert(Self::PRIVATE);
        }

        if value.contains(jvm_class::ClassAccessFlags::FINAL) {
            mods.insert(Self::FINAL);
        }

        if value.contains(jvm_class::ClassAccessFlags::ABSTRACT) {
            mods.insert(Self::ABSTRACT);
        }

        mods
    }
}

impl From<jvm_class::InnerClassAccessFlags> for Modifiers {
    fn from(value: jvm_class::InnerClassAccessFlags) -> Self {
        let mut mods = Modifiers::empty();

        if value.contains(jvm_class::InnerClassAccessFlags::PUBLIC) {
            mods.insert(Self::PUBLIC);
        }

        if value.contains(jvm_class::InnerClassAccessFlags::PRIVATE) {
            mods.insert(Self::PRIVATE);
        }

        if value.contains(jvm_class::InnerClassAccessFlags::PROTECTED) {
            mods.insert(Self::PROTECTED);
        }

        if value.contains(jvm_class::InnerClassAccessFlags::STATIC) {
            mods.insert(Self::STATIC);
        }

        if value.contains(jvm_class::InnerClassAccessFlags::FINAL) {
            mods.insert(Self::FINAL);
        }

        if value.contains(jvm_class::InnerClassAccessFlags::ABSTRACT) {
            mods.insert(Self::ABSTRACT);
        }

        mods
    }
}

impl From<jvm_class::FieldAccessFlags> for Modifiers {
    fn from(value: jvm_class::FieldAccessFlags) -> Self {
        let mut mods = Modifiers::empty();

        if value.contains(jvm_class::FieldAccessFlags::PUBLIC) {
            mods.insert(Self::PUBLIC);
        }

        if value.contains(jvm_class::FieldAccessFlags::PRIVATE) {
            mods.insert(Self::PRIVATE);
        }

        if value.contains(jvm_class::FieldAccessFlags::PROTECTED) {
            mods.insert(Self::PROTECTED);
        }

        if value.contains(jvm_class::FieldAccessFlags::STATIC) {
            mods.insert(Self::STATIC);
        }

        if value.contains(jvm_class::FieldAccessFlags::FINAL) {
            mods.insert(Self::FINAL);
        }

        if value.contains(jvm_class::FieldAccessFlags::VOLATILE) {
            mods.insert(Self::VOLATILE);
        }

        if value.contains(jvm_class::FieldAccessFlags::TRANSIENT) {
            mods.insert(Self::TRANSIENT);
        }

        mods
    }
}

impl From<jvm_class::MethodAccessFlags> for Modifiers {
    fn from(value: jvm_class::MethodAccessFlags) -> Self {
        let mut mods = Modifiers::empty();

        if value.contains(jvm_class::MethodAccessFlags::PUBLIC) {
            mods.insert(Self::PUBLIC);
        }

        if value.contains(jvm_class::MethodAccessFlags::PRIVATE) {
            mods.insert(Self::PRIVATE);
        }

        if value.contains(jvm_class::MethodAccessFlags::PROTECTED) {
            mods.insert(Self::PROTECTED);
        }

        if value.contains(jvm_class::MethodAccessFlags::STATIC) {
            mods.insert(Self::STATIC);
        }

        if value.contains(jvm_class::MethodAccessFlags::FINAL) {
            mods.insert(Self::FINAL);
        }

        if value.contains(jvm_class::MethodAccessFlags::SYNCHRONIZED) {
            mods.insert(Self::SYNCHRONIZED);
        }

        if value.contains(jvm_class::MethodAccessFlags::NATIVE) {
            mods.insert(Self::NATIVE);
        }

        if value.contains(jvm_class::MethodAccessFlags::ABSTRACT) {
            mods.insert(Self::ABSTRACT);
        }

        if value.contains(jvm_class::MethodAccessFlags::STRICT) {
            mods.insert(Self::STRICTFP);
        }

        mods
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct SimpleObjectType<'a> {
    pub ident: &'a str,
    pub type_args: &'a [Type<'a>],
}

impl Display for SimpleObjectType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.type_args.is_empty() {
            return write!(f, "{}", self.ident);
        }

        write!(f, "{}<", self.ident)?;

        for (i, r#type) in self.type_args.iter().enumerate() {
            if i == 0 {
                write!(f, "{type}")?;
            } else {
                write!(f, ", {type}")?;
            }
        }

        write!(f, ">")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Type<'a> {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean,
    Object(&'a [&'a str], &'a [SimpleObjectType<'a>]),
    Array(&'a Type<'a>),
    ParameterRef(&'a str),
    ParameterUnbound,
    ParameterUpperBound(&'a Type<'a>),
    ParameterLowerBound(&'a Type<'a>),
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Byte => write!(f, "byte"),
            Type::Char => write!(f, "char"),
            Type::Double => write!(f, "double"),
            Type::Float => write!(f, "float"),
            Type::Int => write!(f, "int"),
            Type::Long => write!(f, "long"),
            Type::Short => write!(f, "short"),
            Type::Boolean => write!(f, "bool"),
            Type::Object(packages, simple_object_types) => {
                for (i, package) in packages.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{package}")?;
                    } else {
                        write!(f, ".{package}")?;
                    }
                }

                for (i, sot) in simple_object_types.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{}{sot}", if packages.is_empty() { "" } else { "." })?;
                    } else {
                        write!(f, ".{sot}")?;
                    }
                }

                Ok(())
            }
            Type::Array(inner) => write!(f, "{inner}[]"),
            Type::ParameterRef(name) => write!(f, "{name}"),
            Type::ParameterUnbound => write!(f, "?"),
            Type::ParameterUpperBound(bound) => write!(f, "? extends {bound}"),
            Type::ParameterLowerBound(bound) => write!(f, "? super {bound}"),
        }
    }
}

impl From<sign::types::BaseType> for Type<'_> {
    fn from(value: sign::types::BaseType) -> Self {
        match value {
            sign::types::BaseType::Byte => Self::Byte,
            sign::types::BaseType::Char => Self::Char,
            sign::types::BaseType::Double => Self::Double,
            sign::types::BaseType::Float => Self::Float,
            sign::types::BaseType::Int => Self::Int,
            sign::types::BaseType::Long => Self::Long,
            sign::types::BaseType::Short => Self::Short,
            sign::types::BaseType::Boolean => Self::Boolean,
        }
    }
}

impl<'a> Type<'a> {
    pub(crate) fn from_object(bump: &'a Bump, value: sign::types::ObjectType<'a>) -> Self {
        let mut parts = BumpVec::with_capacity_in(value.1.len(), bump);

        for sot in value.1 {
            let type_args = bump.alloc_slice_fill_iter(
                sot.type_args
                    .iter()
                    .map(|arg| Type::from_type_argument(bump, *arg)),
            );

            let start = parts.len();
            let mut offset = 0;

            for segment in sot.ident.split('$') {
                let end = offset + segment.len();

                if !segment.is_empty() {
                    parts.push(SimpleObjectType {
                        ident: nested_ident(bump, sot.ident, offset, end),
                        type_args: &[],
                    });
                }

                offset = end + 1;
            }

            if parts.len() == start {
                parts.push(SimpleObjectType {
                    ident: sot.ident,
                    type_args: &[],
                });
            }

            if let Some(last) = parts.last_mut() {
                last.type_args = type_args;
            }
        }

        Self::Object(value.0, parts.into_bump_slice())
    }

    fn from_type_argument(bump: &'a Bump, value: sign::types::TypeArgument<'a>) -> Self {
        match value {
            sign::types::TypeArgument::Reference(reference_type) => {
                Self::from_reference(bump, reference_type)
            }
            sign::types::TypeArgument::ParameterUnbound => Self::ParameterUnbound,
            sign::types::TypeArgument::ParameterUpperBound(reference_type) => {
                Self::ParameterUpperBound(bump.alloc(Self::from_reference(bump, reference_type)))
            }
            sign::types::TypeArgument::ParameterLowerBound(reference_type) => {
                Self::ParameterLowerBound(bump.alloc(Self::from_reference(bump, reference_type)))
            }
        }
    }

    pub(crate) fn from_sign(bump: &'a Bump, value: sign::types::SignatureType<'a>) -> Self {
        match value {
            sign::types::SignatureType::Base(base_type) => Self::from(base_type),
            sign::types::SignatureType::Reference(reference_type) => {
                Self::from_reference(bump, reference_type)
            }
        }
    }

    fn from_reference(bump: &'a Bump, value: sign::types::ReferenceType<'a>) -> Self {
        match value {
            sign::types::ReferenceType::ParameterRef(r#ref) => Self::ParameterRef(r#ref),
            sign::types::ReferenceType::Object(object_type) => Self::from_object(bump, object_type),
            sign::types::ReferenceType::Array(signature_type) => {
                Self::Array(bump.alloc(Self::from_sign(bump, *signature_type)))
            }
        }
    }

    fn strip_array(&mut self) -> bool {
        match *self {
            Type::Array(inner) => {
                *self = *inner;
                true
            }
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct TypeParameter<'a> {
    pub name: &'a str,
    pub extends: &'a [Type<'a>],
}

impl Display for TypeParameter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.extends.is_empty() {
            return write!(f, "{}", self.name);
        }

        write!(f, "{} extends ", self.name)?;

        for (i, extend) in self.extends.iter().enumerate() {
            if i == 0 {
                write!(f, "{extend}")?;
            } else {
                write!(f, ", {extend}")?;
            }
        }

        Ok(())
    }
}

impl<'a> TypeParameter<'a> {
    fn from_sign(bump: &'a Bump, value: sign::types::TypeParameter<'a>) -> Self {
        Self {
            name: value.name,
            extends: bump.alloc_slice_fill_iter(
                value
                    .class_bound
                    .into_iter()
                    .chain(value.interface_bounds.iter().copied())
                    .map(|bound| Type::from_reference(bump, bound))
                    .collect::<Vec<_>>(),
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Field<'a> {
    pub modifiers: Modifiers,
    pub r#type: Type<'a>,
    pub name: &'a str,
}

impl Display for Field<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.modifiers.is_empty() {
            write!(f, "{} ", self.modifiers)?;
        }

        write!(f, "{} {};", self.r#type, self.name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MethodArgument<'a> {
    pub r#type: Type<'a>,
    pub name: &'a str,
    pub vararg: bool,
}

impl Display for MethodArgument<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.r#type, self.name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Method<'a> {
    pub modifiers: Modifiers,
    pub type_params: &'a [TypeParameter<'a>],
    pub return_type: Option<Type<'a>>,
    pub name: &'a str,
    pub args: &'a [MethodArgument<'a>],
}

impl Display for Method<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.modifiers.is_empty() {
            write!(f, "{} ", self.modifiers)?;
        }

        if !self.type_params.is_empty() {
            write!(f, "<")?;

            for (i, par) in self.type_params.iter().enumerate() {
                if i == 0 {
                    write!(f, "{par}")?;
                } else {
                    write!(f, ", {par}")?;
                }
            }

            write!(f, ">")?;
        }

        if let Some(r#type) = &self.return_type {
            write!(f, "{type} ")?;
        } else {
            write!(f, "void ")?;
        }

        write!(f, "{}(", self.name)?;

        for (i, arg) in self.args.iter().enumerate() {
            if i == 0 {
                write!(f, "{}", arg)?;
            } else {
                write!(f, ", {}", arg)?;
            }
        }

        write!(f, ");")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ClassType {
    Class,
    Interface,
    Enum,
}

impl Display for ClassType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClassType::Class => write!(f, "class"),
            ClassType::Interface => write!(f, "interface"),
            ClassType::Enum => write!(f, "enum"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Class<'a> {
    pub modifiers: Modifiers,
    pub r#type: ClassType,

    pub name: &'a str,

    pub type_params: &'a [TypeParameter<'a>],

    pub extends: Option<Type<'a>>,
    pub implements: &'a [Type<'a>],

    pub fields: &'a [Field<'a>],
    pub methods: &'a [Method<'a>],

    pub children: &'a [Class<'a>],

    pub self_type: Type<'a>,

    pub anonymous: Option<&'a str>,
}

impl<'a> Class<'a> {
    pub(crate) fn rename(&mut self, bump: &'a Bump, name: &'a str) {
        let type_params = self.type_params;

        let type_args = bump.alloc_slice_fill_iter(
            type_params
                .iter()
                .map(|param| Type::ParameterRef(param.name)),
        );

        self.name = name;
        self.self_type = Type::Object(
            &[],
            bump.alloc_slice_fill_iter(std::iter::once(SimpleObjectType {
                ident: name,
                type_args,
            })),
        );
    }
}

impl Display for Class<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.modifiers.is_empty() {
            write!(f, "{} ", self.modifiers)?;
        }

        write!(f, "{} {}", self.r#type, self.name)?;

        if !self.type_params.is_empty() {
            write!(f, "<")?;

            for (i, par) in self.type_params.iter().enumerate() {
                if i == 0 {
                    write!(f, "{par}")?;
                } else {
                    write!(f, ", {par}")?;
                }
            }

            write!(f, ">")?;
        }

        if let Some(r#type) = &self.extends {
            write!(f, " extends {type}")?;
        }

        if !self.implements.is_empty() {
            write!(f, " implements ")?;

            for (i, r#type) in self.implements.iter().enumerate() {
                if i == 0 {
                    write!(f, "{type}")?;
                } else {
                    write!(f, ", {type}")?;
                }
            }
        }

        writeln!(f, " {{")?;

        for child in self.children {
            let block = child.to_string();
            let lines = block.split("\n");

            for line in lines {
                writeln!(f, "\t{line}")?;
            }
        }

        for field in self.fields {
            writeln!(f, "\t{}", field)?;
        }

        for method in self.methods {
            writeln!(f, "\t{}", method)?;
        }

        write!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Root<'a> {
    pub package: &'a str,
    pub classes: &'a [Class<'a>],
}

impl Display for Root<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.package.is_empty() {
            writeln!(f, "package {};", self.package)?;
        }

        for class in self.classes {
            writeln!(f, "{class}")?;
        }

        Ok(())
    }
}

pub fn intern<'a>(bump: &'a Bump, value: Cow<'_, str>) -> &'a str {
    bump.alloc_str(&value)
}

pub trait FromUnderlying<'a, 'u, T> {
    fn from_underlying(
        bump: &'a Bump,
        constant_pool: &jvm_class::ConstantPool<'u>,
        underlying: &T,
    ) -> anyhow::Result<Self>
    where
        Self: Sized;
}

impl<'a, 'u> FromUnderlying<'a, 'u, jvm_class::FieldInfo<'u>> for Field<'a> {
    fn from_underlying(
        bump: &'a Bump,
        constant_pool: &jvm_class::ConstantPool<'u>,
        underlying: &jvm_class::FieldInfo<'u>,
    ) -> anyhow::Result<Self> {
        let modifiers = Modifiers::from(underlying.access_flags);

        let name = intern(bump, underlying.name(constant_pool)?);

        let mut r#type = Type::from_sign(
            bump,
            sign::types::SignatureType::try_parse(
                bump,
                intern(bump, underlying.descriptor(constant_pool)?),
            )?,
        );

        if let Some(raw_signature) = underlying.signature(constant_pool)?
            && let Ok(generic) =
                sign::types::SignatureType::try_parse(bump, intern(bump, raw_signature))
        {
            r#type = Type::from_sign(bump, generic);
        }

        Ok(Self {
            modifiers,
            name,
            r#type,
        })
    }
}

struct MethodCx<'a> {
    is_ctor: bool,
    self_type: Type<'a>,
    synthetic_ctor_args: usize,
}

impl<'a> Method<'a> {
    fn map<'u>(
        bump: &'a Bump,
        constant_pool: &jvm_class::ConstantPool<'u>,
        underlying: &jvm_class::MethodInfo<'u>,
        cx: &MethodCx<'a>,
    ) -> anyhow::Result<Self> {
        let modifiers = Modifiers::from(underlying.access_flags);

        let name = if cx.is_ctor {
            "__ctor"
        } else {
            intern(bump, underlying.name(constant_pool)?)
        };

        let descriptor = sign::types::MethodSignature::try_parse(
            bump,
            intern(bump, underlying.descriptor(constant_pool)?),
        )?;

        let skip = if cx.is_ctor {
            cx.synthetic_ctor_args.min(descriptor.args.len())
        } else {
            0
        };

        let mut arg_types = &descriptor.args[skip..];
        let mut return_type = descriptor.return_type;
        let mut type_params = descriptor.type_params;

        if let Some(raw_signature) = underlying.signature(constant_pool)?
            && let Ok(generic) =
                sign::types::MethodSignature::try_parse(bump, intern(bump, raw_signature))
            && generic.args.len() == arg_types.len()
        {
            arg_types = generic.args;
            return_type = generic.return_type;
            type_params = generic.type_params;
        }

        let type_params = bump.alloc_slice_fill_iter(
            type_params
                .iter()
                .map(|param| TypeParameter::from_sign(bump, *param)),
        );

        let declared_names = declared_parameter_names(bump, constant_pool, underlying)?;
        let is_vararg = underlying
            .access_flags
            .contains(jvm_class::MethodAccessFlags::VARARGS);
        let last_index = arg_types.len().saturating_sub(1);

        let args = bump.alloc_slice_fill_iter(
            arg_types
                .iter()
                .enumerate()
                .map(|(index, signature_type)| {
                    let mut r#type = Type::from_sign(bump, *signature_type);

                    let vararg = is_vararg && index == last_index && r#type.strip_array();

                    let name = declared_names
                        .get(skip + index)
                        .copied()
                        .flatten()
                        .unwrap_or_else(|| {
                            bumpalo::format!(in bump, "arg{}", index).into_bump_str()
                        });

                    MethodArgument {
                        r#type,
                        name,
                        vararg,
                    }
                })
                .collect::<Vec<_>>(),
        );

        let return_type = if cx.is_ctor {
            Some(cx.self_type)
        } else {
            return_type.map(|r#type| Type::from_sign(bump, r#type))
        };

        Ok(Self {
            modifiers,
            type_params,
            return_type,
            name,
            args,
        })
    }
}

impl<'a, 'u> FromUnderlying<'a, 'u, jvm_class::MethodInfo<'u>> for Method<'a> {
    fn from_underlying(
        bump: &'a Bump,
        constant_pool: &jvm_class::ConstantPool<'u>,
        underlying: &jvm_class::MethodInfo<'u>,
    ) -> anyhow::Result<Self> {
        Self::map(
            bump,
            constant_pool,
            underlying,
            &MethodCx {
                is_ctor: false,
                self_type: Type::Object(&[], &[]),
                synthetic_ctor_args: 0,
            },
        )
    }
}

impl<'a, 'u> FromUnderlying<'a, 'u, jvm_class::Class<'u>> for Class<'a> {
    fn from_underlying(
        bump: &'a Bump,
        constant_pool: &jvm_class::ConstantPool<'u>,
        underlying: &jvm_class::Class<'u>,
    ) -> anyhow::Result<Self> {
        let fqn = underlying.name()?;

        let inner = inner_class_entry(underlying, &fqn)?;

        let name = match inner.and_then(|entry| entry.inner_name_index) {
            Some(index) => intern(bump, constant_pool.str(index)?),
            None => intern(bump, Cow::Borrowed(simple_name(&fqn))),
        };

        let modifiers = match inner {
            Some(entry) => Modifiers::from(entry.inner_class_access_flags),
            None => Modifiers::from(underlying.access_flags),
        };

        let extends: Option<Type>;
        let implements: &[Type];
        let type_params: &[TypeParameter];

        let signature = match underlying.signature(constant_pool)? {
            Some(raw_signature) => {
                sign::types::ClassSignature::try_parse(bump, intern(bump, raw_signature)).ok()
            }
            None => None,
        };

        if let Some(signature) = signature {
            extends = Some(Type::from_object(bump, signature.super_class));

            implements = bump.alloc_slice_fill_iter(
                signature
                    .super_interfaces
                    .iter()
                    .map(|iface| Type::from_object(bump, *iface)),
            );

            type_params = bump.alloc_slice_fill_iter(
                signature
                    .type_params
                    .iter()
                    .map(|param| TypeParameter::from_sign(bump, *param)),
            );
        } else {
            extends = match underlying.super_class {
                Some(super_class_idx) => Some(binary_name_to_type(
                    bump,
                    constant_pool,
                    &class_name(constant_pool, super_class_idx)?,
                )?),
                None => None,
            };

            implements = bump.alloc_slice_fill_iter(
                underlying
                    .interfaces
                    .iter()
                    .map(|iface_idx| -> anyhow::Result<Type> {
                        binary_name_to_type(
                            bump,
                            constant_pool,
                            &class_name(constant_pool, *iface_idx)?,
                        )
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?,
            );

            type_params = &[];
        };

        let r#type = if underlying
            .access_flags
            .contains(jvm_class::ClassAccessFlags::INTERFACE)
        {
            ClassType::Interface
        } else if let Some(extends) = &extends
            && let Type::Object(path, sot) = extends
            && path.len() == 2
            && path[0] == "java"
            && path[1] == "lang"
            && sot.len() == 1
            && sot[0].ident == "Enum"
        {
            ClassType::Enum
        } else {
            ClassType::Class
        };

        let self_type = Type::Object(
            &[],
            bump.alloc_slice_fill_iter(std::iter::once(SimpleObjectType {
                ident: name,
                type_args: bump.alloc_slice_fill_iter(
                    type_params
                        .iter()
                        .map(|param| Type::ParameterRef(param.name)),
                ),
            })),
        );

        let synthetic_ctor_args = if r#type == ClassType::Enum {
            2
        } else if inner.is_some_and(|entry| {
            entry.outer_class_info_index.is_some()
                && !entry
                    .inner_class_access_flags
                    .contains(jvm_class::InnerClassAccessFlags::STATIC)
        }) {
            1
        } else {
            0
        };

        let fields = bump.alloc_slice_fill_iter(
            underlying
                .fields
                .iter()
                .filter(|field| {
                    !field
                        .access_flags
                        .contains(jvm_class::FieldAccessFlags::SYNTHETIC)
                })
                .map(|underlying| Field::from_underlying(bump, constant_pool, underlying))
                .collect::<anyhow::Result<Vec<_>>>()?,
        );

        let mut methods = Vec::with_capacity(underlying.methods.len());
        for underlying in &underlying.methods {
            if underlying.access_flags.intersects(
                jvm_class::MethodAccessFlags::SYNTHETIC | jvm_class::MethodAccessFlags::BRIDGE,
            ) {
                continue;
            }

            let raw_name = underlying.name(constant_pool)?;
            if raw_name == "<clinit>" {
                continue;
            }

            methods.push(Method::map(
                bump,
                constant_pool,
                underlying,
                &MethodCx {
                    is_ctor: raw_name == "<init>",
                    self_type,
                    synthetic_ctor_args,
                },
            )?);
        }
        let methods = bump.alloc_slice_fill_iter(methods);

        Ok(Self {
            modifiers,
            r#type,
            name,
            type_params,
            extends,
            implements,
            fields,
            methods,
            children: &[],
            self_type,
            anonymous: None,
        })
    }
}

fn declared_parameter_names<'a>(
    bump: &'a Bump,
    constant_pool: &jvm_class::ConstantPool<'_>,
    method: &jvm_class::MethodInfo<'_>,
) -> anyhow::Result<Vec<Option<&'a str>>> {
    let Some(jvm_class::AttributeInfo::MethodParameters { parameters }) = method
        .attributes
        .iter()
        .find(|attribute| matches!(attribute, jvm_class::AttributeInfo::MethodParameters { .. }))
    else {
        return Ok(Vec::new());
    };

    parameters
        .iter()
        .map(|parameter| {
            parameter
                .name_index
                .map(|index| -> anyhow::Result<&'a str> {
                    Ok(intern(bump, constant_pool.str(index)?))
                })
                .transpose()
        })
        .collect()
}

fn inner_class_entry<'c>(
    class: &'c jvm_class::Class<'_>,
    binary_name: &str,
) -> anyhow::Result<Option<&'c jvm_class::InnerClass>> {
    let Some(entries) = class.inner_classes()? else {
        return Ok(None);
    };

    let constant_pool = &class.constant_pool;

    for entry in entries {
        if class_name(constant_pool, entry.inner_class_info_index)? == binary_name {
            return Ok(Some(entry));
        }
    }

    Ok(None)
}

fn class_name<'a>(
    constant_pool: &jvm_class::ConstantPool<'a>,
    index: jvm_class::CpIndex,
) -> anyhow::Result<Cow<'a, str>> {
    Ok(constant_pool.str(constant_pool.class_name_index(index)?)?)
}

/// Anonymous classes are flattened into a single identifier by the class mapper
/// (`Outer$1` becomes `Outer_1`), so a reference to one has to name the flattened
/// identifier instead of the bare nesting step.
fn nested_ident<'a>(bump: &'a Bump, ident: &'a str, start: usize, end: usize) -> &'a str {
    let segment = &ident[start..end];

    if start == 0 || !segment.starts_with(|ch: char| ch.is_ascii_digit()) {
        return segment;
    }

    intern(bump, Cow::Owned(ident[..end].replace('$', "_")))
}

fn simple_name(binary_name: &str) -> &str {
    let simple = binary_name
        .rsplit_once('/')
        .map(|(_, name)| name)
        .unwrap_or(binary_name);

    simple
        .rsplit('$')
        .find(|segment| !segment.is_empty())
        .unwrap_or(simple)
}

fn binary_name_to_type<'a>(
    bump: &'a Bump,
    _constant_pool: &jvm_class::ConstantPool<'_>,
    internal_name: &str,
) -> anyhow::Result<Type<'a>> {
    Ok(Type::from_sign(
        bump,
        sign::types::SignatureType::try_parse(
            bump,
            bumpalo::format!(in bump, "L{};", internal_name).into_bump_str(),
        )?,
    ))
}
