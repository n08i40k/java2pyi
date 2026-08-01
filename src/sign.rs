pub mod types {
    use anyhow::Context;
    use bumpalo::Bump;

    #[derive(Debug, Clone, Copy)]
    pub enum BaseType {
        Byte,
        Char,
        Double,
        Float,
        Int,
        Long,
        Short,
        Boolean,
    }

    #[derive(Debug, Clone, Copy)]
    pub struct ObjectType<'a>(pub &'a [&'a str], pub &'a [SimpleObjectType<'a>]);

    #[derive(Debug, Clone, Copy)]
    pub struct SimpleObjectType<'a> {
        pub ident: &'a str,
        pub type_args: &'a [TypeArgument<'a>],
    }

    // public Map<T> var;
    //            ^
    //
    // public void example(Map<T> arg);
    //                         ^
    #[derive(Debug, Clone, Copy)]
    pub enum TypeArgument<'a> {
        Reference(ReferenceType<'a>),
        ParameterUnbound,
        ParameterUpperBound(ReferenceType<'a>),
        ParameterLowerBound(ReferenceType<'a>),
    }

    // Possible ReferenceType::Array values
    #[derive(Debug, Clone, Copy)]
    pub enum SignatureType<'a> {
        Base(BaseType),
        Reference(ReferenceType<'a>),
    }

    impl<'a> SignatureType<'a> {
        pub fn try_parse(bump: &'a Bump, signature: &'a str) -> anyhow::Result<Self> {
            let mut parser = super::Parser::new(bump, signature);
            let result = parser
                .parse_signature_type()
                .with_context(|| format!("failed to parse field signature {signature:?}"))?;
            parser.finish()?;
            Ok(result)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum ReferenceType<'a> {
        ParameterRef(&'a str),
        Object(ObjectType<'a>),
        Array(&'a SignatureType<'a>),
    }

    #[derive(Debug, Clone, Copy)]
    pub struct TypeParameter<'a> {
        pub name: &'a str,
        pub class_bound: Option<ReferenceType<'a>>,
        pub interface_bounds: &'a [ReferenceType<'a>],
    }

    #[derive(Debug, Clone, Copy)]
    pub struct MethodSignature<'a> {
        pub type_params: &'a [TypeParameter<'a>],
        pub args: &'a [SignatureType<'a>],
        pub return_type: Option<SignatureType<'a>>,
    }

    impl<'a> MethodSignature<'a> {
        pub fn try_parse(bump: &'a Bump, signature: &'a str) -> anyhow::Result<Self> {
            let mut parser = super::Parser::new(bump, signature);
            let result = parser
                .parse_method_signature()
                .with_context(|| format!("failed to parse method signature {signature:?}"))?;
            parser.finish()?;
            Ok(result)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct ClassSignature<'a> {
        pub type_params: &'a [TypeParameter<'a>],
        pub super_class: ObjectType<'a>,
        pub super_interfaces: &'a [ObjectType<'a>],
    }

    impl<'a> ClassSignature<'a> {
        pub fn try_parse(bump: &'a Bump, signature: &'a str) -> anyhow::Result<Self> {
            let mut parser = super::Parser::new(bump, signature);
            let result = parser
                .parse_class_signature()
                .with_context(|| format!("failed to parse class signature {signature:?}"))?;
            parser.finish()?;
            Ok(result)
        }
    }

    pub(super) fn bail_unexpected(pos: usize, input: &str, expected: &str) -> anyhow::Error {
        anyhow::anyhow!("expected {expected} at position {pos} in {input:?}")
    }
}

use bumpalo::Bump;
use bumpalo::collections::Vec as BumpVec;
use types::*;

struct Parser<'a> {
    bump: &'a Bump,
    input: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(bump: &'a Bump, input: &'a str) -> Self {
        Self {
            bump,
            input,
            bytes: input.as_bytes(),
            pos: 0,
        }
    }

    fn finish(&self) -> anyhow::Result<()> {
        if self.pos != self.bytes.len() {
            anyhow::bail!(
                "trailing input at position {} in {:?}",
                self.pos,
                self.input
            );
        }
        Ok(())
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn expect(&mut self, c: u8) -> anyhow::Result<()> {
        match self.bump() {
            Some(actual) if actual == c => Ok(()),
            _ => Err(types::bail_unexpected(
                self.pos,
                self.input,
                &format!("'{}'", c as char),
            )),
        }
    }

    fn is_separator(c: u8) -> bool {
        matches!(c, b'.' | b';' | b'[' | b'/' | b'<' | b'>' | b':')
    }

    fn parse_identifier(&mut self) -> anyhow::Result<&'a str> {
        let start = self.pos;

        while let Some(c) = self.peek() {
            if Self::is_separator(c) {
                break;
            }

            self.pos += 1;
        }

        if self.pos == start {
            return Err(types::bail_unexpected(self.pos, self.input, "identifier"));
        }

        Ok(&self.input[start..self.pos])
    }

    fn parse_base_type(&mut self) -> Option<BaseType> {
        let base_type = match self.peek()? {
            b'B' => BaseType::Byte,
            b'C' => BaseType::Char,
            b'D' => BaseType::Double,
            b'F' => BaseType::Float,
            b'I' => BaseType::Int,
            b'J' => BaseType::Long,
            b'S' => BaseType::Short,
            b'Z' => BaseType::Boolean,
            _ => return None,
        };

        self.pos += 1;
        Some(base_type)
    }

    fn parse_signature_type(&mut self) -> anyhow::Result<SignatureType<'a>> {
        if let Some(base_type) = self.parse_base_type() {
            return Ok(SignatureType::Base(base_type));
        }

        Ok(SignatureType::Reference(self.parse_reference_type()?))
    }

    fn parse_reference_type(&mut self) -> anyhow::Result<ReferenceType<'a>> {
        match self.peek() {
            Some(b'[') => {
                self.pos += 1;
                let inner = self.parse_signature_type()?;
                Ok(ReferenceType::Array(self.bump.alloc(inner)))
            }
            Some(b'L') => Ok(ReferenceType::Object(self.parse_object_type()?)),
            Some(b'T') => {
                self.pos += 1;
                let name = self.parse_identifier()?;
                self.expect(b';')?;
                Ok(ReferenceType::ParameterRef(name))
            }
            _ => Err(types::bail_unexpected(
                self.pos,
                self.input,
                "a reference type",
            )),
        }
    }

    fn parse_object_type(&mut self) -> anyhow::Result<ObjectType<'a>> {
        self.expect(b'L')?;

        let mut segments = BumpVec::new_in(self.bump);
        segments.push(self.parse_identifier()?);
        while self.peek() == Some(b'/') {
            self.pos += 1;
            segments.push(self.parse_identifier()?);
        }

        let ident = segments.pop().unwrap();
        let package = segments.into_bump_slice();

        let type_args = self.parse_optional_type_args()?;

        let mut types = BumpVec::new_in(self.bump);
        types.push(SimpleObjectType { ident, type_args });

        while self.peek() == Some(b'.') {
            self.pos += 1;
            let simple = self.parse_simple_object_type()?;
            types.push(simple);
        }

        self.expect(b';')?;

        Ok(ObjectType(package, types.into_bump_slice()))
    }

    fn parse_simple_object_type(&mut self) -> anyhow::Result<SimpleObjectType<'a>> {
        let ident = self.parse_identifier()?;
        let type_args = self.parse_optional_type_args()?;
        Ok(SimpleObjectType { ident, type_args })
    }

    fn parse_optional_type_args(&mut self) -> anyhow::Result<&'a [TypeArgument<'a>]> {
        if self.peek() != Some(b'<') {
            return Ok(&[]);
        }
        self.pos += 1;

        let mut args = BumpVec::new_in(self.bump);
        loop {
            let arg = self.parse_type_argument()?;
            args.push(arg);
            if self.peek() == Some(b'>') {
                self.pos += 1;
                break;
            }
        }

        Ok(args.into_bump_slice())
    }

    fn parse_type_argument(&mut self) -> anyhow::Result<TypeArgument<'a>> {
        match self.peek() {
            Some(b'*') => {
                self.pos += 1;
                Ok(TypeArgument::ParameterUnbound)
            }
            Some(b'+') => {
                self.pos += 1;
                Ok(TypeArgument::ParameterUpperBound(
                    self.parse_reference_type()?,
                ))
            }
            Some(b'-') => {
                self.pos += 1;
                Ok(TypeArgument::ParameterLowerBound(
                    self.parse_reference_type()?,
                ))
            }
            _ => Ok(TypeArgument::Reference(self.parse_reference_type()?)),
        }
    }

    fn parse_type_parameter(&mut self) -> anyhow::Result<TypeParameter<'a>> {
        let name = self.parse_identifier()?;
        self.expect(b':')?;

        let class_bound = match self.peek() {
            Some(b'L') | Some(b'T') | Some(b'[') => Some(self.parse_reference_type()?),
            _ => None,
        };

        let mut interface_bounds = BumpVec::new_in(self.bump);
        while self.peek() == Some(b':') {
            self.pos += 1;
            let bound = self.parse_reference_type()?;
            interface_bounds.push(bound);
        }

        Ok(TypeParameter {
            name,
            class_bound,
            interface_bounds: interface_bounds.into_bump_slice(),
        })
    }

    fn parse_type_parameters(&mut self) -> anyhow::Result<&'a [TypeParameter<'a>]> {
        if self.peek() != Some(b'<') {
            return Ok(&[]);
        }
        self.pos += 1;

        let mut params = BumpVec::new_in(self.bump);
        while self.peek() != Some(b'>') {
            let param = self.parse_type_parameter()?;
            params.push(param);
        }
        self.pos += 1;

        Ok(params.into_bump_slice())
    }

    fn parse_method_signature(&mut self) -> anyhow::Result<MethodSignature<'a>> {
        let type_params = self.parse_type_parameters()?;

        self.expect(b'(')?;
        let mut args = BumpVec::new_in(self.bump);
        while self.peek() != Some(b')') {
            let arg = self.parse_signature_type()?;
            args.push(arg);
        }
        self.pos += 1;

        let return_type = if self.peek() == Some(b'V') {
            self.pos += 1;
            None
        } else {
            Some(self.parse_signature_type()?)
        };

        while self.peek() == Some(b'^') {
            self.pos += 1;
            self.parse_reference_type()?;
        }

        Ok(MethodSignature {
            type_params,
            args: args.into_bump_slice(),
            return_type,
        })
    }

    fn parse_class_signature(&mut self) -> anyhow::Result<ClassSignature<'a>> {
        let type_params = self.parse_type_parameters()?;
        let super_class = self.parse_object_type()?;

        let mut super_interfaces = BumpVec::new_in(self.bump);
        while self.peek() == Some(b'L') {
            let iface = self.parse_object_type()?;
            super_interfaces.push(iface);
        }

        Ok(ClassSignature {
            type_params,
            super_class,
            super_interfaces: super_interfaces.into_bump_slice(),
        })
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use crate::ir::Type;

    use super::*;

    fn field(descriptor: &str) -> String {
        let bump = Bump::new();
        let descriptor = bump.alloc_str(descriptor);

        Type::from_sign(&bump, SignatureType::try_parse(&bump, descriptor).unwrap()).to_string()
    }

    #[test]
    fn field_descriptors_become_types() {
        assert_eq!(field("I"), "int");
        assert_eq!(field("[I"), "int[]");
        assert_eq!(field("Ljava/lang/String;"), "java.lang.String");
        assert_eq!(field("[[Ljava/lang/String;"), "java.lang.String[][]");
    }

    #[test]
    fn nested_names_become_path_steps() {
        assert_eq!(field("Ljava/util/Map$Entry;"), "java.util.Map.Entry");
        assert_eq!(field("LMain$1$Inner;"), "Main.Main_1.Inner");
    }

    #[test]
    fn anonymous_names_keep_the_flattened_identifier() {
        assert_eq!(field("LMain$1;"), "Main.Main_1");
        assert_eq!(field("LMain$1$2;"), "Main.Main_1.Main_1_2");
        assert_eq!(field("La/b/Main$1;"), "a.b.Main.Main_1");
    }

    #[test]
    fn obfuscated_names_are_accepted() {
        assert_eq!(field("La/b/1c;"), "a.b.1c");
    }

    #[test]
    fn type_arguments_stay_on_the_part_they_were_written_on() {
        assert_eq!(
            field("Ljava/util/Map<Ljava/lang/String;[I>;"),
            "java.util.Map<java.lang.String, int[]>"
        );
        assert_eq!(
            field("Ljava/util/Map<TK;TV;>.Entry<TK;TV;>;"),
            "java.util.Map<K, V>.Entry<K, V>"
        );
    }

    #[test]
    fn wildcards_keep_their_boundary() {
        assert_eq!(field("Ljava/util/List<*>;"), "java.util.List<?>");
        assert_eq!(
            field("Ljava/util/List<+Ljava/lang/Number;>;"),
            "java.util.List<? extends java.lang.Number>"
        );
        assert_eq!(
            field("Ljava/util/List<-Ljava/lang/Number;>;"),
            "java.util.List<? super java.lang.Number>"
        );
    }

    #[test]
    fn method_descriptors_split_into_arguments_and_a_return_type() {
        let bump = Bump::new();

        let signature = MethodSignature::try_parse(&bump, "(I[Ljava/lang/String;)V").unwrap();

        assert!(signature.type_params.is_empty());
        assert_eq!(
            signature
                .args
                .iter()
                .map(|arg| Type::from_sign(&bump, *arg).to_string())
                .collect::<Vec<_>>()
                .join(", "),
            "int, java.lang.String[]"
        );
        assert!(signature.return_type.is_none());
    }

    #[test]
    fn method_signatures_carry_type_parameters_and_throws() {
        let bump = Bump::new();

        let signature = MethodSignature::try_parse(
            &bump,
            "<T:Ljava/lang/Object;>(TT;)TT;^Ljava/io/IOException;",
        )
        .unwrap();

        assert_eq!(signature.type_params.len(), 1);
        assert_eq!(signature.type_params[0].name, "T");
        assert_eq!(Type::from_sign(&bump, signature.args[0]).to_string(), "T");
        assert_eq!(
            Type::from_sign(&bump, signature.return_type.unwrap()).to_string(),
            "T"
        );
    }

    #[test]
    fn class_signatures_split_into_bounds_and_supertypes() {
        let bump = Bump::new();

        let signature = ClassSignature::try_parse(
            &bump,
            "<K::Ljava/lang/Comparable<TK;>;>Ljava/lang/Object;Ljava/util/List<TK;>;",
        )
        .unwrap();

        assert_eq!(signature.type_params.len(), 1);
        assert_eq!(signature.type_params[0].name, "K");
        assert_eq!(
            Type::from_object(&bump, signature.super_class).to_string(),
            "java.lang.Object"
        );
        assert_eq!(
            Type::from_object(&bump, signature.super_interfaces[0]).to_string(),
            "java.util.List<K>"
        );
    }

    #[test]
    fn trailing_input_is_rejected() {
        let bump = Bump::new();

        assert!(SignatureType::try_parse(&bump, "II").is_err());
        assert!(SignatureType::try_parse(&bump, "Ljava/lang/String").is_err());
    }
}
