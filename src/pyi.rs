use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use crate::index_tree::LocalIndexTree;
use crate::model::{
    ClassRef, EnumRef, Exclusions, InterfaceRef, ResolvedType, Root, TypeRef, array_depth,
    named_type, primitive_python_type,
};
use crate::{preprocess::Scope, status};
use java_ast_parser::ast::{self, Function, GenericImpl, GenericWildcardBoundary, Modifiers};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

const PYI_PACKAGE: &str = "java2pyi";
const PYI_TYPES_SUBPACKAGE: &str = "types";

trait QualifiedTypeFormat {
    fn fmt(&self) -> String;
}

impl QualifiedTypeFormat for ast::QualifiedType<'_> {
    fn fmt(&self) -> String {
        self.iter()
            .map(|x| x.to_string())
            .collect::<Box<[_]>>()
            .join(".")
    }
}

pub fn write_pyi_by_package<E>(
    scopes: &[Scope],
    mixer_records: HashMap<String, String>,
    exclusions: Arc<Exclusions>,
    write_package: impl Fn(&str, String) -> Result<(), E> + Sync,
) -> Result<(), E>
where
    E: Send,
{
    let definition_paths = Arc::new(collect_definition_paths(
        scopes.iter().map(|scope| scope.ast.as_ref()),
        &exclusions,
    ));

    let mut scopes_by_package: HashMap<&str, Vec<&Scope>> = HashMap::new();
    for scope in scopes {
        scopes_by_package
            .entry(scope.ast.ast().package)
            .or_default()
            .push(scope);
    }

    let total_packages = scopes_by_package.len();
    status::update(&format!("Serializing 0/{}", total_packages));
    let progress = AtomicUsize::new(0);
    let mixer = Mixer::new(mixer_records);

    scopes_by_package
        .into_par_iter()
        .try_for_each(|(package, package_scopes)| {
            let module_imports =
                collect_module_imports(&package_scopes, &definition_paths, &exclusions);

            let mut emitter = PyiEmitter::new(
                definition_paths.clone(),
                module_imports,
                &mixer,
                exclusions.clone(),
            );

            emitter.emit_header();

            let empty_type_params = BTreeSet::new();
            for scope in &package_scopes {
                let ast = scope.ast.ast();
                for class_cell in ast.classes.iter().map(ClassRef::new) {
                    emitter.emit_class(class_cell, &scope.local_index_tree, &empty_type_params);
                }
                for interface_cell in ast.interfaces.iter().map(InterfaceRef::new) {
                    emitter.emit_interface(
                        interface_cell,
                        &scope.local_index_tree,
                        &empty_type_params,
                    );
                }
                for enum_cell in ast.enums.iter().map(EnumRef::new) {
                    emitter.emit_enum(enum_cell, &scope.local_index_tree, &empty_type_params);
                }
            }

            let completed = progress.fetch_add(1, Ordering::Relaxed) + 1;
            let label = if package.is_empty() {
                "<root>"
            } else {
                package
            };
            status::update(&format!(
                "Serializing {}/{}: {}",
                completed, total_packages, label
            ));

            write_package(package, emitter.finish())
        })?;

    let mixer_package = format!("{PYI_PACKAGE}.{PYI_TYPES_SUBPACKAGE}");
    write_package(&mixer_package, mixer.gen_stub())
}

struct MixerEntry {
    from_java_ty: String,
    to_python_ty: String,
    fq_union_name: String,
    union_name: String,
}

impl MixerEntry {
    pub fn new(from_java_ty: String, to_python_ty: String) -> Self {
        let name = format!("{}Like", from_java_ty.rsplit(".").next().unwrap());
        Self {
            from_java_ty,
            to_python_ty,
            fq_union_name: format!("{PYI_PACKAGE}.{PYI_TYPES_SUBPACKAGE}.{name}"),
            union_name: name,
        }
    }

    pub fn gen_union(&self) -> String {
        format!(
            "{} = Union[{}, {}]",
            self.union_name, self.from_java_ty, self.to_python_ty
        )
    }
}

struct Mixer {
    records: HashMap<String, MixerEntry>,
}

impl Mixer {
    pub fn new(mut records: HashMap<String, String>) -> Self {
        records.insert(String::from("java.lang.Object"), String::from("Any"));
        records.insert(String::from("java.lang.Boolean"), String::from("bool"));
        records.insert(String::from("java.lang.Integer"), String::from("int"));
        records.insert(String::from("java.lang.Long"), String::from("int"));
        records.insert(String::from("java.lang.Float"), String::from("float"));
        records.insert(String::from("java.lang.Double"), String::from("float"));
        records.insert(String::from("java.lang.String"), String::from("str"));

        Self {
            records: records
                .into_iter()
                .map(|(k, v)| (k.clone(), MixerEntry::new(k, v)))
                .collect(),
        }
    }

    pub fn try_mix(&self, java_ty: &str) -> String {
        self.records
            .get(java_ty)
            .map(|entry| entry.fq_union_name.clone())
            .unwrap_or_else(|| java_ty.to_string())
    }

    pub fn gen_stub(&self) -> String {
        let imports = self
            .records
            .values()
            .map(|entry| entry.from_java_ty.rsplit_once(".").unwrap().0.to_string())
            .collect::<HashSet<String>>()
            .into_iter()
            .map(|ns| format!("import {ns}"))
            .collect::<Box<[_]>>()
            .join("\n");

        let body = self
            .records
            .values()
            .map(MixerEntry::gen_union)
            .collect::<Box<[_]>>()
            .join("\n");

        format!("from typing import Any, Union\n{imports}\n\n{body}")
    }
}

struct PyiEmitter<'a> {
    output: String,
    indent: usize,
    type_renderer: TypeRenderer<'a>,
    definition_paths: Arc<DefinitionPaths>,
    module_imports: BTreeSet<String>,
    exclusions: Arc<Exclusions>,
}

impl<'a> PyiEmitter<'a> {
    fn new(
        definition_paths: Arc<DefinitionPaths>,
        module_imports: BTreeSet<String>,
        mixer: &'a Mixer,
        exclusions: Arc<Exclusions>,
    ) -> Self {
        Self {
            output: String::new(),
            indent: 0,
            type_renderer: TypeRenderer::new(definition_paths.clone(), mixer),
            definition_paths,
            module_imports,
            exclusions,
        }
    }

    fn emit_header(&mut self) {
        self.line("from __future__ import annotations".to_string());
        self.line(format!("import {}.{}", PYI_PACKAGE, PYI_TYPES_SUBPACKAGE));
        let module_imports = std::mem::take(&mut self.module_imports);
        for module_import in module_imports {
            self.line(format!("import {}", module_import));
        }
        self.line("from typing import Any, overload".to_string());
        self.blank_line();
    }

    fn emit_class(
        &mut self,
        class_cell: ClassRef,
        local_index_tree: &LocalIndexTree,
        outer_type_params: &BTreeSet<String>,
    ) {
        if self.exclusions.contains(TypeRef::Class(class_cell)) {
            return;
        }
        let class = class_cell.borrow();
        let class_type_params = extend_type_params(outer_type_params, &class.generic_decls);
        let type_params_suffix = format_type_params(&class.generic_decls);
        let class_path = self.definition_paths.class_path(&class_cell);
        let mut rendered_bases = collect_class_base_types(
            class,
            &self.type_renderer,
            local_index_tree,
            class_cell,
            &class_type_params,
        );
        let mut inserted_special_base = false;
        if let Some(special_base) = java_stdlib_python_base(&class_path, &class.generic_decls)
            && !rendered_bases
                .bases
                .iter()
                .any(|base| base == &special_base)
        {
            rendered_bases.bases.insert(0, special_base);
            inserted_special_base = true;
        }
        if class_path != "java.lang.Object" && class.extend.is_none() {
            let object_base = "java.lang.Object".to_string();
            if !rendered_bases.bases.iter().any(|base| base == &object_base) {
                let insert_at = if inserted_special_base { 1 } else { 0 };
                let bounded_index = insert_at.min(rendered_bases.bases.len());
                rendered_bases.bases.insert(bounded_index, object_base);
            }
        }
        let bases_suffix = if rendered_bases.bases.is_empty() {
            String::new()
        } else {
            format!("({})", rendered_bases.bases.join(", "))
        };

        let mut line = format!(
            "class {}{}{}:",
            class.name, type_params_suffix, bases_suffix
        );
        if !rendered_bases.unknown.is_empty() {
            line.push_str(&format!(
                "  # unknown type(s) [{}] used in {}",
                rendered_bases.unknown.join(", "),
                class_path
            ));
        }
        self.line(line);
        self.indent += 1;

        let mut has_members = false;

        for variable in &class.fields {
            has_members = true;
            let rendered = self.type_renderer.render_in_scope(
                &class_path,
                &variable.r#type,
                local_index_tree,
                Some(class_cell),
                &class_type_params,
            );
            let ident = sanitize_ident(variable.name);
            let mut line = format!("{}: {}", ident, rendered.text);
            if rendered.has_unknown() {
                line.push_str(&format!(
                    "  # unknown type(s) [{}] used in {}.{}",
                    rendered.unknown.join(", "),
                    class_path,
                    variable.name
                ));
            }
            self.line(line);
        }

        let function_groups = group_functions(&class.functions);
        for function_group in function_groups {
            let use_overload = function_group.len() > 1;
            for function in function_group {
                has_members = true;
                let function_type_params =
                    extend_type_params(&class_type_params, &function.generic_decls);
                self.emit_function(
                    function,
                    use_overload,
                    &class_path,
                    local_index_tree,
                    Some(class_cell),
                    &function_type_params,
                );
            }
        }

        for nested_class in class.classes.iter().map(ClassRef::new) {
            has_members = true;
            self.emit_class(nested_class, local_index_tree, &class_type_params);
        }
        for nested_interface in class.interfaces.iter().map(InterfaceRef::new) {
            has_members = true;
            self.emit_interface(nested_interface, local_index_tree, &class_type_params);
        }
        for nested_enum in class.enums.iter().map(EnumRef::new) {
            has_members = true;
            self.emit_enum(nested_enum, local_index_tree, &class_type_params);
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_interface(
        &mut self,
        interface_cell: InterfaceRef,
        local_index_tree: &LocalIndexTree,
        outer_type_params: &BTreeSet<String>,
    ) {
        if self.exclusions.contains(TypeRef::Interface(interface_cell)) {
            return;
        }
        let interface = interface_cell.borrow();
        let interface_type_params = extend_type_params(outer_type_params, &interface.generic_decls);
        let type_params_suffix = format_type_params(&interface.generic_decls);
        let interface_path = self.definition_paths.interface_path(&interface_cell);
        let mut rendered_bases = collect_interface_base_types(
            interface,
            &self.type_renderer,
            local_index_tree,
            &interface_type_params,
        );
        if let Some(special_base) =
            java_stdlib_python_base(&interface_path, &interface.generic_decls)
            && !rendered_bases
                .bases
                .iter()
                .any(|base| base == &special_base)
        {
            rendered_bases.bases.insert(0, special_base);
        }
        let bases_suffix = if rendered_bases.bases.is_empty() {
            "(java.lang.Object)".to_string()
        } else {
            format!("(java.lang.Object, {})", rendered_bases.bases.join(", "))
        };

        let mut line = format!(
            "class {}{}{}:",
            interface.name, type_params_suffix, bases_suffix
        );
        if !rendered_bases.unknown.is_empty() {
            line.push_str(&format!(
                "  # unknown type(s) [{}] used in {}",
                rendered_bases.unknown.join(", "),
                interface_path
            ));
        }
        self.line(line);
        self.indent += 1;

        let mut has_members = false;

        for variable in &interface.fields {
            has_members = true;
            let rendered = self.type_renderer.render_in_scope(
                &interface_path,
                &variable.r#type,
                local_index_tree,
                None,
                &interface_type_params,
            );
            let ident = sanitize_ident(variable.name);
            let mut line = format!("{}: {}", ident, rendered.text);
            if rendered.has_unknown() {
                line.push_str(&format!(
                    "  # unknown type(s) [{}] used in {}.{}",
                    rendered.unknown.join(", "),
                    interface_path,
                    variable.name
                ));
            }
            self.line(line);
        }

        let function_groups = group_functions(&interface.functions);
        for function_group in function_groups {
            let use_overload = function_group.len() > 1;
            for function in function_group {
                has_members = true;
                let function_type_params =
                    extend_type_params(&interface_type_params, &function.generic_decls);
                self.emit_function(
                    function,
                    use_overload,
                    &interface_path,
                    local_index_tree,
                    None,
                    &function_type_params,
                );
            }
        }

        for nested_class in interface.classes.iter().map(ClassRef::new) {
            has_members = true;
            self.emit_class(nested_class, local_index_tree, &interface_type_params);
        }
        for nested_interface in interface.interfaces.iter().map(InterfaceRef::new) {
            has_members = true;
            self.emit_interface(nested_interface, local_index_tree, &interface_type_params);
        }
        for nested_enum in interface.enums.iter().map(EnumRef::new) {
            has_members = true;
            self.emit_enum(nested_enum, local_index_tree, &interface_type_params);
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_enum(
        &mut self,
        enum_cell: EnumRef,
        local_index_tree: &LocalIndexTree,
        outer_type_params: &BTreeSet<String>,
    ) {
        if self.exclusions.contains(TypeRef::Enum(enum_cell)) {
            return;
        }
        let r#enum = enum_cell.borrow();
        let enum_type_params = extend_type_params(outer_type_params, &r#enum.generic_decls);
        let type_params_suffix = format_type_params(&r#enum.generic_decls);
        let enum_path = self.definition_paths.enum_path(&enum_cell);
        let rendered_bases = collect_enum_base_types(
            r#enum,
            &self.type_renderer,
            local_index_tree,
            &enum_type_params,
        );
        let bases = rendered_bases.bases;
        let bases_suffix = if bases.is_empty() {
            "(java.lang.Object)".to_string()
        } else {
            format!("(java.lang.Object, {})", bases.join(", "))
        };

        let mut line = format!(
            "class {}{}{}:",
            r#enum.name, type_params_suffix, bases_suffix
        );
        if !rendered_bases.unknown.is_empty() {
            line.push_str(&format!(
                "  # unknown type(s) [{}] used in {}",
                rendered_bases.unknown.join(", "),
                enum_path
            ));
        }
        self.line(line);
        self.indent += 1;

        let mut has_members = false;

        for variable in &r#enum.fields {
            has_members = true;
            let rendered = self.type_renderer.render_in_scope(
                &enum_path,
                &variable.r#type,
                local_index_tree,
                None,
                &enum_type_params,
            );
            let ident = sanitize_ident(variable.name);
            let mut line = format!("{}: {}", ident, rendered.text);
            if rendered.has_unknown() {
                line.push_str(&format!(
                    "  # unknown type(s) [{}] used in {}.{}",
                    rendered.unknown.join(", "),
                    enum_path,
                    variable.name
                ));
            }
            self.line(line);
        }

        let function_groups = group_functions(&r#enum.functions);
        for function_group in function_groups {
            let use_overload = function_group.len() > 1;
            for function in function_group {
                has_members = true;
                let function_type_params =
                    extend_type_params(&enum_type_params, &function.generic_decls);
                self.emit_function(
                    function,
                    use_overload,
                    &enum_path,
                    local_index_tree,
                    None,
                    &function_type_params,
                );
            }
        }

        for nested_class in r#enum.classes.iter().map(ClassRef::new) {
            has_members = true;
            self.emit_class(nested_class, local_index_tree, &enum_type_params);
        }
        for nested_interface in r#enum.interfaces.iter().map(InterfaceRef::new) {
            has_members = true;
            self.emit_interface(nested_interface, local_index_tree, &enum_type_params);
        }
        for nested_enum in r#enum.enums.iter().map(EnumRef::new) {
            has_members = true;
            self.emit_enum(nested_enum, local_index_tree, &enum_type_params);
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_function(
        &mut self,
        function: &Function<'_>,
        use_overload: bool,
        class_path: &str,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) {
        if use_overload {
            self.line("@overload".to_string());
        }

        let is_static = function.modifiers.intersects(Modifiers::STATIC);
        let is_ctor = function.name == "__ctor";

        if is_static {
            self.line("@staticmethod".to_string());
        }

        let mut args = Vec::with_capacity(function.args.len() + usize::from(!is_static));
        if !is_static {
            if class_path == "java.lang.Object" && function.name == "getClass" {
                args.push("self = None".to_string());
            } else {
                args.push("self".to_string());
            }
        }

        let mut unknown_paths = HashMap::new();
        for argument in &function.args {
            let rendered = self.type_renderer.render_in_scope(
                class_path,
                &argument.r#type,
                local_index_tree,
                scope,
                type_params,
            );

            let arg_prefix = if argument.vararg { "*" } else { "" };
            let ident = sanitize_ident(argument.name);

            args.push(format!("{}{}: {}", arg_prefix, ident, rendered.text));

            if rendered.has_unknown() {
                unknown_paths.insert(
                    format!("{}.{}.{}", class_path, function.name, argument.name),
                    rendered.unknown,
                );
            }
        }

        let rendered_return = if is_ctor {
            self.type_renderer.render_constructor_return(
                class_path,
                &function.return_type,
                local_index_tree,
                scope,
                type_params,
            )
        } else {
            self.type_renderer.render_in_scope(
                class_path,
                &function.return_type,
                local_index_tree,
                scope,
                type_params,
            )
        };

        if rendered_return.has_unknown() {
            unknown_paths.insert(
                format!("{}.{}", class_path, function.name),
                rendered_return.unknown,
            );
        }

        let type_params_suffix = format_type_params(&function.generic_decls);

        let mut line = format!(
            "def {}{}({}) -> {}: ...",
            if is_ctor { "__init__" } else { function.name },
            type_params_suffix,
            args.join(", "),
            rendered_return.text
        );

        if !unknown_paths.is_empty() {
            let paths = unknown_paths.into_iter().collect::<Vec<_>>();
            line.push_str(&format!(
                "  # unknown type(s) used in {}",
                paths
                    .into_iter()
                    .map(|(k, v)| format!("{} -> [{}]", k, v.join(", ")))
                    .collect::<Box<[_]>>()
                    .join("; ")
            ));
        }

        self.line(line);
    }

    fn line(&mut self, text: String) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
        self.output.push_str(&text);
        self.output.push('\n');
    }

    fn blank_line(&mut self) {
        self.output.push('\n');
    }

    fn finish(self) -> String {
        self.output
    }
}

fn group_functions<'a>(functions: &'a [Function<'a>]) -> Vec<Vec<&'a Function<'a>>> {
    let mut order: Vec<&'a str> = Vec::with_capacity(functions.len());
    let mut grouped: HashMap<&'a str, Vec<&Function<'a>>> = HashMap::with_capacity(functions.len());

    for function in functions {
        if !grouped.contains_key(function.name) {
            order.push(function.name);
        }
        grouped.entry(function.name).or_default().push(function);
    }

    order
        .into_iter()
        .filter_map(|name| grouped.remove(&name))
        .collect()
}

fn sanitize_ident(ident: &str) -> String {
    if is_python_keyword(ident) {
        format!("{}_", ident)
    } else {
        ident.to_string()
    }
}

fn is_python_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "False"
            | "None"
            | "True"
            | "and"
            | "as"
            | "assert"
            | "async"
            | "await"
            | "break"
            | "class"
            | "continue"
            | "def"
            | "del"
            | "elif"
            | "else"
            | "except"
            | "finally"
            | "for"
            | "from"
            | "global"
            | "if"
            | "import"
            | "in"
            | "is"
            | "lambda"
            | "match"
            | "nonlocal"
            | "not"
            | "or"
            | "pass"
            | "raise"
            | "return"
            | "try"
            | "while"
            | "with"
            | "yield"
    )
}

fn collect_module_imports(
    scopes: &[&Scope],
    definition_paths: &DefinitionPaths,
    exclusions: &Exclusions,
) -> BTreeSet<String> {
    let mut modules = BTreeSet::new();

    fn add_class_module(
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        class_cell: ClassRef,
    ) {
        if let Some(module_path) = definition_paths.class_module(class_cell)
            && !module_path.is_empty()
        {
            modules.insert(module_path.to_string());
        }
    }

    fn add_interface_module(
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        interface_cell: InterfaceRef,
    ) {
        if let Some(module_path) = definition_paths.interface_module(interface_cell)
            && !module_path.is_empty()
        {
            modules.insert(module_path.to_string());
        }
    }

    fn collect_from_generic(
        generic: &GenericImpl<'_>,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) {
        match generic {
            GenericImpl::Type(r#type) => collect_from_type(
                r#type,
                definition_paths,
                modules,
                local_index_tree,
                scope,
                type_params,
            ),
            GenericImpl::Wildcard(boundary) => match boundary {
                GenericWildcardBoundary::None => {}
                GenericWildcardBoundary::Extends(bound) | GenericWildcardBoundary::Super(bound) => {
                    collect_from_type(
                        bound,
                        definition_paths,
                        modules,
                        local_index_tree,
                        scope,
                        type_params,
                    );
                }
            },
        }
    }

    fn collect_from_type(
        r#type: &ast::QualifiedType<'_>,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) {
        match resolve_type(local_index_tree, scope, r#type, type_params) {
            Some(ResolvedType::Class(class_cell)) => {
                add_class_module(definition_paths, modules, class_cell);
            }
            Some(ResolvedType::Interface(interface_cell)) => {
                add_interface_module(definition_paths, modules, interface_cell);
            }
            _ => {}
        }

        for part in r#type {
            if let Some((_, generic_impls)) = named_type(part) {
                for generic in generic_impls {
                    collect_from_generic(
                        generic,
                        definition_paths,
                        modules,
                        local_index_tree,
                        scope,
                        type_params,
                    );
                }
            }
        }
    }

    fn collect_from_function(
        function: &Function<'_>,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) {
        collect_from_type(
            &function.return_type,
            definition_paths,
            modules,
            local_index_tree,
            scope,
            type_params,
        );
        for argument in &function.args {
            collect_from_type(
                &argument.r#type,
                definition_paths,
                modules,
                local_index_tree,
                scope,
                type_params,
            );
        }
    }

    fn collect_from_class(
        class_cell: ClassRef,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        local_index_tree: &LocalIndexTree,
        outer_type_params: &BTreeSet<String>,
        exclusions: &Exclusions,
    ) {
        if exclusions.contains(TypeRef::Class(class_cell)) {
            return;
        }
        let class = class_cell.borrow();
        let class_type_params = extend_type_params(outer_type_params, &class.generic_decls);

        if let Some(extend) = &class.extend {
            collect_from_type(
                extend,
                definition_paths,
                modules,
                local_index_tree,
                Some(class_cell),
                &class_type_params,
            );
        }
        for implemented in &class.implements {
            collect_from_type(
                implemented,
                definition_paths,
                modules,
                local_index_tree,
                Some(class_cell),
                &class_type_params,
            );
        }

        for field in &class.fields {
            collect_from_type(
                &field.r#type,
                definition_paths,
                modules,
                local_index_tree,
                Some(class_cell),
                &class_type_params,
            );
        }

        for function in &class.functions {
            let function_type_params =
                extend_type_params(&class_type_params, &function.generic_decls);
            collect_from_function(
                function,
                definition_paths,
                modules,
                local_index_tree,
                Some(class_cell),
                &function_type_params,
            );
        }

        for nested in class.classes.iter().map(ClassRef::new) {
            collect_from_class(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &class_type_params,
                exclusions,
            );
        }
        for nested in class.interfaces.iter().map(InterfaceRef::new) {
            collect_from_interface(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &class_type_params,
                exclusions,
            );
        }
        for nested in class.enums.iter().map(EnumRef::new) {
            collect_from_enum(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &class_type_params,
                exclusions,
            );
        }
    }

    fn collect_from_interface(
        interface_cell: InterfaceRef,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        local_index_tree: &LocalIndexTree,
        outer_type_params: &BTreeSet<String>,
        exclusions: &Exclusions,
    ) {
        if exclusions.contains(TypeRef::Interface(interface_cell)) {
            return;
        }
        let interface = interface_cell.borrow();
        let interface_type_params = extend_type_params(outer_type_params, &interface.generic_decls);

        for extend in &interface.extends {
            collect_from_type(
                extend,
                definition_paths,
                modules,
                local_index_tree,
                None,
                &interface_type_params,
            );
        }

        for field in &interface.fields {
            collect_from_type(
                &field.r#type,
                definition_paths,
                modules,
                local_index_tree,
                None,
                &interface_type_params,
            );
        }

        for function in &interface.functions {
            let function_type_params =
                extend_type_params(&interface_type_params, &function.generic_decls);
            collect_from_function(
                function,
                definition_paths,
                modules,
                local_index_tree,
                None,
                &function_type_params,
            );
        }

        for nested in interface.classes.iter().map(ClassRef::new) {
            collect_from_class(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &interface_type_params,
                exclusions,
            );
        }
        for nested in interface.interfaces.iter().map(InterfaceRef::new) {
            collect_from_interface(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &interface_type_params,
                exclusions,
            );
        }
        for nested in interface.enums.iter().map(EnumRef::new) {
            collect_from_enum(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &interface_type_params,
                exclusions,
            );
        }
    }

    fn collect_from_enum(
        enum_cell: EnumRef,
        definition_paths: &DefinitionPaths,
        modules: &mut BTreeSet<String>,
        local_index_tree: &LocalIndexTree,
        outer_type_params: &BTreeSet<String>,
        exclusions: &Exclusions,
    ) {
        if exclusions.contains(TypeRef::Enum(enum_cell)) {
            return;
        }
        let r#enum = enum_cell.borrow();
        let enum_type_params = extend_type_params(outer_type_params, &r#enum.generic_decls);

        for implemented in &r#enum.implements {
            collect_from_type(
                implemented,
                definition_paths,
                modules,
                local_index_tree,
                None,
                &enum_type_params,
            );
        }

        for field in &r#enum.fields {
            collect_from_type(
                &field.r#type,
                definition_paths,
                modules,
                local_index_tree,
                None,
                &enum_type_params,
            );
        }

        for function in &r#enum.functions {
            let function_type_params =
                extend_type_params(&enum_type_params, &function.generic_decls);
            collect_from_function(
                function,
                definition_paths,
                modules,
                local_index_tree,
                None,
                &function_type_params,
            );
        }

        for nested in r#enum.classes.iter().map(ClassRef::new) {
            collect_from_class(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &enum_type_params,
                exclusions,
            );
        }
        for nested in r#enum.interfaces.iter().map(InterfaceRef::new) {
            collect_from_interface(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &enum_type_params,
                exclusions,
            );
        }
        for nested in r#enum.enums.iter().map(EnumRef::new) {
            collect_from_enum(
                nested,
                definition_paths,
                modules,
                local_index_tree,
                &enum_type_params,
                exclusions,
            );
        }
    }

    let empty_type_params = BTreeSet::new();
    for scope in scopes {
        let ast = scope.ast.ast();
        for class_cell in ast.classes.iter().map(ClassRef::new) {
            collect_from_class(
                class_cell,
                definition_paths,
                &mut modules,
                &scope.local_index_tree,
                &empty_type_params,
                exclusions,
            );
        }
        for interface_cell in ast.interfaces.iter().map(InterfaceRef::new) {
            collect_from_interface(
                interface_cell,
                definition_paths,
                &mut modules,
                &scope.local_index_tree,
                &empty_type_params,
                exclusions,
            );
        }
        for enum_cell in ast.enums.iter().map(EnumRef::new) {
            collect_from_enum(
                enum_cell,
                definition_paths,
                &mut modules,
                &scope.local_index_tree,
                &empty_type_params,
                exclusions,
            );
        }
    }

    modules
}

struct RenderedBases {
    bases: Vec<String>,
    unknown: Box<[String]>,
}

fn generic_ident_or_any(generics: &[ast::GenericDecl<'_>], index: usize) -> String {
    generics
        .get(index)
        .map(|generic| generic.name.to_string())
        .unwrap_or_else(|| "Any".to_string())
}

fn java_stdlib_python_base(
    definition_path: &str,
    generics: &[ast::GenericDecl<'_>],
) -> Option<String> {
    match definition_path {
        "java.util.Map" => {
            let key = generic_ident_or_any(generics, 0);
            let value = generic_ident_or_any(generics, 1);
            Some(format!("dict[{}, {}]", key, value))
        }
        "java.util.List" => Some(format!("list[{}]", generic_ident_or_any(generics, 0))),
        "java.util.Set" => Some(format!("set[{}]", generic_ident_or_any(generics, 0))),
        "java.lang.Boolean" => Some("bool".to_string()),
        "java.lang.Integer" | "java.lang.Byte" | "java.lang.Long" | "java.lang.Short" => {
            Some("int".to_string())
        }
        "java.lang.Double" | "java.lang.Float" => Some("float".to_string()),
        "java.lang.String" => Some("str".to_string()),
        _ => None,
    }
}

fn collect_class_base_types(
    class: &ast::Class<'_>,
    type_renderer: &TypeRenderer,
    local_index_tree: &LocalIndexTree,
    class_cell: ClassRef,
    type_params: &BTreeSet<String>,
) -> RenderedBases {
    let mut bases =
        Vec::with_capacity(class.implements.len() + usize::from(class.extend.is_some()));
    let mut unknown = Vec::new();

    if let Some(extend) = &class.extend {
        let rendered =
            type_renderer.render(extend, local_index_tree, Some(class_cell), type_params);
        unknown.extend(rendered.unknown);
        if unknown.is_empty() {
            bases.push(rendered.text);
        } else {
            bases.push("java.lang.Object".to_string());
        }
    }

    for implemented in &class.implements {
        let rendered =
            type_renderer.render(implemented, local_index_tree, Some(class_cell), type_params);
        unknown.extend(rendered.unknown);
        bases.push(rendered.text);
    }

    RenderedBases {
        bases,
        unknown: unknown.into_boxed_slice(),
    }
}

fn collect_interface_base_types(
    interface: &ast::Interface<'_>,
    type_renderer: &TypeRenderer,
    local_index_tree: &LocalIndexTree,
    type_params: &BTreeSet<String>,
) -> RenderedBases {
    let mut bases = Vec::with_capacity(interface.extends.len());
    let mut unknown = Vec::new();

    for extend in &interface.extends {
        let rendered = type_renderer.render(extend, local_index_tree, None, type_params);
        unknown.extend(rendered.unknown);
        bases.push(rendered.text);
    }

    RenderedBases {
        bases,
        unknown: unknown.into_boxed_slice(),
    }
}

fn collect_enum_base_types(
    r#enum: &ast::Enum<'_>,
    type_renderer: &TypeRenderer,
    local_index_tree: &LocalIndexTree,
    type_params: &BTreeSet<String>,
) -> RenderedBases {
    let mut bases = Vec::with_capacity(r#enum.implements.len());
    let mut unknown = Vec::new();

    for implemented in &r#enum.implements {
        let rendered = type_renderer.render(implemented, local_index_tree, None, type_params);
        unknown.extend(rendered.unknown);
        bases.push(rendered.text);
    }

    RenderedBases {
        bases,
        unknown: unknown.into_boxed_slice(),
    }
}

fn extend_type_params(
    base: &BTreeSet<String>,
    generics: &[ast::GenericDecl<'_>],
) -> BTreeSet<String> {
    let mut combined = base.clone();
    for generic in generics {
        combined.insert(generic.name.to_string());
    }
    combined
}

fn format_type_params(generics: &[ast::GenericDecl<'_>]) -> String {
    if generics.is_empty() {
        return String::new();
    }

    let params = generics
        .iter()
        .map(|generic| generic.name.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{}]", params)
}

struct TypeRenderer<'a> {
    definition_paths: Arc<DefinitionPaths>,
    mixer: &'a Mixer,
}

struct RenderedType {
    text: String,
    unknown: Box<[String]>,
}

impl RenderedType {
    fn known(text: String) -> Self {
        Self {
            text,
            unknown: Box::from([]),
        }
    }

    fn unknown(qty: &ast::QualifiedType<'_>) -> Self {
        Self {
            text: "Any".to_string(),
            unknown: Box::from([qty.fmt()]),
        }
    }

    fn has_unknown(&self) -> bool {
        !self.unknown.is_empty()
    }
}

fn resolve_type(
    local_index_tree: &LocalIndexTree,
    scope: Option<ClassRef>,
    qty: &ast::QualifiedType<'_>,
    type_params: &BTreeSet<String>,
) -> Option<ResolvedType> {
    if qty.len() == 1
        && let Some((ident, _)) = qty.last().and_then(named_type)
        && type_params.contains(ident)
    {
        return None;
    }

    local_index_tree.search(scope, qty)
}

impl<'a> TypeRenderer<'a> {
    fn new(definition_paths: Arc<DefinitionPaths>, mixer: &'a Mixer) -> Self {
        Self {
            definition_paths,
            mixer,
        }
    }

    fn render_generic(
        &self,
        ty_gen: &GenericImpl<'_>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        match &ty_gen {
            GenericImpl::Type(ty) => self.render(ty, local_index_tree, scope, type_params),
            GenericImpl::Wildcard(boundary) => match boundary {
                GenericWildcardBoundary::None => RenderedType::known("Any".to_string()),
                GenericWildcardBoundary::Extends(bound) | GenericWildcardBoundary::Super(bound) => {
                    let rendered = self.render(bound, local_index_tree, scope, type_params);
                    RenderedType {
                        text: "Any".to_string(),
                        unknown: rendered.unknown,
                    }
                }
            },
        }
    }

    fn render(
        &self,
        qty: &ast::QualifiedType<'_>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let Some(last) = qty.last() else {
            return RenderedType::unknown(qty);
        };

        let mut rendered = self.render_type(qty, local_index_tree, scope, type_params);
        let depth = array_depth(last);
        if depth > 0 {
            for _ in 0..depth {
                rendered.text = format!("list[{}]", rendered.text);
            }
        }

        rendered
    }

    fn render_in_scope(
        &self,
        scope_path: &str,
        qty: &ast::QualifiedType<'_>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let rendered = self.render(qty, local_index_tree, scope, type_params);
        if !rendered.has_unknown() {
            return rendered;
        }

        let Some(ty) = qty.last() else {
            return rendered;
        };

        let Some((ident, generics)) = named_type(ty) else {
            return rendered;
        };

        if qty.len() != 1 || type_params.contains(ident) {
            return rendered;
        }

        let candidate = format!("{}.{}", scope_path, ident);
        if !self
            .definition_paths
            .known_paths
            .contains(candidate.as_str())
        {
            return rendered;
        }

        let mut nested =
            self.render_named_type(candidate, generics, local_index_tree, scope, type_params);
        let depth = array_depth(ty);
        if depth > 0 {
            for _ in 0..depth {
                nested.text = format!("list[{}]", nested.text);
            }
        }

        nested
    }

    fn render_constructor_return(
        &self,
        class_path: &str,
        qty: &ast::QualifiedType<'_>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let Some(last) = qty.last() else {
            return RenderedType::known(class_path.to_string());
        };

        let generics = named_type(last)
            .map(|(_, generics)| generics)
            .unwrap_or(&[]);
        let mut rendered = self.render_named_type(
            class_path.to_string(),
            generics,
            local_index_tree,
            scope,
            type_params,
        );
        let depth = array_depth(last);
        if depth > 0 {
            for _ in 0..depth {
                rendered.text = format!("list[{}]", rendered.text);
            }
        }

        rendered
    }

    fn render_type(
        &self,
        qty: &ast::QualifiedType<'_>,
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let ty = qty.last().unwrap();

        let Some((ident, generic_impls)) = named_type(ty) else {
            return RenderedType::known(primitive_python_type(ty).unwrap_or("Any").to_string());
        };

        match resolve_type(local_index_tree, scope, qty, type_params) {
            Some(ResolvedType::Class(class_cell)) => {
                let name = self
                    .definition_paths
                    .class_paths
                    .get(&class_cell)
                    .map(|path| path.to_string())
                    .unwrap_or_else(|| class_cell.ident().to_string());
                self.render_named_type(name, generic_impls, local_index_tree, scope, type_params)
            }
            Some(ResolvedType::Interface(interface_cell)) => {
                let name = self
                    .definition_paths
                    .interface_paths
                    .get(&interface_cell)
                    .map(|path| path.to_string())
                    .unwrap_or_else(|| interface_cell.ident().to_string());
                self.render_named_type(name, generic_impls, local_index_tree, scope, type_params)
            }
            None if type_params.contains(ident) => self.render_named_type(
                ident.to_string(),
                generic_impls,
                local_index_tree,
                scope,
                type_params,
            ),
            None => RenderedType::unknown(qty),
        }
    }

    fn render_named_type(
        &self,
        base: String,
        generics: &[GenericImpl<'_>],
        local_index_tree: &LocalIndexTree,
        scope: Option<ClassRef>,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        if generics.is_empty() {
            return RenderedType::known(self.mixer.try_mix(&base));
        }

        let mut unknown = Vec::new();
        let mut args = Vec::with_capacity(generics.len());
        for arg in generics {
            let rendered = self.render_generic(arg, local_index_tree, scope, type_params);
            unknown.extend(rendered.unknown);
            args.push(rendered.text);
        }
        let args = args.join(", ");

        RenderedType {
            text: format!("{}[{}]", base, args),
            unknown: unknown.into_boxed_slice(),
        }
    }
}

fn collect_definition_paths<'a>(
    roots: impl IntoIterator<Item = &'a Root>,
    exclusions: &Exclusions,
) -> DefinitionPaths {
    let mut paths = DefinitionPaths {
        class_paths: HashMap::new(),
        class_modules: HashMap::new(),
        enum_paths: HashMap::new(),
        interface_paths: HashMap::new(),
        interface_modules: HashMap::new(),
        known_paths: HashSet::new(),
    };

    fn walk_class(
        paths: &mut DefinitionPaths,
        class_cell: ClassRef,
        parent_path: Option<&str>,
        module_path: Option<&Arc<str>>,
        exclusions: &Exclusions,
    ) {
        if exclusions.contains(TypeRef::Class(class_cell)) {
            return;
        }
        let class = class_cell.borrow();
        let class_path: Arc<str> = if let Some(parent_path) = parent_path {
            format!("{}.{}", parent_path, class.name)
        } else {
            class.name.to_string()
        }
        .into();

        paths.class_paths.insert(class_cell, class_path.clone());
        paths.known_paths.insert(class_path.clone());

        if let Some(module_path) = module_path
            && !module_path.as_ref().is_empty()
        {
            paths.class_modules.insert(class_cell, module_path.clone());
        }

        for nested in class.classes.iter().map(ClassRef::new) {
            walk_class(
                paths,
                nested,
                Some(class_path.as_ref()),
                module_path,
                exclusions,
            );
        }
        for nested in class.interfaces.iter().map(InterfaceRef::new) {
            walk_interface(
                paths,
                nested,
                Some(class_path.as_ref()),
                module_path,
                exclusions,
            );
        }
        for nested in class.enums.iter().map(EnumRef::new) {
            walk_enum(
                paths,
                nested,
                Some(class_path.as_ref()),
                module_path,
                exclusions,
            );
        }
    }

    fn walk_interface(
        paths: &mut DefinitionPaths,
        interface_cell: InterfaceRef,
        parent_path: Option<&str>,
        module_path: Option<&Arc<str>>,
        exclusions: &Exclusions,
    ) {
        if exclusions.contains(TypeRef::Interface(interface_cell)) {
            return;
        }
        let interface = interface_cell.borrow();
        let interface_path: Arc<str> = if let Some(parent_path) = parent_path {
            format!("{}.{}", parent_path, interface.name)
        } else {
            interface.name.to_string()
        }
        .into();

        paths
            .interface_paths
            .insert(interface_cell, interface_path.clone());
        paths.known_paths.insert(interface_path.clone());

        if let Some(module_path) = module_path
            && !module_path.as_ref().is_empty()
        {
            paths
                .interface_modules
                .insert(interface_cell, module_path.clone());
        }

        for nested in interface.classes.iter().map(ClassRef::new) {
            walk_class(
                paths,
                nested,
                Some(interface_path.as_ref()),
                module_path,
                exclusions,
            );
        }
        for nested in interface.interfaces.iter().map(InterfaceRef::new) {
            walk_interface(
                paths,
                nested,
                Some(interface_path.as_ref()),
                module_path,
                exclusions,
            );
        }
        for nested in interface.enums.iter().map(EnumRef::new) {
            walk_enum(
                paths,
                nested,
                Some(interface_path.as_ref()),
                module_path,
                exclusions,
            );
        }
    }

    fn walk_enum(
        paths: &mut DefinitionPaths,
        enum_cell: EnumRef,
        parent_path: Option<&str>,
        module_path: Option<&Arc<str>>,
        exclusions: &Exclusions,
    ) {
        if exclusions.contains(TypeRef::Enum(enum_cell)) {
            return;
        }
        let r#enum = enum_cell.borrow();
        let enum_path: Arc<str> = if let Some(parent_path) = parent_path {
            format!("{}.{}", parent_path, r#enum.name)
        } else {
            r#enum.name.to_string()
        }
        .into();

        paths.enum_paths.insert(enum_cell, enum_path.clone());
        paths.known_paths.insert(enum_path.clone());

        for nested in r#enum.classes.iter().map(ClassRef::new) {
            walk_class(
                paths,
                nested,
                Some(enum_path.as_ref()),
                module_path,
                exclusions,
            );
        }
        for nested in r#enum.interfaces.iter().map(InterfaceRef::new) {
            walk_interface(
                paths,
                nested,
                Some(enum_path.as_ref()),
                module_path,
                exclusions,
            );
        }
        for nested in r#enum.enums.iter().map(EnumRef::new) {
            walk_enum(
                paths,
                nested,
                Some(enum_path.as_ref()),
                module_path,
                exclusions,
            );
        }
    }

    for root in roots {
        let ast = root.ast();
        let module_path: Option<Arc<str>> = if ast.package.trim().trim_matches('.').is_empty() {
            None
        } else {
            Some(Arc::from(ast.package.trim().trim_matches('.')))
        };
        let package_prefix = module_path.as_deref();

        for class_cell in ast.classes.iter().map(ClassRef::new) {
            walk_class(
                &mut paths,
                class_cell,
                package_prefix,
                module_path.as_ref(),
                exclusions,
            );
        }
        for interface_cell in ast.interfaces.iter().map(InterfaceRef::new) {
            walk_interface(
                &mut paths,
                interface_cell,
                package_prefix,
                module_path.as_ref(),
                exclusions,
            );
        }
        for enum_cell in ast.enums.iter().map(EnumRef::new) {
            walk_enum(
                &mut paths,
                enum_cell,
                package_prefix,
                module_path.as_ref(),
                exclusions,
            );
        }
    }

    paths
}

#[derive(Debug, Clone)]
struct DefinitionPaths {
    class_paths: HashMap<ClassRef, Arc<str>>,
    class_modules: HashMap<ClassRef, Arc<str>>,
    enum_paths: HashMap<EnumRef, Arc<str>>,
    interface_paths: HashMap<InterfaceRef, Arc<str>>,
    interface_modules: HashMap<InterfaceRef, Arc<str>>,
    known_paths: HashSet<Arc<str>>,
}

impl DefinitionPaths {
    fn class_path(&self, class_cell: &ClassRef) -> String {
        self.class_paths
            .get(class_cell)
            .map(|path| path.to_string())
            .unwrap_or_else(|| class_cell.ident().to_string())
    }

    fn class_module(&self, class_cell: ClassRef) -> Option<&str> {
        self.class_modules
            .get(&class_cell)
            .map(|module| module.as_ref())
    }

    fn interface_module(&self, interface_cell: InterfaceRef) -> Option<&str> {
        self.interface_modules
            .get(&interface_cell)
            .map(|module| module.as_ref())
    }

    fn enum_path(&self, enum_cell: &EnumRef) -> String {
        self.enum_paths
            .get(enum_cell)
            .map(|path| path.to_string())
            .unwrap_or_else(|| enum_cell.ident().to_string())
    }

    fn interface_path(&self, interface_cell: &InterfaceRef) -> String {
        self.interface_paths
            .get(interface_cell)
            .map(|path| path.to_string())
            .unwrap_or_else(|| interface_cell.ident().to_string())
    }
}
