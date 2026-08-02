use std::borrow::Cow;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use crate::exclude::Exclusions;
use crate::index_tree::GlobalIndexTree;
use crate::ir::{self, ClassType, Method, Modifiers, Type, TypeParameter};
use crate::model::{
    ClassRef, Root, array_depth, base_type, object_parts, primitive_python_type, type_args,
};
use crate::{scope::Scope, status};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

const PYI_PACKAGE: &str = "java2pyi";
const PYI_TYPES_SUBPACKAGE: &str = "types";

pub fn write_pyi_by_package<'a, E>(
    scopes: &[Scope<'a>],
    mixer_records: HashMap<String, String>,
    exclusions: Arc<Exclusions<'a>>,
    write_package: impl Fn(&str, String) -> Result<(), E> + Sync,
) -> Result<(), E>
where
    E: Send,
{
    let definition_paths = Arc::new(collect_definition_paths(
        scopes.iter().map(|scope| scope.root),
        &exclusions,
    ));

    let mut scopes_by_package: HashMap<&str, Vec<&Scope>> = HashMap::new();
    for scope in scopes {
        scopes_by_package
            .entry(scope.root.ir().package)
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

            let has_deprecated = sanitize_path(package) != package
                || package_scopes.iter().any(|scope| {
                    scope
                        .root
                        .ir()
                        .classes
                        .iter()
                        .map(ClassRef::new)
                        .any(|class_cell| uses_deprecated(class_cell, &exclusions, false))
                });

            let mut emitter = PyiEmitter::new(
                definition_paths.clone(),
                module_imports,
                &mixer,
                exclusions.clone(),
            );

            emitter.emit_header(has_deprecated);

            let empty_type_params = BTreeSet::new();
            for scope in &package_scopes {
                for class_cell in scope.root.ir().classes.iter().map(ClassRef::new) {
                    emitter.emit_class(
                        class_cell,
                        &scope.index_tree,
                        &empty_type_params,
                        &[],
                        false,
                    );
                }

                emitter.emit_deferred(&scope.index_tree);
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

            write_package(&sanitize_path(package), emitter.finish())
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
                .map(|(k, v)| {
                    let k = sanitize_path(&k).into_owned();
                    (k.clone(), MixerEntry::new(k, v))
                })
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
        let mut entries = self.records.values().collect::<Vec<_>>();
        entries.sort_by(|left, right| left.from_java_ty.cmp(&right.from_java_ty));

        let mut namespaces = entries
            .iter()
            .map(|entry| entry.from_java_ty.rsplit_once(".").unwrap().0)
            .collect::<Vec<_>>();
        namespaces.dedup();

        let imports = namespaces
            .into_iter()
            .map(|ns| format!("import {ns}"))
            .collect::<Box<[_]>>()
            .join("\n");

        let body = entries
            .into_iter()
            .map(MixerEntry::gen_union)
            .collect::<Box<[_]>>()
            .join("\n");

        format!("from typing import Any, Union\n{imports}\n\n{body}")
    }
}

struct PyiEmitter<'a, 'm> {
    output: String,
    indent: usize,
    type_renderer: TypeRenderer<'a, 'm>,
    definition_paths: Arc<DefinitionPaths<'a>>,
    module_imports: BTreeSet<String>,
    exclusions: Arc<Exclusions<'a>>,
    deferred: VecDeque<DeferredClass<'a>>,
}

struct DeferredClass<'a> {
    class: ClassRef<'a>,
    scope_params: Vec<TypeParameter<'a>>,
}

impl<'a, 'm> PyiEmitter<'a, 'm> {
    fn new(
        definition_paths: Arc<DefinitionPaths<'a>>,
        module_imports: BTreeSet<String>,
        mixer: &'m Mixer,
        exclusions: Arc<Exclusions<'a>>,
    ) -> Self {
        Self {
            output: String::new(),
            indent: 0,
            type_renderer: TypeRenderer::new(definition_paths.clone(), mixer),
            definition_paths,
            module_imports,
            exclusions,
            deferred: VecDeque::new(),
        }
    }

    fn emit_header(&mut self, has_deprecated: bool) {
        self.line("from __future__ import annotations".to_string());
        self.line(format!("import {}.{}", PYI_PACKAGE, PYI_TYPES_SUBPACKAGE));
        let module_imports = std::mem::take(&mut self.module_imports);
        for module_import in module_imports {
            self.line(format!("import {}", sanitize_path(&module_import)));
        }
        self.line("from typing import Any, ClassVar, overload".to_string());
        if has_deprecated {
            self.line("from typing_extensions import deprecated".to_string());
        }
        self.blank_line();
    }

    fn emit_deferred(&mut self, index_tree: &GlobalIndexTree<'a>) {
        let empty_type_params = BTreeSet::new();

        while let Some(deferred) = self.deferred.pop_front() {
            self.emit_class(
                deferred.class,
                index_tree,
                &empty_type_params,
                &deferred.scope_params,
                false,
            );
        }
    }

    fn emit_class(
        &mut self,
        class_cell: ClassRef<'a>,
        index_tree: &GlobalIndexTree<'a>,
        outer_type_params: &BTreeSet<String>,
        scope_params: &[TypeParameter<'a>],
        nested: bool,
    ) {
        if self.exclusions.contains(class_cell) {
            return;
        }
        let class = &*class_cell;
        let declared_type_params = declared_type_params(class, scope_params);
        let class_type_params = extend_type_params(outer_type_params, &declared_type_params);
        let type_params_suffix = format_type_params(&declared_type_params);
        let class_path = self.definition_paths.path(&class_cell);
        let mut rendered_bases =
            collect_base_types(class, &self.type_renderer, index_tree, &class_type_params);
        let mut inserted_special_base = false;
        if let Some(special_base) = java_stdlib_python_base(&class_path, class.type_params)
            && !rendered_bases
                .bases
                .iter()
                .any(|base| base == &special_base)
        {
            rendered_bases.bases.insert(0, special_base);
            inserted_special_base = true;
        }

        let bases_suffix = match class.r#type {
            ClassType::Class | ClassType::Enum => {
                if class_path != "java.lang.Object" && class.extends.is_none() {
                    let object_base = "java.lang.Object".to_string();
                    if !rendered_bases.bases.iter().any(|base| base == &object_base) {
                        let insert_at = if inserted_special_base { 1 } else { 0 };
                        let bounded_index = insert_at.min(rendered_bases.bases.len());
                        rendered_bases.bases.insert(bounded_index, object_base);
                    }
                }

                if rendered_bases.bases.is_empty() {
                    String::new()
                } else {
                    format!("({})", rendered_bases.bases.join(", "))
                }
            }
            ClassType::Interface => {
                if rendered_bases.bases.is_empty() {
                    "(java.lang.Object)".to_string()
                } else {
                    format!("(java.lang.Object, {})", rendered_bases.bases.join(", "))
                }
            }
        };

        let emitted_name = sanitize_ident(class.name);
        let mut line = format!(
            "class {}{}{}:",
            emitted_name, type_params_suffix, bases_suffix
        );
        if !rendered_bases.unknown.is_empty() {
            line.push_str(&format!(
                "  # unknown type(s) [{}] used in {}",
                rendered_bases.unknown.join(", "),
                class_path
            ));
        }

        let emitted_path = sanitize_path(&class_path);

        let mut notes = Vec::new();
        if let Some(fqn) = class.anonymous {
            notes.push(format!("Anonymous class {}", fqn));
        } else if nested && class.modifiers.contains(Modifiers::PRIVATE) {
            notes.push(format!("Private class {}", emitted_path));
        }
        if class.anonymous.is_none() && emitted_path != class_path {
            notes.push(format!("Renamed from {}", class_path));
        }
        if !notes.is_empty() {
            self.line(format!("@deprecated(\"{}\")", notes.join("; ")));
        }

        self.line(line);
        self.indent += 1;

        let mut has_members = false;

        let docstring = match class.r#type {
            ClassType::Interface => Some("Java interface."),
            ClassType::Class if class.modifiers.contains(Modifiers::ABSTRACT) => {
                Some("Java abstract class.")
            }
            _ => None,
        };

        if let Some(docstring) = docstring {
            has_members = true;
            self.line(format!("\"\"\"{}\"\"\"", docstring));
        }

        for variable in class.fields {
            has_members = true;
            let rendered = self.type_renderer.render_in_scope(
                &class_path,
                &variable.r#type,
                index_tree,
                &class_type_params,
            );
            let ident = sanitize_ident(variable.name);
            let mut line = if variable.modifiers.contains(Modifiers::STATIC) {
                format!("{}: ClassVar[{}]", ident, rendered.text)
            } else {
                format!("{}: {}", ident, rendered.text)
            };

            let mut notes = Vec::new();
            if rendered.has_unknown() {
                notes.push(format!(
                    "unknown type(s) [{}] used in {}.{}",
                    rendered.unknown.join(", "),
                    class_path,
                    variable.name
                ));
            }
            if ident != variable.name {
                notes.push(format!("renamed from {}.{}", class_path, variable.name));
            }
            if !notes.is_empty() {
                line.push_str(&format!("  # {}", notes.join("; ")));
            }

            self.line(line);
        }

        let method_groups = group_methods(class.methods);
        for method_group in method_groups {
            let use_overload = method_group.len() > 1;
            for method in method_group {
                has_members = true;
                let method_type_params = extend_type_params(&class_type_params, method.type_params);
                self.emit_method(
                    method,
                    use_overload,
                    &class_path,
                    index_tree,
                    &method_type_params,
                );
            }
        }

        // Names declared here shadow the enclosing ones, so they must not be carried twice.
        let child_scope_params = if class.children.is_empty() {
            Vec::new()
        } else {
            scope_params
                .iter()
                .filter(|outer| {
                    !declared_type_params
                        .iter()
                        .any(|declared| declared.name == outer.name)
                })
                .chain(declared_type_params.as_ref())
                .copied()
                .collect()
        };

        for child in class.children.iter().map(ClassRef::new) {
            if self.exclusions.contains(child) {
                continue;
            }

            if child.anonymous.is_some() {
                self.deferred.push_back(DeferredClass {
                    class: child,
                    scope_params: child_scope_params.clone(),
                });
                continue;
            }

            has_members = true;
            self.emit_class(
                child,
                index_tree,
                &class_type_params,
                &child_scope_params,
                true,
            );
        }

        if !has_members {
            self.line("...".to_string());
        }

        self.indent -= 1;
        self.blank_line();
    }

    fn emit_method(
        &mut self,
        method: &Method<'_>,
        use_overload: bool,
        class_path: &str,
        index_tree: &GlobalIndexTree<'a>,
        type_params: &BTreeSet<String>,
    ) {
        if use_overload {
            self.line("@overload".to_string());
        }

        let is_static = method.modifiers.intersects(Modifiers::STATIC);
        let is_ctor = method.name == "__ctor";
        let emitted_name = if is_ctor {
            Cow::Borrowed("__init__")
        } else {
            sanitize_ident(method.name)
        };

        if is_static {
            self.line("@staticmethod".to_string());
        }

        let mut notes = Vec::new();
        if method.modifiers.contains(Modifiers::PRIVATE) {
            notes.push(format!(
                "Private method {}.{}",
                sanitize_path(class_path),
                emitted_name
            ));
        }
        if !is_ctor && emitted_name != method.name {
            notes.push(format!("Renamed from {}.{}", class_path, method.name));
        }
        if !notes.is_empty() {
            self.line(format!("@deprecated(\"{}\")", notes.join("; ")));
        }

        let mut args = Vec::with_capacity(method.args.len() + usize::from(!is_static));
        if !is_static {
            if class_path == "java.lang.Object" && method.name == "getClass" {
                args.push("self = None".to_string());
            } else {
                args.push("self".to_string());
            }
        }

        let mut unknown_paths = HashMap::new();
        let mut renamed_args = Vec::new();
        for argument in method.args {
            let rendered = self.type_renderer.render_in_scope(
                class_path,
                &argument.r#type,
                index_tree,
                type_params,
            );

            let arg_prefix = if argument.vararg { "*" } else { "" };
            let ident = match sanitize_ident(argument.name) {
                ident if !is_static && ident == "self" => Cow::Owned(format!("{}_", ident)),
                ident => ident,
            };

            args.push(format!("{}{}: {}", arg_prefix, ident, rendered.text));

            if ident != argument.name {
                renamed_args.push(format!("{}.{}.{}", class_path, method.name, argument.name));
            }

            if rendered.has_unknown() {
                unknown_paths.insert(
                    format!("{}.{}.{}", class_path, method.name, argument.name),
                    rendered.unknown,
                );
            }
        }

        let rendered_return = match &method.return_type {
            None => RenderedType::known("None".to_string()),
            Some(return_type) if is_ctor => self.type_renderer.render_constructor_return(
                class_path,
                return_type,
                index_tree,
                type_params,
            ),
            Some(return_type) => {
                self.type_renderer
                    .render_in_scope(class_path, return_type, index_tree, type_params)
            }
        };

        if rendered_return.has_unknown() {
            unknown_paths.insert(
                format!("{}.{}", class_path, method.name),
                rendered_return.unknown,
            );
        }

        let type_params_suffix = format_type_params(method.type_params);

        let mut line = format!(
            "def {}{}({}) -> {}: ...",
            emitted_name,
            type_params_suffix,
            args.join(", "),
            rendered_return.text
        );

        let mut comments = Vec::new();
        if !unknown_paths.is_empty() {
            let mut paths = unknown_paths.into_iter().collect::<Vec<_>>();
            paths.sort_by(|(left, _), (right, _)| left.cmp(right));

            comments.push(format!(
                "unknown type(s) used in {}",
                paths
                    .into_iter()
                    .map(|(k, v)| format!("{} -> [{}]", k, v.join(", ")))
                    .collect::<Box<[_]>>()
                    .join("; ")
            ));
        }
        if !renamed_args.is_empty() {
            comments.push(format!("renamed arg(s) {}", renamed_args.join(", ")));
        }
        if !comments.is_empty() {
            line.push_str(&format!("  # {}", comments.join("; ")));
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

fn group_methods<'a>(methods: &'a [Method<'a>]) -> Vec<Vec<&'a Method<'a>>> {
    let mut order: Vec<&'a str> = Vec::with_capacity(methods.len());
    let mut grouped: HashMap<&'a str, Vec<&Method<'a>>> = HashMap::with_capacity(methods.len());

    for method in methods {
        if !grouped.contains_key(method.name) {
            order.push(method.name);
        }
        grouped.entry(method.name).or_default().push(method);
    }

    order
        .into_iter()
        .filter_map(|name| grouped.remove(&name))
        .collect()
}

fn is_unknown_char(c: char) -> bool {
    !matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')
}

fn is_unsanitized(ident: &str) -> bool {
    is_python_keyword(ident) || ident.contains(is_unknown_char)
}

fn sanitize_ident(ident: &str) -> Cow<'_, str> {
    if is_python_keyword(ident) {
        Cow::Owned(format!("{}_", ident))
    } else if ident.contains(is_unknown_char) {
        Cow::Owned(
            ident
                .chars()
                .map(|c| if is_unknown_char(c) { '_' } else { c })
                .collect::<String>(),
        )
    } else {
        Cow::Borrowed(ident)
    }
}

fn sanitize_path(path: &str) -> Cow<'_, str> {
    if path.split('.').any(is_unsanitized) {
        Cow::Owned(
            path.split('.')
                .map(sanitize_ident)
                .collect::<Box<[_]>>()
                .join("."),
        )
    } else {
        Cow::Borrowed(path)
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

fn collect_module_imports<'a>(
    scopes: &[&Scope<'a>],
    definition_paths: &DefinitionPaths<'a>,
    exclusions: &Exclusions<'a>,
) -> BTreeSet<String> {
    let mut modules = BTreeSet::new();

    fn collect_from_type<'a>(
        r#type: &Type<'_>,
        definition_paths: &DefinitionPaths<'a>,
        modules: &mut BTreeSet<String>,
        index_tree: &GlobalIndexTree<'a>,
        type_params: &BTreeSet<String>,
    ) {
        match base_type(r#type) {
            Type::ParameterUpperBound(bound) | Type::ParameterLowerBound(bound) => {
                collect_from_type(bound, definition_paths, modules, index_tree, type_params);
                return;
            }
            Type::Object(..) => {}
            _ => return,
        }

        if let Some(class_cell) = resolve_type(index_tree, r#type, type_params)
            && let Some(module_path) = definition_paths.module(class_cell)
            && !module_path.is_empty()
        {
            modules.insert(module_path.to_string());
        }

        for argument in type_args(r#type) {
            collect_from_type(argument, definition_paths, modules, index_tree, type_params);
        }
    }

    fn collect_from_method<'a>(
        method: &Method<'_>,
        definition_paths: &DefinitionPaths<'a>,
        modules: &mut BTreeSet<String>,
        index_tree: &GlobalIndexTree<'a>,
        type_params: &BTreeSet<String>,
    ) {
        if let Some(return_type) = &method.return_type {
            collect_from_type(
                return_type,
                definition_paths,
                modules,
                index_tree,
                type_params,
            );
        }

        for argument in method.args {
            collect_from_type(
                &argument.r#type,
                definition_paths,
                modules,
                index_tree,
                type_params,
            );
        }
    }

    fn collect_from_class<'a>(
        class_cell: ClassRef<'a>,
        definition_paths: &DefinitionPaths<'a>,
        modules: &mut BTreeSet<String>,
        index_tree: &GlobalIndexTree<'a>,
        outer_type_params: &BTreeSet<String>,
        exclusions: &Exclusions<'a>,
    ) {
        if exclusions.contains(class_cell) {
            return;
        }
        let class = &*class_cell;
        let class_type_params = extend_type_params(outer_type_params, class.type_params);

        for supertype in base_types(class) {
            collect_from_type(
                supertype,
                definition_paths,
                modules,
                index_tree,
                &class_type_params,
            );
        }

        for field in class.fields {
            collect_from_type(
                &field.r#type,
                definition_paths,
                modules,
                index_tree,
                &class_type_params,
            );
        }

        for method in class.methods {
            let method_type_params = extend_type_params(&class_type_params, method.type_params);
            collect_from_method(
                method,
                definition_paths,
                modules,
                index_tree,
                &method_type_params,
            );
        }

        for child in class.children.iter().map(ClassRef::new) {
            collect_from_class(
                child,
                definition_paths,
                modules,
                index_tree,
                &class_type_params,
                exclusions,
            );
        }
    }

    let empty_type_params = BTreeSet::new();
    for scope in scopes {
        for class_cell in scope.root.ir().classes.iter().map(ClassRef::new) {
            collect_from_class(
                class_cell,
                definition_paths,
                &mut modules,
                &scope.index_tree,
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

fn generic_ident_or_any(type_params: &[TypeParameter<'_>], index: usize) -> String {
    type_params
        .get(index)
        .map(|param| sanitize_ident(param.name).into_owned())
        .unwrap_or_else(|| "Any".to_string())
}

fn java_stdlib_python_base(
    definition_path: &str,
    type_params: &[TypeParameter<'_>],
) -> Option<String> {
    match definition_path {
        "java.util.Map" => {
            let key = generic_ident_or_any(type_params, 0);
            let value = generic_ident_or_any(type_params, 1);
            Some(format!("dict[{}, {}]", key, value))
        }
        "java.util.List" => Some(format!("list[{}]", generic_ident_or_any(type_params, 0))),
        "java.util.Set" => Some(format!("set[{}]", generic_ident_or_any(type_params, 0))),
        "java.lang.Boolean" => Some("bool".to_string()),
        "java.lang.Integer" | "java.lang.Byte" | "java.lang.Long" | "java.lang.Short" => {
            Some("int".to_string())
        }
        "java.lang.Double" | "java.lang.Float" => Some("float".to_string()),
        "java.lang.String" => Some("str".to_string()),
        _ => None,
    }
}

fn base_types<'s, 'a>(class: &'s ir::Class<'a>) -> impl Iterator<Item = &'s Type<'a>> {
    extends_of(class).into_iter().chain(class.implements)
}

fn extends_of<'s, 'a>(class: &'s ir::Class<'a>) -> Option<&'s Type<'a>> {
    match class.r#type {
        ClassType::Class | ClassType::Enum => class.extends.as_ref(),
        ClassType::Interface => None,
    }
}

fn collect_base_types<'a>(
    class: &ir::Class<'_>,
    type_renderer: &TypeRenderer<'a, '_>,
    index_tree: &GlobalIndexTree<'a>,
    type_params: &BTreeSet<String>,
) -> RenderedBases {
    let mut bases = Vec::with_capacity(class.implements.len() + 1);
    let mut unknown = Vec::new();

    let mut is_first_supertype = extends_of(class).is_some();

    for supertype in base_types(class) {
        let rendered = type_renderer.render(supertype, index_tree, type_params);
        unknown.extend(rendered.unknown);

        if is_first_supertype && !unknown.is_empty() {
            bases.push("java.lang.Object".to_string());
        } else {
            bases.push(rendered.text);
        }

        is_first_supertype = false;
    }

    RenderedBases {
        bases,
        unknown: unknown.into_boxed_slice(),
    }
}

fn uses_deprecated<'a>(
    class_cell: ClassRef<'a>,
    exclusions: &Exclusions<'a>,
    nested: bool,
) -> bool {
    if exclusions.contains(class_cell) {
        return false;
    }

    if class_cell.anonymous.is_some() {
        return true;
    }

    if nested && class_cell.modifiers.contains(Modifiers::PRIVATE) {
        return true;
    }

    if sanitize_ident(class_cell.name) != class_cell.name {
        return true;
    }

    class_cell.methods.iter().any(|method| {
        method.modifiers.contains(Modifiers::PRIVATE) || sanitize_ident(method.name) != method.name
    }) || class_cell
        .children
        .iter()
        .map(ClassRef::new)
        .any(|child| uses_deprecated(child, exclusions, true))
}

fn declared_type_params<'a>(
    class: &ir::Class<'a>,
    scope_params: &[TypeParameter<'a>],
) -> Cow<'a, [TypeParameter<'a>]> {
    if class.anonymous.is_none() || scope_params.is_empty() {
        return Cow::Borrowed(class.type_params);
    }

    let mut referenced = BTreeSet::new();
    collect_param_refs(class, &mut referenced);

    Cow::Owned(
        scope_params
            .iter()
            .filter(|param| {
                referenced.contains(param.name)
                    && !class.type_params.iter().any(|own| own.name == param.name)
            })
            .chain(class.type_params)
            .copied()
            .collect(),
    )
}

fn collect_param_refs<'a>(class: &ir::Class<'a>, into: &mut BTreeSet<&'a str>) {
    for supertype in base_types(class) {
        collect_type_param_refs(supertype, into);
    }

    for field in class.fields {
        collect_type_param_refs(&field.r#type, into);
    }

    for method in class.methods {
        let mut refs = BTreeSet::new();

        if let Some(return_type) = &method.return_type {
            collect_type_param_refs(return_type, &mut refs);
        }
        for argument in method.args {
            collect_type_param_refs(&argument.r#type, &mut refs);
        }

        extend_unshadowed(into, refs, method.type_params);
    }

    for child in class.children {
        if child.anonymous.is_some() {
            continue;
        }

        let mut refs = BTreeSet::new();
        collect_param_refs(child, &mut refs);

        extend_unshadowed(into, refs, child.type_params);
    }
}

fn extend_unshadowed<'a>(
    into: &mut BTreeSet<&'a str>,
    refs: BTreeSet<&'a str>,
    shadowing: &[TypeParameter<'_>],
) {
    into.extend(
        refs.into_iter()
            .filter(|name| !shadowing.iter().any(|param| param.name == *name)),
    );
}

fn collect_type_param_refs<'a>(r#type: &Type<'a>, into: &mut BTreeSet<&'a str>) {
    match r#type {
        Type::ParameterRef(name) => {
            into.insert(name);
        }
        Type::Array(inner)
        | Type::ParameterUpperBound(inner)
        | Type::ParameterLowerBound(inner) => collect_type_param_refs(inner, into),
        Type::Object(_, parts) => {
            for part in *parts {
                for argument in part.type_args {
                    collect_type_param_refs(argument, into);
                }
            }
        }
        _ => {}
    }
}

fn extend_type_params(
    base: &BTreeSet<String>,
    type_params: &[TypeParameter<'_>],
) -> BTreeSet<String> {
    let mut combined = base.clone();
    for param in type_params {
        combined.insert(param.name.to_string());
    }
    combined
}

fn format_type_params(type_params: &[TypeParameter<'_>]) -> String {
    if type_params.is_empty() {
        return String::new();
    }

    let params = type_params
        .iter()
        .map(|param| sanitize_ident(param.name))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{}]", params)
}

struct TypeRenderer<'a, 'm> {
    definition_paths: Arc<DefinitionPaths<'a>>,
    mixer: &'m Mixer,
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

    fn unknown(ty: &Type<'_>) -> Self {
        Self {
            text: "Any".to_string(),
            unknown: Box::from([ty.to_string()]),
        }
    }

    fn has_unknown(&self) -> bool {
        !self.unknown.is_empty()
    }
}

fn resolve_type<'a>(
    index_tree: &GlobalIndexTree<'a>,
    ty: &Type<'_>,
    type_params: &BTreeSet<String>,
) -> Option<ClassRef<'a>> {
    if let Some((packages, parts)) = object_parts(ty)
        && packages.is_empty()
        && parts.len() == 1
        && type_params.contains(parts[0].ident)
    {
        return None;
    }

    index_tree.search(ty)
}

impl<'a, 'm> TypeRenderer<'a, 'm> {
    fn new(definition_paths: Arc<DefinitionPaths<'a>>, mixer: &'m Mixer) -> Self {
        Self {
            definition_paths,
            mixer,
        }
    }

    fn render(
        &self,
        ty: &Type<'_>,
        index_tree: &GlobalIndexTree,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let mut rendered = self.render_base(ty, index_tree, type_params);

        for _ in 0..array_depth(ty) {
            rendered.text = format!("list[{}]", rendered.text);
        }

        rendered
    }

    fn render_base(
        &self,
        ty: &Type<'_>,
        index_tree: &GlobalIndexTree,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        match base_type(ty) {
            Type::ParameterUnbound => RenderedType::known("Any".to_string()),
            Type::ParameterUpperBound(bound) | Type::ParameterLowerBound(bound) => {
                let rendered = self.render(bound, index_tree, type_params);
                RenderedType {
                    text: "Any".to_string(),
                    unknown: rendered.unknown,
                }
            }
            Type::ParameterRef(name) if type_params.contains(*name) => {
                RenderedType::known(sanitize_ident(name).into_owned())
            }
            object @ Type::Object(..) => match resolve_type(index_tree, object, type_params) {
                Some(class_cell) => self.render_named_type(
                    self.definition_paths.path(&class_cell),
                    type_args(object),
                    index_tree,
                    type_params,
                ),
                None => RenderedType::unknown(object),
            },
            unresolved @ Type::ParameterRef(_) => RenderedType::unknown(unresolved),
            primitive => RenderedType::known(
                primitive_python_type(primitive)
                    .unwrap_or("Any")
                    .to_string(),
            ),
        }
    }

    fn render_in_scope(
        &self,
        scope_path: &str,
        ty: &Type<'_>,
        index_tree: &GlobalIndexTree,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let rendered = self.render(ty, index_tree, type_params);
        if !rendered.has_unknown() {
            return rendered;
        }

        let Some((packages, parts)) = object_parts(ty) else {
            return rendered;
        };

        if !packages.is_empty() || parts.len() != 1 || type_params.contains(parts[0].ident) {
            return rendered;
        }

        let candidate = format!("{}.{}", scope_path, parts[0].ident);
        if !self
            .definition_paths
            .known_paths
            .contains(candidate.as_str())
        {
            return rendered;
        }

        let mut nested = self.render_named_type(candidate, type_args(ty), index_tree, type_params);
        for _ in 0..array_depth(ty) {
            nested.text = format!("list[{}]", nested.text);
        }

        nested
    }

    fn render_constructor_return(
        &self,
        class_path: &str,
        ty: &Type<'_>,
        index_tree: &GlobalIndexTree,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let mut rendered = self.render_named_type(
            class_path.to_string(),
            type_args(ty),
            index_tree,
            type_params,
        );

        for _ in 0..array_depth(ty) {
            rendered.text = format!("list[{}]", rendered.text);
        }

        rendered
    }

    fn render_named_type(
        &self,
        base: String,
        arguments: &[Type<'_>],
        index_tree: &GlobalIndexTree,
        type_params: &BTreeSet<String>,
    ) -> RenderedType {
        let base = sanitize_path(&base);

        if arguments.is_empty() {
            return RenderedType::known(self.mixer.try_mix(&base));
        }

        let mut unknown = Vec::new();
        let mut args = Vec::with_capacity(arguments.len());
        for argument in arguments {
            let rendered = self.render(argument, index_tree, type_params);
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
    exclusions: &Exclusions<'a>,
) -> DefinitionPaths<'a> {
    let mut paths = DefinitionPaths {
        paths: HashMap::new(),
        modules: HashMap::new(),
        known_paths: HashSet::new(),
    };

    fn walk_class<'a>(
        paths: &mut DefinitionPaths<'a>,
        class_cell: ClassRef<'a>,
        parent_path: Option<&str>,
        module_path: Option<&Arc<str>>,
        exclusions: &Exclusions<'a>,
    ) {
        if exclusions.contains(class_cell) {
            return;
        }
        let class = &*class_cell;
        let class_path: Arc<str> = match (class.anonymous.is_some(), parent_path) {
            (true, _) => match module_path {
                Some(module) if !module.is_empty() => format!("{}.{}", module, class.name),
                _ => class.name.to_string(),
            },
            (false, Some(parent_path)) => format!("{}.{}", parent_path, class.name),
            (false, None) => class.name.to_string(),
        }
        .into();

        paths.paths.insert(class_cell, class_path.clone());
        paths.known_paths.insert(class_path.clone());

        if let Some(module_path) = module_path
            && !module_path.as_ref().is_empty()
        {
            paths.modules.insert(class_cell, module_path.clone());
        }

        for child in class.children.iter().map(ClassRef::new) {
            walk_class(
                paths,
                child,
                Some(class_path.as_ref()),
                module_path,
                exclusions,
            );
        }
    }

    for root in roots {
        let ir = root.ir();
        let module_path: Option<Arc<str>> = if ir.package.trim().trim_matches('.').is_empty() {
            None
        } else {
            Some(Arc::from(ir.package.trim().trim_matches('.')))
        };
        let package_prefix = module_path.as_deref();

        for class_cell in ir.classes.iter().map(ClassRef::new) {
            walk_class(
                &mut paths,
                class_cell,
                package_prefix,
                module_path.as_ref(),
                exclusions,
            );
        }
    }

    paths
}

#[derive(Debug, Clone)]
struct DefinitionPaths<'a> {
    paths: HashMap<ClassRef<'a>, Arc<str>>,
    modules: HashMap<ClassRef<'a>, Arc<str>>,
    known_paths: HashSet<Arc<str>>,
}

impl<'a> DefinitionPaths<'a> {
    fn path(&self, class_cell: &ClassRef<'a>) -> String {
        self.paths
            .get(class_cell)
            .map(|path| path.to_string())
            .unwrap_or_else(|| class_cell.name.to_string())
    }

    fn module(&self, class_cell: ClassRef<'a>) -> Option<&str> {
        self.modules.get(&class_cell).map(|module| module.as_ref())
    }
}
