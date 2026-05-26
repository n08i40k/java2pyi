#![allow(clippy::mutable_key_type)]

use std::{
    collections::{HashMap, HashSet},
    env, fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use rayon::{
    ThreadPoolBuilder,
    iter::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator},
};

use crate::model::{ClassRef, EnumRef, Exclusions, InterfaceRef, Root, TypeRef};
use crate::preprocess::{parse_java_ast, preprocess_asts};
use crate::pyi::write_pyi_by_package;

mod index_tree;
mod model;
mod preprocess;
mod pyi;
mod status;

const DEFAULT_RAYON_STACK_SIZE_MB: usize = 8;

fn main() {
    if std::env::var("RUST_LOG").is_err() {
        unsafe {
            std::env::set_var("RUST_LOG", "info");
        }
    }

    env_logger::init();

    let options = match parse_args(env::args().collect()) {
        Ok(options) => options,
        Err(message) => {
            if message != "help requested" {
                eprintln!("{}", message);
                eprintln!();
            }
            eprintln!("{}", usage());
            return;
        }
    };

    let files = match collect_java_files(&options.inputs, &options.excludes) {
        Ok(files) => files,
        Err(message) => {
            eprintln!("{}", message);
            return;
        }
    };

    if files.is_empty() {
        status::clear();
        eprintln!("no .java files found in provided inputs");
        return;
    }

    let total_files = files.len();
    status::update(&format!("Parsing 0/{}", total_files));
    let worker_pool = ThreadPoolBuilder::new()
        .stack_size(rayon_stack_size())
        .build()
        .unwrap();

    let asts = {
        let parse_result = worker_pool.install(|| {
            files
                .par_iter()
                .enumerate()
                .filter_map(|(index, file)| {
                    let display_name = file
                        .file_name()
                        .and_then(|name| name.to_str())
                        .map(str::to_string)
                        .unwrap_or_else(|| file.to_string_lossy().to_string());

                    status::update(&format!(
                        "Parsing {}/{}: {}",
                        index + 1,
                        total_files,
                        display_name
                    ));

                    match parse_java_ast(file) {
                        Ok(ast) => Some(Ok(Arc::new(ast))),
                        Err(e) => match &e.inner {
                            java_ast_parser::Error::UnrecognizedEof { .. } => None,
                            _ => Some(Err((file.display().to_string(), e.to_string()))),
                        },
                    }
                })
                .collect::<Result<Vec<_>, _>>()
        });

        match parse_result {
            Ok(asts) => asts,
            Err((path, error)) => {
                status::clear();
                eprintln!("failed to parse {}\n{}", path, error);
                return;
            }
        }
    };
    let mut asts = asts;
    let exclusions = apply_type_exclusions(&mut asts, &options);

    if asts.is_empty() {
        status::clear();
        eprintln!("no parsable .java files found after applying exclusions");
        return;
    }

    let scopes = worker_pool.install(|| preprocess_asts(&asts, &exclusions));

    let write_result = worker_pool.install(|| {
        write_pyi_by_package(
            &scopes,
            options.mixer_records,
            Arc::new(exclusions),
            |package, contents| {
                let file_path = package_to_path(&options.out_dir, package);
                if let Some(parent) = file_path.parent() {
                    fs::create_dir_all(parent)
                        .map_err(|error| (file_path.display().to_string(), error.to_string()))?;
                }
                fs::write(&file_path, contents)
                    .map_err(|error| (file_path.display().to_string(), error.to_string()))?;
                ensure_parent_inits(&file_path, &options.out_dir)
                    .map_err(|error| (file_path.display().to_string(), error.to_string()))?;

                Ok::<(), (String, String)>(())
            },
        )
    });

    if let Err((path, error)) = write_result {
        status::clear();
        eprintln!("failed to write {}\n{}", path, error);
        return;
    }
    status::clear();
}

fn rayon_stack_size() -> usize {
    let configured_mb = env::var("JAVA2PYI_RAYON_STACK_SIZE_MB")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_RAYON_STACK_SIZE_MB);

    configured_mb.saturating_mul(1024 * 1024)
}

fn package_to_path(out_dir: &Path, package: &str) -> PathBuf {
    let mut path = PathBuf::from(out_dir);

    if package.is_empty() {
        path.push("__init__.pyi");
        return path;
    }

    for part in package.split('.') {
        path.push(part);
    }

    path.push("__init__.pyi");
    path
}

fn ensure_parent_inits(file_path: &Path, out_dir: &Path) -> std::io::Result<()> {
    let mut current = file_path.parent();
    while let Some(dir) = current {
        if dir == out_dir {
            break;
        }

        let init_path = dir.join("__init__.py");
        if init_path != file_path && !init_path.exists() {
            fs::write(&init_path, "")?;
        }

        current = dir.parent();
    }

    Ok(())
}

struct CliOptions {
    inputs: Vec<PathBuf>,
    out_dir: PathBuf,
    excludes: Vec<PathBuf>,
    exclude_packages: Vec<String>,
    exclude_identifiers: HashSet<String>,
    mixer_records: HashMap<String, String>,
}

fn parse_args(args: Vec<String>) -> Result<CliOptions, String> {
    let mut inputs = Vec::new();
    let mut out_dir = PathBuf::from("out");
    let mut excludes = Vec::new();
    let mut exclude_packages = Vec::new();
    let mut exclude_identifiers = HashSet::new();
    let mut mixer_records = HashMap::new();
    let mut iter = args.into_iter();
    let _program = iter.next();

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "-i" | "--input" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --input".to_string())?;
                inputs.push(PathBuf::from(value));
            }
            "-o" | "--out" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --out".to_string())?;
                out_dir = PathBuf::from(value);
            }
            "-x" | "--exclude" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --exclude".to_string())?;
                excludes.push(PathBuf::from(value));
            }
            "-xp" | "--exclude-packages" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --exclude-packages".to_string())?;
                let value = normalize_qualified_name(&value);
                if !value.is_empty() {
                    exclude_packages.push(value);
                }
            }
            "-xi" | "--exclude-identifiers" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --exclude-identifiers".to_string())?;
                let value = normalize_qualified_name(&value);
                if !value.is_empty() {
                    exclude_identifiers.insert(value);
                }
            }
            "-m" | "--mix" => {
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --mix".to_string())?
                    .split(",")
                    .map(|kv| {
                        kv.split_once(":")
                            .map(|(l, r)| (l.to_string(), r.to_string()))
                            .ok_or_else(|| "invalid value passed in --mix".to_string())
                    })
                    .collect::<Result<HashMap<String, String>, _>>()?;

                mixer_records.extend(value);
            }
            "-h" | "--help" => {
                return Err(String::from("help requested"));
            }
            _ => {
                if arg.starts_with('-') {
                    return Err(format!("unknown option: {}", arg));
                }
                inputs.push(PathBuf::from(arg));
            }
        }
    }

    if inputs.is_empty() {
        return Err(String::from("no inputs provided"));
    }

    Ok(CliOptions {
        inputs,
        out_dir,
        excludes,
        exclude_packages,
        exclude_identifiers,
        mixer_records,
    })
}

fn usage() -> String {
    [
        "Usage:",
        "  java2pyi -i <path> [-i <path> ...] [-x <path> ...] [-xp <package> ...] [-xi <identifier> ...] [--out <dir>]",
        "",
        "Options:",
        "  -i, --input <path>      Input file or directory (recurses for .java)",
        "  -x, --exclude <path>    Exclude file or directory (repeatable)",
        "  -xp, --exclude-packages <package>",
        "                         Exclude package and subpackages from indexing/serialization",
        "  -xi, --exclude-identifiers <identifier>",
        "                         Exclude class/interface/enum and nested types from indexing/serialization",
        "  -m, --mix [<Java FQTN>:<Python built-in type name>,]",
        "                         Allow to use Python built-in type as Java type in variables, function args and return types",
        "  -o, --out <dir>         Output directory (default: out)",
        "  -h, --help              Show this help",
    ]
    .join("\n")
}

fn collect_java_files(inputs: &[PathBuf], excludes: &[PathBuf]) -> Result<Vec<PathBuf>, String> {
    let mut files = Vec::new();

    for input in inputs {
        if is_excluded(input, excludes) {
            continue;
        }
        let metadata = fs::metadata(input)
            .map_err(|err| format!("failed to read {}: {}", input.display(), err))?;

        if metadata.is_file() {
            if is_java_file(input) {
                files.push(input.clone());
            }
        } else if metadata.is_dir() {
            collect_java_files_in_dir(input, &mut files, excludes)?;
        }
    }

    Ok(files)
}

fn collect_java_files_in_dir(
    dir: &Path,
    files: &mut Vec<PathBuf>,
    excludes: &[PathBuf],
) -> Result<(), String> {
    for entry in
        fs::read_dir(dir).map_err(|err| format!("failed to read {}: {}", dir.display(), err))?
    {
        let entry = entry.map_err(|err| format!("failed to read {}: {}", dir.display(), err))?;
        let path = entry.path();
        if is_excluded(&path, excludes) {
            continue;
        }
        let metadata = entry
            .metadata()
            .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;

        if metadata.is_dir() {
            collect_java_files_in_dir(&path, files, excludes)?;
        } else if metadata.is_file() && is_java_file(&path) {
            files.push(path);
        }
    }

    Ok(())
}

fn is_java_file(path: &Path) -> bool {
    path.extension().and_then(|ext| ext.to_str()) == Some("java")
}

fn is_excluded(path: &Path, excludes: &[PathBuf]) -> bool {
    excludes.iter().any(|exclude| path.starts_with(exclude))
}

fn normalize_qualified_name(value: &str) -> String {
    value.trim().trim_matches('.').to_string()
}

fn is_excluded_package(package: &str, exclude_packages: &[String]) -> bool {
    exclude_packages.iter().any(|prefix| {
        package == prefix
            || package
                .strip_prefix(prefix)
                .is_some_and(|suffix| suffix.starts_with('.'))
    })
}

fn qualified_type_path(prefix: &str, ident: &str) -> String {
    if prefix.is_empty() {
        ident.to_string()
    } else {
        format!("{}.{}", prefix, ident)
    }
}

fn collect_excluded_classes(
    cells: &[java_ast_parser::ast::Class<'_>],
    prefix: &str,
    exclude_identifiers: &HashSet<String>,
    exclusions: &mut Exclusions,
) {
    for class in cells {
        let class_ref = ClassRef::new(class);
        let path = qualified_type_path(prefix, class_ref.ident());
        if exclude_identifiers.contains(&path) {
            exclusions.insert(TypeRef::Class(class_ref));
            continue;
        }

        collect_excluded_classes(&class.classes, &path, exclude_identifiers, exclusions);
        collect_excluded_interfaces(&class.interfaces, &path, exclude_identifiers, exclusions);
        collect_excluded_enums(&class.enums, &path, exclude_identifiers, exclusions);
    }
}

fn collect_excluded_interfaces(
    cells: &[java_ast_parser::ast::Interface<'_>],
    prefix: &str,
    exclude_identifiers: &HashSet<String>,
    exclusions: &mut Exclusions,
) {
    for interface in cells {
        let interface_ref = InterfaceRef::new(interface);
        let path = qualified_type_path(prefix, interface_ref.ident());
        if exclude_identifiers.contains(&path) {
            exclusions.insert(TypeRef::Interface(interface_ref));
            continue;
        }

        collect_excluded_classes(&interface.classes, &path, exclude_identifiers, exclusions);
        collect_excluded_interfaces(
            &interface.interfaces,
            &path,
            exclude_identifiers,
            exclusions,
        );
        collect_excluded_enums(&interface.enums, &path, exclude_identifiers, exclusions);
    }
}

fn collect_excluded_enums(
    cells: &[java_ast_parser::ast::Enum<'_>],
    prefix: &str,
    exclude_identifiers: &HashSet<String>,
    exclusions: &mut Exclusions,
) {
    for r#enum in cells {
        let enum_ref = EnumRef::new(r#enum);
        let path = qualified_type_path(prefix, enum_ref.ident());
        if exclude_identifiers.contains(&path) {
            exclusions.insert(TypeRef::Enum(enum_ref));
            continue;
        }

        collect_excluded_classes(&r#enum.classes, &path, exclude_identifiers, exclusions);
        collect_excluded_interfaces(&r#enum.interfaces, &path, exclude_identifiers, exclusions);
        collect_excluded_enums(&r#enum.enums, &path, exclude_identifiers, exclusions);
    }
}

fn apply_type_exclusions(asts: &mut Vec<Arc<Root>>, options: &CliOptions) -> Exclusions {
    let mut exclusions = Exclusions::default();
    if options.exclude_packages.is_empty() && options.exclude_identifiers.is_empty() {
        return exclusions;
    }

    asts.retain(|root| {
        let ast = root.ast();
        if is_excluded_package(ast.package, &options.exclude_packages) {
            return false;
        }

        collect_excluded_classes(
            &ast.classes,
            ast.package,
            &options.exclude_identifiers,
            &mut exclusions,
        );
        collect_excluded_interfaces(
            &ast.interfaces,
            ast.package,
            &options.exclude_identifiers,
            &mut exclusions,
        );
        collect_excluded_enums(
            &ast.enums,
            ast.package,
            &options.exclude_identifiers,
            &mut exclusions,
        );

        ast.classes
            .iter()
            .map(ClassRef::new)
            .any(|class| !exclusions.contains(TypeRef::Class(class)))
            || ast
                .interfaces
                .iter()
                .map(InterfaceRef::new)
                .any(|interface| !exclusions.contains(TypeRef::Interface(interface)))
            || ast
                .enums
                .iter()
                .map(EnumRef::new)
                .any(|r#enum| !exclusions.contains(TypeRef::Enum(r#enum)))
    });

    exclusions
}
