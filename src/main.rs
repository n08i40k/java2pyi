#![allow(clippy::mutable_key_type)]

use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use rayon::{
    ThreadPoolBuilder,
    iter::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator},
};

use crate::preprocess::{parse_java_ast, preprocess_asts};
use crate::pyi::generate_pyi_by_package;

mod index_tree;
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
        let mut result = Vec::new();

        let parse_results = worker_pool.install(|| {
            files
                .par_iter()
                .enumerate()
                .map(|(index, file)| {
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
                        Ok(ast) => Ok(Some(Arc::new(ast))),
                        Err(e) => match &e.inner {
                            java_ast_parser::Error::UnrecognizedEof { .. } => Ok(None),
                            _ => Err((file.display().to_string(), e.to_string())),
                        },
                    }
                })
                .collect::<Vec<_>>()
        });

        for parse_result in parse_results {
            match parse_result {
                Ok(Some(ast)) => result.push(ast),
                Ok(None) => {}
                Err((path, error)) => {
                    status::clear();
                    eprintln!("failed to parse {}\n{}", path, error);
                    return;
                }
            }
        }

        result
    };

    if asts.is_empty() {
        status::clear();
        eprintln!("no parsable .java files found");
        return;
    }

    worker_pool.install(|| preprocess_asts(&asts));

    let outputs = worker_pool.install(|| generate_pyi_by_package(&asts));
    let output_items = outputs.into_iter().collect::<Vec<_>>();

    let total_outputs = output_items.len();
    status::update(&format!("Writing 0/{}", total_outputs));
    let write_progress = AtomicUsize::new(0);
    let write_results = worker_pool.install(|| {
        output_items
            .par_iter()
            .map(|(package, contents)| {
                let file_path = package_to_path(&options.out_dir, package);
                if let Some(parent) = file_path.parent() {
                    fs::create_dir_all(parent)
                        .map_err(|error| (file_path.display().to_string(), error.to_string()))?;
                }
                fs::write(&file_path, contents)
                    .map_err(|error| (file_path.display().to_string(), error.to_string()))?;
                ensure_parent_inits(&file_path, &options.out_dir)
                    .map_err(|error| (file_path.display().to_string(), error.to_string()))?;

                let completed = write_progress.fetch_add(1, Ordering::Relaxed) + 1;
                let label = if package.is_empty() {
                    "<root>"
                } else {
                    package.as_str()
                };
                status::update(&format!(
                    "Writing {}/{}: {}",
                    completed, total_outputs, label
                ));

                Ok::<(), (String, String)>(())
            })
            .collect::<Vec<_>>()
    });

    for write_result in write_results {
        if let Err((path, error)) = write_result {
            status::clear();
            eprintln!("failed to write {}\n{}", path, error);
            return;
        }
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
}

fn parse_args(args: Vec<String>) -> Result<CliOptions, String> {
    let mut inputs = Vec::new();
    let mut out_dir = PathBuf::from("out");
    let mut excludes = Vec::new();
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
    })
}

fn usage() -> String {
    [
        "Usage:",
        "  java2pyi -i <path> [-i <path> ...] [-x <path> ...] [--out <dir>]",
        "",
        "Options:",
        "  -i, --input <path>      Input file or directory (recurses for .java)",
        "  -x, --exclude <path>    Exclude file or directory (repeatable)",
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
