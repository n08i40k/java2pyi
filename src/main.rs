#![allow(clippy::mutable_key_type)]

use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use rayon::{
    ThreadPoolBuilder,
    iter::{IntoParallelRefIterator, ParallelIterator},
};

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    cli::{ArgsError, Options},
    model::Root,
    pyi::write_pyi_by_package,
    scope::Scope,
};

mod cli;
mod exclude;
mod index_tree;
mod ir;
mod jar;
mod model;
mod pyi;
mod scope;
mod sign;
mod status;

const DEFAULT_RAYON_STACK_SIZE_MB: usize = 8;

fn main() {
    if env::var("RUST_LOG").is_err() {
        unsafe {
            env::set_var("RUST_LOG", "info");
        }
    }

    env_logger::init();

    let options = match cli::parse(env::args()) {
        Ok(options) => options,
        Err(ArgsError::HelpRequested) => {
            eprintln!("{}", cli::usage());
            return;
        }
        Err(ArgsError::Invalid(message)) => {
            eprintln!("{}\n", message);
            eprintln!("{}", cli::usage());
            return;
        }
    };

    if let Err(message) = run(&options) {
        status::clear();
        eprintln!("{}", message);
    }
}

fn run(options: &Options) -> Result<(), String> {
    let jars = cli::collect_jars(&options.inputs, &options.excludes)?;
    if jars.is_empty() {
        return Err(String::from("no .jar files found in provided inputs"));
    }

    let worker_pool = ThreadPoolBuilder::new()
        .stack_size(rayon_stack_size())
        .build()
        .map_err(|error| format!("failed to start worker pool: {}", error))?;

    let mut roots = read_jars(&worker_pool, &jars)?;

    exclude::retain(
        &mut roots,
        &options.exclude_packages,
        &options.exclude_identifiers,
    );

    if roots.is_empty() {
        return Err(String::from(
            "no parsable inputs found after applying exclusions",
        ));
    }

    let exclusions = exclude::collect(&roots, &options.exclude_identifiers);

    let scopes = worker_pool.install(|| Scope::from_roots(&roots, &exclusions));

    worker_pool.install(|| {
        write_pyi_by_package(
            &scopes,
            options.mixer_records.clone(),
            Arc::new(exclusions),
            |package, contents| write_package(&options.out_dir, package, contents),
        )
    })?;

    status::clear();
    Ok(())
}

fn read_jars(worker_pool: &rayon::ThreadPool, jars: &[PathBuf]) -> Result<Vec<Arc<Root>>, String> {
    status::update(&format!("Unpacking jars 0/{}", jars.len()));

    let unpacked = worker_pool
        .install(|| {
            jar::unpack(jars, |done, path| {
                status::update(&format!(
                    "Unpacking jars {}/{}: {}",
                    done,
                    jars.len(),
                    path.display()
                ))
            })
        })
        .map_err(|error| error.to_string())?;

    if !unpacked.skipped.is_empty() {
        status::clear();
        eprintln!(
            "skipped {} nested class file(s) without an enclosing class file",
            unpacked.skipped.len()
        );
    }

    let total_units = unpacked.units.len();
    status::update(&format!("Mapping classes 0/{}", total_units));

    let done = AtomicUsize::new(0);

    worker_pool.install(|| {
        unpacked
            .units
            .par_iter()
            .map(|unit| {
                status::update(&format!(
                    "Mapping classes {}/{}: {}",
                    done.fetch_add(1, Ordering::Relaxed) + 1,
                    total_units,
                    unit.binary_name
                ));

                unit.to_root()
                    .map(Arc::new)
                    .map_err(|error| error.to_string())
            })
            .collect::<Result<Vec<_>, _>>()
    })
}

fn write_package(out_dir: &Path, package: &str, contents: String) -> Result<(), String> {
    let file_path = package_to_path(out_dir, package);

    let write = || -> std::io::Result<()> {
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent)?;
        }

        fs::write(&file_path, &contents)?;
        ensure_parent_inits(&file_path, out_dir)
    };

    write().map_err(|error| format!("failed to write {}\n{}", file_path.display(), error))
}

fn package_to_path(out_dir: &Path, package: &str) -> PathBuf {
    let mut path = PathBuf::from(out_dir);

    for part in package.split('.').filter(|part| !part.is_empty()) {
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

fn rayon_stack_size() -> usize {
    let configured_mb = env::var("JAVA2PYI_RAYON_STACK_SIZE_MB")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_RAYON_STACK_SIZE_MB);

    configured_mb.saturating_mul(1024 * 1024)
}
