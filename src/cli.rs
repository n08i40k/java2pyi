use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

pub struct Options {
    pub inputs: Vec<PathBuf>,
    pub out_dir: PathBuf,
    pub excludes: Vec<PathBuf>,
    pub exclude_packages: Vec<String>,
    pub exclude_identifiers: HashSet<String>,
    pub mixer_records: HashMap<String, String>,
}

pub enum ArgsError {
    Invalid(String),
    HelpRequested,
}

pub fn parse(args: impl IntoIterator<Item = String>) -> Result<Options, ArgsError> {
    let mut inputs = Vec::new();
    let mut out_dir = PathBuf::from("out");
    let mut excludes = Vec::new();
    let mut exclude_packages = Vec::new();
    let mut exclude_identifiers = HashSet::new();
    let mut mixer_records = HashMap::new();

    let mut iter = args.into_iter();
    let _program = iter.next();

    while let Some(arg) = iter.next() {
        let mut value = |option: &str| {
            iter.next()
                .ok_or_else(|| ArgsError::Invalid(format!("missing value for {}", option)))
        };

        match arg.as_str() {
            "-i" | "--input" => inputs.push(PathBuf::from(value("--input")?)),
            "-o" | "--out" => out_dir = PathBuf::from(value("--out")?),
            "-x" | "--exclude" => excludes.push(PathBuf::from(value("--exclude")?)),
            "-xp" | "--exclude-packages" => {
                let value = normalize_qualified_name(&value("--exclude-packages")?);
                if !value.is_empty() {
                    exclude_packages.push(value);
                }
            }
            "-xi" | "--exclude-identifiers" => {
                let value = normalize_qualified_name(&value("--exclude-identifiers")?);
                if !value.is_empty() {
                    exclude_identifiers.insert(value);
                }
            }
            "-m" | "--mix" => {
                for record in value("--mix")?.split(',') {
                    let (java, python) = record
                        .split_once(':')
                        .ok_or_else(|| ArgsError::Invalid("invalid value in --mix".into()))?;

                    mixer_records.insert(java.to_string(), python.to_string());
                }
            }
            "-h" | "--help" => return Err(ArgsError::HelpRequested),
            _ if arg.starts_with('-') => {
                return Err(ArgsError::Invalid(format!("unknown option: {}", arg)));
            }
            _ => inputs.push(PathBuf::from(arg)),
        }
    }

    if inputs.is_empty() {
        return Err(ArgsError::Invalid(String::from("no inputs provided")));
    }

    Ok(Options {
        inputs,
        out_dir,
        excludes,
        exclude_packages,
        exclude_identifiers,
        mixer_records,
    })
}

pub fn usage() -> String {
    [
        "Usage:",
        "  java2pyi -i <path> [-i <path> ...] [-x <path> ...] [-xp <package> ...] [-xi <identifier> ...] [--out <dir>]",
        "",
        "Options:",
        "  -i, --input <path>      Input file or directory (recurses for .jar)",
        "  -x, --exclude <path>    Exclude file or directory (repeatable)",
        "  -xp, --exclude-packages <package>",
        "                         Exclude package and subpackages from indexing/serialization",
        "  -xi, --exclude-identifiers <identifier>",
        "                         Exclude class/interface/enum and nested types from indexing/serialization",
        "  -m, --mix [<Java FQTN>:<Python built-in type name>,]",
        "                         Allow to use Python built-in type as Java type in variables, method args and return types",
        "  -o, --out <dir>         Output directory (default: out)",
        "  -h, --help              Show this help",
    ]
    .join("\n")
}

pub fn collect_jars(inputs: &[PathBuf], excludes: &[PathBuf]) -> Result<Vec<PathBuf>, String> {
    let mut collected = Vec::new();

    for input in inputs {
        collect_into(input, excludes, &mut collected)?;
    }

    Ok(collected)
}

fn collect_into(
    path: &Path,
    excludes: &[PathBuf],
    collected: &mut Vec<PathBuf>,
) -> Result<(), String> {
    if excludes.iter().any(|exclude| path.starts_with(exclude)) {
        return Ok(());
    }

    let metadata =
        fs::metadata(path).map_err(|err| format!("failed to read {}: {}", path.display(), err))?;

    if metadata.is_file() {
        if path.extension().and_then(|ext| ext.to_str()) == Some("jar") {
            collected.push(path.to_path_buf());
        }

        return Ok(());
    }

    if metadata.is_dir() {
        for entry in fs::read_dir(path)
            .map_err(|err| format!("failed to read {}: {}", path.display(), err))?
        {
            let entry =
                entry.map_err(|err| format!("failed to read {}: {}", path.display(), err))?;

            collect_into(&entry.path(), excludes, collected)?;
        }
    }

    Ok(())
}

fn normalize_qualified_name(value: &str) -> String {
    value.trim().trim_matches('.').to_string()
}
