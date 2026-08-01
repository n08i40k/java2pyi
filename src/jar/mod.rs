#![allow(dead_code)]

use std::{
    collections::BTreeMap,
    fmt, fs,
    io::{self, BufWriter},
    ops::Range,
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use memmap2::Mmap;
use rayon::iter::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator};
use tempfile::TempDir;

use crate::model::Root;

pub mod mapper;

#[derive(Debug)]
pub struct ClassFile {
    pub binary_name: String,
    blob: Arc<Mmap>,
    range: Range<usize>,
}

impl ClassFile {
    pub fn simple_name(&self) -> &str {
        binary_simple_name(&self.binary_name)
    }

    pub fn bytes(&self) -> &[u8] {
        &self.blob[self.range.clone()]
    }

    pub fn parse(&self) -> Result<jvm_class::Class<'_>, Error> {
        jvm_class::Class::parse(self.bytes()).map_err(|source| Error::Class {
            entry: self.binary_name.clone(),
            source,
        })
    }
}

#[derive(Debug)]
pub struct ClassUnit {
    pub package: String,
    pub binary_name: String,
    pub main: ClassFile,
    pub nested: Vec<ClassFile>,
}

impl ClassUnit {
    pub fn simple_name(&self) -> &str {
        binary_simple_name(&self.binary_name)
    }

    pub fn parse(&self) -> Result<ParsedUnit<'_>, Error> {
        Ok(ParsedUnit {
            package: &self.package,
            binary_name: &self.binary_name,
            main: self.main.parse()?,
            nested: self
                .nested
                .iter()
                .map(|file| {
                    Ok(ParsedClass {
                        binary_name: &file.binary_name,
                        class: file.parse()?,
                    })
                })
                .collect::<Result<Vec<_>, Error>>()?,
        })
    }

    pub fn to_root(&self) -> Result<Root, Error> {
        let parsed = self.parse()?;

        Root::build(|arena| mapper::map_root(arena, &parsed)).map_err(Error::Map)
    }
}

#[derive(Debug)]
pub struct ParsedClass<'a> {
    pub binary_name: &'a str,
    pub class: jvm_class::Class<'a>,
}

impl<'a> ParsedClass<'a> {
    pub fn simple_name(&self) -> &'a str {
        binary_simple_name(self.binary_name)
    }
}

#[derive(Debug)]
pub struct ParsedUnit<'a> {
    pub package: &'a str,
    pub binary_name: &'a str,
    pub main: jvm_class::Class<'a>,
    pub nested: Vec<ParsedClass<'a>>,
}

impl<'a> ParsedUnit<'a> {
    pub fn simple_name(&self) -> &'a str {
        binary_simple_name(self.binary_name)
    }

    pub fn children_of(&self, binary_name: &str) -> impl Iterator<Item = &ParsedClass<'a>> {
        self.nested.iter().filter(move |nested| {
            nested
                .binary_name
                .strip_prefix(binary_name)
                .and_then(|suffix| suffix.strip_prefix('$'))
                .is_some_and(|suffix| !suffix.contains('$'))
        })
    }
}

#[derive(Debug)]
pub struct Unpacked {
    pub units: Vec<ClassUnit>,
    pub skipped: Vec<String>,
    dir: TempDir,
}

#[derive(Debug)]
pub enum Error {
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
    Zip {
        path: PathBuf,
        source: zip::result::ZipError,
    },
    Class {
        entry: String,
        source: jvm_class::Error,
    },
    Map(anyhow::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io { path, source } => {
                write!(f, "failed to read {}: {}", path.display(), source)
            }
            Error::Zip { path, source } => {
                write!(f, "failed to unpack {}: {}", path.display(), source)
            }
            Error::Class { entry, source } => {
                write!(f, "failed to read class {}: {}", entry, source)
            }
            Error::Map(error) => write!(f, "failed to map class: {:#}", error),
        }
    }
}

impl std::error::Error for Error {}

pub fn unpack(
    paths: &[PathBuf],
    progress: impl Fn(usize, &Path) + Sync,
) -> Result<Unpacked, Error> {
    let dir = TempDir::with_prefix("java2pyi-").map_err(|source| Error::Io {
        path: PathBuf::from("<scratch dir>"),
        source,
    })?;

    let done = AtomicUsize::new(0);

    let unpacked = paths
        .par_iter()
        .enumerate()
        .map(|(index, path)| {
            let result = unpack_jar(path, &dir.path().join(format!("{}.classes", index)));

            progress(done.fetch_add(1, Ordering::Relaxed) + 1, path);

            result
        })
        .collect::<Result<Vec<_>, Error>>()?;

    let mut units = Vec::new();
    let mut skipped = Vec::new();

    for (jar_units, jar_skipped) in unpacked {
        units.extend(jar_units);
        skipped.extend(jar_skipped);
    }

    Ok(Unpacked {
        units,
        skipped,
        dir,
    })
}

fn unpack_jar(path: &Path, blob_path: &Path) -> Result<(Vec<ClassUnit>, Vec<String>), Error> {
    let io_error = |source| Error::Io {
        path: path.to_path_buf(),
        source,
    };
    let blob_error = |source| Error::Io {
        path: blob_path.to_path_buf(),
        source,
    };

    let file = fs::File::open(path).map_err(io_error)?;

    let mut archive = zip::ZipArchive::new(file).map_err(|source| Error::Zip {
        path: path.to_path_buf(),
        source,
    })?;

    let blob = fs::File::options()
        .read(true)
        .write(true)
        .create_new(true)
        .open(blob_path)
        .map_err(blob_error)?;

    let mut writer = BufWriter::new(blob);
    let mut offset = 0usize;
    let mut entries: Vec<(String, Range<usize>)> = Vec::new();

    for index in 0..archive.len() {
        let mut entry = archive.by_index(index).map_err(|source| Error::Zip {
            path: path.to_path_buf(),
            source,
        })?;

        if !entry.is_file() {
            continue;
        }

        let Some(binary_name) = entry_binary_name(entry.name()) else {
            continue;
        };

        let written = io::copy(&mut entry, &mut writer).map_err(blob_error)? as usize;

        entries.push((binary_name, offset..offset + written));
        offset += written;
    }

    drop(archive);

    let blob = writer
        .into_inner()
        .map_err(|error| blob_error(error.into()))?;

    if entries.is_empty() {
        return Ok((Vec::new(), Vec::new()));
    }

    let blob = Arc::new(unsafe { Mmap::map(&blob) }.map_err(blob_error)?);

    let mut groups: BTreeMap<String, Vec<ClassFile>> = BTreeMap::new();

    for (binary_name, range) in entries {
        groups
            .entry(top_level_binary_name(&binary_name).to_string())
            .or_default()
            .push(ClassFile {
                binary_name,
                blob: Arc::clone(&blob),
                range,
            });
    }

    let mut units = Vec::with_capacity(groups.len());
    let mut skipped = Vec::new();

    for (binary_name, mut files) in groups {
        files.sort_by(|left, right| left.binary_name.cmp(&right.binary_name));

        let Some(main_index) = files
            .iter()
            .position(|file| file.binary_name == binary_name)
        else {
            skipped.extend(files.into_iter().map(|file| file.binary_name));
            continue;
        };

        let main = files.remove(main_index);

        units.push(ClassUnit {
            package: binary_package(&binary_name).replace('/', "."),
            binary_name,
            main,
            nested: files,
        });
    }

    Ok((units, skipped))
}

fn entry_binary_name(name: &str) -> Option<String> {
    let binary_name = name.strip_suffix(".class")?;

    if name.starts_with("META-INF/") {
        return None;
    }

    let simple_name = binary_simple_name(binary_name);
    if simple_name == "module-info" || simple_name == "package-info" {
        return None;
    }

    Some(binary_name.to_string())
}

fn binary_package(binary_name: &str) -> &str {
    match binary_name.rfind('/') {
        Some(index) => &binary_name[..index],
        None => "",
    }
}

fn binary_simple_name(binary_name: &str) -> &str {
    match binary_name.rfind('/') {
        Some(index) => &binary_name[index + 1..],
        None => binary_name,
    }
}

fn top_level_binary_name(binary_name: &str) -> &str {
    let package_len = binary_package(binary_name).len();

    match binary_simple_name(binary_name).find('$') {
        Some(index) if package_len == 0 => &binary_name[..index],
        Some(index) => &binary_name[..package_len + 1 + index],
        None => binary_name,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_are_split_around_package_and_nesting() {
        assert_eq!(binary_package("java/util/Map$Entry"), "java/util");
        assert_eq!(binary_package("Main"), "");

        assert_eq!(binary_simple_name("java/util/Map$Entry"), "Map$Entry");
        assert_eq!(binary_simple_name("Main"), "Main");

        assert_eq!(
            top_level_binary_name("java/util/Map$Entry"),
            "java/util/Map"
        );
        assert_eq!(top_level_binary_name("java/util/Map"), "java/util/Map");
        assert_eq!(top_level_binary_name("Main$1"), "Main");
    }

    #[test]
    fn only_class_entries_are_kept() {
        assert_eq!(
            entry_binary_name("java/util/Map.class").as_deref(),
            Some("java/util/Map")
        );
        assert_eq!(entry_binary_name("java/util/Map.java"), None);
        assert_eq!(entry_binary_name("META-INF/versions/9/Foo.class"), None);
        assert_eq!(entry_binary_name("module-info.class"), None);
        assert_eq!(entry_binary_name("java/util/package-info.class"), None);
    }

    #[test]
    fn root_outlives_the_arena_borrow() {
        let root = Root::build::<()>(|arena| {
            Ok(crate::ir::Root {
                package: arena.alloc_str("java.util"),
                classes: &[],
            })
        })
        .unwrap();

        assert_eq!(root.ir().package, "java.util");
    }
}
