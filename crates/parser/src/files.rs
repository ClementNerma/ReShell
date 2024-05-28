use std::{
    collections::HashMap,
    fmt::Debug,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use parsy::{FileId, SourceFileID};

pub type FileLoader = Box<
    dyn Fn(&str, FileId, &FilesMap) -> Result<(SourceFileLocation, String), String> + Send + Sync,
>;

#[derive(Clone)]
pub struct FilesMap(Arc<RwLock<FilesMapInner>>);

struct FilesMapInner {
    map: HashMap<SourceFileID, SourceFile>,
    counter: u64,
    file_loader: FileLoader,
}

impl FilesMap {
    pub fn new(file_loader: FileLoader) -> Self {
        Self(Arc::new(RwLock::new(FilesMapInner {
            map: HashMap::new(),
            counter: 0,
            file_loader,
        })))
    }

    pub fn load_file(&self, path: &str, relative_to: FileId) -> Result<SourceFile, String> {
        let (location, content) = {
            let inner = self.0.read().unwrap();

            (inner.file_loader)(path, relative_to, self)?
        };

        let file_id = self.register_file(location, content);

        Ok(self.get_file(file_id).unwrap())
    }

    pub fn register_file(&self, location: SourceFileLocation, content: String) -> SourceFileID {
        let mut inner = self.0.write().unwrap();

        inner.counter += 1;

        let id = SourceFileID::from(inner.counter);

        inner.map.insert(
            id,
            SourceFile {
                id,
                location,
                content,
            },
        );

        id
    }

    pub fn get_file(&self, id: SourceFileID) -> Option<SourceFile> {
        self.0.read().unwrap().map.get(&id).cloned()
    }
}

impl Debug for FilesMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(inner) = self;

        let FilesMapInner {
            map,
            counter,
            file_loader: _,
        } = &*inner.read().unwrap();

        f.debug_struct("FilesMap")
            .field("map", &map)
            .field("counter", &counter)
            .field("file_loader", &"<function>")
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: SourceFileID,
    pub location: SourceFileLocation,
    pub content: String,
}

#[derive(Debug, Clone)]
pub enum SourceFileLocation {
    CustomName(String),
    RealFile(PathBuf),
}
