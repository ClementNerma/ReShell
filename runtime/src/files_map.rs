use std::{collections::HashMap, path::PathBuf};

use rand::RngCore;

#[derive(Debug)]
pub struct FilesMap {
    map: HashMap<u64, SourceFile>,
}

impl FilesMap {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn register_file(&mut self, path: ScopableFilePath, content: String) -> u64 {
        let id = rand::thread_rng().next_u64();

        self.map.insert(id, SourceFile { id, path, content });

        id
    }

    pub fn has_file(&self, id: u64) -> bool {
        self.map.contains_key(&id)
    }

    pub fn get_file(&self, id: u64) -> Option<&SourceFile> {
        self.map.get(&id)
    }

    pub fn get_file_path(&self, id: u64) -> Option<&PathBuf> {
        self.get_file(id).and_then(|file| match &file.path {
            ScopableFilePath::InMemory(_) => None,
            ScopableFilePath::RealFile(path) => Some(path),
        })
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: u64,
    pub path: ScopableFilePath,
    pub content: String,
}

#[derive(Debug, Clone)]
pub enum ScopableFilePath {
    InMemory(&'static str),
    RealFile(PathBuf),
}
