#![allow(unused)]
#![allow(missing_docs)]
//! TODO

use datetime::Datetime;
use de::{self, Deserializer, Line, RawKey, RawValue, RawValueType};
use ser;
use std::{
    borrow::Cow,
    collections::{hash_map, HashMap, HashSet},
    fmt,
    io::Write,
    str::FromStr,
};

mod entry;
mod index;
mod iter;
pub use self::entry::DocEntry;
pub use self::index::DocIndex;

#[derive(Debug)]
pub struct TomlDocument {
    has_bom: bool,
    // line_ending: LineEndingStyle,
    root: DocTable,
    /// List of bracketed tables in the order they appear.
    table_order: Vec<TablePath>,
}

/// A chain of keys to a nested table.
#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Debug, Clone)]
struct TablePath(Vec<IndexKey>);

impl TablePath {
    fn new() -> TablePath {
        TablePath(Vec::new())
    }

    fn new_from_parts(parts: Vec<String>) -> TablePath {
        let parts = parts
            .into_iter()
            .map(|part| IndexKey::Table(part))
            .collect();
        TablePath(parts)
    }

    fn push(&mut self, key: IndexKey) {
        self.0.push(key)
    }

    fn join(&self, key: IndexKey) -> TablePath {
        let mut path = self.clone();
        path.0.push(key);
        path
    }

    fn starts_with(&self, part: &str) -> bool {
        match &self.0[0] {
            IndexKey::Table(s) => part == s,
            IndexKey::Array(s, _) => part == s,
        }
    }
}

#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Debug, Clone)]
enum IndexKey {
    Table(String),
    Array(String, usize),
}

#[derive(Clone, Debug)]
pub struct DocKey {
    /// True if a `[standard]` or `[[array]]` table.
    /// TODO: This needs some clarification, since keys can sometimes be
    /// demoted to regular key=value.  Maybe make sure that demotion always
    /// clears `text` and this? Consider removing this.
    is_bracketed: bool,
    /// This is used to determine if whitespace should be added during rendering.
    /// True if it came from parsing a document.
    /// False if added via the mutation API.
    is_original: bool,
    /// Whitespace/comments in front of the key.
    pretext: Option<String>,
    /// Whitespace in front of the key (same line).
    indent: Option<String>,
    /// Original text of the key.
    /// If None, the text will be generated from `parts`.
    text: Option<String>,
    /// Parsed parts of the key, split on dots.
    parts: Vec<String>,
    /// Whitespace after the key.
    posttext: Option<String>,
}

#[derive(Debug)]
pub struct DocValue {
    /// This is used to determine if whitespace should be added during rendering.
    /// True if it came from parsing a document.
    /// False if added via the mutation API.
    is_original: bool,
    /// Whitespace in front of the value.
    pretext: Option<String>,
    /// Original text of the value.
    text: Option<String>,
    /// Whitespace/comment following the value.
    posttext: Option<String>,
    /// Parsed value.
    parsed: DocValueType,
}

#[derive(Debug)]
pub enum DocValueType {
    Reserved,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Datetime(Datetime),
    Array { aot: bool, values: Vec<DocValue> },
    Table(DocTable),
}

/// TODO: docme
/// This serves multiple purposes.
/// - Bracketed tables `[foo.bar]`.
/// - Inline tables `inline = {key = "value"}`
/// - Intermediate tables. Tables created by dotted keys (both bracketed and inline).
#[derive(Debug)]
pub struct DocTable {
    /// Whitespace/comments in front of the header.
    header_pretext: Option<String>,
    /// The original text of the name (everything between the brackets,
    /// including whitespace). None for inline or intermediate tables.
    header_key: Option<DocKey>,
    /// All text following the closing bracket of the table name.
    header_posttext: Option<String>,
    // TODO: Change to enum?
    is_array: bool,
    is_inline: bool,
    is_intermediate: bool,
    is_modified: bool,
    /// Comments/whitespace at the end of the file.
    table_posttext: Option<String>,
    indentation: Option<String>,
    /// Sequence of key/values in the table in the order they appear.
    ///
    /// These entries may become stale, so care must be taken when inspecting them. (XXX)
    /// Standard tables are not listed here, they are only listed in `TomlDocument::table_order`.
    items: Vec<(DocKey, TablePath)>,
    /// Map of values in this table.
    map: HashMap<String, DocValue>,
}

impl TomlDocument {
    pub fn new() -> TomlDocument {
        TomlDocument {
            has_bom: false,
            root: DocTable::new_root(),
            table_order: Vec::new(),
        }
    }

    pub fn from_str(s: &str) -> Result<TomlDocument, de::Error> {
        let mut d = Deserializer::new(s);
        let mut tables = Vec::new();
        let mut cur_table = DocTable::new_root();
        // Comments/whitespace are accumulated in `pretext` until the next
        // value is received.
        let mut pretext = String::new();
        let mut has_bom = false;

        let opt_split_off = |s: &mut String| -> Option<String> {
            if s.is_empty() {
                None
            } else {
                Some(s.split_off(0))
            }
        };

        while let Some(line) = d.line()? {
            match line {
                Line::Header {
                    indent,
                    at,
                    array,
                    key,
                    header_posttext,
                } => {
                    tables.push(cur_table);
                    pretext.push_str(indent);
                    cur_table = DocTable::new();
                    cur_table.header_pretext = opt_split_off(&mut pretext);
                    cur_table.header_key = Some(DocKey::from_raw(None, key));
                    cur_table.header_posttext = opt_str(header_posttext);
                    cur_table.is_array = array;
                }
                Line::KeyValue { key, value } => {
                    let doc_key = DocKey::from_raw(opt_split_off(&mut pretext), key);
                    let doc_value = DocValue::from_raw(value)?;
                    cur_table.add_dotted_key(doc_key, doc_value)?;
                }
                Line::Whitespace(s) => {
                    if cur_table.is_root() && cur_table.items.is_empty() && is_blank(s) {
                        // Comment blocks at the top of the document belong to the root table.
                        let mut root_pretext =
                            cur_table.header_pretext.get_or_insert_with(String::new);
                        root_pretext.push_str(&pretext);
                        pretext.clear();
                        root_pretext.push_str(s);
                    } else {
                        pretext.push_str(s);
                    }
                }
                Line::Comment(s) => pretext.push_str(s),
                Line::Bom => has_bom = true,
            }
        }
        if !pretext.is_empty() {
            cur_table.table_posttext = Some(pretext);
        }
        tables.push(cur_table);

        let root = tables.remove(0);
        let mut doc = TomlDocument {
            has_bom: has_bom,
            root: root,
            table_order: Vec::new(),
        };

        for table in tables {
            let mut path = TablePath::new();
            doc.root.add_table(table, 0, &mut path)?;
            doc.table_order.push(path);
        }

        Ok(doc)
    }

    pub fn to_string(&self) -> String {
        let p_set: HashSet<&TablePath> = self.table_order.iter().collect();
        let mut new_ps = Vec::new();
        self.root
            .collect_new_tables(&p_set, &mut new_ps, TablePath::new());
        new_ps.sort_unstable();
        // TODO, insert these sorted next to longest match.
        // for new_path in new_ps {
        // find the longest match in table_order
        // collect all in table_order >= longest match
        // insert just before entry with sort order > new_path
        // }
        // [package]
        // [profile.dev]
        // [profile.foo]
        // [profile.bar.dev-overrides]

        // [dependencies]

        // [target.'cfg(unix)'.dependencies]
        // [target.'cfg(windows)'.dependencies]

        let new_table_order: Vec<&TablePath> =
            self.table_order.iter().chain(new_ps.iter()).collect();

        let mut res: Vec<u8> = Vec::new();
        if self.has_bom {
            res.push(b'\xef');
            res.push(b'\xbb');
            res.push(b'\xbf');
        }
        let mut writer = Box::new(res);
        self.root.render(&mut writer);
        for path in new_table_order {
            if let Some(table) = self.root.get_path(path, 0) {
                table.render(&mut writer);
            }
        }
        // The rendering code always uses utf-8.
        unsafe { String::from_utf8_unchecked(*writer) }
    }

    pub fn get(&self, key: &str) -> Option<&DocValue> {
        self.root.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut DocValue> {
        self.root.get_mut(key)
    }

    pub fn iter(&self) -> iter::IterTable {
        self.root.iter()
    }

    pub fn iter_mut(&mut self) -> iter::IterTableMut {
        self.root.iter_mut()
    }

    pub fn clear(&mut self) {
        self.root.clear();
        self.table_order.clear();
    }

    pub fn remove(&mut self, key: &str) -> Option<DocValue> {
        let res = self.root.remove(key);
        if res.is_some() {
            self.table_order.retain(|path| !path.starts_with(key));
        }
        res
    }

    // TODO: Consider making `key` a Into<DocKey>.
    // TODO: consider making `value` support Deserialize.
    pub fn insert(
        &mut self,
        key: DocKey,
        value: DocValue,
    ) -> Result<Option<(DocKey, DocValue)>, de::Error> {
        self.root.insert(key, value, true)
    }

    // insert_table("[foo]\nbar=1")
    // insert_table("key=1")  // fail
    // insert_table("[foo]\nbar=1[baz]=2") // fail, multiple tables
    // insert_table("[[bin]]name='foo'") // ok, append
    // pub fn insert_table(&mut self, table: DocTable) -> Result<Option<DocValue>, de::Error> {
    //     if table.header_key.is_none() {
    //         if table.len() == 1 {
    //             let (key, value) = table.into_iter().next().unwrap();
    //             if !value.is_bracketed_table() {
    //                 panic!("Table must have a single header."); // TODO Error
    //             }
    //             match value.parsed {
    //                 DocValueType::Table(t) => return self.insert_table(t),
    //                 t @ _ => panic!("Table must have a single header, got {}", t.type_str()),
    //             }
    //         }
    //         panic!("Table must have a single header."); // TODO error
    //     }
    //     // TODO: what to do if !is_bracketed_table
    //     if table.is_inline {
    //         panic!("table is inline"); // TODO error
    //     }
    //     if table.is_intermediate {
    //         panic!("table is intermediate"); // TODO error
    //     }
    //     // TODO: how to handle array of tables with multiple entries
    //     //       Also, may need some verification that the `header` bits of the table
    //     let mut p = TablePath::new();
    //     return Ok(None);
    //     // unimplemented!();
    //     // let old = self.root.insert_table(table, 0, &mut path)?;
    //     // Find a good place to put this.
    //     // **** TODO
    //     // Ok(old)
    // }

    pub fn entry<K>(&mut self, key: K) -> DocEntry
    where
        K: Into<DocKey>,
    {
        self.root.entry(key)
    }
}

impl fmt::Display for TomlDocument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_string())
    }
}

// Hack to avoid duplicating immutable/mutable versions.
// There's probably a better way.
macro_rules! get_path {
    ($self:ident, $path:ident, $path_idx:ident, $get_path:ident, $get:ident, $($m:ident)*) => {
        // TODO: Replace these .0 with with methods.
        match &$path.0[$path_idx] {
            IndexKey::Table(name) => {
                if let Some(dv) = $self.map.$get(name) {
                    if $path.0.len() - 1 == $path_idx {
                        Some(dv)
                    } else {
                        if let DocValueType::Table(table) = & $($m)* dv.parsed {
                            table.$get_path($path, $path_idx + 1)
                        } else {
                            panic!("get_path expected table");
                        }
                    }
                } else {
                    None
                }
            }
            IndexKey::Array(name, arr_index) => {
                if let Some(dv) = $self.map.$get(name) {
                    if let DocValueType::Array{values, ..} = & $($m)* dv.parsed {
                        let indexed = & $($m)* values[*arr_index];
                        if $path.0.len() - 1 == $path_idx {
                            Some(indexed)
                        } else {
                            if let DocValueType::Table(table) = & $($m)* indexed.parsed {
                                table.$get_path($path, $path_idx + 1)
                            } else {
                                panic!("get_path expected array table");
                            }
                        }
                    } else {
                        // TODO: Handle this if the array was removed and replaced.
                        panic!("get_path expected array");
                    }
                } else {
                    None
                }
            }
        }
    }
}

impl DocTable {
    pub fn new() -> DocTable {
        DocTable {
            header_pretext: None,
            header_key: None,
            header_posttext: None,
            is_array: false,
            is_inline: false,
            is_intermediate: false,
            is_modified: false,
            table_posttext: None,
            indentation: None,
            items: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn new_root() -> DocTable {
        let mut dt = DocTable::new();
        // The root table's key is an empty list.
        dt.header_key = Some(DocKey::new(Vec::new(), None));
        dt
    }

    pub fn from_str(s: &str) -> Result<DocTable, de::Error> {
        let doc = TomlDocument::from_str(s)?;
        Ok(doc.root)
    }

    pub fn is_root(&self) -> bool {
        if let Some(key) = &self.header_key {
            key.parts.is_empty()
        } else {
            false
        }
    }

    /// Add a key/value to this table.
    fn add_dotted_key(&mut self, key: DocKey, value: DocValue) -> Result<(), de::Error> {
        let mut path = TablePath::new();
        self.add_dotted_key_r(&key, value, 0, &mut path)?;
        self.items.push((key, path));
        Ok(())
    }

    // TODO: merge with add_table?
    fn add_dotted_key_r(
        &mut self,
        key: &DocKey,
        value: DocValue,
        cur_part: usize,
        path: &mut TablePath,
    ) -> Result<(), de::Error> {
        let is_im = cur_part < key.parts.len() - 1;
        if !is_im {
            // The value is stored in the leaf.
            let part = key.parts[cur_part].clone();
            let map_key = part.clone();
            if self.map.insert(map_key, value).is_some() {
                return Err(de::Error::from_kind(de::ErrorKind::DuplicateKey(
                    part.to_string(),
                )));
            }
            path.push(IndexKey::Table(part));
            return Ok(());
        }
        // Insert or add an intermediate table.
        let part = key.parts[cur_part].clone();
        match self.map.entry(part) {
            hash_map::Entry::Vacant(entry) => {
                let mut im_table = DocTable::new();
                im_table.is_inline = true;
                im_table.is_intermediate = true;
                let part = entry.key().clone();
                let doc_value = DocValue::new(DocValueType::Table(im_table));
                let mut dv = entry.insert(doc_value);
                path.push(IndexKey::Table(part));
                if let DocValueType::Table(ref mut t) = dv.parsed {
                    t.add_dotted_key_r(key, value, cur_part + 1, path)?;
                }
            }
            hash_map::Entry::Occupied(mut entry) => {
                let part = entry.key().clone();
                let dv = entry.get_mut();
                match &mut dv.parsed {
                    DocValueType::Table(t) => {
                        // TODO: check t.is_intermediate?
                        if !t.is_intermediate || !t.is_inline || t.is_array {
                            return Err(de::Error::from_kind(de::ErrorKind::DottedKeyInvalidType));
                        }
                        path.push(IndexKey::Table(part));
                        t.add_dotted_key_r(key, value, cur_part + 1, path)?;
                    }
                    _ => {
                        return Err(de::Error::from_kind(de::ErrorKind::DuplicateKey(
                            part.to_string(),
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    /// Add a bracketed table.
    ///
    /// - `cur_part`: Index into `table.header_key.parts`.
    /// - `path`: The path index to the table is accumulated in this value.
    fn add_table(
        &mut self,
        mut table: DocTable,
        cur_part: usize,
        path: &mut TablePath,
    ) -> Result<(), de::Error> {
        let header_parts = table.header_key.as_ref().unwrap().parts.clone(); // TODO: clone?
        let part = header_parts[cur_part].clone();
        let is_im = cur_part < header_parts.len() - 1;
        match self.map.entry(part) {
            hash_map::Entry::Vacant(entry) => {
                let part = entry.key().clone();
                if is_im {
                    let mut im_table = DocTable::new();
                    im_table.is_intermediate = true;
                    let doc_value = DocValue::new(DocValueType::Table(im_table));
                    let mut dv = entry.insert(doc_value);
                    path.push(IndexKey::Table(part));
                    if let DocValueType::Table(ref mut t) = dv.parsed {
                        t.add_table(table, cur_part + 1, path)?;
                    }
                } else {
                    let doc_key = DocKey::new(header_parts.clone(), None);
                    let doc_value = if table.is_array {
                        path.push(IndexKey::Array(part, 0));
                        let values = vec![DocValue::new(DocValueType::Table(table))];
                        DocValue::new(DocValueType::Array {
                            aot: true,
                            values: values,
                        })
                    } else {
                        path.push(IndexKey::Table(part));
                        DocValue::new(DocValueType::Table(table))
                    };
                    entry.insert(doc_value);
                }
            }
            hash_map::Entry::Occupied(mut entry) => {
                let part = entry.key().clone();
                let dv = entry.get_mut();
                match &mut dv.parsed {
                    DocValueType::Table(t) => {
                        if t.is_inline {
                            return Err(de::Error::from_kind(de::ErrorKind::DuplicateKey(
                                part.to_string(),
                            )));
                        }
                        // todo: reject !t.is_im?
                        path.push(IndexKey::Table(part.clone()));
                        // TODO: Check is_array?
                        // TODO: Must be regular table, not inline or dotted.
                        if is_im {
                            t.add_table(table, cur_part + 1, path)?;
                        } else {
                            if table.is_array {
                                // TODO: include location/key
                                return Err(de::Error::from_kind(de::ErrorKind::RedefineAsArray));
                            }
                            // Merge tables.
                            if !t.is_intermediate {
                                return Err(de::Error::from_kind(de::ErrorKind::DuplicateTable(
                                    part.to_string(),
                                )));
                            }
                            t.header_pretext = table.header_pretext;
                            t.header_key = table.header_key;
                            t.header_posttext = table.header_posttext;
                            // TODO: check is_array
                            t.is_intermediate = false;
                            t.table_posttext = table.table_posttext;
                            t.indentation = table.indentation;
                            assert!(t.items.is_empty());
                            t.items = table.items;
                            for (key, value) in table.map.drain() {
                                match t.map.entry(key) {
                                    hash_map::Entry::Occupied(entry) => {
                                        return Err(de::Error::from_kind(
                                            de::ErrorKind::DuplicateKey(entry.key().to_string()),
                                        ));
                                    }
                                    hash_map::Entry::Vacant(entry) => {
                                        entry.insert(value);
                                    }
                                }
                            }
                        }
                    }
                    DocValueType::Array { aot: true, values } => {
                        let arr_len = values.len();
                        if is_im {
                            let mut last = match values.last_mut() {
                                Some(l) => l,
                                None => {
                                    // TODO: This error could be better. It's more of a type clash.
                                    return Err(de::Error::from_kind(de::ErrorKind::DuplicateKey(
                                        part.to_string(),
                                    )));
                                }
                            };
                            // TODO: must be array from table array
                            match &mut last.parsed {
                                DocValueType::Table(t) => {
                                    if !t.is_array || t.is_inline {
                                        // TODO: This error could be better. It's more of a type clash.
                                        return Err(de::Error::from_kind(
                                            de::ErrorKind::DuplicateKey(part.to_string()),
                                        ));
                                    }
                                    path.push(IndexKey::Array(part, arr_len - 1));
                                    t.add_table(table, cur_part + 1, path)?;
                                }
                                _ => {
                                    // TODO: This error could be better. It's more of a type clash.
                                    return Err(de::Error::from_kind(de::ErrorKind::DuplicateKey(
                                        part.to_string(),
                                    )));
                                }
                            }
                        } else {
                            if !table.is_array {
                                // TODO: This error could be better. It's more of a type clash.
                                return Err(de::Error::from_kind(de::ErrorKind::DuplicateTable(
                                    part.to_string(),
                                )));
                            }
                            path.push(IndexKey::Array(part, arr_len));
                            let doc_value = DocValue::new(DocValueType::Table(table));
                            values.push(doc_value);
                        }
                    }
                    _ => {
                        return Err(de::Error::from_kind(de::ErrorKind::DuplicateKey(
                            part.to_string(),
                        )));
                    }
                }
            }
        }
        Ok(())
    }

    fn insert(
        &mut self,
        key: DocKey,
        value: DocValue,
        replace: bool,
    ) -> Result<Option<(DocKey, DocValue)>, de::Error> {
        let mut path = TablePath::new();
        // TODO: Replace with entry api ?
        let res = self.insert_r(&key, value, 0, &mut path, true, None)?;
        self.items.push((key, path));
        self.is_modified = true;
        Ok(res)
    }

    fn insert_r(
        &mut self,
        key: &DocKey,
        value: DocValue,
        cur_part: usize,
        path: &mut TablePath,
        replace: bool,
        old: Option<(DocKey, DocValue)>,
    ) -> Result<Option<(DocKey, DocValue)>, de::Error> {
        let is_im = cur_part < key.parts.len() - 1;
        let part = key.parts[cur_part].clone();
        match self.map.entry(part) {
            hash_map::Entry::Vacant(entry) => {
                let part = entry.key().clone();
                if is_im {
                    let mut im_table = DocTable::new();
                    im_table.is_intermediate = true;
                    im_table.is_inline = !key.is_bracketed;
                    im_table.is_modified = true;
                    let im_value = DocValue::new(DocValueType::Table(im_table));
                    let mut dv = entry.insert(im_value);
                    path.push(IndexKey::Table(part));
                    // Get the intermediate table that was just inserted.
                    if let DocValueType::Table(ref mut t) = dv.parsed {
                        return t.insert_r(key, value, cur_part + 1, path, replace, old);
                    }
                    unreachable!();
                } else {
                    let doc_value = if value.is_table_array_member() {
                        path.push(IndexKey::Array(part, 0));
                        let values = vec![value];
                        DocValue::new(DocValueType::Array {
                            aot: true,
                            values: values,
                        })
                    } else {
                        path.push(IndexKey::Table(part));
                        value
                    };
                    entry.insert(doc_value);
                    return Ok(old);
                }
            }
            hash_map::Entry::Occupied(mut entry) => {
                unimplemented!();
                let part = entry.key().clone();
                // Let replace fall through below to try again with vacant entry.
                if !replace {
                    if is_im {
                        let dv = entry.get_mut();
                        match &mut dv.parsed {
                            DocValueType::Table(t) => {
                                if t.is_intermediate {
                                    // when one of the following already exists:
                                    //           a.b.c.d = ...
                                    //           [a.b.c.d]
                                    //           [[a.b.c.d]]

                                } else {
                                    // when one of the following already exists:
                                    //           a.b.c = {}
                                    //           [a.b.c]
                                    return Err(de::Error::from_kind(
                                        de::ErrorKind::DuplicateTable(part.to_string()),
                                    ));
                                }
                            }
                            _ => unimplemented!(),
                        }
                    } else {
                        // Determine if this leaf can merge with the existing value.
                        // Inserting a.b.c = ...
                        //           [a.b.c]
                        //           [[a.b.c]]
                        // let new_table = match value.parsed {

                        // }
                        let dv = entry.get_mut();
                        match &mut dv.parsed {
                            DocValueType::Table(t) => {
                                // *** !is_im LEAF ENTRY ***
                                if t.is_intermediate {
                                    // when one of the following already exists:
                                    //           a.b.c.d = ...
                                    //           [a.b.c.d]
                                    //           [[a.b.c.d]]
                                    // Promote from intermediate.
                                } else {
                                    // when one of the following already exists:
                                    //           a.b.c = {}
                                    //           [a.b.c]
                                    return Err(de::Error::from_kind(
                                        de::ErrorKind::DuplicateTable(part.to_string()),
                                    ));
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
            }
        }
        // Fallthrough from occupied entry that can't be merged.
        if replace {
            assert!(old.is_none());
            let old_key = DocKey::new(key.parts[..=cur_part].to_vec(), None);
            let old_value = self.map.remove(&key.parts[cur_part]).unwrap();
            let new_old = Some((old_key, old_value));
            // Try again with a Vacant entry.
            return self.insert_r(key, value, cur_part, path, replace, new_old);
        } else {
            // TODO: Show full key path, with index that failed.
            return Err(de::Error::from_kind(de::ErrorKind::DuplicateKey(
                key.parts[cur_part].to_string(),
            )));
        }
    }

    fn render(&self, mut output: &mut dyn Write) {
        macro_rules! write_eq {
            ($doc_key:ident, $dv:ident) => {
                match ($doc_key.is_original, $dv.is_original) {
                    (true, true) => output.write_all(b"="),
                    (true, false) => output.write_all(b"= "),
                    (false, true) => output.write_all(b" ="),
                    (false, false) => output.write_all(b" = "),
                }
            }
        }
        // TODO: think hard about this is_intermediate
        // Could be useful if someone yanks out an intermediate table.
        if self.is_inline || self.is_intermediate {
            output.write_all(b"{");
            let mut first = true;
            for (doc_key, path) in &self.items {
                if let Some(dv) = self.get_path(path, 0) {
                    if first {
                        first = false;
                        if self.is_modified {
                            output.write_all(b" ");
                        }
                        // TODO: Don't include doc_key pretext
                    } else {
                        output.write_all(b", ");
                    }
                    // TODO: whitespace/comments here need to be fixed.
                    doc_key.render(output);
                    write_eq!(doc_key, dv);
                    // TODO: if last, don't include posttext if is_modified
                    dv.render(output);
                }
            }
            // TODO: Newline?
            if self.items.is_empty() || !self.is_modified {
                output.write_all(b"}");
            } else {
                output.write_all(b" }");
            }
        } else {
            if let Some(pretext) = &self.header_pretext {
                output.write_all(pretext.as_bytes());
            }
            if let Some(key) = &self.header_key {
                // TODO: Do this more cleanly.
                // Maybe root table could be part of an enum table type?
                if !key.parts.is_empty() {
                    if self.is_array {
                        output.write_all(b"[[");
                    } else {
                        output.write_all(b"[");
                    }
                    key.render(output);
                    if self.is_array {
                        output.write_all(b"]]");
                    } else {
                        output.write_all(b"]");
                    }
                }
            }
            if let Some(posttext) = &self.header_posttext {
                output.write_all(posttext.as_bytes());
            }
            for (doc_key, path) in &self.items {
                if let Some(dv) = self.get_path(path, 0) {
                    doc_key.render(output);
                    write_eq!(doc_key, dv);
                    dv.render(output);
                    if self.is_modified && !dv
                        .posttext
                        .as_ref()
                        .map(|s| s.ends_with("\n"))
                        .unwrap_or(false)
                    {
                        output.write_all(b"\n");
                    }
                }
            }
            if let Some(text) = &self.table_posttext {
                output.write_all(text.as_bytes());
            }
        }
    }

    fn get_path(&self, path: &TablePath, path_idx: usize) -> Option<&DocValue> {
        get_path!(self, path, path_idx, get_path, get,)
    }

    fn get_path_mut(&mut self, path: &TablePath, path_idx: usize) -> Option<&mut DocValue> {
        get_path!(self, path, path_idx, get_path_mut, get_mut, mut)
    }

    pub fn get(&self, key: &str) -> Option<&DocValue> {
        self.map.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut DocValue> {
        self.map.get_mut(key)
    }

    // TODO: remove_entry

    pub fn remove(&mut self, key: &str) -> Option<DocValue> {
        if let Some(mut dv) = self.map.remove(key) {
            self.is_modified = true;
            // When detaching intermediate tables, convert them to something
            // useful that could potentially be re-used.
            match &mut dv.parsed {
                DocValueType::Table(t) => {
                    if t.is_intermediate {
                        t.is_intermediate = false;
                        t.is_modified = true;
                        // TODO: inline intermediate tables?
                        assert!(t.items.is_empty());
                        t.items = t
                            .map
                            .keys()
                            .map(|k| {
                                // TODO: Consider moving this to a method.
                                let mut path = TablePath::new();
                                path.push(IndexKey::Table(k.to_string()));
                                (DocKey::new(vec![k.to_string()], None), path)
                            })
                            .collect();
                    }
                }
                DocValueType::Array { values, .. } => {
                    // TODO
                }
                _ => {}
            }
            Some(dv)
        } else {
            None
        }
    }

    pub fn iter(&self) -> iter::IterTable {
        iter::IterTable {
            items: self.map.iter(),
        }
    }

    pub fn iter_mut(&mut self) -> iter::IterTableMut {
        iter::IterTableMut {
            items: self.map.iter_mut(),
        }
    }

    pub fn into_iter(self) -> iter::IntoIterTable {
        iter::IntoIterTable {
            items: self.map.into_iter(),
        }
    }

    pub fn clear(&mut self) {
        self.items.clear();
        self.map.clear();
        self.is_modified = true;
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn entry<K>(&mut self, key: K) -> DocEntry
    where
        K: Into<DocKey>,
    {
        let key = key.into();
        let path = TablePath::new_from_parts(key.parts.clone());
        self.entry_r(key, 0, path)
    }

    fn entry_r(&mut self, key: DocKey, cur_part: usize, path: TablePath) -> DocEntry {
        let is_im = cur_part < key.parts.len() - 1;
        let part = key.parts[cur_part].clone();
        match self.map.entry(part) {
            hash_map::Entry::Vacant(entry) => DocEntry::Vacant(entry::VacantDocEntry {
                key: key,
                cur_part: cur_part,
                path: path,
                entry: entry,
                items: &mut self.items,
                is_modified: &mut self.is_modified,
            }),
            hash_map::Entry::Occupied(entry) => unimplemented!(),
        }
    }

    /// Called by VacantDocEntry::insert to actually insert a value.
    fn entry_insert<'a>(&mut self, key: &DocKey, value: DocValue, cur_part: usize) -> &mut DocValue {
        let is_im = cur_part < key.parts.len() - 1;
        let part = key.parts[cur_part].clone();
        self.is_modified = true;
        match self.map.entry(part) {
            hash_map::Entry::Vacant(entry) => {
                if is_im {
                    let mut im_table = DocTable::new();
                    im_table.is_intermediate = true;
                    im_table.is_inline = !key.is_bracketed;
                    let im_value = DocValue::new(DocValueType::Table(im_table));
                    let mut dv = entry.insert(im_value);
                    // path.push(IndexKey::Table(part));
                    // Get the intermediate table that was just inserted.
                    if let DocValueType::Table(ref mut t) = dv.parsed {
                        return t.entry_insert(key, value, cur_part + 1);
                    }
                    unreachable!();
                } else {
                    let doc_value = if value.is_table_array_member() {
                        // path.push(IndexKey::Array(part, 0));
                        let values = vec![value];
                        DocValue::new(DocValueType::Array {
                            aot: true,
                            values: values,
                        })
                    } else {
                        // path.push(IndexKey::Table(part));
                        value
                    };
                    return entry.insert(doc_value);
                }
            }
            hash_map::Entry::Occupied(entry) => unimplemented!(),
        }
    }

    fn collect_new_tables(
        &self,
        p_set: &HashSet<&TablePath>,
        new_ps: &mut Vec<TablePath>,
        path: TablePath,
    ) {
        for (key, value) in self.map.iter() {
            match &value.parsed {
                DocValueType::Table(table) => {
                    let new_path = path.join(IndexKey::Table(key.clone()));
                    if table.header_key.is_some() {
                        if !p_set.contains(&new_path) {
                            new_ps.push(new_path.clone())
                        }
                    }
                    table.collect_new_tables(p_set, new_ps, new_path);
                }
                DocValueType::Array { aot: true, values } => {
                    for (i, value) in values.iter().enumerate() {
                        let new_path = path.join(IndexKey::Array(key.clone(), i));
                        if !p_set.contains(&new_path) {
                            new_ps.push(new_path.clone())
                        }
                        if let DocValueType::Table(table) = &value.parsed {
                            table.collect_new_tables(p_set, new_ps, new_path);
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

impl DocKey {
    fn new(parts: Vec<String>, text: Option<String>) -> DocKey {
        DocKey {
            is_bracketed: false,
            is_original: false,
            pretext: None,
            indent: None,
            text: text,
            parts: parts,
            posttext: None,
        }
    }

    fn from_raw(pretext: Option<String>, raw: RawKey) -> DocKey {
        DocKey {
            is_bracketed: raw.text.starts_with("["),
            is_original: true,
            pretext: pretext,
            indent: opt_str(raw.pretext),
            text: Some(raw.text.to_string()),
            parts: raw.parts.into_iter().map(|p| p.into_owned()).collect(),
            posttext: opt_str(raw.posttext),
        }
    }

    pub fn from_str(s: &str) -> Result<DocKey, de::Error> {
        let mut d = Deserializer::new(s);
        let raw = d.dotted_key(None)?;
        let mut key = DocKey::from_raw(None, raw);
        // When parsed individually, it is typically inserted into a document,
        // in which case the rendering code should match the style of where
        // it is inserted.
        key.pretext = None;
        key.indent = None;
        key.posttext = None;
        key.is_original = false;
        Ok(key)
    }

    fn render(&self, mut output: &mut dyn Write) {
        if let Some(pretext) = &self.pretext {
            output.write_all(pretext.as_bytes());
        }
        if let Some(indent) = &self.indent {
            output.write_all(indent.as_bytes());
        }
        if let Some(text) = &self.text {
            output.write_all(text.as_bytes());
        } else {
            // TODO: Escaping
            output.write_all(&self.parts.join(".").as_bytes());
        }
        if let Some(posttext) = &self.posttext {
            output.write_all(posttext.as_bytes());
        }
    }
}

impl fmt::Display for DocKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(text) = &self.text {
            f.write_str(text)
        } else {
            // TODO: escaping
            f.write_str(&self.parts.join("."))
        }
    }
}

impl FromStr for DocKey {
    type Err = de::Error;
    fn from_str(s: &str) -> Result<DocKey, Self::Err> {
        DocKey::from_str(s)
    }
}

impl From<&str> for DocKey {
    fn from(s: &str) -> Self {
        DocKey::new(vec![s.to_string()], None)
    }
}

impl From<String> for DocKey {
    fn from(s: String) -> Self {
        DocKey::new(vec![s], None)
    }
}

impl DocValue {
    fn as_table_mut(&mut self) -> Option<&mut DocTable> {
        match self.parsed {
            DocValueType::Table(ref mut t) => Some(t),
            _ => None,
        }
    }

    // fn is_table_array(&self) -> bool {
    //     match self.value {
    //         TomlValueType::Array(ref a) => {
    //             match a.last() {
    //                 Some(&TomlValue{value: TomlValueType::Table(ref t), ..}) if t.is_array => true,
    //                 _ => false
    //             }
    //         }
    //         _ => false,
    //     }
    // }

    fn new(value: DocValueType) -> DocValue {
        DocValue {
            is_original: false,
            pretext: None,
            text: None,
            posttext: None,
            parsed: value,
        }
    }

    pub fn new_inline_table() -> DocValue {
        let mut t = DocTable::new();
        t.is_inline = true;
        DocValue::new(DocValueType::Table(t))
    }

    pub fn new_standard_table(mut header: DocKey) -> DocValue {
        let mut t = DocTable::new();
        t.header_posttext = Some("\n".to_string());
        t.header_key = Some(header);
        DocValue::new(DocValueType::Table(t))
    }

    fn from_raw(raw: RawValue) -> Result<DocValue, de::Error> {
        let parsed = DocValueType::from_raw(raw.parsed)?;
        Ok(DocValue {
            is_original: true,
            pretext: opt_str(raw.pretext),
            text: Some(raw.text.to_string()),
            posttext: opt_str(raw.posttext),
            parsed: parsed,
        })
    }

    pub fn from_str(s: &str) -> Result<DocValue, de::Error> {
        let mut d = Deserializer::new(s);
        let value = d.value()?;
        let mut dv = DocValue::from_raw(value)?;
        // When parsed individually, it is typically inserted into a document,
        // in which case the rendering code should match the style of where
        // it is inserted.
        dv.pretext = None;
        dv.is_original = false;
        // TODO: If posttext contains newlines, and it is used in an
        // inline table, that would cause invalid TOML.
        Ok(dv)
    }

    pub fn to_string(&self) -> String {
        let mut res: Box<Vec<u8>> = Box::new(Vec::new());
        self.render(&mut res);
        unsafe { String::from_utf8_unchecked(*res) }
    }

    fn render(&self, mut output: &mut dyn Write) {
        if self.is_reserved() {
            return;
        }
        // if self.parsed.is_intermediate() {
        //     self.parsed.render(output)
        // }
        if let Some(pretext) = &self.pretext {
            output.write_all(pretext.as_bytes());
        }
        match (self.is_modified(), &self.text) {
            (true, _) | (false, None) => self.parsed.render(output),
            (false, Some(text)) => {output.write_all(text.as_bytes());}
        }
        if let Some(posttext) = &self.posttext {
            output.write_all(posttext.as_bytes());
        }
    }

    pub fn is_integer(&self) -> bool {
        self.as_integer().is_some()
    }

    pub fn is_float(&self) -> bool {
        self.as_float().is_some()
    }

    pub fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }

    pub fn is_string(&self) -> bool {
        self.as_str().is_some()
    }

    pub fn is_datetime(&self) -> bool {
        self.as_datetime().is_some()
    }

    pub fn is_array(&self) -> bool {
        match &self.parsed {
            DocValueType::Array { .. } => true,
            _ => false,
        }
    }

    pub fn is_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => true,
            _ => false,
        }
    }

    // TODO: Decide if these should be pub
    fn is_bracketed_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => !t.is_inline && !t.is_intermediate,
            _ => false,
        }
    }

    fn is_intermediate_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => t.is_intermediate,
            _ => false,
        }
    }

    fn is_table_array_member(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => t.is_array,
            _ => false,
        }
    }

    fn is_array_of_tables(&self) -> bool {
        match &self.parsed {
            DocValueType::Array { aot: true, .. } => true,
            _ => false,
        }
    }

    fn is_inline_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => t.is_inline,
            _ => false,
        }
    }

    fn is_reserved(&self) -> bool {
        match &self.parsed {
            DocValueType::Reserved => true,
            _ => false,
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self.parsed {
            DocValueType::Integer(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self.parsed {
            DocValueType::Float(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self.parsed {
            DocValueType::Boolean(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match &self.parsed {
            DocValueType::String(s) => Some(&s),
            _ => None,
        }
    }

    pub fn as_datetime(&self) -> Option<&Datetime> {
        match &self.parsed {
            DocValueType::Datetime(dt) => Some(&dt),
            _ => None,
        }
    }

    pub fn get<I: DocIndex>(&self, index: I) -> Option<&DocValue> {
        index.index_get(self)
    }

    pub fn get_mut<I: DocIndex>(&mut self, index: I) -> Option<&mut DocValue> {
        index.index_get_mut(self)
    }

    pub fn remove<I: DocIndex>(&mut self, index: I) -> Option<DocValue> {
        index.index_remove(self)
    }

    pub fn clear(&mut self) {
        match &mut self.parsed {
            DocValueType::Array { values, .. } => values.clear(),
            DocValueType::Table(t) => t.clear(),
            t @ _ => panic!("clear on {} type", t.type_str()),
        }
    }

    pub fn is_empty(&self) -> bool {
        match &self.parsed {
            DocValueType::Array { values, .. } => values.is_empty(),
            DocValueType::Table(t) => t.map.is_empty(),
            t @ _ => panic!("is_empty on {} type", t.type_str()),
        }
    }

    pub fn len(&self) -> usize {
        match &self.parsed {
            DocValueType::Array { values, .. } => values.len(),
            DocValueType::Table(t) => t.len(),
            t @ _ => panic!("len on {} type", t.type_str()),
        }
    }

    pub fn is_modified(&self) -> bool {
        match &self.parsed {
            DocValueType::Array {..} => false, //TODO
            DocValueType::Table(t) => t.is_modified,
            _ => false,
        }
    }

    pub fn iter_table(&self) -> iter::IterTable {
        match &self.parsed {
            DocValueType::Table(t) => t.iter(),
            t @ _ => panic!("iter_table on {} type", t.type_str()),
        }
    }

    pub fn iter_table_mut(&mut self) -> iter::IterTableMut {
        match &mut self.parsed {
            DocValueType::Table(t) => t.iter_mut(),
            t @ _ => panic!("iter_table_mut on {} type", t.type_str()),
        }
    }

    pub fn into_iter_table(self) -> iter::IntoIterTable {
        match self.parsed {
            DocValueType::Table(t) => t.into_iter(),
            t @ _ => panic!("into_iter_table on {} type", t.type_str()),
        }
    }

    pub fn iter_array(&self) -> iter::IterArray {
        match &self.parsed {
            DocValueType::Array { values, .. } => iter::IterArray {
                items: values.iter(),
            },
            t @ _ => panic!("iter_array on {} type", t.type_str()),
        }
    }

    pub fn iter_array_mut(&mut self) -> iter::IterArrayMut {
        match &mut self.parsed {
            DocValueType::Array { values, .. } => iter::IterArrayMut {
                items: values.iter_mut(),
            },
            t @ _ => panic!("iter_array_mut on {} type", t.type_str()),
        }
    }

    pub fn into_iter_array(self) -> iter::IntoIterArray {
        match self.parsed {
            DocValueType::Array { values, .. } => iter::IntoIterArray {
                items: values.into_iter(),
            },
            t @ _ => panic!("into_iter_array on {} type", t.type_str()),
        }
    }

    pub fn push(&mut self, mut value: DocValue) {
        if self.is_array_of_tables() {
            match &mut value.parsed {
                DocValueType::Table(t) => {
                    t.is_array = true;
                    t.is_inline = false;
                    // TODO: Need to decide how to handle Array of Tables.
                    // t.header_key = self.last().
                    unimplemented!();
                }
                t @ _ => panic!(
                    "cannot push non-table ({}) to array of tables",
                    t.type_str()
                ),
            }
        }
        match &mut self.parsed {
            // TODO: Make sure if aot, that value is standard table.
            DocValueType::Array { values, .. } => values.push(value),
            t @ _ => panic!("cannot push onto {} type", t.type_str()),
        }
    }

    pub fn entry<K>(&mut self, key: K) -> DocEntry
    where
        K: Into<DocKey>,
    {
        match &mut self.parsed {
            DocValueType::Table(t) => {
                t.entry(key)
            }
            t @ _ => panic!("entry not allowed with {} type", t.type_str()),
        }
    }

    pub fn insert(
        &mut self,
        key: DocKey,
        value: DocValue,
    ) -> Result<Option<(DocKey, DocValue)>, de::Error> {
        match &mut self.parsed {
            DocValueType::Table(t) => {
                // TODO: Move this to insert method?
                t.insert(key, value, true)
            }
            t @ _ => panic!("cannot insert into {} type", t.type_str()),
        }
    }

    // pub fn entry<K>(&mut self, key: K) -> DocEntry
    //     where K: Into<DocKey>
    // {
    //     match &mut self.parsed {
    //         DocValueType::Table(t) => t.entry(key),
    //         t @ _ => panic!("cannot index into {} type", t.type_str())
    //     }
    // }
}

impl FromStr for DocValue {
    type Err = de::Error;
    fn from_str(s: &str) -> Result<DocValue, Self::Err> {
        DocValue::from_str(s)
    }
}

impl DocValueType {
    fn from_raw(raw: RawValueType) -> Result<DocValueType, de::Error> {
        let dt = match raw {
            RawValueType::Integer(i) => DocValueType::Integer(i),
            RawValueType::Float(f) => DocValueType::Float(f),
            RawValueType::Boolean(b) => DocValueType::Boolean(b),
            RawValueType::String(s) => DocValueType::String(s.to_string()),
            RawValueType::Datetime(dt) => match dt.parse() {
                Ok(dt_value) => DocValueType::Datetime(dt_value),
                Err(e) => return Err(de::Error::custom(e.to_string())),
            },
            RawValueType::Array(arr) => {
                let values = arr
                    .into_iter()
                    .map(DocValue::from_raw)
                    .collect::<Result<Vec<_>, de::Error>>()?;
                let aot = values
                    .first()
                    .map(DocValue::is_table_array_member)
                    .unwrap_or(false);
                DocValueType::Array {
                    aot: aot,
                    values: values,
                }
            }
            RawValueType::RawTable(entries) => {
                let mut table = DocTable::new();
                table.is_inline = true;
                for (key, value) in entries {
                    let doc_key = DocKey::from_raw(None, key);
                    let doc_value = DocValue::from_raw(value)?;
                    table.add_dotted_key(doc_key, doc_value)?;
                }
                DocValueType::Table(table)
            }
            RawValueType::InlineTable(_) | RawValueType::DottedTable(_) => {
                panic!("inline table unexpected")
            }
        };
        Ok(dt)
    }

    /// Render if the original string is not available.
    fn render(&self, mut output: &mut dyn Write) {
        match self {
            DocValueType::Reserved => {}
            DocValueType::Integer(i) => drop(write!(output, "{}", i)),
            DocValueType::Float(f) => {
                let f = *f;
                // TODO: consider sharing with serialize_float
                if (f.is_nan() || f == 0.0) && f.is_sign_negative() {
                    drop(output.write_all(b"-"));
                }
                if f.is_nan() {
                    drop(output.write_all(b"nan"));
                } else {
                    drop(write!(output, "{}", f));
                }
                if f % 1.0 == 0.0 {
                    drop(output.write_all(b".0"));
                }
            }
            DocValueType::Boolean(b) => drop(write!(output, "{}", b)),
            DocValueType::String(s) => {
                // TODO: there is a mistmatch between the ser module using
                // only strings and this module preferring `io::Write` trait.
                // Perhaps ser could also be transitioned to use Write?
                let mut tmp = String::new();
                ser::emit_str(&mut tmp, s, false, Some(&ser::StringSettings::pretty()));
                output.write_all(tmp.as_bytes());
            }
            DocValueType::Datetime(d) => drop(write!(output, "{}", d)),
            DocValueType::Array { values, .. } => {
                // TODO: check `aot`
                output.write_all(b"[");
                for val in values {
                    val.render(output);
                }
                output.write_all(b"]");
            }
            DocValueType::Table(table) => table.render(output),
        }
    }

    fn as_table(&self) -> &DocTable {
        match self {
            DocValueType::Table(t) => t,
            t @ _ => panic!("as_table on {} type", t.type_str()),
        }
    }

    fn is_intermediate_table(&self) -> bool {
        if let DocValueType::Table(t) = self {
            t.is_intermediate
        } else {
            false
        }
    }

    /// Returns a human-readable representation of the type of this value.
    pub fn type_str(&self) -> &'static str {
        match *self {
            DocValueType::Reserved => "reserved",
            DocValueType::String(..) => "string",
            DocValueType::Integer(..) => "integer",
            DocValueType::Float(..) => "float",
            DocValueType::Boolean(..) => "boolean",
            DocValueType::Datetime(..) => "datetime",
            DocValueType::Array { .. } => "array",
            DocValueType::Table(..) => "table",
        }
    }
}

fn is_blank(s: &str) -> bool {
    s.chars()
        .all(|c| c == ' ' || c == '\t' || c == '\r' || c == '\n')
}

fn opt_str(s: &str) -> Option<String> {
    if s.is_empty() {
        None
    } else {
        Some(String::from(s))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_doc() {
        rt("");
        rt("  \n");
        rt("foo=1");
        rt("foo = 1\n");
        rt("  foo  =  1  # comment\n");
        rt("# Comment");
        rt("# Comment\n");
        rt("# Comment\n\
            # Comment 2\n\
            \n\
            # Comment 3\n\
            foo = 1 # comment\n
            # Comment 4\n\
            ");
        rt("int = 123");
        rt("float = 3.14");
        rt("boolean = true");
        rt("boolean = false");
        rt("s = \"\\\"foo\\\"\"");
        rt(r#"s = """foo """ "#);
        rt("s = 'foo'");
        rt("s = '''foo'''");
        rt("dt = 2019-01-01");
        rt("dt = 1987-07-05T17:45:00Z");
        rt("a = []");
        rt("a = [1]");
        rt("a = [ 1 # comment\n\
            , \n\
            2 ]  # Trailing comment");
        rt("it = {}");
        rt("it = {  }  # Comment");
        rt("it = {foo = 1}");
        rt("it = {foo=1, bar=true} # Comment");

        rt("[a]");
        rt(" [ a  ]  # Comment");
        rt("# Comment\n[a] #Comment");
        rt("[a]\n\
            k1 = 1");
        rt("[a.b]\n\
            # Comment \n\
            k1 = 1\n\
            [a.c]\n\
            # C2\n\
            k2 = 2");

        rt("a.b = 1");
        rt("a.b.c = 1\n\
            a.b.d = 1");
        rt("\u{feff}");
        rt("\u{feff}a = 1");
        rt("");
        rt("");
        rt("");
        rt("");
    }

    #[test]
    fn test_doc2() {
        rt("a = 1\r\n");
    }

    fn rt(text: &str) {
        let doc = TomlDocument::from_str(text).unwrap();
        println!("doc={:#?}", doc);
        let rendered = doc.to_string();
        assert_eq!(text, rendered);
    }

    extern crate test;

    #[bench]
    fn bench_ser(b: &mut test::Bencher) {
        let text = std::fs::read_to_string("big.toml").unwrap();
        let doc = TomlDocument::from_str(&text).unwrap();
        b.iter(|| {
            doc.to_string();
        })
    }
}
