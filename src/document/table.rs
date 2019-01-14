use super::{
    entry, iter, DocArray, DocEntry, DocKey, DocValue, DocValueType, IndexKey, TablePath,
    TomlDocument,
};
use de;
use std::{
    collections::{hash_map, HashMap, HashSet},
    io::Write,
};

/// TODO: docme
/// This serves multiple purposes.
/// - Bracketed tables `[foo.bar]`.
/// - Inline tables `inline = {key = "value"}`
/// - Intermediate tables. Tables created by dotted keys (both bracketed and inline).
#[derive(Debug)]
pub struct DocTable {
    /// Whitespace/comments in front of the header.
    pub(super) header_pretext: Option<String>,
    /// The original text of the name (everything between the brackets,
    /// including whitespace). None for inline or intermediate tables.
    pub(super) header_key: Option<DocKey>,
    /// All text following the closing bracket of the table name.
    pub(super) header_posttext: Option<String>,
    // TODO: Change to enum?
    pub(super) is_array: bool,
    pub(super) is_inline: bool,
    pub(super) is_intermediate: bool,
    pub(super) is_modified: bool,
    /// Comments/whitespace at the end of the file.
    pub(super) table_posttext: Option<String>,
    pub(super) indentation: Option<String>,
    /// Sequence of key/values in the table in the order they appear.
    ///
    /// These entries may become stale, so care must be taken when inspecting them. (XXX)
    /// Standard tables are not listed here, they are only listed in `TomlDocument::table_order`.
    pub(super) items: Vec<(DocKey, TablePath)>,
    /// Map of values in this table.
    pub(super) map: HashMap<String, DocValue>,
}

// Hack to avoid duplicating immutable/mutable versions.
// There's probably a better way.
macro_rules! get_path {
    ($self:ident, $path:ident, $path_idx:ident, $get_path:ident, $get:ident, $($m:ident)*) => {
        // TODO: Replace these .0 with with methods.
        // TODO: Do these [index] operations need to be fallible if the path is old?
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
                    if let DocValueType::Array(array) = & $($m)* dv.parsed {
                        let indexed = & $($m)* array.values[*arr_index];
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

    pub(super) fn new_root() -> DocTable {
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
    pub(super) fn add_dotted_key(&mut self, key: DocKey, value: DocValue) -> Result<(), de::Error> {
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
    pub(super) fn add_table(
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
                        let array = DocArray::new(true, values);
                        DocValue::new(DocValueType::Array(array))
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
                    DocValueType::Array(array) if array.is_aot => {
                        let arr_len = array.values.len();
                        if is_im {
                            let mut last = match array.values.last_mut() {
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
                            array.values.push(doc_value);
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

    pub(super) fn insert(
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
                        let array = DocArray::new(true, values);
                        DocValue::new(DocValueType::Array(array))
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

    pub(super) fn render(&self, mut output: &mut dyn Write) {
        macro_rules! write_eq {
            ($doc_key:ident, $dv:ident) => {
                match ($doc_key.is_original, $dv.is_original) {
                    (true, true) => output.write_all(b"="),
                    (true, false) => output.write_all(b"= "),
                    (false, true) => output.write_all(b" ="),
                    (false, false) => output.write_all(b" = "),
                }
            };
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
                    if self.is_modified
                        && !dv
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

    pub(super) fn get_path(&self, path: &TablePath, path_idx: usize) -> Option<&DocValue> {
        get_path!(self, path, path_idx, get_path, get,)
    }

    pub(super) fn get_path_mut(
        &mut self,
        path: &TablePath,
        path_idx: usize,
    ) -> Option<&mut DocValue> {
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
                DocValueType::Array(array) => {
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
    pub(super) fn entry_insert<'a>(
        &mut self,
        key: &DocKey,
        value: DocValue,
        cur_part: usize,
    ) -> &mut DocValue {
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
                        let array = DocArray::new(true, values);
                        DocValue::new(DocValueType::Array(array))
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

    pub(super) fn collect_new_tables(
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
                DocValueType::Array(array) if array.is_aot => {
                    for (i, value) in array.values.iter().enumerate() {
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
