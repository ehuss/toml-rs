#![allow(unused)]
#![allow(missing_docs)]
//! TODO

use datetime::Datetime;
use de::{self, Deserializer, Line, RawKey, RawValue, RawValueType};
use ser;
use std::{
    borrow::Cow,
    collections::{hash_map, HashMap},
    io::Write,
    str::FromStr,
};

mod index;
pub use self::index::DocIndex;

#[derive(Debug)]
pub struct TomlDocument {
    has_bom: bool,
    // line_ending: LineEndingStyle,
    root: DocTable,
    /// List of bracketed tables in the order they appear.
    table_order: Vec<PathIndex>,
}

/// A chain of keys to a nested table.
#[derive(Debug)]
struct PathIndex(Vec<IndexKey>);

impl PathIndex {
    fn new() -> PathIndex {
        PathIndex(Vec::new())
    }

    fn push(&mut self, key: IndexKey) {
        self.0.push(key)
    }
}

#[derive(Debug)]
enum IndexKey {
    Table(String),
    Array(String, usize),
}

#[derive(Debug)]
struct DocKey {
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
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Datetime(Datetime),
    Array(Vec<DocValue>),
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
    /// including whitespace).
    header_key: Option<DocKey>,
    /// All text following the closing bracket of the table name.
    header_posttext: Option<String>,
    // TODO: Change to enum?
    is_array: bool,
    is_inline: bool,
    is_intermediate: bool,
    /// Comments/whitespace at the end of the file.
    table_posttext: Option<String>,
    indentation: Option<String>,
    /// Sequence of key/values in the table in the order they appear.
    items: Vec<(DocKey, PathIndex)>,
    /// Map of values in this table.
    map: HashMap<String, DocValue>,
}

impl TomlDocument {
    pub fn from_str(s: &str) -> Result<TomlDocument, de::Error> {
        let mut d = Deserializer::new(s);
        let mut tables = Vec::new();
        let mut cur_table = DocTable::new();
        // The root table's key is an empty list.
        cur_table.header_key = Some(DocKey::new(Vec::new(), None));
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
                        let mut root_pretext =
                            cur_table.header_pretext.get_or_insert_with(String::new);
                        // Comment blocks at the top of the document belong to the root table.
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
            let mut pi = PathIndex::new();
            doc.root.add_table(table, 0, &mut pi)?;
            doc.table_order.push(pi);
        }

        Ok(doc)
    }

    pub fn to_string(&self) -> String {
        let mut res: Vec<u8> = Vec::new();
        if self.has_bom {
            res.push(b'\xef');
            res.push(b'\xbb');
            res.push(b'\xbf');
        }
        let mut writer = Box::new(res);
        self.root.render(&mut writer);
        for path in &self.table_order {
            if let Some(table) = self.get_path(path) {
                table.render(&mut writer);
            }
        }
        // The rendering code always uses utf-8.
        unsafe { String::from_utf8_unchecked(*writer) }
    }

    fn get_path(&self, path: &PathIndex) -> Option<&DocValue> {
        self.root.get_path(path, 0)
    }

    pub fn get(&self, key: &str) -> Option<&DocValue> {
        self.root.get(key)
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
            table_posttext: None,
            indentation: None,
            items: Vec::new(),
            map: HashMap::new(),
        }
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
        let mut pi = PathIndex::new();
        self.add_dotted_key_r(&key, value, 0, &mut pi)?;
        self.items.push((key, pi));
        Ok(())
    }

    // TODO: merge with add_table?
    fn add_dotted_key_r(
        &mut self,
        key: &DocKey,
        value: DocValue,
        cur_part: usize,
        pi: &mut PathIndex,
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
            pi.push(IndexKey::Table(part));
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
                pi.push(IndexKey::Table(part));
                if let DocValueType::Table(ref mut t) = dv.parsed {
                    t.add_dotted_key_r(key, value, cur_part + 1, pi)?;
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
                        pi.push(IndexKey::Table(part));
                        t.add_dotted_key_r(key, value, cur_part + 1, pi)?;
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
    /// - `pi`: The path index to the table is accumulated in this value.
    fn add_table(
        &mut self,
        mut table: DocTable,
        cur_part: usize,
        pi: &mut PathIndex,
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
                    pi.push(IndexKey::Table(part));
                    if let DocValueType::Table(ref mut t) = dv.parsed {
                        t.add_table(table, cur_part + 1, pi)?;
                    }
                } else {
                    let doc_key = DocKey::new(header_parts.clone(), None);
                    let doc_value = if table.is_array {
                        pi.push(IndexKey::Array(part, 0));
                        let items = vec![DocValue::new(DocValueType::Table(table))];
                        DocValue::new(DocValueType::Array(items))
                    } else {
                        pi.push(IndexKey::Table(part));
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
                        pi.push(IndexKey::Table(part.clone()));
                        // TODO: Check is_array?
                        // TODO: Must be regular table, not inline or dotted.
                        if is_im {
                            t.add_table(table, cur_part + 1, pi)?;
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
                    DocValueType::Array(arr) => {
                        let arr_len = arr.len();
                        if is_im {
                            let mut last = match arr.last_mut() {
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
                                    pi.push(IndexKey::Array(part, arr_len - 1));
                                    t.add_table(table, cur_part + 1, pi)?;
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
                            pi.push(IndexKey::Array(part, arr_len));
                            let doc_value = DocValue::new(DocValueType::Table(table));
                            arr.push(doc_value);
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

    fn render(&self, mut output: &mut dyn Write) {
        if let Some(pretext) = &self.header_pretext {
            output.write_all(pretext.as_bytes());
        }
        if let Some(key) = &self.header_key {
            assert!(!self.is_intermediate);
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
        for (doc_key, pi) in &self.items {
            let dv = self.get_path(pi, 0).expect("missing item path");
            doc_key.render(output);
            output.write_all(b"=");
            dv.render(output);
        }
        if let Some(text) = &self.table_posttext {
            output.write_all(text.as_bytes());
        }
    }

    fn get_path(&self, path: &PathIndex, path_idx: usize) -> Option<&DocValue> {
        // TODO: Replace these .0 with with methods.
        match &path.0[path_idx] {
            IndexKey::Table(name) => {
                if let Some(dv) = self.map.get(name) {
                    if path.0.len() - 1 == path_idx {
                        Some(dv)
                    } else {
                        if let DocValueType::Table(table) = &dv.parsed {
                            table.get_path(path, path_idx + 1)
                        } else {
                            panic!("get_path expected table, got {:?}", dv);
                        }
                    }
                } else {
                    None
                }
            }
            IndexKey::Array(name, arr_index) => {
                if let Some(dv) = self.map.get(name) {
                    if let DocValueType::Array(array) = &dv.parsed {
                        let indexed = &array[*arr_index];
                        if path.0.len() - 1 == path_idx {
                            Some(indexed)
                        } else {
                            if let DocValueType::Table(table) = &indexed.parsed {
                                table.get_path(path, path_idx + 1)
                            } else {
                                panic!("get_path expected array table, got {:?}", indexed);
                            }
                        }
                    } else {
                        panic!("get_path expected array, got {:?}", dv);
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn get(&self, key: &str) -> Option<&DocValue> {
        self.map.get(key)
    }
}

impl DocKey {
    fn new(parts: Vec<String>, text: Option<String>) -> DocKey {
        DocKey {
            pretext: None,
            indent: None,
            text: text,
            parts: parts,
            posttext: None,
        }
    }

    fn from_raw(pretext: Option<String>, raw: RawKey) -> DocKey {
        DocKey {
            pretext: pretext,
            indent: opt_str(raw.pretext),
            text: opt_str(raw.text),
            parts: raw.parts.into_iter().map(|p| p.into_owned()).collect(),
            posttext: opt_str(raw.posttext),
        }
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
            pretext: None,
            text: None,
            posttext: None,
            parsed: value,
        }
    }

    fn from_raw(raw: RawValue) -> Result<DocValue, de::Error> {
        Ok(DocValue {
            pretext: opt_str(raw.pretext),
            text: opt_str(raw.text),
            posttext: opt_str(raw.posttext),
            parsed: DocValueType::from_raw(raw.parsed)?,
        })
    }

    pub fn from_str(s: &str) -> Result<DocValue, de::Error> {
        let mut d = Deserializer::new(s);
        let value = d.value()?;
        DocValue::from_raw(value)
    }

    pub fn to_string(&self) -> String {
        let mut res: Box<Vec<u8>> = Box::new(Vec::new());
        self.render(&mut res);
        unsafe { String::from_utf8_unchecked(*res) }
    }

    fn render(&self, mut output: &mut dyn Write) {
        // if self.parsed.is_intermediate() {
        //     self.parsed.render(output)
        // }
        if let Some(pretext) = &self.pretext {
            output.write_all(pretext.as_bytes());
        }
        if let Some(text) = &self.text {
            output.write_all(text.as_bytes());
        } else {
            self.parsed.render(output);
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
            DocValueType::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => true,
            _ => false,
        }
    }

    pub fn is_std_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => !t.is_inline && !t.is_intermediate,
            _ => false,
        }
    }

    pub fn is_array_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => t.is_array,
            DocValueType::Array(a) => {
                a.last().map_or(false, |dv| dv.is_array_table())
            }
            _ => false,
        }
    }

    pub fn is_inline_table(&self) -> bool {
        match &self.parsed {
            DocValueType::Table(t) => t.is_inline,
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
        index.index(self)
    }

    pub fn is_empty(&self) -> Option<bool> {
        match &self.parsed {
            DocValueType::Array(a) => Some(a.is_empty()),
            DocValueType::Table(t) => Some(t.map.is_empty()),
            _ => None,
        }
    }

    pub fn len(&self) -> Option<usize> {
        match &self.parsed {
            DocValueType::Array(a) => Some(a.len()),
            DocValueType::Table(t) => Some(t.map.len()),
            _ => None,
        }
    }
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
            RawValueType::Array(arr) => DocValueType::Array(
                arr.into_iter()
                    .map(DocValue::from_raw)
                    .collect::<Result<Vec<_>, de::Error>>()?,
            ),
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
            DocValueType::Array(arr) => {
                output.write_all(b"[");
                for val in arr {
                    val.render(output);
                }
                output.write_all(b"]");
            }
            DocValueType::Table(table) => table.render(output),
        }
    }

    fn is_intermediate_table(&self) -> bool {
        if let DocValueType::Table(t) = self {
            t.is_intermediate
        } else {
            false
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
