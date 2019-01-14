#![allow(unused)]
#![allow(missing_docs)]
//! TODO

use de::{self, Deserializer, Line};
use std::{collections::HashSet, fmt};

mod array;
mod entry;
mod index;
mod iter;
mod key;
mod table;
mod value;
use self::array::DocArray;
use self::entry::DocEntry;
use self::index::DocIndex;
pub use self::key::DocKey;
use self::table::DocTable;
pub use self::value::DocValue;
use self::value::DocValueType;

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
