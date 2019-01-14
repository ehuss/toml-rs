use super::{DocKey, DocTable, DocValue, DocValueType, TablePath, TomlDocument};
use std::ops;

pub trait DocIndex: Sealed {
    #[doc(hidden)]
    fn index_get<'v>(&self, val: &'v DocValue) -> Option<&'v DocValue>;
    fn index_get_mut<'v>(&self, val: &'v mut DocValue) -> Option<&'v mut DocValue>;
    fn index_get_doc<'v>(&self, doc: &'v TomlDocument) -> Option<&'v DocValue>;
    fn index_get_doc_mut<'v>(&self, doc: &'v mut TomlDocument) -> Option<&'v mut DocValue>;

    fn index_or_insert_value<'v>(&self, val: &'v mut DocValue) -> &'v mut DocValue;
    fn index_or_insert_doc<'v>(&self, doc: &'v mut TomlDocument) -> &'v mut DocValue;
    ///
    /// Used by `DocValue::remove` to support generic keys.
    fn index_remove<'v>(&self, val: &'v mut DocValue) -> Option<DocValue>;
}

#[doc(hidden)]
pub trait Sealed {}
impl Sealed for usize {}
impl Sealed for str {}
impl Sealed for String {}
impl Sealed for DocKey {}
impl<'v, T: Sealed + ?Sized> Sealed for &'v T {}

// TODO: I cannot get the lifetimes to work for ops::Index to use DocIndex.
// Temporary workaround is to implement the 3 types manually below.
impl<I> ops::Index<I> for DocValue
where
    I: DocIndex,
{
    type Output = DocValue;

    fn index(&self, index: I) -> &DocValue {
        // TODO: unwrap or else with the failed key name
        index.index_get(self).expect("index not found")
    }
}

impl<I> ops::Index<I> for TomlDocument
where
    I: DocIndex,
{
    type Output = DocValue;

    fn index(&self, index: I) -> &DocValue {
        index.index_get_doc(self).expect("index not found")
    }
}

impl<I> ops::IndexMut<I> for DocValue
where
    I: DocIndex,
{
    fn index_mut(&mut self, index: I) -> &mut DocValue {
        index.index_get_mut(self).expect("index not found")
    }
}

impl ops::IndexMut<&str> for TomlDocument {
    fn index_mut(&mut self, index: &str) -> &mut DocValue {
        index.index_get_doc_mut(self).expect("index not found")
    }
}

impl DocIndex for usize {
    fn index_get<'v>(&self, val: &'v DocValue) -> Option<&'v DocValue> {
        match &val.parsed {
            DocValueType::Array(array) => array.get(*self),
            _ => None,
        }
    }

    fn index_get_mut<'v>(&self, val: &'v mut DocValue) -> Option<&'v mut DocValue> {
        match &mut val.parsed {
            DocValueType::Array(array) => array.get_mut(*self),
            _ => None,
        }
    }

    fn index_get_doc<'v>(&self, doc: &'v TomlDocument) -> Option<&'v DocValue> {
        panic!("usize index into TomlDocument not allowed");
    }
    fn index_get_doc_mut<'v>(&self, doc: &'v mut TomlDocument) -> Option<&'v mut DocValue> {
        panic!("usize index into TomlDocument not allowed");
    }

    fn index_or_insert_value<'v>(&self, val: &'v mut DocValue) -> &'v mut DocValue {
        unimplemented!()
        // match &mut val.parsed {
        //     DocValueType::Array { values, .. } => {
        //         let len = values.len();
        //         values.get_mut(*self).unwrap_or_else(|| {
        //             panic!(
        //                 "index {} out of bounds for array length length {}",
        //                 self, len
        //             )
        //         })
        //     }
        //     t @ _ => panic!(
        //         "usize index only supported for array type, not {}",
        //         t.type_str()
        //     ),
        // }
    }

    fn index_or_insert_doc<'v>(&self, doc: &'v mut TomlDocument) -> &'v mut DocValue {
        panic!("TomlDocument cannot be indexed by usize.");
    }

    fn index_remove<'v>(&self, val: &'v mut DocValue) -> Option<DocValue> {
        match &mut val.parsed {
            DocValueType::Array(array) => Some(array.remove(*self)),
            _ => None,
        }
    }
}

impl DocIndex for str {
    fn index_get<'v>(&self, val: &'v DocValue) -> Option<&'v DocValue> {
        match &val.parsed {
            DocValueType::Table(t) => t.get(self),
            _ => None,
        }
    }

    fn index_get_mut<'v>(&self, val: &'v mut DocValue) -> Option<&'v mut DocValue> {
        match &mut val.parsed {
            DocValueType::Table(t) => t.get_mut(self),
            _ => None,
        }
    }

    fn index_get_doc<'v>(&self, doc: &'v TomlDocument) -> Option<&'v DocValue> {
        doc.root.get(self)
    }
    fn index_get_doc_mut<'v>(&self, doc: &'v mut TomlDocument) -> Option<&'v mut DocValue> {
        doc.root.get_mut(self)
    }

    fn index_or_insert_value<'v>(&self, val: &'v mut DocValue) -> &'v mut DocValue {
        if val.is_reserved() {
            *val = DocValue::new(DocValueType::Table(DocTable::new()));
        }
        // match *v {
        //     Value::Object(ref mut map) => map.entry(self.to_owned()).or_insert(Value::Null),
        //     _ => panic!("cannot access key {:?} in JSON {}", self, Type(v)),
        // }

        match &mut val.parsed {
            DocValueType::Table(table) => {
                unimplemented!();
                // table.entry(self).or_insert(DocValue::new(DocValueType::Reserved))
            }
            t @ _ => panic!(
                "string index only supported for table type, not {}",
                t.type_str()
            ),
        }
    }

    fn index_or_insert_doc<'v>(&self, doc: &'v mut TomlDocument) -> &'v mut DocValue {
        unimplemented!();
    }

    fn index_remove<'v>(&self, val: &'v mut DocValue) -> Option<DocValue> {
        match &mut val.parsed {
            DocValueType::Table(t) => t.remove(self),
            _ => None,
        }
    }
}

impl DocIndex for String {
    fn index_get<'v>(&self, val: &'v DocValue) -> Option<&'v DocValue> {
        self[..].index_get(val)
    }

    fn index_get_mut<'v>(&self, val: &'v mut DocValue) -> Option<&'v mut DocValue> {
        self[..].index_get_mut(val)
    }

    fn index_get_doc<'v>(&self, doc: &'v TomlDocument) -> Option<&'v DocValue> {
        self[..].index_get_doc(doc)
    }
    fn index_get_doc_mut<'v>(&self, doc: &'v mut TomlDocument) -> Option<&'v mut DocValue> {
        self[..].index_get_doc_mut(doc)
    }

    fn index_remove<'v>(&self, val: &'v mut DocValue) -> Option<DocValue> {
        self[..].index_remove(val)
    }

    fn index_or_insert_value<'v>(&self, v: &'v mut DocValue) -> &'v mut DocValue {
        unimplemented!()
        // match &mut val.parsed {
        //     DocValueType::Table(table) => {
        //         table.entry(self).or_insert(DocValue::placeholder)
        //     }
        //     t @ _ => panic!("string index only supported for table type, not {}", t.type_str()),
        // }
    }

    fn index_or_insert_doc<'v>(&self, doc: &'v mut TomlDocument) -> &'v mut DocValue {
        unimplemented!();
    }
}

impl<'s, T: ?Sized> DocIndex for &'s T
where
    T: DocIndex,
{
    fn index_get<'v>(&self, val: &'v DocValue) -> Option<&'v DocValue> {
        (**self).index_get(val)
    }

    fn index_get_mut<'v>(&self, val: &'v mut DocValue) -> Option<&'v mut DocValue> {
        (**self).index_get_mut(val)
    }

    fn index_get_doc<'v>(&self, doc: &'v TomlDocument) -> Option<&'v DocValue> {
        (**self).index_get_doc(doc)
    }
    fn index_get_doc_mut<'v>(&self, doc: &'v mut TomlDocument) -> Option<&'v mut DocValue> {
        (**self).index_get_doc_mut(doc)
    }

    fn index_remove<'v>(&self, val: &'v mut DocValue) -> Option<DocValue> {
        (**self).index_remove(val)
    }

    fn index_or_insert_value<'v>(&self, val: &'v mut DocValue) -> &'v mut DocValue {
        (**self).index_or_insert_value(val)
    }

    fn index_or_insert_doc<'v>(&self, val: &'v mut TomlDocument) -> &'v mut DocValue {
        (**self).index_or_insert_doc(val)
    }
}

impl DocIndex for DocKey {
    fn index_get<'v>(&self, val: &'v DocValue) -> Option<&'v DocValue> {
        match &val.parsed {
            DocValueType::Table(t) => {
                // TODO: Assert not [[array table]]
                let pi = TablePath::new_from_parts(self.parts.clone());
                t.get_path(&pi, 0)
            }
            _ => None,
        }
    }

    fn index_get_mut<'v>(&self, val: &'v mut DocValue) -> Option<&'v mut DocValue> {
        match &mut val.parsed {
            DocValueType::Table(t) => {
                // TODO: Assert not [[array table]]
                let pi = TablePath::new_from_parts(self.parts.clone());
                t.get_path_mut(&pi, 0)
            }
            _ => None,
        }
    }

    fn index_get_doc<'v>(&self, doc: &'v TomlDocument) -> Option<&'v DocValue> {
        let pi = TablePath::new_from_parts(self.parts.clone());
        doc.root.get_path(&pi, 0)
    }
    fn index_get_doc_mut<'v>(&self, doc: &'v mut TomlDocument) -> Option<&'v mut DocValue> {
        let pi = TablePath::new_from_parts(self.parts.clone());
        doc.root.get_path_mut(&pi, 0)
    }

    fn index_or_insert_value<'v>(&self, val: &'v mut DocValue) -> &'v mut DocValue {
        unimplemented!()
        // match &mut val.parsed {
        //     DocValueType::Table(table) => {
        //         table.entry(self).or_insert(DocValue::placeholder)
        //     }
        //     t @ _ => panic!("string index only supported for table type, not {}", t.type_str()),
        // }
    }

    fn index_or_insert_doc<'v>(&self, doc: &'v mut TomlDocument) -> &'v mut DocValue {
        unimplemented!();
    }

    fn index_remove<'v>(&self, val: &'v mut DocValue) -> Option<DocValue> {
        unimplemented!()
        // match &mut val.parsed {
        //     DocValueType::Table(t) => t.remove(self),
        //     _ => None,
        // }
    }
}
