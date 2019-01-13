use super::{iter, opt_str, DocEntry, DocIndex, DocKey, DocTable};
use datetime::Datetime;
use de::{self, Deserializer, RawValue, RawValueType};
use ser;
use std::{io::Write, str::FromStr};

#[derive(Debug)]
pub struct DocValue {
    /// This is used to determine if whitespace should be added during rendering.
    /// True if it came from parsing a document.
    /// False if added via the mutation API.
    pub(super) is_original: bool,
    /// Whitespace in front of the value.
    pub(super) pretext: Option<String>,
    /// Original text of the value.
    pub(super) text: Option<String>,
    /// Whitespace/comment following the value.
    pub(super) posttext: Option<String>,
    /// Parsed value.
    pub(super) parsed: DocValueType,
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

    pub(super) fn new(value: DocValueType) -> DocValue {
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

    pub(super) fn from_raw(raw: RawValue) -> Result<DocValue, de::Error> {
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

    pub(super) fn render(&self, mut output: &mut dyn Write) {
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
            (false, Some(text)) => {
                output.write_all(text.as_bytes());
            }
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
    pub(super) fn is_bracketed_table(&self) -> bool {
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

    pub(super) fn is_table_array_member(&self) -> bool {
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

    pub(super) fn is_reserved(&self) -> bool {
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
            DocValueType::Array { .. } => false, //TODO
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
            DocValueType::Table(t) => t.entry(key),
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
