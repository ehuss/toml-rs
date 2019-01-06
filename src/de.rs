//! Deserializing TOML into Rust structures.
//!
//! This module contains all the Serde support for deserializing TOML documents
//! into Rust structures. Note that some top-level functions here are also
//! provided at the top of the crate.

use std::borrow::Cow;
use std::error;
use std::f64;
use std::fmt;
use std::str;
use std::vec;

use serde::de;
use serde::de::value::BorrowedStrDeserializer;
use serde::de::IntoDeserializer;

use datetime;
use spanned;
use tokens::{Error as TokenError, Span, Token, Tokenizer};

/// Deserializes a byte slice into a type.
///
/// This function will attempt to interpret `bytes` as UTF-8 data and then
/// deserialize `T` from the TOML document provided.
pub fn from_slice<'de, T>(bytes: &'de [u8]) -> Result<T, Error>
where
    T: de::Deserialize<'de>,
{
    match str::from_utf8(bytes) {
        Ok(s) => from_str(s),
        Err(e) => Err(Error::custom(e.to_string())),
    }
}

/// Deserializes a string into a type.
///
/// This function will attempt to interpret `s` as a TOML document and
/// deserialize `T` from the document.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate serde_derive;
/// extern crate toml;
///
/// #[derive(Deserialize)]
/// struct Config {
///     title: String,
///     owner: Owner,
/// }
///
/// #[derive(Deserialize)]
/// struct Owner {
///     name: String,
/// }
///
/// fn main() {
///     let config: Config = toml::from_str(r#"
///         title = 'TOML Example'
///
///         [owner]
///         name = 'Lisa'
///     "#).unwrap();
///
///     assert_eq!(config.title, "TOML Example");
///     assert_eq!(config.owner.name, "Lisa");
/// }
/// ```
pub fn from_str<'de, T>(s: &'de str) -> Result<T, Error>
where
    T: de::Deserialize<'de>,
{
    let mut d = Deserializer::new(s);
    let ret = T::deserialize(&mut d)?;
    d.end()?;
    Ok(ret)
}

/// Errors that can occur when deserializing a type.
#[derive(Debug, Clone)]
pub struct Error {
    inner: Box<ErrorInner>,
}

#[derive(Debug, Clone)]
struct ErrorInner {
    kind: ErrorKind,
    line: Option<usize>,
    col: usize,
    message: String,
    key: Vec<String>,
}

/// Errors that can occur when deserializing a type.
#[derive(Debug, Clone)]
pub(crate) enum ErrorKind {
    /// EOF was reached when looking for a value
    UnexpectedEof,

    /// An invalid character not allowed in a string was found
    InvalidCharInString(char),

    /// An invalid character was found as an escape
    InvalidEscape(char),

    /// An invalid character was found in a hex escape
    InvalidHexEscape(char),

    /// An invalid escape value was specified in a hex escape in a string.
    ///
    /// Valid values are in the plane of unicode codepoints.
    InvalidEscapeValue(u32),

    /// A newline in a string was encountered when one was not allowed.
    NewlineInString,

    /// An unexpected character was encountered, typically when looking for a
    /// value.
    Unexpected(char),

    /// An unterminated string was found where EOF was found before the ending
    /// EOF mark.
    UnterminatedString,

    /// A newline was found in a table key.
    NewlineInTableKey,

    /// A newline was found in an inline table.
    NewlineInInlineTable,

    /// A number failed to parse
    NumberInvalid,

    /// A date or datetime was invalid
    DateInvalid,

    /// Wanted one sort of token, but found another.
    Wanted {
        /// Expected token type
        expected: &'static str,
        /// Actually found token type
        found: &'static str,
    },

    /// An array was decoded but the types inside of it were mixed, which is
    /// disallowed by TOML.
    MixedArrayType,

    /// A duplicate table definition was found.
    DuplicateTable(String),

    /// A duplicate key was found.
    DuplicateKey(String),

    /// A previously defined table was redefined as an array.
    RedefineAsArray,

    /// An empty table key was found.
    EmptyTableKey,

    /// Multiline strings are not allowed for key
    MultilineStringKey,

    /// A custom error which could be generated when deserializing a particular
    /// type.
    Custom,

    /// A tuple with a certain number of elements was expected but something
    /// else was found.
    ExpectedTuple(usize),

    /// Expected table keys to be in increasing tuple index order, but something
    /// else was found.
    ExpectedTupleIndex {
        /// Expected index.
        expected: usize,
        /// Key that was specified.
        found: String,
    },

    /// An empty table was expected but entries were found
    ExpectedEmptyTable,

    /// Dotted key attempted to extend something that is not a table.
    DottedKeyInvalidType,

    /// An unexpected key was encountered.
    ///
    /// Used when deserializing a struct with a limited set of fields.
    UnexpectedKeys {
        /// The unexpected keys.
        keys: Vec<String>,
        /// Keys that may be specified.
        available: &'static [&'static str],
    },

    #[doc(hidden)]
    __Nonexhaustive,
}

/// Deserialization implementation for TOML.
pub struct Deserializer<'a> {
    require_newline_after_table: bool,
    allow_duplciate_after_longer_table: bool,
    input: &'a str,
    tokens: Tokenizer<'a>,
}

impl<'de, 'b> de::Deserializer<'de> for &'b mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        let mut tables = self.tables()?;

        visitor.visit_map(MapVisitor {
            values: Vec::new().into_iter(),
            next_value: None,
            depth: 0,
            cur: 0,
            cur_parent: 0,
            max: tables.len(),
            tables: &mut tables,
            array: false,
            de: self,
        })
    }

    // Called when the type to deserialize is an enum, as opposed to a field in the type.
    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        let value = self.string_or_table()?;
        match value.parsed {
            RawValueType::String(val) => visitor.visit_enum(val.into_deserializer()),
            RawValueType::InlineTable(values) | RawValueType::DottedTable(values) => {
                visitor.visit_enum(InlineTableDeserializer {
                    values: values.into_iter(),
                    next_value: None,
                })
            }
            e @ _ => Err(Error::from_kind(ErrorKind::Wanted {
                expected: "string or table",
                found: e.type_name(),
            })),
        }
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map struct unit newtype_struct
        ignored_any unit_struct tuple_struct tuple option identifier
    }
}

struct Table<'a> {
    at: usize,
    header: Vec<Cow<'a, str>>,
    values: Option<Vec<(Cow<'a, str>, RawValue<'a>)>>,
    array: bool,
}

#[doc(hidden)]
pub struct MapVisitor<'de: 'b, 'b> {
    values: vec::IntoIter<(Cow<'de, str>, RawValue<'de>)>,
    next_value: Option<(Cow<'de, str>, RawValue<'de>)>,
    depth: usize,
    cur: usize,
    cur_parent: usize,
    max: usize,
    tables: &'b mut [Table<'de>],
    array: bool,
    de: &'b mut Deserializer<'de>,
}

impl<'de, 'b> de::MapAccess<'de> for MapVisitor<'de, 'b> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.cur_parent == self.max || self.cur == self.max {
            return Ok(None);
        }

        loop {
            assert!(self.next_value.is_none());
            if let Some((key, value)) = self.values.next() {
                let ret = seed.deserialize(StrDeserializer::new(key.clone()))?;
                self.next_value = Some((key, value));
                return Ok(Some(ret));
            }

            let next_table = {
                let prefix = &self.tables[self.cur_parent].header[..self.depth];
                self.tables[self.cur..self.max]
                    .iter()
                    .enumerate()
                    .find(|&(_, t)| {
                        if t.values.is_none() {
                            return false;
                        }
                        match t.header.get(..self.depth) {
                            Some(header) => header == prefix,
                            None => false,
                        }
                    })
                    .map(|(i, _)| i + self.cur)
            };

            let pos = match next_table {
                Some(pos) => pos,
                None => return Ok(None),
            };
            self.cur = pos;

            // Test to see if we're duplicating our parent's table, and if so
            // then this is an error in the toml format
            if self.cur_parent != pos {
                if self.tables[self.cur_parent].header == self.tables[pos].header {
                    let at = self.tables[pos].at;
                    let name = self.tables[pos].header.join(".");
                    return Err(self.de.error(at, ErrorKind::DuplicateTable(name)));
                }

                // If we're here we know we should share the same prefix, and if
                // the longer table was defined first then we want to narrow
                // down our parent's length if possible to ensure that we catch
                // duplicate tables defined afterwards.
                if !self.de.allow_duplciate_after_longer_table {
                    let parent_len = self.tables[self.cur_parent].header.len();
                    let cur_len = self.tables[pos].header.len();
                    if cur_len < parent_len {
                        self.cur_parent = pos;
                    }
                }
            }

            let table = &mut self.tables[pos];

            // If we're not yet at the appropriate depth for this table then we
            // just next the next portion of its header and then continue
            // decoding.
            if self.depth != table.header.len() {
                let key = &table.header[self.depth];
                let key = seed.deserialize(StrDeserializer::new(key.clone()))?;
                return Ok(Some(key));
            }

            // Rule out cases like:
            //
            //      [[foo.bar]]
            //      [[foo]]
            if table.array {
                let kind = ErrorKind::RedefineAsArray;
                return Err(self.de.error(table.at, kind));
            }

            self.values = table
                .values
                .take()
                .expect("Unable to read table values")
                .into_iter();
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        if let Some((k, v)) = self.next_value.take() {
            match seed.deserialize(ValueDeserializer::new(v)) {
                Ok(v) => return Ok(v),
                Err(mut e) => {
                    e.add_key_context(&k);
                    return Err(e);
                }
            }
        }

        let array =
            self.tables[self.cur].array && self.depth == self.tables[self.cur].header.len() - 1;
        self.cur += 1;
        let res = seed.deserialize(MapVisitor {
            values: Vec::new().into_iter(),
            next_value: None,
            depth: self.depth + if array { 0 } else { 1 },
            cur_parent: self.cur - 1,
            cur: 0,
            max: self.max,
            array: array,
            tables: &mut *self.tables,
            de: &mut *self.de,
        });
        res.map_err(|mut e| {
            e.add_key_context(&self.tables[self.cur - 1].header[self.depth]);
            e
        })
    }
}

impl<'de, 'b> de::SeqAccess<'de> for MapVisitor<'de, 'b> {
    type Error = Error;

    fn next_element_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        assert!(self.next_value.is_none());
        assert!(self.values.next().is_none());

        if self.cur_parent == self.max {
            return Ok(None);
        }

        let next = self.tables[..self.max]
            .iter()
            .enumerate()
            .skip(self.cur_parent + 1)
            .find(|&(_, table)| table.array && table.header == self.tables[self.cur_parent].header)
            .map(|p| p.0)
            .unwrap_or(self.max);

        let ret = seed.deserialize(MapVisitor {
            values: self.tables[self.cur_parent]
                .values
                .take()
                .expect("Unable to read table values")
                .into_iter(),
            next_value: None,
            depth: self.depth + 1,
            cur_parent: self.cur_parent,
            max: next,
            cur: 0,
            array: false,
            tables: &mut self.tables,
            de: &mut self.de,
        })?;
        self.cur_parent = next;
        Ok(Some(ret))
    }
}

impl<'de, 'b> de::Deserializer<'de> for MapVisitor<'de, 'b> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        if self.array {
            visitor.visit_seq(self)
        } else {
            visitor.visit_map(self)
        }
    }

    // `None` is interpreted as a missing field so be sure to implement `Some`
    // as a present field.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map struct unit identifier
        ignored_any unit_struct tuple_struct tuple enum
    }
}

struct StrDeserializer<'a> {
    key: Cow<'a, str>,
}

impl<'a> StrDeserializer<'a> {
    fn new(key: Cow<'a, str>) -> StrDeserializer<'a> {
        StrDeserializer { key: key }
    }
}

impl<'de> de::Deserializer<'de> for StrDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        match self.key {
            Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
            Cow::Owned(s) => visitor.visit_string(s),
        }
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map struct option unit newtype_struct
        ignored_any unit_struct tuple_struct tuple enum identifier
    }
}

pub(crate) struct ValueDeserializer<'a> {
    value: RawValue<'a>,
    validate_struct_keys: bool,
}

impl<'a> ValueDeserializer<'a> {
    fn new(value: RawValue<'a>) -> ValueDeserializer<'a> {
        ValueDeserializer {
            value: value,
            validate_struct_keys: false,
        }
    }

    fn with_struct_key_validation(mut self) -> Self {
        self.validate_struct_keys = true;
        self
    }
}

impl<'de> de::Deserializer<'de> for ValueDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value.parsed {
            RawValueType::Integer(i) => visitor.visit_i64(i),
            RawValueType::Boolean(b) => visitor.visit_bool(b),
            RawValueType::Float(f) => visitor.visit_f64(f),
            RawValueType::String(Cow::Borrowed(s)) => visitor.visit_borrowed_str(s),
            RawValueType::String(Cow::Owned(s)) => visitor.visit_string(s),
            RawValueType::Datetime(s) => visitor.visit_map(DatetimeDeserializer {
                date: s,
                visited: false,
            }),
            RawValueType::Array(values) => {
                let mut s = de::value::SeqDeserializer::new(values.into_iter());
                let ret = visitor.visit_seq(&mut s)?;
                s.end()?;
                Ok(ret)
            }
            RawValueType::InlineTable(values) | RawValueType::DottedTable(values) => {
                visitor.visit_map(InlineTableDeserializer {
                    values: values.into_iter(),
                    next_value: None,
                })
            }
            RawValueType::RawTable(_) => panic!("raw table unexpected"),
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        if name == datetime::NAME && fields == &[datetime::FIELD] {
            if let RawValueType::Datetime(s) = self.value.parsed {
                return visitor.visit_map(DatetimeDeserializer {
                    date: s,
                    visited: false,
                });
            }
        }

        if self.validate_struct_keys {
            match &self.value.parsed {
                &RawValueType::InlineTable(ref values) | &RawValueType::DottedTable(ref values) => {
                    let extra_fields = values
                        .iter()
                        .filter_map(|key_value| {
                            let (ref key, ref _val) = *key_value;
                            if !fields.contains(&&(**key)) {
                                Some(key.clone())
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<Cow<'de, str>>>();

                    if !extra_fields.is_empty() {
                        return Err(Error::from_kind(ErrorKind::UnexpectedKeys {
                            keys: extra_fields
                                .iter()
                                .map(|k| k.to_string())
                                .collect::<Vec<_>>(),
                            available: fields,
                        }));
                    }
                }
                _ => {}
            }
        }

        if name == spanned::NAME && fields == &[spanned::START, spanned::END, spanned::VALUE] {
            let start = self.value.start;
            let end = self.value.end;

            return visitor.visit_map(SpannedDeserializer {
                start: Some(start),
                value: Some(self.value),
                end: Some(end),
            });
        }

        self.deserialize_any(visitor)
    }

    // `None` is interpreted as a missing field so be sure to implement `Some`
    // as a present field.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value.parsed {
            RawValueType::String(val) => visitor.visit_enum(val.into_deserializer()),
            RawValueType::InlineTable(values) | RawValueType::DottedTable(values) => {
                if values.len() != 1 {
                    Err(Error::from_kind(ErrorKind::Wanted {
                        expected: "exactly 1 element",
                        found: if values.is_empty() {
                            "zero elements"
                        } else {
                            "more than 1 element"
                        },
                    }))
                } else {
                    visitor.visit_enum(InlineTableDeserializer {
                        values: values.into_iter(),
                        next_value: None,
                    })
                }
            }
            e @ _ => Err(Error::from_kind(ErrorKind::Wanted {
                expected: "string or inline table",
                found: e.type_name(),
            })),
        }
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map unit identifier
        ignored_any unit_struct tuple_struct tuple
    }
}

impl<'de> de::IntoDeserializer<'de, Error> for RawValue<'de> {
    type Deserializer = ValueDeserializer<'de>;

    fn into_deserializer(self) -> Self::Deserializer {
        ValueDeserializer::new(self)
    }
}

struct SpannedDeserializer<'a> {
    start: Option<usize>,
    end: Option<usize>,
    value: Option<RawValue<'a>>,
}

impl<'de> de::MapAccess<'de> for SpannedDeserializer<'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.start.is_some() {
            seed.deserialize(BorrowedStrDeserializer::new(spanned::START))
                .map(Some)
        } else if self.end.is_some() {
            seed.deserialize(BorrowedStrDeserializer::new(spanned::END))
                .map(Some)
        } else if self.value.is_some() {
            seed.deserialize(BorrowedStrDeserializer::new(spanned::VALUE))
                .map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        if let Some(start) = self.start.take() {
            seed.deserialize(start.into_deserializer())
        } else if let Some(end) = self.end.take() {
            seed.deserialize(end.into_deserializer())
        } else if let Some(value) = self.value.take() {
            seed.deserialize(value.into_deserializer())
        } else {
            panic!("next_value_seed called before next_key_seed")
        }
    }
}

struct DatetimeDeserializer<'a> {
    visited: bool,
    date: &'a str,
}

impl<'de> de::MapAccess<'de> for DatetimeDeserializer<'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.visited {
            return Ok(None);
        }
        self.visited = true;
        seed.deserialize(DatetimeFieldDeserializer).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(StrDeserializer::new(self.date.into()))
    }
}

struct DatetimeFieldDeserializer;

impl<'de> de::Deserializer<'de> for DatetimeFieldDeserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(datetime::FIELD)
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map struct option unit newtype_struct
        ignored_any unit_struct tuple_struct tuple enum identifier
    }
}

struct InlineTableDeserializer<'a> {
    values: vec::IntoIter<(Cow<'a, str>, RawValue<'a>)>,
    next_value: Option<RawValue<'a>>,
}

impl<'de> de::MapAccess<'de> for InlineTableDeserializer<'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        let (key, value) = match self.values.next() {
            Some(pair) => pair,
            None => return Ok(None),
        };
        self.next_value = Some(value);
        seed.deserialize(StrDeserializer::new(key)).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let value = self.next_value.take().expect("Unable to read table values");
        seed.deserialize(ValueDeserializer::new(value))
    }
}

impl<'de> de::EnumAccess<'de> for InlineTableDeserializer<'de> {
    type Error = Error;
    type Variant = TableEnumDeserializer<'de>;

    fn variant_seed<V>(mut self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let (key, value) = match self.values.next() {
            Some(pair) => pair,
            None => {
                return Err(Error::from_kind(ErrorKind::Wanted {
                    expected: "table with exactly 1 entry",
                    found: "empty table",
                }))
            }
        };

        seed.deserialize(StrDeserializer::new(key))
            .map(|val| (val, TableEnumDeserializer { value: value }))
    }
}

/// Deserializes table values into enum variants.
struct TableEnumDeserializer<'a> {
    value: RawValue<'a>,
}

impl<'de> de::VariantAccess<'de> for TableEnumDeserializer<'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self.value.parsed {
            RawValueType::InlineTable(values) | RawValueType::DottedTable(values) => {
                if values.len() == 0 {
                    Ok(())
                } else {
                    Err(Error::from_kind(ErrorKind::ExpectedEmptyTable))
                }
            }
            e @ _ => Err(Error::from_kind(ErrorKind::Wanted {
                expected: "table",
                found: e.type_name(),
            })),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer::new(self.value))
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value.parsed {
            RawValueType::InlineTable(values) | RawValueType::DottedTable(values) => {
                let tuple_values = values
                    .into_iter()
                    .enumerate()
                    .map(|(index, (key, value))| match key.parse::<usize>() {
                        Ok(key_index) if key_index == index => Ok(value),
                        Ok(_) | Err(_) => Err(Error::from_kind(ErrorKind::ExpectedTupleIndex {
                            expected: index,
                            found: key.to_string(),
                        })),
                    })
                    // Fold all values into a `Vec`, or return the first error.
                    .fold(Ok(Vec::with_capacity(len)), |result, value_result| {
                        result.and_then(move |mut tuple_values| match value_result {
                            Ok(value) => {
                                tuple_values.push(value);
                                Ok(tuple_values)
                            }
                            // `Result<de::Value, Self::Error>` to `Result<Vec<_>, Self::Error>`
                            Err(e) => Err(e),
                        })
                    })?;

                if tuple_values.len() == len {
                    de::Deserializer::deserialize_seq(
                        ValueDeserializer::new(RawValue {
                            parsed: RawValueType::Array(tuple_values),
                            ..self.value
                        }),
                        visitor,
                    )
                } else {
                    Err(Error::from_kind(ErrorKind::ExpectedTuple(len)))
                }
            }
            e @ _ => Err(Error::from_kind(ErrorKind::Wanted {
                expected: "table",
                found: e.type_name(),
            })),
        }
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(
            ValueDeserializer::new(self.value).with_struct_key_validation(),
            "", // TODO: this should be the variant name
            fields,
            visitor,
        )
    }
}

impl<'a> Deserializer<'a> {
    /// Creates a new deserializer which will be deserializing the string
    /// provided.
    pub fn new(input: &'a str) -> Deserializer<'a> {
        Deserializer {
            tokens: Tokenizer::new(input),
            input: input,
            require_newline_after_table: true,
            allow_duplciate_after_longer_table: false,
        }
    }

    /// The `Deserializer::end` method should be called after a value has been
    /// fully deserialized.  This allows the `Deserializer` to validate that the
    /// input stream is at the end or that it only has trailing
    /// whitespace/comments.
    pub fn end(&mut self) -> Result<(), Error> {
        Ok(())
    }

    /// Historical versions of toml-rs accidentally allowed a newline after a
    /// table definition, but the TOML spec requires a newline after a table
    /// definition header.
    ///
    /// This option can be set to `false` (the default is `true`) to emulate
    /// this behavior for backwards compatibility with older toml-rs versions.
    pub fn set_require_newline_after_table(&mut self, require: bool) {
        self.require_newline_after_table = require;
    }

    /// Historical versions of toml-rs accidentally allowed a duplicate table
    /// header after a longer table header was previously defined. This is
    /// invalid according to the TOML spec, however.
    ///
    /// This option can be set to `true` (the default is `false`) to emulate
    /// this behavior for backwards compatibility with older toml-rs versions.
    pub fn set_allow_duplicate_after_longer_table(&mut self, allow: bool) {
        self.allow_duplciate_after_longer_table = allow;
    }

    /// Parse everything.
    fn tables(&mut self) -> Result<Vec<Table<'a>>, Error> {
        let mut tables = Vec::new();
        let mut cur_table = Table {
            at: 0,
            header: Vec::new(),
            values: None,
            array: false,
        };

        while let Some(line) = self.line()? {
            match line {
                Line::Header {
                    at,
                    key,
                    array,
                    ..
                } => {
                    if !cur_table.header.is_empty() || cur_table.values.is_some() {
                        tables.push(cur_table);
                    }
                    cur_table = Table {
                        at: at,
                        header: key.parts,
                        values: Some(Vec::new()),
                        array: array,
                    };
                }
                Line::KeyValue{key, mut value} => {
                    if cur_table.values.is_none() {
                        cur_table.values = Some(Vec::new());
                    }
                    self.flatten_inline_tables(&mut value)?;
                    self.add_dotted_key(key.parts, value, cur_table.values.as_mut().unwrap())?;
                }
                Line::Whitespace(_) => {}
                Line::Comment{..} => {}
                Line::Bom => {}
            }
        }
        if !cur_table.header.is_empty() || cur_table.values.is_some() {
            tables.push(cur_table);
        }
        Ok(tables)
    }

    pub(crate) fn line(&mut self) -> Result<Option<Line<'a>>, Error> {
        let start = self.tokens.current();
        let indent = self.eat_whitespace()?;
        // TODO: Peek here seems a little inefficient.
        // TODO: capture line-ending style
        match self.peek()? {
            Some((_, Token::Newline)) => {
                self.next()?;  // Skip newline.
                Ok(Some(Line::Whitespace(&self.tokens.input()[start..self.tokens.current()])))
            }
            Some((_, Token::Comment(_))) => {
                self.next()?;  // Skip comment.
                self.expect_newline_or_eof()?;
                Ok(Some(Line::Comment(&self.tokens.input()[start..self.tokens.current()])))
            }
            Some((_, Token::LeftBracket)) => self.table_header(indent).map(Some),
            Some((_, Token::Bom)) => {
                self.next()?;  // Skip bom.
                Ok(Some(Line::Bom))
            }
            Some(_) => self.key_value(indent).map(Some),
            None => Ok(None),
        }
    }

    fn table_header(&mut self, indent: &'a str) -> Result<Line<'a>, Error> {
        let header_start = self.tokens.current();
        self.expect(Token::LeftBracket)?;
        let array = self.eat(Token::LeftBracket)?;
        let raw_key = self.dotted_key(None)?;
        self.expect(Token::RightBracket)?;
        if array {
            self.expect(Token::RightBracket)?;
        }
        let post_start = self.tokens.current();
        self.eat_whitespace()?;
        if !self.eat_comment()? && self.require_newline_after_table {
            self.expect_newline_or_eof()?;
        }
        let posttext = &self.tokens.input()[post_start..self.tokens.current()];
        Ok(Line::Header {
            indent: indent,
            at: header_start,
            array: array,
            key: raw_key,
            header_posttext: posttext,
        })
    }

    fn key_value(&mut self, indent: &'a str) -> Result<Line<'a>, Error> {
        // key [ws] `=` [ws] val [ws] [comment] newline-or-eof
        let raw_key = self.dotted_key(Some(indent))?;
        self.expect(Token::Equals)?;
        let value = self.value()?;
        // Require newline or eof.
        if !value.has_trailing_newline() && !self.tokens.is_eof() {
            match self.peek()? {
                Some((Span{start, ..}, token)) => {
                    return Err(self.error(start, ErrorKind::Wanted {
                        expected: "newline",
                        found: token.describe()
                    }));
                }
                None => {}
            }
        }
        Ok(Line::KeyValue{key: raw_key, value: value})
    }

    pub(crate) fn value(&mut self) -> Result<RawValue<'a>, Error> {
        let pretext = self.eat_whitespace()?;
        let start = self.tokens.current();
        // TODO: avoid repetition here
        let value = match self.next()? {
            Some((Span { start, end }, Token::String { val, .. })) => {
                let text = &self.tokens.input()[start..end];
                let posttext = self.eat_posttext()?;
                RawValue {
                    pretext: pretext,
                    start: start,
                    end: end,
                    text: text,
                    posttext: posttext,
                    parsed: RawValueType::String(val)
                }
            }
            Some((Span { start, end }, Token::Keylike("true"))) => {
                let text = &self.tokens.input()[start..end];
                let posttext = self.eat_posttext()?;
                RawValue {
                    pretext: pretext,
                    start: start,
                    end: end,
                    text: text,
                    posttext: posttext,
                    parsed: RawValueType::Boolean(true)
                }
            }
            Some((Span { start, end }, Token::Keylike("false"))) => {
                let text = &self.tokens.input()[start..end];
                let posttext = self.eat_posttext()?;
                RawValue {
                    pretext: pretext,
                    start: start,
                    end: end,
                    text: text,
                    posttext: posttext,
                    parsed: RawValueType::Boolean(false)
                }
            }
            Some((span, Token::Keylike(key))) => self.number_or_date(pretext, span, key)?,
            Some((_span, Token::Plus)) => self.number_leading_plus(pretext, start)?,
            Some((Span { start, .. }, Token::LeftBrace)) => {
                let table = self.inline_table()?;
                let end = self.tokens.current();
                let text = &self.tokens.input()[start..end];
                let posttext = self.eat_posttext()?;
                RawValue {
                    pretext: pretext,
                    start: start,
                    end: end,
                    text: text,
                    posttext: posttext,
                    parsed: RawValueType::RawTable(table),
                }
            }
            Some((Span { start, .. }, Token::LeftBracket)) => {
                let (span, array) = self.array()?;
                let text = &self.tokens.input()[start..span.end];
                let posttext = self.eat_posttext()?;
                RawValue {
                    pretext: pretext,
                    start: start,
                    end: span.end,
                    text: text,
                    posttext: posttext,
                    parsed: RawValueType::Array(array),
                }
            }
            Some(token) => {
                return Err(self.error(
                    start,
                    ErrorKind::Wanted {
                        expected: "a value",
                        found: token.1.describe(),
                    },
                ))
            }
            None => return Err(self.eof()),
        };
        Ok(value)
    }

    fn number_or_date(&mut self, pretext: &'a str, span: Span, s: &'a str) -> Result<RawValue<'a>, Error> {
        // TODO: change to closure, passing in self?
        macro_rules! to_datetime {
            ($span:expr, $value:expr) => {
                // TODO: consider making this a method to avoid duplication in number()
                let text = &self.tokens.input()[$span.start..$span.end]; // TODO ..=?
                let posttext = self.eat_posttext()?;
                return Ok(RawValue {
                    pretext: pretext,
                    start: $span.start,
                    end: $span.end,
                    text: text,
                    posttext: posttext,
                    parsed: RawValueType::Datetime($value),
                })
            }
        }
        if s.contains('T') || (s.len() > 1 && s[1..].contains('-')) && !s.contains("e-") {
            let (span, s) = self.datetime(span, s, false)?;
            to_datetime!(span, s);
        } else if self.eat(Token::Colon)? {
            let (span, s) = self.datetime(span, s, true)?;
            to_datetime!(span, s);
        } else {
            self.number(pretext, span, s)
        }
    }

    /// Returns a string or table value type.
    ///
    /// Used to deserialize enums. Unit enums may be represented as a string or a table, all other
    /// structures (tuple, newtype, struct) must be represented as a table.
    fn string_or_table(&mut self) -> Result<RawValue<'a>, Error> {
        match self.peek()? {
            Some((_, Token::LeftBracket)) => {
                let tables = self.tables()?;
                if tables.len() != 1 {
                    return Err(Error::from_kind(ErrorKind::Wanted {
                        expected: "exactly 1 table",
                        found: if tables.is_empty() {
                            "zero tables"
                        } else {
                            "more than 1 table"
                        },
                    }));
                }

                let table = tables
                    .into_iter()
                    .next()
                    .expect("Expected exactly one table");
                let header = table
                    .header
                    .last()
                    .expect("Expected at least one header value for table.");

                let end = table
                    .values
                    .as_ref()
                    .and_then(|values| values.last())
                    .map(|&(_, ref val)| val.end)
                    .unwrap_or_else(|| header.len());
                let raw_value = RawValue {
                    pretext: "", // TODO
                    text: "",  // TODO
                    posttext: "",  // TODO
                    start: table.at,
                    end: end,
                    parsed: RawValueType::InlineTable(table.values.unwrap_or_else(Vec::new)),
                };
                let values = vec![(header.clone(), raw_value)];
                let raw_value_table = RawValue {
                    pretext: "", // TODO
                    text: "",  // TODO
                    posttext: "",  // TODO
                    start: table.at,
                    end: end,
                    parsed: RawValueType::InlineTable(values),
                };
                Ok(raw_value_table)
            }
            Some(_) => {
                let mut value = self.value()?;
                if let RawValueType::RawTable(_) = value.parsed {
                    self.flatten_inline_tables(&mut value)?;
                }
                Ok(value)
            }
            None => Err(self.eof()),
        }
    }

    fn number(&mut self, pretext: &'a str, span: Span, s: &'a str) -> Result<RawValue<'a>, Error> {
        let parsed = if s.starts_with("0x") {
            RawValueType::Integer(self.integer(&s[2..], 16)?)
        } else if s.starts_with("0o") {
            RawValueType::Integer(self.integer(&s[2..], 8)?)
        } else if s.starts_with("0b") {
            RawValueType::Integer(self.integer(&s[2..], 2)?)
        } else if s.contains('e') || s.contains('E') {
            RawValueType::Float(self.float(s, None)?)
        } else if self.eat(Token::Period)? {
            let at = self.tokens.current();
            match self.next()? {
                Some((_, Token::Keylike(after))) => {
                    RawValueType::Float(self.float(s, Some(after))?)
                }
                _ => return Err(self.error(at, ErrorKind::NumberInvalid)),
            }
        } else if s == "inf" {
            RawValueType::Float(f64::INFINITY)
        } else if s == "-inf" {
            RawValueType::Float(f64::NEG_INFINITY)
        } else if s == "nan" {
            RawValueType::Float(f64::NAN)
        } else if s == "-nan" {
            RawValueType::Float(-f64::NAN)
        } else {
            RawValueType::Integer(self.integer(s, 10)?)
        };
        let text = &self.tokens.input()[span.start..self.tokens.current()]; // TODO ..=?
        let posttext = self.eat_posttext()?;
        Ok(RawValue {
            pretext: pretext,
            start: span.start,
            end: span.end,
            text: text,
            posttext: posttext,
            parsed: parsed,
        })
    }

    fn number_leading_plus(&mut self, pretext: &'a str, start: usize) -> Result<RawValue<'a>, Error> {
        match self.next()? {
            Some((Span { end, .. }, Token::Keylike(s))) => self.number(
                pretext,
                Span{start: start, end: end},
                s
            ),
            _ => Err(self.error(start, ErrorKind::NumberInvalid)),
        }
    }

    fn integer(&self, s: &'a str, radix: u32) -> Result<i64, Error> {
        let allow_sign = radix == 10;
        let allow_leading_zeros = radix != 10;
        let (prefix, suffix) = self.parse_integer(s, allow_sign, allow_leading_zeros, radix)?;
        let start = self.tokens.substr_offset(s);
        if suffix != "" {
            return Err(self.error(start, ErrorKind::NumberInvalid));
        }
        i64::from_str_radix(&prefix.replace("_", "").trim_left_matches('+'), radix)
            .map_err(|_e| self.error(start, ErrorKind::NumberInvalid))
    }

    fn parse_integer(
        &self,
        s: &'a str,
        allow_sign: bool,
        allow_leading_zeros: bool,
        radix: u32,
    ) -> Result<(&'a str, &'a str), Error> {
        let start = self.tokens.substr_offset(s);

        let mut first = true;
        let mut first_zero = false;
        let mut underscore = false;
        let mut end = s.len();
        for (i, c) in s.char_indices() {
            let at = i + start;
            if i == 0 && (c == '+' || c == '-') && allow_sign {
                continue;
            }

            if c == '0' && first {
                first_zero = true;
            } else if c.to_digit(radix).is_some() {
                if !first && first_zero && !allow_leading_zeros {
                    return Err(self.error(at, ErrorKind::NumberInvalid));
                }
                underscore = false;
            } else if c == '_' && first {
                return Err(self.error(at, ErrorKind::NumberInvalid));
            } else if c == '_' && !underscore {
                underscore = true;
            } else {
                end = i;
                break;
            }
            first = false;
        }
        if first || underscore {
            return Err(self.error(start, ErrorKind::NumberInvalid));
        }
        Ok((&s[..end], &s[end..]))
    }

    fn float(&mut self, s: &'a str, after_decimal: Option<&'a str>) -> Result<f64, Error> {
        let (integral, mut suffix) = self.parse_integer(s, true, false, 10)?;
        let start = self.tokens.substr_offset(integral);

        let mut fraction = None;
        if let Some(after) = after_decimal {
            if suffix != "" {
                return Err(self.error(start, ErrorKind::NumberInvalid));
            }
            let (a, b) = self.parse_integer(after, false, true, 10)?;
            fraction = Some(a);
            suffix = b;
        }

        let mut exponent = None;
        if suffix.starts_with('e') || suffix.starts_with('E') {
            let (a, b) = if suffix.len() == 1 {
                self.eat(Token::Plus)?;
                match self.next()? {
                    Some((_, Token::Keylike(s))) => self.parse_integer(s, false, false, 10)?,
                    _ => return Err(self.error(start, ErrorKind::NumberInvalid)),
                }
            } else {
                self.parse_integer(&suffix[1..], true, false, 10)?
            };
            if b != "" {
                return Err(self.error(start, ErrorKind::NumberInvalid));
            }
            exponent = Some(a);
        }

        let mut number = integral
            .trim_left_matches('+')
            .chars()
            .filter(|c| *c != '_')
            .collect::<String>();
        if let Some(fraction) = fraction {
            number.push_str(".");
            number.extend(fraction.chars().filter(|c| *c != '_'));
        }
        if let Some(exponent) = exponent {
            number.push_str("E");
            number.extend(exponent.chars().filter(|c| *c != '_'));
        }
        number
            .parse()
            .map_err(|_e| self.error(start, ErrorKind::NumberInvalid))
            .and_then(|n: f64| {
                if n.is_finite() {
                    Ok(n)
                } else {
                    Err(self.error(start, ErrorKind::NumberInvalid))
                }
            })
    }

    fn datetime(
        &mut self,
        mut span: Span,
        date: &'a str,
        colon_eaten: bool,
    ) -> Result<(Span, &'a str), Error> {
        let start = self.tokens.substr_offset(date);

        // Check for space separated date and time.
        let mut lookahead = self.tokens.clone();
        if let Ok(Some((_, Token::Whitespace(" ")))) = lookahead.next() {
            // Check if hour follows.
            if let Ok(Some((_, Token::Keylike(_)))) = lookahead.next() {
                self.next()?; // skip space
                self.next()?; // skip keylike hour
            }
        }

        if colon_eaten || self.eat(Token::Colon)? {
            // minutes
            match self.next()? {
                Some((_, Token::Keylike(_))) => {}
                _ => return Err(self.error(start, ErrorKind::DateInvalid)),
            }
            // Seconds
            self.expect(Token::Colon)?;
            match self.next()? {
                Some((Span { end, .. }, Token::Keylike(_))) => {
                    span.end = end;
                }
                _ => return Err(self.error(start, ErrorKind::DateInvalid)),
            }
            // Fractional seconds
            if self.eat(Token::Period)? {
                match self.next()? {
                    Some((Span { end, .. }, Token::Keylike(_))) => {
                        span.end = end;
                    }
                    _ => return Err(self.error(start, ErrorKind::DateInvalid)),
                }
            }

            // offset
            if self.eat(Token::Plus)? {
                match self.next()? {
                    Some((Span { end, .. }, Token::Keylike(_))) => {
                        span.end = end;
                    }
                    _ => return Err(self.error(start, ErrorKind::DateInvalid)),
                }
            }
            if self.eat(Token::Colon)? {
                match self.next()? {
                    Some((Span { end, .. }, Token::Keylike(_))) => {
                        span.end = end;
                    }
                    _ => return Err(self.error(start, ErrorKind::DateInvalid)),
                }
            }
        }

        let end = self.tokens.current();
        Ok((span, &self.tokens.input()[start..end]))
    }

    // TODO(#140): shouldn't buffer up this entire table in memory, it'd be
    // great to defer parsing everything until later.
    fn inline_table(&mut self) -> Result<Vec<(RawKey<'a>, RawValue<'a>)>, Error> {
        let mut ret = Vec::new();
        self.eat_whitespace()?;
        if self.eat(Token::RightBrace)? {
            return Ok(ret);
        }
        loop {
            let raw_key = self.dotted_key(None)?;
            self.expect(Token::Equals)?;
            let raw_value = self.value()?;
            if raw_value.has_trailing_newline() {
                return Err(self.error(self.tokens.current()-1, ErrorKind::NewlineInInlineTable));
            }
            ret.push((raw_key, raw_value));
            if self.eat(Token::RightBrace)? {
                return Ok(ret);
            }
            self.expect(Token::Comma)?;
        }
    }

    // TODO(#140): shouldn't buffer up this entire array in memory, it'd be
    // great to defer parsing everything until later.
    fn array(&mut self) -> Result<(Span, Vec<RawValue<'a>>), Error> {
        let mut ret = Vec::new();

        let intermediate = |me: &mut Deserializer| {
            loop {
                me.eat_whitespace()?;
                if !me.eat(Token::Newline)? && !me.eat_comment()? {
                    break;
                }
            }
            Ok(())
        };

        loop {
            intermediate(self)?;
            if let Some(span) = self.eat_spanned(Token::RightBracket)? {
                return Ok((span, ret));
            }
            let at = self.tokens.current();
            let value = self.value()?;
            if let Some(last) = ret.last() {
                if !value.same_type(last) {
                    return Err(self.error(at, ErrorKind::MixedArrayType));
                }
            }
            ret.push(value);
            intermediate(self)?;
            if !self.eat(Token::Comma)? {
                break;
            }
        }
        intermediate(self)?;
        let span = self.expect_spanned(Token::RightBracket)?;
        Ok((span, ret))
    }

    fn table_key(&mut self) -> Result<Cow<'a, str>, Error> {
        self.tokens
            .table_key()
            .map(|t| t.1)
            .map_err(|e| self.token_error(e))
    }

    fn dotted_key(&mut self, indent: Option<&'a str>) -> Result<RawKey<'a>, Error> {
        let mut parts = Vec::new();
        let pretext = if let Some(indent) = indent {
            indent
        } else {
            self.eat_whitespace()?
        };
        let start = self.tokens.current();
        parts.push(self.table_key()?);
        let mut end = self.tokens.current();
        let mut posttext = self.eat_whitespace()?;
        while self.eat(Token::Period)? {
            self.eat_whitespace()?;
            parts.push(self.table_key()?);
            end = self.tokens.current();
            posttext = self.eat_whitespace()?;
        }
        let text = &self.tokens.input()[start..end];
        Ok(RawKey {
            pretext: pretext,
            text: text,
            parts: parts,
            posttext: posttext,
        })
    }

    /// Stores a value in the appropriate hierarchical structure positioned based on the dotted key.
    ///
    /// Given the following definition: `multi.part.key = "value"`, `multi` and `part` are
    /// intermediate parts which are mapped to the relevant fields in the deserialized type's data
    /// hierarchy.
    ///
    /// # Parameters
    ///
    /// * `key_parts`: Each segment of the dotted key, e.g. `part.one` maps to
    ///                `vec![Cow::Borrowed("part"), Cow::Borrowed("one")].` FIXME
    /// * `value`: The parsed value.
    /// * `values`: The `Vec` to store the value in.
    fn add_dotted_key(
        &self,
        mut key_parts: Vec<Cow<'a, str>>,
        value: RawValue<'a>,
        values: &mut Vec<(Cow<'a, str>, RawValue<'a>)>,
    ) -> Result<(), Error> {
        let key = key_parts.remove(0);
        if key_parts.is_empty() {
            values.push((key, value));
            return Ok(());
        }
        match values.iter_mut().find(|&&mut (ref k, _)| *k == key) {
            Some(&mut (
                _,
                RawValue {
                    parsed: RawValueType::DottedTable(ref mut v),
                    ..
                },
            )) => {
                return self.add_dotted_key(key_parts, value, v);
            }
            Some(&mut (_, RawValue { start, .. })) => {
                return Err(self.error(start, ErrorKind::DottedKeyInvalidType));
            }
            None => {}
        }
        // The start/end value is somewhat misleading here.
        let table_values = RawValue {
            // TODO
            // pretext: &'a str,
            // text: &'a str,
            // posttext: &'a str,
            // start: value.start,
            // end: value.end,
            parsed: RawValueType::DottedTable(Vec::new()),
            ..value
        };
        values.push((key, table_values));
        let last_i = values.len() - 1;
        if let (
            _,
            RawValue {
                parsed: RawValueType::DottedTable(ref mut v),
                ..
            },
        ) = values[last_i]
        {
            self.add_dotted_key(key_parts, value, v)?;
        }
        Ok(())
    }

    fn flatten_inline_tables(&self, value: &mut RawValue<'a>) -> Result<(), Error> {
        match value.parsed {
            RawValueType::RawTable(_) => {
                let items = Vec::new();
                let new_parsed = RawValueType::InlineTable(items);
                let old_parsed = std::mem::replace(&mut value.parsed, new_parsed);
                if let RawValueType::RawTable(old_items) = old_parsed {
                    if let RawValueType::InlineTable(new_items) = &mut value.parsed {
                        for (old_key, mut old_value) in old_items {
                            self.flatten_inline_tables(&mut old_value)?;
                            self.add_dotted_key(old_key.parts, old_value, new_items)?;
                        }
                    }
                }
            }
            RawValueType::InlineTable(ref mut items) | RawValueType::DottedTable(ref mut items) => {
                // TODO: Is this necessary?
                for (_, ref mut v) in items {
                    self.flatten_inline_tables(v)?;
                }
            }
            RawValueType::Array(ref mut items) => {
                for mut item in items {
                    self.flatten_inline_tables(&mut item)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn eat_whitespace(&mut self) -> Result<&'a str, Error> {
        self.tokens
            .eat_whitespace()
            .map_err(|e| self.token_error(e))
    }

    fn eat_comment(&mut self) -> Result<bool, Error> {
        self.tokens.eat_comment().map_err(|e| self.token_error(e))
    }

    fn eat_newline(&mut self) -> Result<bool, Error> {
        self.tokens.eat_newline().map_err(|e| self.token_error(e))
    }

    fn expect_newline_or_eof(&mut self) -> Result<(), Error> {
        self.tokens
            .expect_newline_or_eof()
            .map_err(|e| self.token_error(e))
    }

    fn eat_posttext(&mut self) -> Result<&'a str, Error> {
        let start = self.tokens.current();
        self.eat_whitespace()?;
        if !self.eat_comment()? {
            self.eat_newline()?;
        }
        Ok(&self.tokens.input()[start..self.tokens.current()])
    }

    fn eat(&mut self, expected: Token<'a>) -> Result<bool, Error> {
        self.tokens.eat(expected).map_err(|e| self.token_error(e))
    }

    fn eat_spanned(&mut self, expected: Token<'a>) -> Result<Option<Span>, Error> {
        self.tokens
            .eat_spanned(expected)
            .map_err(|e| self.token_error(e))
    }

    fn expect(&mut self, expected: Token<'a>) -> Result<(), Error> {
        self.tokens
            .expect(expected)
            .map_err(|e| self.token_error(e))
    }

    fn expect_spanned(&mut self, expected: Token<'a>) -> Result<Span, Error> {
        self.tokens
            .expect_spanned(expected)
            .map_err(|e| self.token_error(e))
    }

    fn next(&mut self) -> Result<Option<(Span, Token<'a>)>, Error> {
        self.tokens.next().map_err(|e| self.token_error(e))
    }

    fn peek(&mut self) -> Result<Option<(Span, Token<'a>)>, Error> {
        self.tokens.peek().map_err(|e| self.token_error(e))
    }

    fn eof(&self) -> Error {
        self.error(self.input.len(), ErrorKind::UnexpectedEof)
    }

    fn token_error(&self, error: TokenError) -> Error {
        match error {
            TokenError::InvalidCharInString(at, ch) => {
                self.error(at, ErrorKind::InvalidCharInString(ch))
            }
            TokenError::InvalidEscape(at, ch) => self.error(at, ErrorKind::InvalidEscape(ch)),
            TokenError::InvalidEscapeValue(at, v) => {
                self.error(at, ErrorKind::InvalidEscapeValue(v))
            }
            TokenError::InvalidHexEscape(at, ch) => self.error(at, ErrorKind::InvalidHexEscape(ch)),
            TokenError::NewlineInString(at) => self.error(at, ErrorKind::NewlineInString),
            TokenError::Unexpected(at, ch) => self.error(at, ErrorKind::Unexpected(ch)),
            TokenError::UnterminatedString(at) => self.error(at, ErrorKind::UnterminatedString),
            TokenError::NewlineInTableKey(at) => self.error(at, ErrorKind::NewlineInTableKey),
            TokenError::Wanted {
                at,
                expected,
                found,
            } => self.error(
                at,
                ErrorKind::Wanted {
                    expected: expected,
                    found: found,
                },
            ),
            TokenError::EmptyTableKey(at) => self.error(at, ErrorKind::EmptyTableKey),
            TokenError::MultilineStringKey(at) => self.error(at, ErrorKind::MultilineStringKey),
        }
    }

    fn error(&self, at: usize, kind: ErrorKind) -> Error {
        let mut err = Error::from_kind(kind);
        let (line, col) = self.to_linecol(at);
        err.inner.line = Some(line);
        err.inner.col = col;
        err
    }

    /// Converts a byte offset from an error message to a (line, column) pair
    ///
    /// All indexes are 0-based.
    fn to_linecol(&self, offset: usize) -> (usize, usize) {
        let mut cur = 0;
        for (i, line) in self.input.lines().enumerate() {
            if cur + line.len() + 1 > offset {
                return (i, offset - cur);
            }
            cur += line.len() + 1;
        }
        (self.input.lines().count(), 0)
    }
}

impl Error {
    /// Produces a (line, column) pair of the position of the error if available
    ///
    /// All indexes are 0-based.
    pub fn line_col(&self) -> Option<(usize, usize)> {
        self.inner.line.map(|line| (line, self.inner.col))
    }

    pub(crate) fn from_kind(kind: ErrorKind) -> Error {
        Error {
            inner: Box::new(ErrorInner {
                kind: kind,
                line: None,
                col: 0,
                message: String::new(),
                key: Vec::new(),
            }),
        }
    }

    pub(crate) fn custom(s: String) -> Error {
        Error {
            inner: Box::new(ErrorInner {
                kind: ErrorKind::Custom,
                line: None,
                col: 0,
                message: s,
                key: Vec::new(),
            }),
        }
    }

    /// Do not call this method, it may be removed at any time, it's just an
    /// internal implementation detail.
    #[doc(hidden)]
    pub fn add_key_context(&mut self, key: &str) {
        self.inner.key.insert(0, key.to_string());
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner.kind {
            ErrorKind::UnexpectedEof => "unexpected eof encountered".fmt(f)?,
            ErrorKind::InvalidCharInString(c) => write!(
                f,
                "invalid character in string: `{}`",
                c.escape_default().collect::<String>()
            )?,
            ErrorKind::InvalidEscape(c) => write!(
                f,
                "invalid escape character in string: `{}`",
                c.escape_default().collect::<String>()
            )?,
            ErrorKind::InvalidHexEscape(c) => write!(
                f,
                "invalid hex escape character in string: `{}`",
                c.escape_default().collect::<String>()
            )?,
            ErrorKind::InvalidEscapeValue(c) => write!(f, "invalid escape value: `{}`", c)?,
            ErrorKind::NewlineInString => "newline in string found".fmt(f)?,
            ErrorKind::Unexpected(ch) => write!(
                f,
                "unexpected character found: `{}`",
                ch.escape_default().collect::<String>()
            )?,
            ErrorKind::UnterminatedString => "unterminated string".fmt(f)?,
            ErrorKind::NewlineInTableKey => "found newline in table key".fmt(f)?,
            ErrorKind::NewlineInInlineTable => "found newline in inline table".fmt(f)?,
            ErrorKind::Wanted { expected, found } => {
                write!(f, "expected {}, found {}", expected, found)?
            }
            ErrorKind::NumberInvalid => "invalid number".fmt(f)?,
            ErrorKind::DateInvalid => "invalid date".fmt(f)?,
            ErrorKind::MixedArrayType => "mixed types in an array".fmt(f)?,
            ErrorKind::DuplicateTable(ref s) => {
                write!(f, "redefinition of table `{}`", s)?;
            }
            ErrorKind::DuplicateKey(ref s) => {
                write!(f, "duplicate key `{}`", s)?;
            }
            ErrorKind::RedefineAsArray => "table redefined as array".fmt(f)?,
            ErrorKind::EmptyTableKey => "empty table key found".fmt(f)?,
            ErrorKind::MultilineStringKey => "multiline strings are not allowed for key".fmt(f)?,
            ErrorKind::Custom => self.inner.message.fmt(f)?,
            ErrorKind::ExpectedTuple(l) => write!(f, "expected table with length {}", l)?,
            ErrorKind::ExpectedTupleIndex {
                expected,
                ref found,
            } => write!(f, "expected table key `{}`, but was `{}`", expected, found)?,
            ErrorKind::ExpectedEmptyTable => "expected empty table".fmt(f)?,
            ErrorKind::DottedKeyInvalidType => {
                "dotted key attempted to extend non-table type".fmt(f)?
            }
            ErrorKind::UnexpectedKeys {
                ref keys,
                available,
            } => write!(
                f,
                "unexpected keys in table: `{:?}`, available keys: `{:?}`",
                keys, available
            )?,
            ErrorKind::__Nonexhaustive => panic!(),
        }

        if !self.inner.key.is_empty() {
            write!(f, " for key `")?;
            for (i, k) in self.inner.key.iter().enumerate() {
                if i > 0 {
                    write!(f, ".")?;
                }
                write!(f, "{}", k)?;
            }
            write!(f, "`")?;
        }

        if let Some(line) = self.inner.line {
            write!(f, " at line {}", line + 1)?;
        }

        Ok(())
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self.inner.kind {
            ErrorKind::UnexpectedEof => "unexpected eof encountered",
            ErrorKind::InvalidCharInString(_) => "invalid char in string",
            ErrorKind::InvalidEscape(_) => "invalid escape in string",
            ErrorKind::InvalidHexEscape(_) => "invalid hex escape in string",
            ErrorKind::InvalidEscapeValue(_) => "invalid escape value in string",
            ErrorKind::NewlineInString => "newline in string found",
            ErrorKind::Unexpected(_) => "unexpected or invalid character",
            ErrorKind::UnterminatedString => "unterminated string",
            ErrorKind::NewlineInTableKey => "found newline in table key",
            ErrorKind::NewlineInInlineTable => "found newline in inline table",
            ErrorKind::Wanted { .. } => "expected a token but found another",
            ErrorKind::NumberInvalid => "invalid number",
            ErrorKind::DateInvalid => "invalid date",
            ErrorKind::MixedArrayType => "mixed types in an array",
            ErrorKind::DuplicateTable(_) => "duplicate table",
            ErrorKind::DuplicateKey(_) => "duplicate key",
            ErrorKind::RedefineAsArray => "table redefined as array",
            ErrorKind::EmptyTableKey => "empty table key found",
            ErrorKind::MultilineStringKey => "invalid multiline string for key",
            ErrorKind::Custom => "a custom error",
            ErrorKind::ExpectedTuple(_) => "expected table length",
            ErrorKind::ExpectedTupleIndex { .. } => "expected table key",
            ErrorKind::ExpectedEmptyTable => "expected empty table",
            ErrorKind::DottedKeyInvalidType => "dotted key invalid type",
            ErrorKind::UnexpectedKeys { .. } => "unexpected keys in table",
            ErrorKind::__Nonexhaustive => panic!(),
        }
    }
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error::custom(msg.to_string())
    }
}

pub(crate) enum Line<'a> {
    Header {
        /// Whitespace before opening bracket.
        indent: &'a str,
        /// Position where opening bracket starts.
        at: usize,
        /// True if array table.
        array: bool,
        /// Name information, including any whitespace inside the brackets.
        key: RawKey<'a>,
        /// Whitespace/comment following the closing bracket.
        header_posttext: &'a str,
    },
    KeyValue {
        key: RawKey<'a>,
        value: RawValue<'a>,
    },
    Whitespace(&'a str),
    Comment(&'a str),
    /// Byte-order mark
    Bom,
}

#[derive(Debug)]
pub(crate) struct RawKey<'a> {
    /// Whitespace in front of the key.
    pub(crate) pretext: &'a str,
    /// Original text of the key.
    pub(crate) text: &'a str,
    /// Parsed parts of the key, split on dots.
    pub(crate) parts: Vec<Cow<'a, str>>,
    /// Whitespace after the key.
    pub(crate) posttext: &'a str,
}

#[derive(Debug)]
pub(crate) struct RawValue<'a> {
    /// Whitespace in front of the value.
    pub(crate) pretext: &'a str,
    /// Original text of the value.
    pub(crate) text: &'a str,
    /// Whitespace/comment following the value.
    pub(crate) posttext: &'a str,
    /// Offset where value starts.
    pub(crate) start: usize,
    /// Offset immediately after the value.
    pub(crate) end: usize,
    /// Parsed value.
    pub(crate) parsed: RawValueType<'a>,
}

#[derive(Debug)]
pub(crate) enum RawValueType<'a> {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(Cow<'a, str>),
    Datetime(&'a str),
    Array(Vec<RawValue<'a>>),
    /// Inline table `{key = value, ...}`, containing dotted keys.
    /// This is only exposed in `Deserializer::line`. Other interfaces such as
    /// `Deserializer::tables` will convert these to `InlineTable` by
    /// flattening them.
    RawTable(Vec<(RawKey<'a>, RawValue<'a>)>),
    /// Inline table, after dotted keys have been flattened out.
    InlineTable(Vec<(Cow<'a, str>, RawValue<'a>)>),
    /// Intermediate inline table created by a dotted key.
    /// This is used so that the parser can reject inline tables being mixed
    /// with dotted keys. If it is decided that restriction is not necessary,
    /// then this can be removed.
    DottedTable(Vec<(Cow<'a, str>, RawValue<'a>)>),
}

impl<'a> RawValueType<'a> {
    fn type_name(&self) -> &'static str {
        match *self {
            RawValueType::String(..) => "string",
            RawValueType::Integer(..) => "integer",
            RawValueType::Float(..) => "float",
            RawValueType::Boolean(..) => "boolean",
            RawValueType::Datetime(..) => "datetime",
            RawValueType::Array(..) => "array",
            RawValueType::RawTable(..) => "raw table",
            RawValueType::InlineTable(..) => "inline table",
            RawValueType::DottedTable(..) => "dotted table",
        }
    }
}

impl<'a> RawValue<'a> {
    fn same_type(&self, other: &RawValue<'a>) -> bool {
        match (&self.parsed, &other.parsed) {
            (&RawValueType::String(..), &RawValueType::String(..))
            | (&RawValueType::Integer(..), &RawValueType::Integer(..))
            | (&RawValueType::Float(..), &RawValueType::Float(..))
            | (&RawValueType::Boolean(..), &RawValueType::Boolean(..))
            | (&RawValueType::Datetime(..), &RawValueType::Datetime(..))
            | (&RawValueType::Array(..), &RawValueType::Array(..))
            | (&RawValueType::RawTable(..), &RawValueType::RawTable(..))
            | (&RawValueType::InlineTable(..), &RawValueType::InlineTable(..))
            | (&RawValueType::DottedTable(..), &RawValueType::DottedTable(..)) => true,

            _ => false,
        }
    }

    fn has_trailing_newline(&self) -> bool {
        let bytes = self.posttext.as_bytes();
        !bytes.is_empty() && bytes[bytes.len()-1] == b'\n'
    }
}
