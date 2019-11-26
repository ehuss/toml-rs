//! Deserializing TOML into Rust structures.
//!
//! This module contains all the Serde support for deserializing TOML documents
//! into Rust structures. Note that some top-level functions here are also
//! provided at the top of the crate.

use std::borrow::Cow;
use std::collections::HashMap;
use std::error;
use std::f64;
use std::fmt;
use std::iter;
use std::marker::PhantomData;
use std::str;
use std::vec;

use serde::de;
use serde::de::value::BorrowedStrDeserializer;
use serde::de::IntoDeserializer;

use crate::datetime;
use crate::spanned;
use crate::tokens::{Error as TokenError, Span, Token, Tokenizer};

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
        Err(e) => Err(Error::custom(None, e.to_string())),
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
/// use serde_derive::Deserialize;
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    inner: Box<ErrorInner>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ErrorInner {
    kind: ErrorKind,
    line: Option<usize>,
    col: usize,
    at: Option<usize>,
    message: String,
    key: Vec<String>,
}

/// Errors that can occur when deserializing a type.
#[derive(Debug, PartialEq, Eq, Clone)]
enum ErrorKind {
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

    /// A duplicate table definition was found.
    DuplicateTable(String),

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
        let table_indices = build_table_indices(&tables);
        let table_pindices = build_table_pindices(&tables);

        let res = visitor.visit_map(MapVisitor {
            values: Vec::new().into_iter().peekable(),
            next_value: None,
            depth: 0,
            cur: 0,
            cur_parent: 0,
            max: tables.len(),
            table_indices: &table_indices,
            table_pindices: &table_pindices,
            tables: &mut tables,
            array: false,
            de: self,
        });
        res.map_err(|mut err| {
            // Errors originating from this library (toml), have an offset
            // attached to them already. Other errors, like those originating
            // from serde (like "missing field") or from a custom deserializer,
            // do not have offsets on them. Here, we do a best guess at their
            // location, by attributing them to the "current table" (the last
            // item in `tables`).
            err.fix_offset(|| tables.last().map(|table| table.at));
            err.fix_linecol(|at| self.to_linecol(at));
            err
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
        let (value, name) = self.string_or_table()?;
        match value.parsed {
            RawValueType::String(val) => visitor.visit_enum(val.into_deserializer()),
            RawValueType::InlineTable(values) => visitor.visit_enum(InlineTableDeserializer {
                values: values.0.into_iter(),
                next_value: None,
            }),
            RawValueType::DottedTable(_) => visitor.visit_enum(DottedTableDeserializer {
                name: name.expect("Expected table header to be passed."),
                value,
            }),
            e => Err(Error::from_kind(
                Some(value.span.start),
                ErrorKind::Wanted {
                    expected: "string or table",
                    found: e.type_name(),
                },
            )),
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
        if name == spanned::NAME && fields == [spanned::START, spanned::END, spanned::VALUE] {
            let start = 0;
            let end = self.input.len();

            let res = visitor.visit_map(SpannedDeserializer {
                phantom_data: PhantomData,
                start: Some(start),
                value: Some(self),
                end: Some(end),
            });
            return res;
        }

        self.deserialize_any(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map unit newtype_struct
        ignored_any unit_struct tuple_struct tuple option identifier
    }
}

// Builds a datastructure that allows for efficient sublinear lookups.
// The returned HashMap contains a mapping from table header (like [a.b.c])
// to list of tables with that precise name. The tables are being identified
// by their index in the passed slice. We use a list as the implementation
// uses this data structure for arrays as well as tables,
// so if any top level [[name]] array contains multiple entries,
// there are multiple entires in the list.
// The lookup is performed in the `SeqAccess` implementation of `MapVisitor`.
// The lists are ordered, which we exploit in the search code by using
// bisection.
fn build_table_indices<'de>(tables: &[Table<'de>]) -> HashMap<Vec<Cow<'de, str>>, Vec<usize>> {
    let mut res = HashMap::new();
    for (i, table) in tables.iter().enumerate() {
        let header = table.header.iter().map(|v| v.1.clone()).collect::<Vec<_>>();
        res.entry(header).or_insert(Vec::new()).push(i);
    }
    res
}

// Builds a datastructure that allows for efficient sublinear lookups.
// The returned HashMap contains a mapping from table header (like [a.b.c])
// to list of tables whose name at least starts with the specified
// name. So searching for [a.b] would give both [a.b.c.d] as well as [a.b.e].
// The tables are being identified by their index in the passed slice.
//
// A list is used for two reasons: First, the implementation also
// stores arrays in the same data structure and any top level array
// of size 2 or greater creates multiple entries in the list with the
// same shared name. Second, there can be multiple tables sharing
// the same prefix.
//
// The lookup is performed in the `MapAccess` implementation of `MapVisitor`.
// The lists are ordered, which we exploit in the search code by using
// bisection.
fn build_table_pindices<'de>(tables: &[Table<'de>]) -> HashMap<Vec<Cow<'de, str>>, Vec<usize>> {
    let mut res = HashMap::new();
    for (i, table) in tables.iter().enumerate() {
        let header = table.header.iter().map(|v| v.1.clone()).collect::<Vec<_>>();
        for len in 0..=header.len() {
            res.entry(header[..len].to_owned())
                .or_insert(Vec::new())
                .push(i);
        }
    }
    res
}

fn headers_equal<'a, 'b>(hdr_a: &[(Span, Cow<'a, str>)], hdr_b: &[(Span, Cow<'b, str>)]) -> bool {
    if hdr_a.len() != hdr_b.len() {
        return false;
    }
    hdr_a.iter().zip(hdr_b.iter()).all(|(h1, h2)| h1.1 == h2.1)
}

/// A square bracket table.
struct Table<'a> {
    /// Offset where the opening bracket of the table header starts.
    at: usize,
    /// The table header key parts.
    ///
    /// Tuples of `(span, key_part)` where `span` is the original span of the
    /// key part, possibly including surrounding quotes.
    header: Vec<(Span, Cow<'a, str>)>,
    values: Option<FlattenedTable<'a>>,
    array: bool,
}

#[doc(hidden)]
pub struct MapVisitor<'de, 'b> {
    values: iter::Peekable<vec::IntoIter<FlattenedKeyValue<'de>>>,
    next_value: Option<FlattenedKeyValue<'de>>,
    depth: usize,
    cur: usize,
    cur_parent: usize,
    max: usize,
    table_indices: &'b HashMap<Vec<Cow<'de, str>>, Vec<usize>>,
    table_pindices: &'b HashMap<Vec<Cow<'de, str>>, Vec<usize>>,
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
            if let Some(key_value) = self.values.next() {
                let ret = seed.deserialize(StrDeserializer::spanned((
                    key_value.key_span,
                    key_value.key_part.clone(),
                )))?;
                self.next_value = Some(key_value);
                return Ok(Some(ret));
            }

            let next_table = {
                let prefix_stripped = self.tables[self.cur_parent].header[..self.depth]
                    .iter()
                    .map(|v| v.1.clone())
                    .collect::<Vec<_>>();
                self.table_pindices
                    .get(&prefix_stripped)
                    .and_then(|entries| {
                        let start = entries.binary_search(&self.cur).unwrap_or_else(|v| v);
                        if start == entries.len() || entries[start] < self.cur {
                            return None;
                        }
                        entries[start..]
                            .iter()
                            .filter_map(|i| if *i < self.max { Some(*i) } else { None })
                            .map(|i| (i, &self.tables[i]))
                            .find(|(_, table)| table.values.is_some())
                            .map(|p| p.0)
                    })
            };

            let pos = match next_table {
                Some(pos) => pos,
                None => return Ok(None),
            };
            self.cur = pos;

            // Test to see if we're duplicating our parent's table, and if so
            // then this is an error in the toml format
            if self.cur_parent != pos {
                if headers_equal(
                    &self.tables[self.cur_parent].header,
                    &self.tables[pos].header,
                ) {
                    let at = self.tables[pos].at;
                    let name = self.tables[pos]
                        .header
                        .iter()
                        .map(|k| k.1.to_owned())
                        .collect::<Vec<_>>()
                        .join(".");
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
                let key = seed.deserialize(StrDeserializer::spanned(key.clone()))?;
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
                .0
                .into_iter()
                .peekable();
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        if let Some(FlattenedKeyValue {
            key_part,
            key_span: _,
            value,
        }) = self.next_value.take()
        {
            return seed
                .deserialize(ValueDeserializer::new(value))
                .map_err(|mut err| {
                    err.add_key_context(&key_part);
                    err
                });
        }

        let array =
            self.tables[self.cur].array && self.depth == self.tables[self.cur].header.len() - 1;
        self.cur += 1;
        let res = seed.deserialize(MapVisitor {
            values: Vec::new().into_iter().peekable(),
            next_value: None,
            depth: self.depth + if array { 0 } else { 1 },
            cur_parent: self.cur - 1,
            cur: 0,
            max: self.max,
            array,
            table_indices: &*self.table_indices,
            table_pindices: &*self.table_pindices,
            tables: &mut *self.tables,
            de: &mut *self.de,
        });
        res.map_err(|mut e| {
            e.add_key_context(&self.tables[self.cur - 1].header[self.depth].1);
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

        let header_stripped = self.tables[self.cur_parent]
            .header
            .iter()
            .map(|v| v.1.clone())
            .collect::<Vec<_>>();
        let start_idx = self.cur_parent + 1;
        let next = self
            .table_indices
            .get(&header_stripped)
            .and_then(|entries| {
                let start = entries.binary_search(&start_idx).unwrap_or_else(|v| v);
                if start == entries.len() || entries[start] < start_idx {
                    return None;
                }
                entries[start..]
                    .iter()
                    .filter_map(|i| if *i < self.max { Some(*i) } else { None })
                    .map(|i| (i, &self.tables[i]))
                    .find(|(_, table)| table.array)
                    .map(|p| p.0)
            })
            .unwrap_or(self.max);

        let ret = seed.deserialize(MapVisitor {
            values: self.tables[self.cur_parent]
                .values
                .take()
                .expect("Unable to read table values")
                .0
                .into_iter()
                .peekable(),
            next_value: None,
            depth: self.depth + 1,
            cur_parent: self.cur_parent,
            max: next,
            cur: 0,
            array: false,
            table_indices: &*self.table_indices,
            table_pindices: &*self.table_pindices,
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

    fn deserialize_struct<V>(
        mut self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        if name == spanned::NAME
            && fields == [spanned::START, spanned::END, spanned::VALUE]
            && !(self.array && !self.values.peek().is_none())
        {
            // TODO we can't actually emit spans here for the *entire* table/array
            // due to the format that toml uses. Setting the start and end to 0 is
            // *detectable* (and no reasonable span would look like that),
            // it would be better to expose this in the API via proper
            // ADTs like Option<T>.
            let start = 0;
            let end = 0;

            let res = visitor.visit_map(SpannedDeserializer {
                phantom_data: PhantomData,
                start: Some(start),
                value: Some(self),
                end: Some(end),
            });
            return res;
        }

        self.deserialize_any(visitor)
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
        if self.tables.len() != 1 {
            return Err(Error::custom(
                Some(self.cur),
                "enum table must contain exactly one table".into(),
            ));
        }
        let table = &mut self.tables[0];
        let values = table.values.take().expect("table has no values?");
        if table.header.len() == 0 {
            return Err(self.de.error(self.cur, ErrorKind::EmptyTableKey));
        }
        let name = table.header[table.header.len() - 1].1.to_owned();
        let span = Span {
            start: table.at,
            end: table.at,
        };
        visitor.visit_enum(DottedTableDeserializer {
            name,
            value: RawValue {
                pretext: "",
                text: "",
                posttext: "",
                parsed: RawValueType::DottedTable(values),
                span,
            },
        })
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map unit identifier
        ignored_any unit_struct tuple_struct tuple
    }
}

struct StrDeserializer<'a> {
    span: Option<Span>,
    key: Cow<'a, str>,
}

impl<'a> StrDeserializer<'a> {
    fn spanned(inner: (Span, Cow<'a, str>)) -> StrDeserializer<'a> {
        StrDeserializer {
            span: Some(inner.0),
            key: inner.1,
        }
    }
    fn new(key: Cow<'a, str>) -> StrDeserializer<'a> {
        StrDeserializer { span: None, key }
    }
}

impl<'a, 'b> de::IntoDeserializer<'a, Error> for StrDeserializer<'a> {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self::Deserializer {
        self
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

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        if name == spanned::NAME && fields == [spanned::START, spanned::END, spanned::VALUE] {
            if let Some(span) = self.span {
                return visitor.visit_map(SpannedDeserializer {
                    phantom_data: PhantomData,
                    start: Some(span.start),
                    value: Some(StrDeserializer::new(self.key)),
                    end: Some(span.end),
                });
            }
        }
        self.deserialize_any(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map option unit newtype_struct
        ignored_any unit_struct tuple_struct tuple enum identifier
    }
}

/// A deserializer for a `RawValue`.
pub struct ValueDeserializer<'a> {
    value: RawValue<'a>,
    validate_struct_keys: bool,
}

impl<'a> ValueDeserializer<'a> {
    fn new(value: RawValue<'a>) -> ValueDeserializer<'a> {
        ValueDeserializer {
            value,
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
        let start = self.value.span.start;
        let res = match self.value.parsed {
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
            RawValueType::InlineTable(values) | RawValueType::DottedTable(values) => visitor
                .visit_map(InlineTableDeserializer {
                    values: values.0.into_iter(),
                    next_value: None,
                }),
            RawValueType::RawInlineTable(_) => panic!("raw table unexpected"),
        };
        res.map_err(|mut err| {
            // Attribute the error to whatever value returned the error.
            err.fix_offset(|| Some(start));
            err
        })
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
        if name == datetime::NAME && fields == [datetime::FIELD] {
            if let RawValueType::Datetime(s) = self.value.parsed {
                return visitor.visit_map(DatetimeDeserializer {
                    date: s,
                    visited: false,
                });
            }
        }

        if self.validate_struct_keys {
            match &self.value.parsed {
                RawValueType::InlineTable(ref values) | RawValueType::DottedTable(ref values) => {
                    let extra_fields = values
                        .0
                        .iter()
                        .filter_map(|key_value| {
                            if !fields.contains(&key_value.key_part.as_ref()) {
                                Some(key_value.key_part.clone())
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    if !extra_fields.is_empty() {
                        return Err(Error::from_kind(
                            Some(self.value.span.start),
                            ErrorKind::UnexpectedKeys {
                                keys: extra_fields
                                    .iter()
                                    .map(|k| k.to_string())
                                    .collect::<Vec<_>>(),
                                available: fields,
                            },
                        ));
                    }
                }
                _ => {}
            }
        }

        if name == spanned::NAME && fields == [spanned::START, spanned::END, spanned::VALUE] {
            let start = self.value.span.start;
            let end = self.value.span.end;

            return visitor.visit_map(SpannedDeserializer {
                phantom_data: PhantomData,
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
                if values.0.len() != 1 {
                    Err(Error::from_kind(
                        Some(self.value.span.start),
                        ErrorKind::Wanted {
                            expected: "exactly 1 element",
                            found: if values.0.is_empty() {
                                "zero elements"
                            } else {
                                "more than 1 element"
                            },
                        },
                    ))
                } else {
                    visitor.visit_enum(InlineTableDeserializer {
                        values: values.0.into_iter(),
                        next_value: None,
                    })
                }
            }
            e => Err(Error::from_kind(
                Some(self.value.span.start),
                ErrorKind::Wanted {
                    expected: "string or inline table",
                    found: e.type_name(),
                },
            )),
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

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map unit identifier
        ignored_any unit_struct tuple_struct tuple
    }
}

impl<'de, 'b> de::IntoDeserializer<'de, Error> for MapVisitor<'de, 'b> {
    type Deserializer = MapVisitor<'de, 'b>;

    fn into_deserializer(self) -> Self::Deserializer {
        self
    }
}

impl<'de, 'b> de::IntoDeserializer<'de, Error> for &'b mut Deserializer<'de> {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self::Deserializer {
        self
    }
}

impl<'de> de::IntoDeserializer<'de, Error> for RawValue<'de> {
    type Deserializer = ValueDeserializer<'de>;

    fn into_deserializer(self) -> Self::Deserializer {
        ValueDeserializer::new(self)
    }
}

struct SpannedDeserializer<'de, T: de::IntoDeserializer<'de, Error>> {
    phantom_data: PhantomData<&'de ()>,
    start: Option<usize>,
    end: Option<usize>,
    value: Option<T>,
}

impl<'de, T> de::MapAccess<'de> for SpannedDeserializer<'de, T>
where
    T: de::IntoDeserializer<'de, Error>,
{
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

    serde::forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string seq
        bytes byte_buf map struct option unit newtype_struct
        ignored_any unit_struct tuple_struct tuple enum identifier
    }
}

struct DottedTableDeserializer<'a> {
    name: Cow<'a, str>,
    value: RawValue<'a>,
}

impl<'de> de::EnumAccess<'de> for DottedTableDeserializer<'de> {
    type Error = Error;
    type Variant = TableEnumDeserializer<'de>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let (name, value) = (self.name, self.value);
        seed.deserialize(StrDeserializer::new(name))
            .map(|val| (val, TableEnumDeserializer { value }))
    }
}

struct InlineTableDeserializer<'a> {
    values: vec::IntoIter<FlattenedKeyValue<'a>>,
    next_value: Option<RawValue<'a>>,
}

impl<'de> de::MapAccess<'de> for InlineTableDeserializer<'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.values.next() {
            Some(FlattenedKeyValue {
                key_part,
                key_span,
                value,
            }) => {
                self.next_value = Some(value);
                seed.deserialize(StrDeserializer::spanned((key_span, key_part)))
                    .map(Some)
            }
            None => Ok(None),
        }
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
        match self.values.next() {
            Some(FlattenedKeyValue {
                key_part,
                key_span: _,
                value,
            }) => seed
                .deserialize(StrDeserializer::new(key_part))
                .map(|val| (val, TableEnumDeserializer { value })),
            None => {
                Err(Error::from_kind(
                    None, // FIXME: How do we get an offset here?
                    ErrorKind::Wanted {
                        expected: "table with exactly 1 entry",
                        found: "empty table",
                    },
                ))
            }
        }
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
                if values.0.is_empty() {
                    Ok(())
                } else {
                    Err(Error::from_kind(
                        Some(self.value.span.start),
                        ErrorKind::ExpectedEmptyTable,
                    ))
                }
            }
            e => Err(Error::from_kind(
                Some(self.value.span.start),
                ErrorKind::Wanted {
                    expected: "table",
                    found: e.type_name(),
                },
            )),
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
                    .0
                    .into_iter()
                    .enumerate()
                    .map(
                        |(index, key_value)| match key_value.key_part.parse::<usize>() {
                            Ok(key_index) if key_index == index => Ok(key_value.value),
                            Ok(_) | Err(_) => Err(Error::from_kind(
                                Some(key_value.key_span.start),
                                ErrorKind::ExpectedTupleIndex {
                                    expected: index,
                                    found: key_value.key_part.to_string(),
                                },
                            )),
                        },
                    )
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
                    Err(Error::from_kind(
                        Some(self.value.span.start),
                        ErrorKind::ExpectedTuple(len),
                    ))
                }
            }
            e => Err(Error::from_kind(
                Some(self.value.span.start),
                ErrorKind::Wanted {
                    expected: "table",
                    found: e.type_name(),
                },
            )),
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
            input,
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

        while let Some(fragment) = self.fragment()? {
            match fragment {
                Fragment::KeyValue(RawKeyValue { key, value }) => {
                    if cur_table.values.is_none() {
                        cur_table.values = Some(FlattenedTable::new());
                    }
                    self.add_dotted_key(key.parts, value, cur_table.values.as_mut().unwrap())
                        .map_err(|mut err| {
                            for (key_span, _key) in cur_table.header.iter().rev() {
                                err.add_key_context(self.input_slice(key_span));
                            }
                            err
                        })?;
                }
                Fragment::Header(Header {
                    key, array, span, ..
                }) => {
                    if !cur_table.header.is_empty() || cur_table.values.is_some() {
                        tables.push(cur_table);
                    }
                    cur_table = Table {
                        at: span.start,
                        header: key.parts,
                        values: Some(FlattenedTable::new()),
                        array,
                    };
                }
                Fragment::Whitespace(..) => {}
                Fragment::Comment { .. } => {}
                Fragment::Bom => {}
            }
        }
        if !cur_table.header.is_empty() || cur_table.values.is_some() {
            tables.push(cur_table);
        }
        Ok(tables)
    }

    /// Parse and return a single "fragment" of the TOML document.
    ///
    /// See `Fragment` for the kinds of fragments this returns. This is a
    /// low-level API, and is intended to be used to get the original
    /// structure from the TOML document. This does not do much validation
    /// of the surrounding context. Notably:
    ///
    /// * Does not handle duplicate keys.
    /// * The caller is responsible for merging dotted keys. Be careful that
    ///   dotted keys are not merged with inline tables.
    /// * The caller must keep track of the "current" table for the purpose of
    ///   handling arrays-of-tables.
    /// * The caller should reject re-opening old tables.
    ///
    /// Note: This list is not exhaustive, consult the spec and test suites.
    pub fn fragment(&mut self) -> Result<Option<Fragment<'a>>, Error> {
        let start = self.tokens.current();
        let indent = self.eat_whitespace()?;
        // TODO: Peek here seems a little inefficient.
        let fragment = match self.peek()? {
            Some((_, Token::Newline)) => {
                self.next()?; // Skip newline.
                let end = self.tokens.current();
                let sp = Span { start, end };
                let ws = self.input_slice(&sp);
                Fragment::Whitespace(sp, ws)
            }
            Some((_, Token::Comment(_))) => {
                self.next()?; // Skip comment.
                self.expect_newline_or_eof()?;
                let end = self.tokens.current();
                let sp = Span { start, end };
                let comment = self.input_slice(&sp);
                Fragment::Comment(sp, comment)
            }
            Some((_, Token::LeftBracket)) => Fragment::Header(self.table_header(indent)?),
            Some((_, Token::Bom)) => {
                self.next()?; // Skip bom.
                Fragment::Bom
            }
            Some(_) => Fragment::KeyValue(self.key_value(indent)?),
            None => return Ok(None),
        };
        Ok(Some(fragment))
    }

    /// Parse a table header.
    ///
    /// The tokenizer should be pointing at an opening left bracket.
    ///
    /// The given `indent` is a slice of whitespace in front of the header.
    fn table_header(&mut self, indent: &'a str) -> Result<Header<'a>, Error> {
        let start = self.tokens.current();
        self.expect(Token::LeftBracket)?;
        let array = self.eat(Token::LeftBracket)?;
        let key = self.dotted_key(None)?;
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
        let span = Span {
            start,
            end: post_start,
        };
        let header = Header {
            indent,
            array,
            key,
            posttext,
            span,
        };
        Ok(header)
    }

    /// Parse a key/value pair.
    ///
    /// The tokenizer should be pointing at the start of the key.
    ///
    /// The given `indent` is a slice of whitespace in front of the key.
    fn key_value(&mut self, indent: &'a str) -> Result<RawKeyValue<'a>, Error> {
        // key [ws] `=` [ws] val [ws] [comment] newline-or-eof
        let key = self.dotted_key(Some(indent))?;
        self.expect(Token::Equals)?;
        let value = self.value()?;
        // Require newline or eof.
        if !value.has_trailing_newline() && !self.tokens.is_eof() {
            match self.peek()? {
                Some((Span { start, .. }, token)) => {
                    return Err(self.error(
                        start,
                        ErrorKind::Wanted {
                            expected: "newline",
                            found: token.describe(),
                        },
                    ));
                }
                None => {}
            }
        }
        Ok(RawKeyValue { key, value })
    }

    /// Parse a value.
    fn value(&mut self) -> Result<RawValue<'a>, Error> {
        let pretext = self.eat_whitespace()?;
        let start = self.tokens.current();
        macro_rules! make_raw {
            ($span:expr, $parsed:expr) => {
                let text = self.input_slice(&$span);
                let posttext = self.eat_posttext()?;
                return Ok(RawValue {
                    pretext,
                    text,
                    posttext,
                    parsed: $parsed,
                    span: $span,
                });
            };
        }
        let value = match self.next()? {
            Some((span, Token::String { val, .. })) => {
                make_raw!(span, RawValueType::String(val));
            }
            Some((span, Token::Keylike("true"))) => {
                make_raw!(span, RawValueType::Boolean(true));
            }
            Some((span, Token::Keylike("false"))) => {
                make_raw!(span, RawValueType::Boolean(false));
            }
            Some((span, Token::Keylike(key))) => self.number_or_date(pretext, span, key)?,
            Some((_span, Token::Plus)) => self.number_leading_plus(pretext, start)?,
            Some((Span { start, .. }, Token::LeftBrace)) => {
                let table = self.inline_table()?;
                let end = self.tokens.current();
                let span = Span { start, end };
                make_raw!(span, RawValueType::RawInlineTable(table));
            }
            Some((Span { start, .. }, Token::LeftBracket)) => {
                let array = self.array()?;
                let end = self.tokens.current();
                let span = Span { start, end };
                make_raw!(span, RawValueType::Array(array));
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

    fn number_or_date(
        &mut self,
        pretext: &'a str,
        span: Span,
        s: &'a str,
    ) -> Result<RawValue<'a>, Error> {
        macro_rules! to_datetime {
            ($span:expr, $value:expr) => {
                let text = self.input_slice(&$span);
                let posttext = self.eat_posttext()?;
                return Ok(RawValue {
                    pretext,
                    text,
                    posttext,
                    parsed: RawValueType::Datetime($value),
                    span: $span,
                });
            };
        }
        if s.contains('T')
            || s.contains('t')
            || (s.len() > 1 && s[1..].contains('-') && !s.contains("e-") && !s.contains("E-"))
        {
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
    fn string_or_table(&mut self) -> Result<(RawValue<'a>, Option<Cow<'a, str>>), Error> {
        // TOOD: This function is a little too weird.
        match self.peek()? {
            Some((span, Token::LeftBracket)) => {
                let tables = self.tables()?;
                if tables.len() != 1 {
                    return Err(Error::from_kind(
                        Some(span.start),
                        ErrorKind::Wanted {
                            expected: "exactly 1 table",
                            found: if tables.is_empty() {
                                "zero tables"
                            } else {
                                "more than 1 table"
                            },
                        },
                    ));
                }

                let table = tables
                    .into_iter()
                    .next()
                    .expect("Expected exactly one table");
                let header = table
                    .header
                    .last()
                    .expect("Expected at least one header value for table.");

                // TODO: This span really should span the entire table?
                let span = Span {
                    start: table.at,
                    end: table.at,
                };
                Ok((
                    RawValue {
                        pretext: "",
                        text: "",
                        posttext: "",
                        parsed: RawValueType::DottedTable(
                            table.values.unwrap_or_else(FlattenedTable::new),
                        ),
                        span,
                    },
                    Some(header.1.clone()),
                ))
            }
            Some(_) => {
                let mut value = self.value()?;
                if let RawValueType::RawInlineTable(_) = value.parsed {
                    self.flatten_inline_tables(&mut value)?;
                }
                Ok((value, None))
            }
            None => Err(self.eof()),
        }
    }

    /// Parse a number.
    ///
    /// The given `s` value is a keylike value that has already been eaten.
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
        let span = Span {
            start: span.start,
            end: self.tokens.current(),
        };
        let text = self.input_slice(&span);
        let posttext = self.eat_posttext()?;
        Ok(RawValue {
            pretext,
            text,
            posttext,
            parsed,
            span,
        })
    }

    /// Parse a number after a `+` token has already been eaten.
    fn number_leading_plus(
        &mut self,
        pretext: &'a str,
        start: usize,
    ) -> Result<RawValue<'a>, Error> {
        match self.next()? {
            Some((Span { end, .. }, Token::Keylike(s))) => {
                self.number(pretext, Span { start, end }, s)
            }
            _ => Err(self.error(start, ErrorKind::NumberInvalid)),
        }
    }

    /// Parse an integer.
    ///
    /// This is a helper for the `number` method.
    fn integer(&self, s: &'a str, radix: u32) -> Result<i64, Error> {
        let allow_sign = radix == 10;
        let allow_leading_zeros = radix != 10;
        let (prefix, suffix) = self.parse_integer(s, allow_sign, allow_leading_zeros, radix)?;
        let start = self.tokens.substr_offset(s);
        if suffix != "" {
            return Err(self.error(start, ErrorKind::NumberInvalid));
        }
        i64::from_str_radix(&prefix.replace("_", "").trim_start_matches('+'), radix)
            .map_err(|_e| self.error(start, ErrorKind::NumberInvalid))
    }

    /// Low-level integer parser.
    ///
    /// This is used by various things (float, number, etc.) that need to
    /// parse an integer.
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

    /// Low-level float parser.
    ///
    /// The given `after_decimal` is the content after the decimal point.
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
                    Some((_, Token::Keylike(s))) => self.parse_integer(s, false, true, 10)?,
                    _ => return Err(self.error(start, ErrorKind::NumberInvalid)),
                }
            } else {
                self.parse_integer(&suffix[1..], true, true, 10)?
            };
            if b != "" {
                return Err(self.error(start, ErrorKind::NumberInvalid));
            }
            exponent = Some(a);
        } else if !suffix.is_empty() {
            return Err(self.error(start, ErrorKind::NumberInvalid));
        }

        let mut number = integral
            .trim_start_matches('+')
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

    /// Parse a datetime.
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

        Ok((span, self.input_slice(&span)))
    }

    /// Parse an inline table.
    fn inline_table(&mut self) -> Result<Vec<RawKeyValue<'a>>, Error> {
        let mut ret = Vec::new();
        self.eat_whitespace()?;
        if self.eat(Token::RightBrace)? {
            return Ok(ret);
        }
        loop {
            let key = self.dotted_key(None)?;
            self.expect(Token::Equals)?;
            let value = self.value()?;
            if value.has_trailing_newline() {
                return Err(self.error(self.tokens.current() - 1, ErrorKind::NewlineInInlineTable));
            }
            ret.push(RawKeyValue { key, value });
            if self.eat(Token::RightBrace)? {
                return Ok(ret);
            }
            self.expect(Token::Comma)?;
        }
    }

    /// Parse an array.
    fn array(&mut self) -> Result<Vec<RawValue<'a>>, Error> {
        let mut ret = Vec::new();

        let intermediate = |me: &mut Deserializer<'_>| {
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
            if self.eat(Token::RightBracket)? {
                return Ok(ret);
            }
            let value = self.value()?;
            ret.push(value);
            intermediate(self)?;
            if !self.eat(Token::Comma)? {
                break;
            }
        }
        intermediate(self)?;
        self.expect(Token::RightBracket)?;
        Ok(ret)
    }

    /// Parse a simple key (one part of a dotted key).
    fn table_key(&mut self) -> Result<(Span, Cow<'a, str>), Error> {
        self.tokens.table_key().map_err(|e| self.token_error(e))
    }

    /// Parse a dotted key.
    ///
    /// The given indent is optional whitespace in front of the key that has
    /// already been eaten.
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
            pretext,
            text,
            parts,
            posttext,
        })
    }

    /// Adds a key and value to the given flattened table.
    ///
    /// Dotted keys are flattened (recursively). For example,
    /// `multi.part.key = "value"` will be inserted as 3 nested
    /// tables (`multi = {part = {key = "value"}}`).
    fn add_dotted_key(
        &self,
        mut key_parts: Vec<(Span, Cow<'a, str>)>,
        mut value: RawValue<'a>,
        table: &mut FlattenedTable<'a>,
    ) -> Result<(), Error> {
        let (key_span, key_part) = key_parts.remove(0);
        if key_parts.is_empty() {
            // Reached the end, just insert the last value.
            self.flatten_inline_tables(&mut value).map_err(|mut err| {
                err.add_key_context(key_part.as_ref());
                err
            })?;
            table.0.push(FlattenedKeyValue {
                key_part,
                key_span,
                value,
            });
            return Ok(());
        }

        // This clone is unfortunate. I do not see a way to insert into the
        // map and retain a reference to the key. There is a clever solution
        // at https://github.com/rust-lang/rust/pull/60142#issuecomment-487416553
        // but it was never implemented.
        let key_part_copy = key_part.clone();

        // Check if a dotted key table already exists.
        match table.0.iter_mut().find(|fkv| fkv.key_part == key_part) {
            Some(&mut FlattenedKeyValue {
                value:
                    RawValue {
                        parsed: RawValueType::DottedTable(ref mut next_table),
                        ..
                    },
                ..
            }) => {
                return self.add_dotted_key(key_parts, value, next_table);
            }
            Some(&mut FlattenedKeyValue {
                value: RawValue { span, .. },
                ..
            }) => {
                // Cannot mix inner dotted key with anything else (including
                // inline tables).
                let mut err = self.error(span.start, ErrorKind::DottedKeyInvalidType);
                err.add_key_context(&key_part);
                return Err(err);
            }
            None => {}
        }
        // Create the intermediate table.
        let table_values = RawValue {
            pretext: "",
            text: "",
            posttext: "",
            parsed: RawValueType::DottedTable(FlattenedTable::new()),
            span: value.span, // This is somewhat misleading.
        };
        let fkv = FlattenedKeyValue {
            key_part,
            key_span,
            value: table_values,
        };
        table.0.push(fkv);
        // Get a reference to the value just inserted, and recurse into it.
        if let FlattenedKeyValue {
            value:
                RawValue {
                    parsed: RawValueType::DottedTable(ref mut last_table),
                    ..
                },
            ..
        } = table.0.last_mut().unwrap()
        {
            self.add_dotted_key(key_parts, value, last_table)
                .map_err(|mut err| {
                    err.add_key_context(key_part_copy.as_ref());
                    err
                })?;
        }
        Ok(())
    }

    /// Convert `RawInlineTable` to nested `DottedTable` (for intermediate
    /// dotted keys) and `InlineTable` (for the actual table contents).
    ///
    /// The value is mutated in-place.
    fn flatten_inline_tables(&self, value: &mut RawValue<'a>) -> Result<(), Error> {
        match value.parsed {
            RawValueType::RawInlineTable(_) => {
                // Convert this to an InlineTable.
                let new_table = FlattenedTable::new();
                let new_parsed = RawValueType::InlineTable(new_table);
                let old_parsed = std::mem::replace(&mut value.parsed, new_parsed);
                if let RawValueType::RawInlineTable(old_items) = old_parsed {
                    if let RawValueType::InlineTable(new_table) = &mut value.parsed {
                        for RawKeyValue {
                            key: old_key,
                            value: old_value,
                        } in old_items
                        {
                            self.add_dotted_key(old_key.parts, old_value, new_table)?;
                        }
                    }
                }
            }
            RawValueType::InlineTable(_) | RawValueType::DottedTable(_) => {
                // Everything should be raw at this point.
                panic!("did not expect inline or dotted table while flattening");
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

    fn expect(&mut self, expected: Token<'a>) -> Result<(), Error> {
        self.tokens
            .expect(expected)
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

    fn input_slice(&self, span: &Span) -> &'a str {
        &self.tokens.input()[span.start..span.end]
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
            } => self.error(at, ErrorKind::Wanted { expected, found }),
            TokenError::EmptyTableKey(at) => self.error(at, ErrorKind::EmptyTableKey),
            TokenError::MultilineStringKey(at) => self.error(at, ErrorKind::MultilineStringKey),
        }
    }

    fn error(&self, at: usize, kind: ErrorKind) -> Error {
        let mut err = Error::from_kind(Some(at), kind);
        err.fix_linecol(|at| self.to_linecol(at));
        err
    }

    /// Converts a byte offset from an error message to a (line, column) pair
    ///
    /// All indexes are 0-based.
    fn to_linecol(&self, offset: usize) -> (usize, usize) {
        let mut cur = 0;
        // Use split_terminator instead of lines so that if there is a `\r`,
        // it is included in the offset calculation. The `+1` values below
        // account for the `\n`.
        for (i, line) in self.input.split_terminator('\n').enumerate() {
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

    fn from_kind(at: Option<usize>, kind: ErrorKind) -> Error {
        Error {
            inner: Box::new(ErrorInner {
                kind,
                line: None,
                col: 0,
                at,
                message: String::new(),
                key: Vec::new(),
            }),
        }
    }

    fn custom(at: Option<usize>, s: String) -> Error {
        Error {
            inner: Box::new(ErrorInner {
                kind: ErrorKind::Custom,
                line: None,
                col: 0,
                at,
                message: s,
                key: Vec::new(),
            }),
        }
    }

    pub(crate) fn add_key_context(&mut self, key: &str) {
        self.inner.key.insert(0, key.to_string());
    }

    fn fix_offset<F>(&mut self, f: F)
    where
        F: FnOnce() -> Option<usize>,
    {
        // An existing offset is always better positioned than anything we
        // might want to add later.
        if self.inner.at.is_none() {
            self.inner.at = f();
        }
    }

    fn fix_linecol<F>(&mut self, f: F)
    where
        F: FnOnce(usize) -> (usize, usize),
    {
        if let Some(at) = self.inner.at {
            let (line, col) = f(at);
            self.inner.line = Some(line);
            self.inner.col = col;
        }
    }
}

impl std::convert::From<Error> for std::io::Error {
    fn from(e: Error) -> Self {
        std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            ErrorKind::DuplicateTable(ref s) => {
                write!(f, "redefinition of table `{}`", s)?;
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
            write!(f, " at line {} column {}", line + 1, self.inner.col + 1)?;
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
            ErrorKind::DuplicateTable(_) => "duplicate table",
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
        Error::custom(None, msg.to_string())
    }
}

/// A fragment of the TOML document.
#[derive(Debug)]
pub enum Fragment<'a> {
    /// Byte-order mark.
    Bom,
    /// Whitespace.
    Whitespace(Span, &'a str),
    /// A comment.
    Comment(Span, &'a str),
    /// A square bracket table header.
    Header(Header<'a>),
    /// A key/value pair.
    ///
    /// Note that the key may be a dotted key, which will need further
    /// processing.
    KeyValue(RawKeyValue<'a>),
}

/// A square bracket table header.
#[derive(Debug)]
pub struct Header<'a> {
    /// Whitespace before opening bracket.
    pub indent: &'a str,
    /// True if array table.
    pub array: bool,
    /// Name information, including any whitespace inside the brackets.
    pub key: RawKey<'a>,
    /// Whitespace/comment following the closing bracket.
    pub posttext: &'a str,
    /// Span of the header, starting at the opening bracket. This does not
    /// include leading or trailing whitespace.
    pub span: Span,
}

/// An inline table after dotted keys have been flattened.
///
/// Contains a vec of each top-level key/value pair.
#[derive(Debug)]
pub struct FlattenedTable<'a>(Vec<FlattenedKeyValue<'a>>);

impl<'a> FlattenedTable<'a> {
    /// Creates a new FlattenedTable.
    fn new() -> FlattenedTable<'a> {
        FlattenedTable(Vec::new())
    }
}

/// A key/value pair.
///
/// This is only exposed in the "raw" API via `Deserializer::fragment`. When
/// processed by serde, this is converted to `FlattenedKeyValue`.
#[derive(Debug)]
pub struct RawKeyValue<'a> {
    /// The key.
    pub key: RawKey<'a>,
    /// The value.
    pub value: RawValue<'a>,
}

/// A key/value pair after dotted keys have been flattened.
#[derive(Debug)]
pub struct FlattenedKeyValue<'a> {
    /// The key.
    ///
    /// This key may belong to a dotted key, in which case it is just one
    /// segment.
    key_part: Cow<'a, str>,
    /// The span of the original key, including the quotes.
    key_span: Span,
    /// The value.
    value: RawValue<'a>,
}

/// A dotted key.
#[derive(Debug)]
pub struct RawKey<'a> {
    /// Whitespace in front of the key.
    pub pretext: &'a str,
    /// Original text of the key (not including pretext/posttext).
    pub text: &'a str,
    /// Parsed parts of the key, split on dots.
    ///
    /// Quoted key parts have the quotes removed and escapes translated. The
    /// span encompasses the entire quoted string, including the quotes.
    pub parts: Vec<(Span, Cow<'a, str>)>,
    /// Whitespace after the key.
    pub posttext: &'a str,
}

/// A TOML value, including original text information.
#[derive(Debug)]
pub struct RawValue<'a> {
    /// Whitespace in front of the value.
    pub pretext: &'a str,
    /// Original text of the value (not including pretext/posttext).
    pub text: &'a str,
    /// Whitespace and/or comment following the value.
    pub posttext: &'a str,
    /// Parsed value.
    pub parsed: RawValueType<'a>,
    /// Span of the value, not including surrounding whitespace.
    pub span: Span,
}

/// A parsed TOML value.
#[derive(Debug)]
pub enum RawValueType<'a> {
    /// A TOML integer.
    Integer(i64),
    /// A TOML float.
    Float(f64),
    /// A TOML boolean.
    Boolean(bool),
    /// A TOML string.
    ///
    /// Quotes have been removed, and escaping has been translated.
    String(Cow<'a, str>),
    /// A TOML datetime.
    Datetime(&'a str),
    /// A TOML array.
    Array(Vec<RawValue<'a>>),
    /// An inline table `{key = value, ...}`, containing dotted keys.
    ///
    /// This is only exposed in `Deserializer::fragment`. Other interfaces
    /// such as `Deserializer::tables` will convert these to `InlineTable` by
    /// flattening intermediate dotted keys.
    RawInlineTable(Vec<RawKeyValue<'a>>),
    /// Inline table, after dotted keys have been flattened out.
    InlineTable(FlattenedTable<'a>),
    /// Intermediate inline table created by a dotted key.
    ///
    /// * `a.b = 1` will create a `DottedTable` for `a`.
    /// * `foo = {key = 2}` will create an `InlineTable` for `foo`.
    ///
    /// `DottedTable` is essentially the same as `InlineTable`, but is used by
    /// the parser to reject inline tables being mixed with dotted keys. For
    /// example:
    ///
    /// ```toml
    /// a = {foo = 1}
    /// a.b = 2
    /// ```
    ///
    /// Here `a` will start out as an `InlineTable`. When `a.b` is encountered
    /// the parser will not allow it because `InlineTable` and `DottedTable`
    /// are not allowed to be merged.
    DottedTable(FlattenedTable<'a>),
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
            RawValueType::RawInlineTable(..) => "raw table",
            RawValueType::InlineTable(..) => "inline table",
            RawValueType::DottedTable(..) => "dotted table",
        }
    }
}

impl<'a> RawValue<'a> {
    fn has_trailing_newline(&self) -> bool {
        let bytes = self.posttext.as_bytes();
        !bytes.is_empty() && bytes[bytes.len() - 1] == b'\n'
    }
}
