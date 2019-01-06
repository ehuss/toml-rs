use super::{DocValue, DocValueType, TomlDocument};
use std::ops;

pub trait DocIndex: Sealed {
    #[doc(hidden)]
    fn index_get<'a>(&self, val: &'a DocValue) -> Option<&'a DocValue>;
    fn index_get_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue>;
    fn index_remove<'a>(&self, val: &'a mut DocValue) -> Option<DocValue>;
}

#[doc(hidden)]
pub trait Sealed {}
impl Sealed for usize {}
impl Sealed for str {}
impl Sealed for String {}
impl<'a, T: Sealed + ?Sized> Sealed for &'a T {}

// TODO: I cannot get the lifetimes to work for ops::Index to use DocIndex.
// Temporary workaround is to implement the 3 types manually below.
impl<I> ops::Index<I> for DocValue
    where I: DocIndex
{
    type Output = DocValue;

    fn index(&self, index: I) -> &DocValue {
        index.index_get(self).unwrap()
    }
}

impl ops::Index<&str> for TomlDocument {
    type Output = DocValue;

    fn index(&self, index: &str) -> &DocValue {
        self.get(index).expect("index not found")
    }
}

impl<I> ops::IndexMut<I> for DocValue
where
    I: DocIndex,
{
    fn index_mut(&mut self, index: I) -> &mut DocValue {
        self.get_mut(index).expect("index not found")
    }
}

impl ops::IndexMut<&str> for TomlDocument
{
    fn index_mut(&mut self, index: &str) -> &mut DocValue {
        self.get_mut(index).expect("index not found")
    }
}


impl DocIndex for usize {
    fn index_get<'a>(&self, val: &'a DocValue) -> Option<&'a DocValue> {
        match &val.parsed {
            DocValueType::Array(a) => a.get(*self),
            _ => None,
        }
    }

    fn index_get_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
        match &mut val.parsed {
            DocValueType::Array(a) => a.get_mut(*self),
            _ => None,
        }
    }

    fn index_remove<'a>(&self, val: &'a mut DocValue) -> Option<DocValue> {
        match &mut val.parsed {
            DocValueType::Array(a) => Some(a.remove(*self)),
            _ => None,
        }
    }

}

impl DocIndex for str {
    fn index_get<'a>(&self, val: &'a DocValue) -> Option<&'a DocValue> {
        match &val.parsed {
            DocValueType::Table(t) => t.get(self),
            _ => None,
        }
    }

    fn index_get_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
        match &mut val.parsed {
            DocValueType::Table(t) => t.get_mut(self),
            _ => None,
        }
    }

    fn index_remove<'a>(&self, val: &'a mut DocValue) -> Option<DocValue> {
        match &mut val.parsed {
            DocValueType::Table(t) => t.remove(self),
            _ => None,
        }
    }

}

impl DocIndex for String {
    fn index_get<'a>(&self, val: &'a DocValue) -> Option<&'a DocValue> {
        self[..].index_get(val)
    }

    fn index_get_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
        self[..].index_get_mut(val)
    }

    fn index_remove<'a>(&self, val: &'a mut DocValue) -> Option<DocValue> {
        self[..].index_remove(val)
    }
}

impl<'s, T: ?Sized> DocIndex for &'s T
where
    T: DocIndex,
{
    fn index_get<'a>(&self, val: &'a DocValue) -> Option<&'a DocValue> {
        (**self).index_get(val)
    }

    fn index_get_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
        (**self).index_get_mut(val)
    }

    fn index_remove<'a>(&self, val: &'a mut DocValue) -> Option<DocValue> {
        (**self).index_remove(val)
    }
}
