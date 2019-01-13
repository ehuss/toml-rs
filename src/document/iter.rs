use super::{DocValue};
use std::{
    collections::hash_map,
    slice,
    vec,
};

pub struct IterTable<'a> {
    pub(crate) items: hash_map::Iter<'a, String, DocValue>,
}

impl<'a> Iterator for IterTable<'a> {
    type Item = (&'a String, &'a DocValue);
    fn next(&mut self) -> Option<Self::Item> {
        self.items.next()
    }
}

pub struct IterTableMut<'a> {
    pub(crate) items: hash_map::IterMut<'a, String, DocValue>,
}

impl<'a> Iterator for IterTableMut<'a> {
    type Item = (&'a String, &'a mut DocValue);
    fn next(&mut self) -> Option<Self::Item> {
        self.items.next()
    }
}

pub struct IntoIterTable {
    pub(crate) items: hash_map::IntoIter<String, DocValue>,
}

impl<'a> Iterator for IntoIterTable {
    type Item = (String, DocValue);
    fn next(&mut self) -> Option<Self::Item> {
        self.items.next()
    }
}

pub struct IterArray<'a> {
    pub(crate) items: slice::Iter<'a, DocValue>,
}

impl<'a> Iterator for IterArray<'a> {
    type Item = &'a DocValue;
    fn next(&mut self) -> Option<Self::Item> {
        self.items.next()
    }
}

pub struct IterArrayMut<'a> {
    pub(crate) items: slice::IterMut<'a, DocValue>,
}

impl<'a> Iterator for IterArrayMut<'a> {
    type Item = &'a mut DocValue;
    fn next(&mut self) -> Option<Self::Item> {
        self.items.next()
    }
}

pub struct IntoIterArray {
    pub(crate) items: vec::IntoIter<DocValue>,
}

impl<'a> Iterator for IntoIterArray {
    type Item = DocValue;
    fn next(&mut self) -> Option<Self::Item> {
        self.items.next()
    }
}
