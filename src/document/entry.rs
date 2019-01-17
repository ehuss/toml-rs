use self::DocEntry::*;
use super::{DocKey, DocTable, DocValue, DocValueType, TablePath};
use std::collections::hash_map;

pub enum DocEntry<'a> {
    Vacant(VacantDocEntry<'a>),
    Occupied(OccupiedDocEntry<'a>),
}

pub struct VacantDocEntry<'a> {
    pub(super) key: DocKey,
    pub(super) cur_part: usize,
    pub(super) path: TablePath,
    pub(super) entry: hash_map::VacantEntry<'a, String, DocValue>,
    pub(super) items: &'a mut Vec<(DocKey, TablePath)>,
    pub(super) is_modified: &'a mut bool,
    pub(super) is_inline: &'a mut bool,
    pub(super) is_intermediate: &'a mut bool,
}

pub struct OccupiedDocEntry<'a> {
    key: DocKey,
    table: &'a mut DocTable,
}

impl<'a> DocEntry<'a> {
    pub fn or_insert(self, default: DocValue) -> &'a mut DocValue {
        match self {
            Vacant(entry) => entry.insert(default),
            Occupied(entry) => entry.into_mut(),
        }
    }

    pub fn or_insert_with<F: FnOnce() -> DocValue>(self, default: F) -> &'a mut DocValue {
        match self {
            Vacant(entry) => entry.insert(default()),
            Occupied(entry) => entry.into_mut(),
        }
    }

    pub fn key(&self) -> &DocKey {
        match *self {
            Vacant(ref entry) => entry.key(),
            Occupied(ref entry) => entry.key(),
        }
    }

    pub fn and_modify<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut DocValue),
    {
        match self {
            Vacant(entry) => Vacant(entry),
            Occupied(mut entry) => {
                f(entry.get_mut());
                Occupied(entry)
            }
        }
    }
}

impl<'a> VacantDocEntry<'a> {
    pub fn key(&self) -> &DocKey {
        &self.key
    }

    pub fn into_key(self) -> DocKey {
        self.key
    }

    pub fn insert(self, mut value: DocValue) -> &'a mut DocValue {
        // TODO: Check if key is array key.
        *self.is_modified = true;
        let is_im = self.cur_part < self.key.parts.len() - 1;
        // println!("vacant::insert {:?} {:?}", self.key, self.cur_part);
        // println!("path = {:?}", self.path);
        if is_im {
            let mut im_table = DocTable::new();
            im_table.is_intermediate = true;
            // im_table.is_inline = !self.key.is_bracketed;
            let im_value = DocValue::new(DocValueType::Table(im_table));
            let mut dv = self.entry.insert(im_value);
            // Get the intermediate table that was just inserted.
            // println!("IM entry insert");
            if let DocValueType::Table(ref mut t) = dv.parsed {
                let is_bracketed = value.is_bracketed_table();
                // println!("is_bracketed={:?}", is_bracketed);
                let res = t.entry_insert(&self.key, value, self.cur_part + 1);
                if !is_bracketed {
                    // Standard tables are only listed in
                    // TomlDocument::table_order, and must be recomputed at
                    // render time.
                    self.items.push((self.key, self.path));
                }
                return res;
            }
            unreachable!();
        } else {
            // println!("entry insert vacant");
            if let DocValueType::Table(table) = &mut value.parsed {
                // Make sure table types are compatible with what is being
                // inserted into.
                if *self.is_inline {
                    // println!("insert into inline");
                    if !table.is_inline {
                        // println!("insert std into inline");
                        table.is_inline = true;
                        table.header_key = None;
                        table.header_posttext = None;
                        table.header_pretext = None;
                    }
                }
            }
            if *self.is_intermediate && !*self.is_inline {
                // inserting into intermediate inline.
                // Standard tables are OK, others will need to promote.
                if !value.is_bracketed_table() {
                    *self.is_intermediate = false;
                }
            }
            if !value.is_bracketed_table() {
                self.items.push((self.key, self.path));
            }
            return self.entry.insert(value);
        }
    }
}

impl<'a> OccupiedDocEntry<'a> {
    pub fn key(&self) -> &DocKey {
        &self.key
    }

    pub fn remove_entry(self) -> (DocKey, DocValue) {
        unimplemented!();
    }

    pub fn get(&self) -> &DocValue {
        unimplemented!();
    }

    pub fn get_mut(&mut self) -> &mut DocValue {
        unimplemented!();
    }

    pub fn into_mut(self) -> &'a mut DocValue {
        unimplemented!();
    }

    pub fn insert(&mut self, value: DocValue) -> DocValue {
        unimplemented!();
    }

    pub fn remove(self) -> DocValue {
        unimplemented!();
    }

    pub fn replace_entry(self, value: DocValue) -> (DocKey, DocValue) {
        unimplemented!();
    }

    pub fn replace_key(self) -> DocKey {
        unimplemented!();
    }
}
