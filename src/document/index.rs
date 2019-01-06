use super::{DocValue, DocValueType, TomlDocument};
use std::ops;

pub trait DocIndex: Sealed {
    #[doc(hidden)]
    fn index<'a>(&self, val: &'a DocValue<'a>) -> Option<&'a DocValue<'a>>;
}

#[doc(hidden)]
pub trait Sealed {}
impl Sealed for usize {}
impl Sealed for str {}
impl Sealed for String {}
impl<'a, T: Sealed + ?Sized> Sealed for &'a T {}

// TODO: I cannot get the lifetimes to work for ops::Index to use DocIndex.
// Temporary workaround is to implement the 3 types manually below.
// impl<'a, I> ops::Index<I> for DocValue<'a>
//     where I: DocIndex
// {
//     type Output = DocValue<'a>;

//     fn index(&self, index: I) -> &DocValue<'a> {
//         index.index(self).unwrap()
//         // self.get(index).expect("index not found")
//     }
// }

impl<'a> ops::Index<&str> for DocValue<'a> {
    type Output = DocValue<'a>;

    fn index(&self, index: &str) -> &DocValue<'a> {
        match &self.parsed {
            DocValueType::Table(t) => t.get(index).unwrap(),
            _ => panic!("wrong type"),
        }
    }
}

impl<'a> ops::Index<String> for DocValue<'a> {
    type Output = DocValue<'a>;

    fn index(&self, index: String) -> &DocValue<'a> {
        match &self.parsed {
            DocValueType::Table(t) => t.get(&index).unwrap(),
            _ => panic!("wrong type"),
        }
    }
}

impl<'a> ops::Index<usize> for DocValue<'a> {
    type Output = DocValue<'a>;

    fn index(&self, index: usize) -> &DocValue<'a> {
        match &self.parsed {
            DocValueType::Array(a) => a.get(index).unwrap(),
            _ => panic!("wrong type"),
        }
    }
}

impl<'a> ops::Index<&str> for TomlDocument<'a> {
    type Output = DocValue<'a>;

    fn index(&self, index: &str) -> &DocValue<'a> {
        self.get(index).expect("index not found")
    }
}

// match &self.parsed {
//     DocValueType::Table(t) => t.get(&index).unwrap(),
//     _ => panic!("wrong type"),
// }

// match &self.parsed {
//     DocValueType::Table(t) => t.map.get::<str>(&index).unwrap(),
//     _ => panic!("wrong type"),
// }

// self.get(index).expect("index not found")
// self.get(&index).expect("index not found")
// unimplemented!()
//     }
// }

// impl DocIndex for usize {
//     fn index<'a>(&self, val: &'a DocValue<'a>) -> Option<&'a DocValue<'a>> {
//         match &val.parsed {
//             DocValueType::Array(a) => a.get(*self),
//             _ => None,
//         }
//     }

//     // fn index_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
//     //     match *val {
//     //         DocValue::Array(ref mut a) => a.get_mut(*self),
//     //         _ => None,
//     //     }
//     // }
// }

impl DocIndex for str {
    fn index<'a>(&self, val: &'a DocValue<'a>) -> Option<&'a DocValue<'a>> {
        match &val.parsed {
            DocValueType::Table(t) => t.get(self),
            _ => None,
        }
    }

    // fn index_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
    //     match *val {
    //         DocValue::Table(ref mut a) => a.get_mut(self),
    //         _ => None,
    //     }
    // }
}

impl DocIndex for String {
    fn index<'a>(&self, val: &'a DocValue<'a>) -> Option<&'a DocValue<'a>> {
        self[..].index(val)
    }

    // fn index_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
    //     self[..].index_mut(val)
    // }
}

impl<'s, T: ?Sized> DocIndex for &'s T
where
    T: DocIndex,
{
    fn index<'a>(&self, val: &'a DocValue<'a>) -> Option<&'a DocValue<'a>> {
        (**self).index(val)
    }

    //     // fn index_mut<'a>(&self, val: &'a mut DocValue) -> Option<&'a mut DocValue> {
    //     //     (**self).index_mut(val)
    //     // }
}
