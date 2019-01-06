extern crate toml;

use toml::document::{TomlDocument, DocValue};

const FRUITS: &str = "\
# This is a document comment.

# This is a fruit comment.
[[fruit]]
  # name comment
  name = 'apple'

  # physical comment
  [fruit.physical]
    color = 'red'
    shape = 'round'

  [[fruit.variety]]
    name = 'red delicious'

  [[fruit.variety]]
    name = 'granny smith'

# second fruit comment
[[fruit]]
  name = 'banana'

  [[fruit.variety]]
    name = 'plantain'
";

#[test]
fn test_index() {
    let doc = TomlDocument::from_str("a = 1").unwrap();
    let a = &doc["a"];
    assert_eq!(a.as_integer(), Some(1));

    let doc = TomlDocument::from_str("
        a.b.c = 1
        a.d = 2
    ").unwrap();
    let c = &doc["a"]["b"]["c"];
    assert_eq!(c.as_integer(), Some(1));
    let d = &doc["a"]["d"];
    assert_eq!(d.as_integer(), Some(2));
}

#[test]
fn test_docvalue_parse() {
    let txt = " 123  #comment";
    let dv = DocValue::from_str(txt).unwrap();
    assert_eq!(dv.as_integer(), Some(123));
    assert_eq!(dv.to_string(), txt);
    let txt = "{a = 'foo'}";
    let dv: DocValue = txt.parse().unwrap();
    assert_eq!(dv["a"].as_str(), Some("foo"));
    assert_eq!(dv.to_string(), txt);
}

#[test]
fn remove_keys() {
    let mut doc = TomlDocument::from_str("\
        a.b.c = 1\n\
        a.d = 2\n\
    ").unwrap();
    let dv = doc["a"].remove("b").unwrap();
    println!("{:?}", dv.to_string());
    println!("{:?}", dv);
    // TODO: Whitespace is horribly broken here.
    // assert_eq!(dv.to_string(), "{ c = 1 }");
    assert_eq!(doc.to_string(), "a.d = 2\n");
}

#[test]
fn iter() {
    let doc = TomlDocument::from_str("\
        a.b.c = 1\n\
        a.b.d = 2\n\
    ").unwrap();
    let keys: Vec<&String> = doc.iter().map(|x| x.0).collect();
    assert_eq!(&keys, &[&String::from("a")]);
    let mut keys: Vec<&String> = doc["a"]["b"].iter_table().map(|x| x.0).collect();
    keys.sort();
    assert_eq!(&keys, &[&String::from("c"), &String::from("d")]);
}

#[test]
fn document_remove() {
    let mut doc = TomlDocument::from_str(FRUITS).unwrap();
    doc.remove("fruit");
    assert_eq!(doc.to_string(), "# This is a document comment.\n\n");
}

#[test]
fn intermediate_remove() {
    let mut doc = TomlDocument::from_str("\
root_key = 1
# a.b.c comment
[a.b.c]
key = 1
# a.b.d comment
[a.b.d]
key = 2
[a.other]
key = 3
").unwrap();
    doc["a"].remove("b");
    assert_eq!(doc.to_string(), "\
root_key = 1
[a.other]
key = 3
");
}
