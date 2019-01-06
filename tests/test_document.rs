extern crate toml;

use toml::document::{TomlDocument, DocValue};

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
    // assert_eq!(dv.to_string(), );
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
    let keys: Vec<&String> = doc["a"]["b"].table_iter().map(|x| x.0).collect();
    assert_eq!(&keys, &[&String::from("c"), &String::from("d")]);
}
