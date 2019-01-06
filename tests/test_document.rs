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
