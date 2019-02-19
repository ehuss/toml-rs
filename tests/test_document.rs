extern crate toml;

use toml::document::{DocKey, DocValue, TomlDocument};
use toml::Value;

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

fn new_doc(t: &str) -> TomlDocument {
    let doc = TomlDocument::from_str(t).unwrap();
    let rendered = doc.to_string();
    if !t.ends_with("\n") && rendered.ends_with("\n") {
        // Minor deviation, sometimes rendering will need to add a newline
        // because it doesn't know if it safe to avoid it.
        assert_eq!(&rendered[..rendered.len() - 1], t);
    } else {
        assert_eq!(rendered, t);
    }
    doc
}

fn matches(doc: &TomlDocument, expected: &str) {
    let s = doc.to_string();
    assert_eq!(s, expected);
    let value = doc.clone().to_toml_value();
    assert_eq!(value, expected.parse::<Value>().unwrap());
}

#[test]
fn test_index() {
    let doc = TomlDocument::from_str("a = 1").unwrap();
    let a = &doc["a"];
    assert_eq!(a.as_integer(), Some(1));

    let doc = TomlDocument::from_str(
        "
        a.b.c = 1
        a.d = 2
    ",
    )
    .unwrap();
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
    assert_eq!(dv.to_string(), txt.trim_left());
    let txt = "{a = 'foo'} # comment";
    let dv: DocValue = txt.parse().unwrap();
    assert_eq!(dv["a"].as_str(), Some("foo"));
    assert_eq!(dv.to_string(), txt);
    // TODO: Handle comments/newlines at end, when value is added to an inline table.
}

#[test]
fn remove_keys() {
    let mut doc = TomlDocument::from_str(
        "a.b.c = 1\n\
         a.d = 2\n",
    )
    .unwrap();
    let dv = doc["a"].remove("b").unwrap();
    println!("{:?}", dv.to_string());
    println!("{:?}", dv);
    // TODO: Whitespace is horribly broken here.
    // assert_eq!(dv.to_string(), "{ c = 1 }");
    assert_eq!(doc.to_string(), "a.d = 2\n");
}

#[test]
fn iter() {
    let doc = TomlDocument::from_str(
        "a.b.c = 1\n\
         a.b.d = 2\n",
    )
    .unwrap();
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
    let mut doc = TomlDocument::from_str(
        "\
root_key = 1
# a.b.c comment
[a.b.c]
key = 1
# a.b.d comment
[a.b.d]
key = 2
[a.other]
key = 3
",
    )
    .unwrap();
    doc["a"].remove("b");
    assert_eq!(
        doc.to_string(),
        "\
root_key = 1
[a.other]
key = 3
"
    );
}

#[test]
fn test_insert_value() {
    let mut doc = TomlDocument::new();
    doc.insert("a".parse().unwrap(), "1".parse().unwrap())
        .unwrap();
    assert_eq!(doc.to_string(), "a = 1\n");
    doc.insert(
        "  b  ".parse().unwrap(),
        "  2  # Comment\n".parse().unwrap(),
    )
    .unwrap();
    assert_eq!(doc.to_string(), "a = 1\nb = 2  # Comment\n");

    let mut doc = TomlDocument::new();
    doc.insert("a.b.c".parse().unwrap(), "true".parse().unwrap())
        .unwrap();
    assert_eq!(doc.to_string(), "a.b.c = true\n");
}

#[test]
fn test_entry_vacant_insert() {
    // Manually specifying the new table style.
    let mut doc = TomlDocument::new();
    doc.entry("foo")
        .or_insert_with(|| DocValue::new_standard_table());
    matches(&doc, "[foo]\n");

    doc.entry(DocKey::from_str("a.b.c").unwrap())
        .or_insert_with(|| DocValue::new_standard_table());
    // TODO: Move to function, that also checks to_toml_value
    matches(&doc, "[a.b.c]\n[foo]\n");

    doc.entry(DocKey::from_str("x.y.z").unwrap())
        .or_insert_with(|| DocValue::new_inline_table());
    println!("{:#?}", doc);
    matches(&doc, "x.y.z = {}\n[a.b.c]\n[foo]\n");

    // Insert into an existing, empty table.
    let mut doc = new_doc("x = {}");
    doc["x"].entry("y").or_insert_with(|| "1".parse().unwrap());
    matches(&doc, "x = { y = 1 }");
    // Try inserting an std table into an inline.
    doc["x"]
        .entry("std")
        .or_insert_with(|| DocValue::new_standard_table());
    matches(&doc, "x = { y = 1, std = {} }");

    let mut doc = new_doc("x = {foo=1}");
    doc["x"].entry("y").or_insert_with(|| "1".parse().unwrap());
    matches(&doc, "x = { foo=1, y = 1 }");

    // Insert basic key into std table.
    let mut doc = new_doc("[a]");
    doc["a"].entry("k1").or_insert_with(|| "1".parse().unwrap());
    matches(&doc, "[a]\nk1 = 1\n");

    // Try inserting into an std table.
    let mut doc = new_doc("[a.b]");
    doc["a"]
        .entry("c")
        .or_insert_with(|| DocValue::new_standard_table());
    matches(&doc, "[a.b]\n[a.c]\n");

    let mut doc = new_doc("[a.b]");
    doc["a"]
        .entry("c")
        .or_insert_with(|| DocValue::new_inline_table());
    matches(&doc, "[a.b]\n[a]\nc = {}\n");
}
