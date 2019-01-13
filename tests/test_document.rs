extern crate toml;

use toml::document::{TomlDocument, DocValue, DocKey};

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
    assert_eq!(dv.to_string(), txt.trim_left());
    let txt = "{a = 'foo'} # comment";
    let dv: DocValue = txt.parse().unwrap();
    assert_eq!(dv["a"].as_str(), Some("foo"));
    assert_eq!(dv.to_string(), txt);
    // TODO: Handle comments/newlines at end, when value is added to an inline table.
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

#[test]
fn test_insert_value() {
    let mut doc = TomlDocument::new();
    doc.insert("a".parse().unwrap(), "1".parse().unwrap()).unwrap();
    assert_eq!(doc.to_string(), "a = 1\n");
    doc.insert("  b  ".parse().unwrap(), "  2  # Comment\n".parse().unwrap()).unwrap();
    assert_eq!(doc.to_string(), "a = 1\nb = 2  # Comment\n");

    let mut doc = TomlDocument::new();
    doc.insert("a.b.c".parse().unwrap(), "true".parse().unwrap()).unwrap();
    assert_eq!(doc.to_string(), "a.b.c = true\n");
}

#[test]
fn test_entry() {
    let mut doc = TomlDocument::new();
    doc.entry("foo").or_insert_with(|| DocValue::new_standard_table("foo".parse().unwrap()));
    assert_eq!(doc.to_string(), "[foo]\n");

    doc.entry(DocKey::from_str("a.b.c").unwrap())
        .or_insert_with(|| DocValue::new_standard_table("a.b.c".parse().unwrap()));
    assert_eq!(doc.to_string(), "[a.b.c]\n[foo]\n");

    doc.entry(DocKey::from_str("x.y.z").unwrap())
        .or_insert_with(|| DocValue::new_inline_table());
    assert_eq!(doc.to_string(), "x.y.z = {}\n[a.b.c]\n[foo]\n");
    // doc.entry("foo");
    // doc.entry(String::from("foo"));
    // use toml::document::DocKey;
    // doc.entry("foo.bar".parse::<DocKey>().unwrap());
    // doc.entry(DocKey::from_str("foo.bar").unwrap());
    // doc.entry(DocKey::from_str("[profile.dev]").unwrap());
    // doc["[profile.dev]".parse::<DocKey>()]["lto"] = "true".parse().unwrap();
    // doc["[profile.dev]".parse::<DocKey>()]["lto"] = "true".parse().unwrap();

    // Insert into an existing, empty table.
    let t = "x = {}";
    let mut doc = TomlDocument::from_str(t).unwrap();
    assert_eq!(doc.to_string(), t);
    doc["x"].entry("y").or_insert_with(|| "1".parse().unwrap());
    assert_eq!(doc.to_string(), "x = { y = 1 }");

    let t = "x = {foo=1}";
    let mut doc = TomlDocument::from_str(t).unwrap();
    assert_eq!(doc.to_string(), t);
    doc["x"].entry("y").or_insert_with(|| "1".parse().unwrap());
    assert_eq!(doc.to_string(), "x = { foo=1, y = 1 }");
}
