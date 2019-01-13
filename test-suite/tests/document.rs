extern crate toml;

use toml::document::TomlDocument;

/*
    TODO:
    - These are things that weren't handled by the existing test suite.
      Prefer to move this to the test suite.
    - invalid-misc.rs should be moved to `invalid` directory
*/

/// Checks that the text parses and serializes to exactly the same value.
fn rt(text: &str) {
    let doc = TomlDocument::from_str(text).unwrap();
    println!("doc={:#?}", doc);
    let rendered = doc.to_string();
    assert_eq!(text, rendered);
}

/// Checks that it fails to parse.
fn fails(text: &str) {
    if let Ok(_doc) = TomlDocument::from_str(text) {
        panic!("Expected error, document passed:\n{}", text);
    }
}

#[test]
fn pass() {
    rt(r#"
[a.b.c]

[a]

c = 1
"#);
}

#[test]
fn fail() {
    fails(r#"
[a.b.c]

[a]
b.d = 1
"#);
    fails(r#"
a = {}
a.b = 1
"#);
    fails(r#"
a = {}
[a]
"#);
    fails(r#"
[a]
bin = []

[[a.bin.foo]]
k = 1
"#);
    fails(r#"
[a]
bin = [{}]

[[a.bin.foo]]
k = 1
"#);
    fails(r#"
[a]
bin = [1]

[[a.bin.foo]]
k = 1
"#);
    fails(r#"
[[a.bin.foo]]

[a.bin.foo]
k = 1
"#);
}
