use std::error::Error;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use toml::de::{self, Fragment};

fn visit_dirs(dir: &Path, cb: &dyn Fn(&Path) -> Result<(), Box<dyn Error>>) {
    if dir.is_dir() {
        for entry in fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb);
            } else {
                if path.extension() == Some(OsStr::new("toml")) {
                    cb(&path).unwrap_or_else(|e| {
                        panic!("failed to process {:?}: {}", path, e);
                    });
                }
            }
        }
    }
}

fn round_trip_raw(path: &Path) -> Result<(), Box<dyn Error>> {
    let original = fs::read_to_string(path)?;
    let mut de = toml::Deserializer::new(&original);
    let mut output = String::new();

    fn push_raw_key(output: &mut String, key: &de::RawKey) {
        output.push_str(key.pretext);
        output.push_str(key.text);
        output.push_str(key.posttext);
    };

    while let Some(fragment) = de.fragment()? {
        match fragment {
            Fragment::Bom => output.push('\u{feff}'),
            Fragment::Whitespace(_, s) => output.push_str(s),
            Fragment::Comment(_, s) => output.push_str(s),
            Fragment::Header(h) => {
                output.push_str(h.indent);
                output.push('[');
                if h.array {
                    output.push('[');
                }
                push_raw_key(&mut output, &h.key);
                output.push(']');
                if h.array {
                    output.push(']');
                }
                output.push_str(h.posttext);
            }
            Fragment::KeyValue(kv) => {
                push_raw_key(&mut output, &kv.key);
                output.push('=');
                output.push_str(kv.value.pretext);
                output.push_str(kv.value.text);
                output.push_str(kv.value.posttext);
            }
        }
    }

    if original != output {
        return Err(format!(
            "round-trip was not equal\n--- original:\n{}\noutput:\n{}\n",
            original, output
        )
        .into());
    }
    Ok(())
}

#[test]
fn round_trip() {
    // Can recreate the original content exactly from the raw API.
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/valid");
    visit_dirs(&dir, &round_trip_raw);
}
