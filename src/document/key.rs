use super::opt_str;
use de::{self, Deserializer, RawKey};
use std::{fmt, io::Write, str::FromStr};

#[derive(Clone, Debug)]
pub struct DocKey {
    /// True if a `[standard]` or `[[array]]` table.
    /// TODO: This needs some clarification, since keys can sometimes be
    /// demoted to regular key=value.  Maybe make sure that demotion always
    /// clears `text` and this? Consider removing this.
    pub(super) is_bracketed: bool,
    /// This is used to determine if whitespace should be added during rendering.
    /// True if it came from parsing a document.
    /// False if added via the mutation API.
    pub(super) is_original: bool,
    /// Whitespace/comments in front of the key.
    pub(super) pretext: Option<String>,
    /// Whitespace in front of the key (same line).
    pub(super) indent: Option<String>,
    /// Original text of the key.
    /// If None, the text will be generated from `parts`.
    pub(super) text: Option<String>,
    /// Parsed parts of the key, split on dots.
    pub(super) parts: Vec<String>,
    /// Whitespace after the key.
    pub(super) posttext: Option<String>,
}

impl DocKey {
    pub(super) fn new(parts: Vec<String>, text: Option<String>) -> DocKey {
        DocKey {
            is_bracketed: false,
            is_original: false,
            pretext: None,
            indent: None,
            text: text,
            parts: parts,
            posttext: None,
        }
    }

    pub(super) fn from_raw(pretext: Option<String>, raw: RawKey) -> DocKey {
        DocKey {
            is_bracketed: raw.text.starts_with("["),
            is_original: true,
            pretext: pretext,
            indent: opt_str(raw.pretext),
            text: Some(raw.text.to_string()),
            parts: raw.parts.into_iter().map(|p| p.into_owned()).collect(),
            posttext: opt_str(raw.posttext),
        }
    }

    pub fn from_str(s: &str) -> Result<DocKey, de::Error> {
        let mut d = Deserializer::new(s);
        let raw = d.dotted_key(None)?;
        let mut key = DocKey::from_raw(None, raw);
        // When parsed individually, it is typically inserted into a document,
        // in which case the rendering code should match the style of where
        // it is inserted.
        key.pretext = None;
        key.indent = None;
        key.posttext = None;
        key.is_original = false;
        Ok(key)
    }

    pub(super) fn render(&self, mut output: &mut dyn Write) {
        if let Some(pretext) = &self.pretext {
            output.write_all(pretext.as_bytes());
        }
        if let Some(indent) = &self.indent {
            output.write_all(indent.as_bytes());
        }
        if let Some(text) = &self.text {
            output.write_all(text.as_bytes());
        } else {
            // TODO: Escaping
            output.write_all(&self.parts.join(".").as_bytes());
        }
        if let Some(posttext) = &self.posttext {
            output.write_all(posttext.as_bytes());
        }
    }
}

impl fmt::Display for DocKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(text) = &self.text {
            f.write_str(text)
        } else {
            // TODO: escaping
            f.write_str(&self.parts.join("."))
        }
    }
}

impl FromStr for DocKey {
    type Err = de::Error;
    fn from_str(s: &str) -> Result<DocKey, Self::Err> {
        DocKey::from_str(s)
    }
}

impl From<&str> for DocKey {
    fn from(s: &str) -> Self {
        DocKey::new(vec![s.to_string()], None)
    }
}

impl From<String> for DocKey {
    fn from(s: String) -> Self {
        DocKey::new(vec![s], None)
    }
}
