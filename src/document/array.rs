use super::DocValue;
use std::io::Write;

#[derive(Debug)]
pub struct DocArray {
    pub(super) is_aot: bool,
    pub(super) is_modified: bool,
    pub(super) values: Vec<DocValue>,
}

impl DocArray {
    pub fn new(is_aot: bool, values: Vec<DocValue>) -> DocArray {
        DocArray {
            is_aot,
            values,
            is_modified: false,
        }
    }

    pub fn get(&self, index: usize) -> Option<&DocValue> {
        self.values.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut DocValue> {
        self.values.get_mut(index)
    }

    pub fn remove(&mut self, index: usize) -> DocValue {
        self.is_modified = true;
        self.values.remove(index)
    }

    pub fn clear(&mut self) {
        self.is_modified = true;
        self.values.clear();
    }

    pub fn push(&mut self, value: DocValue) {
        self.is_modified = true;
        self.values.push(value);
    }

    pub(super) fn render(&self, mut output: &mut dyn Write) {
        // TODO: check `aot`
        output.write_all(b"[");
        for val in &self.values {
            val.render(output);
        }
        output.write_all(b"]");
    }
}
