use super::{Bytes, GetSize};
use crate::ast::Type;

/// A helper struct for creating memory layout for classes.
pub struct Layout {
    current_size: i32,
}

impl Layout {
    /// Creates a new layout with the given initial size.
    /// Passing initial size is useful when inheriting fields from a parent class.
    pub fn new(initial_size: i32) -> Self {
        Self {
            current_size: initial_size,
        }
    }

    fn pad(&mut self, size: Bytes) {
        let bytes = i32::from(size);
        let padding = match self.current_size % bytes {
            0 => 0,
            n => bytes - n,
        };

        self.current_size += padding;
    }

    /// Adds a new entry (e.g. [`super::class::Field`] of a [`super::class::Class`]) and returns its offset.
    /// Assures that the new entry is aligned to its size.
    pub fn add_entry(&mut self, of_type: Type<'_>) -> i32 {
        self.pad(of_type.size());
        let offset = self.current_size;
        self.current_size += i32::from(of_type.size());

        offset
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::SimpleType;

    #[test]
    fn pad() {
        let mut layout = Layout::new(8);
        layout.pad(Bytes::B1);
        assert_eq!(layout.current_size, 8);

        let mut layout = Layout::new(4);
        layout.pad(Bytes::B4);
        assert_eq!(layout.current_size, 4);
        layout.pad(Bytes::B8);
        assert_eq!(layout.current_size, 8);

        let mut layout = Layout::new(2);
        layout.pad(Bytes::B4);
        assert_eq!(layout.current_size, 4);
        layout.pad(Bytes::B8);
        assert_eq!(layout.current_size, 8);
    }

    #[test]
    fn add_entry() {
        let mut layout = Layout::new(8);

        let entries = [
            layout.add_entry(Type::BOOL),
            layout.add_entry(Type::STR),
            layout.add_entry(Type::class("asd")),
            layout.add_entry(Type::INT),
            layout.add_entry(Type::BOOL),
            layout.add_entry(Type::Arr(SimpleType::Bool)),
            layout.add_entry(Type::INT),
        ];
        assert_eq!(entries, [8, 16, 24, 32, 36, 40, 48]);
        assert_eq!(layout.current_size, 52);

        layout.pad(Bytes::B8);
        assert_eq!(layout.current_size, 56);
    }
}
