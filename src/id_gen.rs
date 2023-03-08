#[derive(Default)]
pub struct IdGenerator(usize);

impl IdGenerator {
    pub fn gen(&mut self) -> usize {
        let id = self.0;
        self.0 = self.0.checked_add(1).expect("id overflow");
        id
    }
}
