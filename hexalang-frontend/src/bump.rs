pub struct Storage<T>
where
    T: Clone,
{
    data: Vec<T>,
}

impl<T> Default for Storage<T>
where
    T: Clone,
{
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

impl<T> Storage<T>
where
    T: Clone,
{
    pub fn allocate(&mut self, t: T) -> u32 {
        self.data.push(t);
        return (self.data.len() - 1) as u32;
    }

    pub fn receive(&self, ptr: u32) -> T {
        return self.data[ptr as usize].clone();
    }

    pub fn allocate_or(&mut self, t: Option<T>, u: u32) -> u32 {
        if let Some(t) = t {
            self.allocate(t)
        } else {
            u
        }
    }
}
