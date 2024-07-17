use crate::instruction::PString;
use std::collections::HashMap;

const GLOBAL_CONTEXT: &str = "Global";

#[derive(Debug)]
pub struct PValue {
    pub node: PString,
    pub value: usize,
    pub label: bool,
}

#[derive(Debug)]
pub struct Context {
    stack: Vec<String>,
    map: HashMap<String, HashMap<String, PValue>>,
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Context {
            stack: vec![],
            map: HashMap::from([(String::from(GLOBAL_CONTEXT), HashMap::new())]),
        }
    }

    pub fn reset(&mut self) {
        self.stack = vec![];
    }

    pub fn find(&self, name: &str) -> Option<&HashMap<String, PValue>> {
        self.map.get(name)
    }

    pub fn current(&self) -> Option<&HashMap<String, PValue>> {
        match self.stack.last() {
            Some(name) => self.map.get(name),
            None => self.map.get(GLOBAL_CONTEXT),
        }
    }

    pub fn current_mut(&mut self) -> Option<&mut HashMap<String, PValue>> {
        match self.stack.last() {
            Some(name) => self.map.get_mut(name),
            None => self.map.get_mut(GLOBAL_CONTEXT),
        }
    }

    pub fn push(&mut self, identifier: &String) {
        let name = match self.stack.last() {
            Some(n) => n.to_owned() + &String::from("::") + identifier,
            None => identifier.to_string(),
        };

        self.stack.push(name.clone());
        self.map.entry(name).or_default();
    }

    pub fn push_stack(&mut self, identifier: &String) {
        let name = match self.stack.last() {
            Some(n) => n.to_owned() + &String::from("::") + identifier,
            None => identifier.to_string(),
        };

        self.stack.push(name.clone());
    }

    pub fn pop(&mut self) -> bool {
        if self.stack.is_empty() {
            return false;
        }

        self.stack.truncate(self.stack.len() - 1);
        true
    }
}
