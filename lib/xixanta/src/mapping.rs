use crate::instruction::{Fill, Node, PString};
use std::collections::HashMap;

use crate::errors::ParseError;
type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Segment {
    pub name: String,
    pub start: u16,
    pub size: usize,
    pub fill_value: Option<Fill>,
}

#[derive(Debug)]
pub struct Mapping {
    pub segments: Vec<Segment>,
    pub nodes: HashMap<String, Vec<Node>>,
    current: String,
}

impl Mapping {
    pub fn new(mut segments: Vec<Segment>) -> Self {
        segments.sort_by(|a, b| b.start.cmp(&a.start));

        let mut nodes = HashMap::new();
        for segment in segments.iter() {
            nodes.insert(segment.name.clone(), vec![]);
        }

        let current_segment = &segments.first().unwrap().name.clone();

        Mapping {
            segments,
            nodes,
            current: current_segment.to_string(),
        }
    }

    pub fn reset(&mut self) {
        // TODO
    }

    pub fn switch(&mut self, id: &PString) -> Result<()> {
        if !self.nodes.contains_key(&id.value) {
            return Err(
                id.parser_error(format!("segment '{}' has not been defined", id.value).as_str())
            );
        }

        id.value.clone_into(&mut self.current);
        Ok(())
    }

    pub fn current(&self) -> &Vec<Node> {
        self.nodes.get(&self.current).unwrap()
    }

    pub fn current_mut(&mut self) -> &mut Vec<Node> {
        self.nodes.get_mut(&self.current).unwrap()
    }

    pub fn push(&mut self, node: Node) {
        self.nodes.get_mut(&self.current).unwrap().push(node);
    }
}
