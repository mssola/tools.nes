use std::fmt;

// TODO: global error
// TODO: more errors, the `parse` thing is a hack!
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
    pub parse: bool,
}

impl std::error::Error for ParseError {}

impl From<std::io::Error> for ParseError {
    fn from(err: std::io::Error) -> Self {
        // TODO
        ParseError {
            line: 0,
            message: err.to_string(),
            parse: true,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parser (line {}): {}.", self.line + 1, self.message)
    }
}
