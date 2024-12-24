use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Parse(ParseError),
    Context(ContextError),
    Eval(EvalError),
}

impl From<ContextError> for Vec<Error> {
    fn from(err: ContextError) -> Self {
        vec![Error::Context(err)]
    }
}

impl From<EvalError> for Vec<Error> {
    fn from(err: EvalError) -> Self {
        vec![Error::Eval(err)]
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Parse(parse_error) => write!(f, "{}", parse_error),
            Error::Context(context_error) => write!(f, "{}", context_error),
            Error::Eval(eval_error) => write!(f, "{}", eval_error),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} (line {})", self.message, self.line + 1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContextErrorReason {
    Redefinition,
    UnknownVariable,
    BadScope,
    Label,
    Bounds,
    BadStart,
    BadEnd,
    Other,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContextError {
    pub line: usize,
    pub reason: ContextErrorReason,
    pub message: String,
    pub global: bool,
}

impl std::error::Error for ContextError {}

impl fmt::Display for ContextError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.global {
            write!(f, "{}", self.message)
        } else {
            write!(f, "{} (line {})", self.message, self.line + 1)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalError {
    pub line: usize,
    pub message: String,
    pub global: bool,
}

impl std::error::Error for EvalError {}

impl From<ContextError> for EvalError {
    fn from(err: ContextError) -> Self {
        EvalError {
            line: err.line,
            message: err.message,
            global: false,
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.global {
            write!(f, "{}", self.message)
        } else {
            write!(f, "{} (line {})", self.message, self.line + 1)
        }
    }
}
