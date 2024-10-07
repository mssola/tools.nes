use crate::errors::ParseError;
use crate::instruction::PString;
use crate::opcodes::{CONTROL_FUNCTIONS, INSTRUCTIONS};
use std::io::{self, BufRead, Read};
use std::ops::Range;

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Value,
    Instruction,
    Indirection,
    Assignment,
    Control,
    Literal,
    Identifier,
    Label,
    Call,
    Empty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PNode {
    pub node_type: NodeType,
    pub value: PString,
    pub left: Option<Box<PNode>>,
    pub right: Option<Box<PNode>>,
    pub args: Option<Vec<Box<PNode>>>,
}

impl PNode {
    pub fn empty() -> PNode {
        Self {
            node_type: NodeType::Empty,
            value: PString::new(),
            left: None,
            right: None,
            args: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    line: usize,
    column: usize,
    offset: usize,
    pub nodes: Vec<Box<PNode>>,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            line: 0,
            column: 0,
            offset: 0,
            nodes: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn reset(&mut self) {
        self.line = 0;
        self.column = 0;
        self.offset = 0;
        self.nodes = Vec::new();
        self.errors = Vec::new();
    }

    pub fn parse(&mut self, reader: impl Read) -> Result<()> {
        for line in io::BufReader::new(reader).lines() {
            if let Err(err) = self.parse_line(line?.as_str()) {
                self.errors.push(err);
            }
            self.line += 1;
        }

        println!("NODES: {:#?}", self.nodes);

        match self.errors.last() {
            Some(err) => Err(err.clone()),
            None => Ok(()),
        }
    }

    fn parse_line(&mut self, line: &str) -> Result<()> {
        self.column = 0;

        // Skip until the first non-whitespace character. If that's not
        // possible, then it's an empty line and we can return early.
        if !self.skip_whitespace(line) {
            return Ok(());
        }

        // Let's pin point the last character we need to care for parsing. This
        // can be either the start position of an inline comment (i.e. ';'), or
        // the real line end.
        let end = if let Some(comment) = line.find(|c: char| c == ';') {
            comment
        } else {
            line.len()
        };

        // It's safe to trim the end of the resulting string. Moreover, doing so
        // can already show lines which are actually empty (e.g. a line which
        // simply contains a comment). If this is the case, just return an empty
        // node.
        let mut l = line.get(self.column..end).unwrap_or_default().trim_end();
        if l.is_empty() {
            return Ok(());
        }

        // Fetch the first element of the line, which we will call it an
        // "identifier" but might be a label or a statement. The label might be
        // followed by more code. Hence, push it first, then fetch the next
        // identifier and finally fall through.
        self.offset = 0;
        let (mut id, mut nt) = self.parse_identifier(l)?;
        if nt == NodeType::Label {
            self.nodes.push(Box::new(PNode {
                node_type: nt,
                value: id,
                left: None,
                right: None,
                args: None,
            }));

            self.skip_whitespace(l);

            // Is it the label alone? If so return early.
            l = line.get(self.column..end).unwrap_or_default().trim_end();
            if l.is_empty() {
                return Ok(());
            }

            // The label is followed by a statement. Let's parse the identifier
            // for it and fall through.
            self.offset = 0;
            (id, nt) = self.parse_identifier(l)?;
            if nt == NodeType::Label {
                return Err(self.parser_error("cannot have multiple labels at the same location"));
            }

            self.skip_whitespace(l);
        }

        self.parse_statement(l, id)
    }

    fn parse_identifier(&mut self, line: &str) -> Result<(PString, NodeType)> {
        let start = self.column;
        let base_offset = self.offset;

        // For the general case we just need to iterate until a whitespace
        // character or an inline comment is found. Then our PString object
        // is merely whatever is on the column..self.column range.
        for c in line.get(self.offset..).unwrap_or("").chars() {
            if c.is_whitespace() || c == ':' || c == '(' || c == ')' || c == '=' {
                let val = String::from(line.get(base_offset..self.offset).unwrap_or("").trim());
                let nt = if c == ':' {
                    NodeType::Label
                } else {
                    NodeType::Value
                };

                // TODO
                // self.next();
                let end = if c == ':' {
                    self.next();
                    self.column - 1
                } else {
                    self.column
                };

                return Ok((
                    PString {
                        value: val,
                        line: self.line,
                        range: Range {
                            start,
                            end, // TODO
                                 // end: self.column - 1,
                        },
                    },
                    nt,
                ));
            } else if !c.is_alphanumeric() && c != '_' {
                // TODO: on the contrary, if alphanumeric or _, just follow
                // through. Otherwise always break. TODO NOT REALLY
                // return Err(self.parser_error("bad character for possible identifier"));
            }

            self.next();
        }

        // The line is merely the identifier (e.g. instruction with implied
        // addressing).
        let id = String::from(line.get(base_offset..).unwrap_or("").trim());
        Ok((
            PString {
                value: id,
                line: self.line,
                range: Range {
                    start,
                    end: self.column,
                },
            },
            NodeType::Value,
        ))
    }

    fn parse_statement(&mut self, line: &str, id: PString) -> Result<()> {
        // There are only two top-level statements: instructions and
        // assignments. Other kinds of expressions can also be used in the
        // middle of assignments or instructions, and so they have to be handled
        // as common expressions. Whether expressions make sense at the
        // different levels is something to be figured out by the assembler.
        match INSTRUCTIONS.get(&id.value) {
            Some(_) => self.parse_instruction(line, id),
            None => {
                if line.contains('=') {
                    self.parse_assignment(line, id)
                } else {
                    let node = self.parse_expression_with_identifier(id, line)?;
                    self.nodes.push(node);
                    Ok(())
                }
            }
        }
    }

    fn parse_instruction(&mut self, line: &str, id: PString) -> Result<()> {
        let mut paren = 0;

        self.skip_whitespace(line);

        if line.contains("=") {
            return Err(self.parser_error(
                format!("cannot used reserved name for the mnemonic '{}'", id.value).as_str(),
            ));
        }

        let indirect = line.chars().nth(self.offset).unwrap_or(',') == '(';
        let l = if indirect {
            self.next();
            self.skip_whitespace(line);
            paren = self.find_matching_paren(line, self.offset)?;
            line.get(self.offset..paren).unwrap_or_default()
        } else {
            line.get(self.offset..).unwrap_or_default()
        };

        self.offset = 0;
        let mut left = if l.is_empty() {
            None
        } else {
            Some(self.parse_left_arm(l)?)
        };

        self.skip_whitespace(l);

        // The parsing of the left arm should have advanced the offset right
        // into the right arm. If there is nothing there, then we have no right
        // arm. Otherwise we have to parse the expression.
        // TODO
        let mut right_str = l.get(self.offset..).unwrap_or_default();
        let mut right = if right_str.is_empty() {
            None
        } else {
            self.offset = 0;
            Some(self.parse_expression(right_str)?)
        };

        if indirect {
            if left.is_none() {
                return Err(self.parser_error("empty indirect addressing"));
            }

            left = Some(Box::new(PNode {
                node_type: NodeType::Indirection,
                value: PString::new(),
                left,
                right: right.clone(),
                args: None,
            }));

            right_str = line.get(paren..).unwrap_or_default();
            if !(right_str.is_empty() || right_str == ")") && right.is_some() {
                return Err(self.parser_error("bad indirect addressing"));
            }

            right = if right_str.is_empty() || right_str == ")" {
                None
            } else {
                self.offset = 0;

                // TODO: ")  ,"
                self.next();
                self.skip_whitespace(right_str);

                // TODO: ",    "
                self.next();
                self.skip_whitespace(right_str);

                Some(self.parse_expression(right_str)?)
            };
        }

        self.nodes.push(Box::new(PNode {
            node_type: NodeType::Instruction,
            value: id,
            left,
            right,
            args: None,
        }));

        Ok(())
    }

    fn parse_assignment(&mut self, line: &str, id: PString) -> Result<()> {
        if let Err(msg) = id.is_valid_identifier() {
            return Err(self.parser_error(&msg));
        }

        // Skip whitespaces and make sure that we have a '=' sign.
        self.skip_whitespace(line);
        if line.chars().nth(self.offset).unwrap_or(' ') != '=' {
            return Err(self.parser_error(format!("unknown instruction '{}'", id.value).as_str()));
        }

        // Skip the '=' sign and any possible whitespaces.
        self.next();
        self.skip_whitespace(line);

        // Parse the expression on the right side of the assignment.
        let rest = line.get(self.offset..).unwrap_or("").trim_end();
        if rest.is_empty() {
            return Err(self.parser_error("incomplete assignment"));
        };
        self.offset = 0;
        let left = Some(self.parse_expression(rest)?);

        self.nodes.push(Box::new(PNode {
            node_type: NodeType::Assignment,
            value: id.clone(),
            left,
            right: None,
            args: None,
        }));

        Ok(())
    }

    fn parse_arguments(&mut self, line: &str) -> Result<Vec<Box<PNode>>> {
        // Skip any possible whitespace before the optional opening paren.
        self.skip_whitespace(line);

        // Scope the end of the argument list. If the arguments are enclosed on
        // parenthesis, take that into account, otherwise we will parse until
        // the end of the cleaned line.
        let paren = line.chars().nth(self.offset).unwrap_or_default() == '(';
        let end = if paren {
            self.next();
            self.skip_whitespace(line);
            self.find_matching_paren(line, self.offset)?
        } else {
            line.len()
        };

        let mut args = Vec::new();

        loop {
            // TODO: trimmed_str out?
            let trimmed_str = line.get(..end).unwrap_or_default().trim_end();
            println!("TRIMME: {:#?}", trimmed_str.get(self.offset..));

            let (arg_end, comma) = self.find_left_end(trimmed_str, false)?;
            let arg_untrimmed = line.get(self.offset..arg_end).unwrap_or_default();
            let arg = arg_untrimmed.trim_end();
            let diff = arg_untrimmed.len() - arg.len();
            // .trim_end();
            println!(
                "ARG_END: {:#?} -- ARG: {:#?} - DIFF: {}",
                arg_end, arg, diff
            );
            if arg.is_empty() {
                break;
            }
            // if !comma {
            //     s = arg.to_owned() + " ";
            //     arg = s.as_str();
            // }

            self.offset = 0;
            args.push(self.parse_expression(arg)?);

            self.offset = arg_end;
            self.column += diff;
            println!("{:#?}", line.get(self.offset..end));
            self.skip_whitespace(line); // TODO
            if comma {
                self.next();
                self.skip_whitespace(line);
            }
        }

        println!("ARGS: {:#?}", args);

        Ok(args)
    }

    fn parse_left_arm(&mut self, line: &str) -> Result<Box<PNode>> {
        let start_column = self.column;

        // We track the start value of the offset and we will keep track of the
        // movement of it on `end`. This allows us to preserve the value on
        // inner calls that might modify the offset value.
        // TODO
        let (end, comma) = self.find_left_end(line, false)?;

        // Set the offset to 0 since we are constraining the string to be
        // parsed.
        let str = line.get(..end).unwrap_or_default().trim_end();
        self.offset = 0;

        // Parse the expression that we can get from the current offset to the
        // computed end.
        let expr = self.parse_expression(str);

        // Set the offset to the end of the line that is shared with the caller.
        // let diff_column = (end - start) - (self.column - start_column);
        self.offset = end;
        self.column = start_column + end;
        if comma {
            self.next();
        }
        expr
    }

    // TODO: revisit inside_paren
    fn find_left_end(&self, line: &str, inside_paren: bool) -> Result<(usize, bool)> {
        let mut idx = self.offset;
        let mut parens = if inside_paren { 1 } else { 0 };
        let mut comma = false;

        for c in line.get(self.offset..).unwrap_or_default().chars() {
            if c == ',' {
                if parens == 0 {
                    comma = true;
                    break;
                }
            } else if c == '(' {
                parens += 1;
            } else if c == ')' {
                parens -= 1;
            }

            idx += 1;

            if parens < 0 {
                return Err(self.parser_error("too many closing parenthesis"));
            }
        }
        if parens > 0 {
            return Err(self.parser_error("unclosed parenthesis"));
        }

        Ok((idx, comma))
    }

    fn find_matching_paren(&self, line: &str, init: usize) -> Result<usize> {
        let mut idx = init;
        let mut parens = 1;

        for c in line.get(init..).unwrap_or_default().chars() {
            if c == '(' {
                parens += 1;
            } else if c == ')' {
                parens -= 1;
            }

            if parens == 0 {
                return Ok(idx);
            } else if parens < 0 {
                return Err(self.parser_error("too many closing parenthesis"));
            }

            idx += 1;
        }
        if parens > 0 {
            return Err(self.parser_error("unclosed parenthesis"));
        }

        Ok(idx)
    }

    // Parse the expression under `line`. Indeces such as `self.column` and
    // `self.offset` are assumed to be correct at this point for the given
    // `line` (e.g. the line might not be a full line but rather a limited range
    // and the offset has been set accordingly). Returns a new node for the
    // expression at hand.
    fn parse_expression(&mut self, line: &str) -> Result<Box<PNode>> {
        let (id, nt) = self.parse_identifier(line)?;

        if nt == NodeType::Label {
            Err(self.parser_error("not expecting a label defined here"))
        } else {
            self.parse_expression_with_identifier(id, line)
        }
    }

    // Parse the expression under `line` by taking into consideration that a
    // part of it has already been parsed and evaluated as the given `id`.
    // Indeces such as `self.column` and `self.offset` are assumed to be correct
    // at this point. Returns a new node for the expression at hand.
    fn parse_expression_with_identifier(&mut self, id: PString, line: &str) -> Result<Box<PNode>> {
        // Reaching this condition is usually a bad sign, but there is so many
        // ways in which it could go wrong, that an `assert!` wouldn't be fair
        // either. Hence, just error out.
        if !id.is_valid() {
            return Err(self.parser_error("invalid identifier"));
        }

        if id.value.starts_with(".") {
            self.parse_control(id, line)
        } else if line.starts_with('$') || line.starts_with('#') || line.starts_with('%') {
            self.parse_literal(id, line)
        } else {
            // If there is an indication that it might be a macro call, process
            // it as such.
            self.skip_whitespace(line);
            if !line
                .get(self.offset..)
                .unwrap_or_default()
                .trim_end()
                .is_empty()
            {
                let args = self.parse_arguments(line)?;
                return Ok(Box::new(PNode {
                    node_type: NodeType::Call,
                    value: id,
                    left: None,
                    right: None,
                    args: if args.is_empty() { None } else { Some(args) },
                }));
            }

            // Blindly return the identifier as a PNode. This might be either a
            // value as-is, or a macro call which we can't make sense at the
            // moment. Eitherway, let the assembler decide.
            Ok(Box::new(PNode {
                node_type: NodeType::Value,
                value: id,
                left: None,
                right: None,
                args: None,
            }))
        }
    }

    // Returns a NodeType::Control node with whatever could be parsed
    // considering the given `id` and rest of the `line`.
    fn parse_control(&mut self, id: PString, line: &str) -> Result<Box<PNode>> {
        let mut left = None;
        let required;

        // Ensure that this is a function that we know of. In the past this was
        // not done and it brought too many problems that made the more
        // "abstract" way of handling this just too complicated.
        if let Some(control) = CONTROL_FUNCTIONS.get(&id.value.to_lowercase()) {
            required = control.required_args;

            // If this control function has an identifier (e.g. `.macro
            // Identifier(args...)`), let's parse it now.
            if control.has_identifier {
                self.skip_whitespace(line);
                left = Some(Box::new(PNode {
                    node_type: NodeType::Value,
                    value: self.parse_identifier(line)?.0,
                    left: None,
                    right: None,
                    args: None,
                }));
            }
        } else {
            return Err(self.parser_error(format!("unknown function '{}'", id.value).as_str()));
        }

        // At this point we reached the arguments (i.e. any identifier required
        // by the control function has already been parsed and set in `left`).
        // Then, just parse the arguments and ensure that it matches the amount
        // required by the function.
        let args = self.parse_arguments(line)?;
        if let Some(args_required) = required {
            if args.len() != args_required {
                return Err(self.parser_error(
                    format!("wrong number of arguments for function '{}'", id.value).as_str(),
                ));
            }
        }

        Ok(Box::new(PNode {
            node_type: NodeType::Control,
            value: id,
            left,
            right: None,
            args: if args.is_empty() { None } else { Some(args) },
        }))
    }

    // Returns a NodeType::Literal node with whatever could be parsed
    // considering the given `id` and rest of the `line`.
    fn parse_literal(&mut self, id: PString, line: &str) -> Result<Box<PNode>> {
        // Force the column to point to the literal character just in case
        // of expressions like '#.hibyte'. Then skip whitespaces for super
        // ugly statements such as '# 20'. This is ugly but we should permit
        // it. A later linter can yell at a programmer for this.
        self.column = id.range.start;
        self.offset = 0;
        self.next();
        self.skip_whitespace(line);

        // With this, just fetch the inner expression and return the literal
        // node.
        let inner = line.get(self.offset..).unwrap_or("");
        self.offset = 0;
        let left = self.parse_expression(inner)?;

        Ok(Box::new(PNode {
            node_type: NodeType::Literal,
            value: id,
            left: Some(left),
            right: None,
            args: None,
        }))
    }

    // Returns a new ParseError by using the current line.
    fn parser_error(&self, msg: &str) -> ParseError {
        ParseError {
            message: String::from(msg),
            line: self.line,
            parse: true,
        }
    }

    // Advances `self.column` and `self.offset` until a non-whitespace character
    // is found. Note that the initial index is bound to `self.offset`. Returns
    // false if the line can be skipped entirely, true otherwise.
    fn skip_whitespace(&mut self, line: &str) -> bool {
        if line.is_empty() {
            return false;
        }

        for c in line.get(self.offset..).unwrap_or("").chars() {
            if !c.is_whitespace() {
                if c == ';' {
                    return false;
                }
                return true;
            }

            self.next();
        }

        true
    }

    // Increment `self.column` and `self.offset` by one.
    fn next(&mut self) {
        self.column += 1;
        self.offset += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_one_valid(parser: &mut Parser, line: &str) {
        assert!(parser.parse(line.as_bytes()).is_ok());
        assert!(parser.nodes.len() == 1);
    }

    fn assert_node(node: &Box<PNode>, nt: NodeType, line: &str, value: &str) {
        assert_eq!(node.node_type, nt);
        assert_eq!(
            node.value.value.as_str(),
            line.get(node.value.range.clone()).unwrap()
        );
        assert_eq!(node.value.value.as_str(), value);
    }

    // Empty

    #[test]
    fn empty_line() {
        let mut parser = Parser::new();
        assert!(!parser.parse("".as_bytes()).is_err());
        assert_eq!(parser.nodes.len(), 0);
    }

    #[test]
    fn spaced_line() {
        let mut parser = Parser::new();
        assert!(!parser.parse("   ".as_bytes()).is_err());
        assert_eq!(parser.nodes.len(), 0);
    }

    #[test]
    fn just_a_comment_line() {
        for line in vec![";; This is a comment", "    ;; Comment"].into_iter() {
            let mut parser = Parser::new();
            assert!(!parser.parse(line.as_bytes()).is_err());
            assert_eq!(parser.nodes.len(), 0);
        }
    }

    // Labels

    #[test]
    fn anonymous_label() {
        let mut parser = Parser::new();
        assert!(parser.parse(":".as_bytes()).is_ok());
        assert_eq!(parser.nodes.len(), 1);
        assert!(parser.nodes.first().unwrap().value.value.is_empty());
        assert_eq!(parser.nodes.first().unwrap().value.range.start, 0);
        assert_eq!(parser.nodes.first().unwrap().value.range.end, 0);

        parser = Parser::new();
        assert!(parser.parse("  :".as_bytes()).is_ok());
        assert_eq!(parser.nodes.len(), 1);
        assert!(parser.nodes.first().unwrap().value.value.is_empty());
        assert_eq!(parser.nodes.first().unwrap().value.range.start, 2);
        assert_eq!(parser.nodes.first().unwrap().value.range.end, 2);
    }

    #[test]
    fn named_label() {
        let mut parser = Parser::new();
        assert!(parser.parse("label:".as_bytes()).is_ok());
        assert_eq!(parser.nodes.len(), 1);
        assert_eq!(parser.nodes.first().unwrap().value.value, "label");
        assert_eq!(parser.nodes.first().unwrap().value.range.start, 0);
        assert_eq!(parser.nodes.first().unwrap().value.range.end, 5);

        parser = Parser::new();
        assert!(parser.parse("  label:".as_bytes()).is_ok());
        assert_eq!(parser.nodes.len(), 1);
        assert_eq!(parser.nodes.first().unwrap().value.value, "label");
        assert_eq!(parser.nodes.first().unwrap().value.range.start, 2);
        assert_eq!(parser.nodes.first().unwrap().value.range.end, 7);
    }

    #[test]
    fn label_with_instruction() {
        let line = "label: dex";

        let mut parser = Parser::new();
        assert!(parser.parse(line.as_bytes()).is_ok());
        assert_eq!(parser.nodes.len(), 2);

        // Label.
        assert_eq!(parser.nodes.first().unwrap().value.value, "label");
        assert_eq!(parser.nodes.first().unwrap().value.range.start, 0);
        assert_eq!(parser.nodes.first().unwrap().value.range.end, 5);

        // Instruction
        assert_node(
            parser.nodes.last().unwrap(),
            NodeType::Instruction,
            line,
            "dex",
        )
    }

    // Literals

    #[test]
    fn parse_pound_literal() {
        for line in vec!["#20", " #20 ", "  #20   ; Comment", "  label:   # 20"].into_iter() {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_eq!(node.node_type, NodeType::Literal);
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Value);
            assert_eq!(left.value.value, "20");
            assert_eq!(line.get(left.value.range).unwrap(), "20");
        }
    }

    #[test]
    fn parse_compound_literal() {
        let line = "#$20";
        let mut parser = Parser::new();
        assert!(parser.parse(line.as_bytes()).is_ok());

        let node = parser.nodes.last().unwrap();
        assert_eq!(node.node_type, NodeType::Literal);
        assert!(node.right.is_none());
        assert!(node.args.is_none());

        let inner = node.left.clone().unwrap();
        assert_eq!(inner.node_type, NodeType::Literal);
        assert_eq!(inner.value.value, "$20");
        assert_eq!(line.get(inner.value.range).unwrap(), "$20");

        let innerinner = inner.left.clone().unwrap();
        assert_eq!(innerinner.node_type, NodeType::Value);
        assert_eq!(innerinner.value.value, "20");
        assert_eq!(line.get(innerinner.value.range).unwrap(), "20");
    }

    #[test]
    fn parse_variable_in_literal() {
        let line = "#Variable";
        let mut parser = Parser::new();
        assert!(parser.parse(line.as_bytes()).is_ok());

        let node = parser.nodes.last().unwrap();
        assert_eq!(node.node_type, NodeType::Literal);
        assert!(node.right.is_none());
        assert!(node.args.is_none());

        let inner = node.left.clone().unwrap();
        assert_eq!(inner.node_type, NodeType::Value);
        assert_eq!(inner.value.value, "Variable");
        assert_eq!(line.get(inner.value.range).unwrap(), "Variable");
    }

    // Regular instructions.

    #[test]
    fn instruction_with_implied() {
        for line in vec![
            "dex",
            "    dex",
            " dex  ",
            "  dex   ; Comment",
            "  label:   dex",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "dex");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn instruction_with_implied_explicit() {
        for line in vec!["inc a", "    inc a", " inc  a  "].into_iter() {
            let mut parser = Parser::new();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.first().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(&node.left.clone().unwrap(), NodeType::Value, line, "a");
        }
    }

    #[test]
    fn instruction_with_zeropage() {
        for line in vec!["inc $20", "    inc   $20", " inc  $20  "].into_iter() {
            let mut parser = Parser::new();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.first().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(&node.left.clone().unwrap(), NodeType::Literal, line, "$20");
        }
    }

    #[test]
    fn instruction_with_immediate() {
        for line in vec!["adc #$20", "  adc #$20  ", "  adc   #$20  "].into_iter() {
            let mut parser = Parser::new();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.first().unwrap();
            assert_node(node, NodeType::Instruction, line, "adc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(&node.left.clone().unwrap(), NodeType::Literal, line, "#$20");
        }
    }

    #[test]
    fn instruction_with_absolute() {
        for line in vec!["inc $2002", "    inc   $2002", " inc  $2002  "].into_iter() {
            let mut parser = Parser::new();
            assert_one_valid(&mut parser, line);

            let node = parser.nodes.first().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            assert_node(
                &node.left.clone().unwrap(),
                NodeType::Literal,
                line,
                "$2002",
            );
        }
    }

    #[test]
    fn instruction_with_absolute_x() {
        for line in vec![
            "inc $2002, x",
            "    inc   $2002, x",
            " inc  $2002,    x  ",
            " label:   inc $2002,   x  ; Comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "inc");
            assert!(node.args.is_none());

            assert_node(
                &node.left.clone().unwrap(),
                NodeType::Literal,
                line,
                "$2002",
            );
            assert_node(&node.right.clone().unwrap(), NodeType::Value, line, "x");
        }
    }

    #[test]
    fn indirect_addressing_bare() {
        for line in vec![
            "lda ($2000)",
            "    lda   ( $2000 )  ; Comment",
            "  : lda (   $2000)",
            "lda($2000)",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Indirection);
            assert_node(&left.left.unwrap(), NodeType::Literal, line, "$2000");
            assert!(left.right.is_none());
        }
    }

    #[test]
    fn indirect_addressing_x() {
        for line in vec![
            "lda ($20, x)",
            "    lda ($20,   x)",
            " lda   ($20,x)  ",
            "  : lda ($20  ,   x) ; Comment",
            "  lda (  $20  ,   x  )  ",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Indirection);
            assert_node(&left.left.unwrap(), NodeType::Literal, line, "$20");
            assert_node(&left.right.unwrap(), NodeType::Value, line, "x");
        }
    }

    #[test]
    fn bad_indirect_addressing_x() {
        let mut parser = Parser::new();

        let err = parser.parse("lda (Variable, x), y".as_bytes());
        assert_eq!(err.unwrap_err().message, "bad indirect addressing");
    }

    #[test]
    fn indirect_addressing_y() {
        for line in vec!["lda ($20), y"].into_iter() {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");

            let left = node.left.clone().unwrap();
            assert_eq!(left.node_type, NodeType::Indirection);
            assert_node(&left.left.unwrap(), NodeType::Literal, line, "$20");
            assert!(left.right.is_none());

            let right = node.right.clone().unwrap();
            assert_node(&right, NodeType::Value, line, "y");
        }
    }

    #[test]
    fn variable_in_instruction() {
        let line = "lda Variable, x";
        let mut parser = Parser::new();
        assert!(parser.parse(line.as_bytes()).is_ok());

        let node = parser.nodes.last().unwrap();
        assert_node(node, NodeType::Instruction, line, "lda");
        assert!(node.args.is_none());

        assert_node(
            &node.left.clone().unwrap(),
            NodeType::Value,
            line,
            "Variable",
        );
        assert_node(&node.right.clone().unwrap(), NodeType::Value, line, "x");
    }

    #[test]
    fn variable_literal_in_instruction() {
        let line = "lda #Variable, x";
        let mut parser = Parser::new();
        assert!(parser.parse(line.as_bytes()).is_ok());

        let node = parser.nodes.last().unwrap();
        assert_node(node, NodeType::Instruction, line, "lda");
        assert!(node.args.is_none());

        assert_node(
            &node.left.clone().unwrap(),
            NodeType::Literal,
            line,
            "#Variable",
        );
        assert_node(&node.right.clone().unwrap(), NodeType::Value, line, "x");
    }

    // Assignments

    #[test]
    fn bad_assignments() {
        let mut parser = Parser::new();

        let mut err = parser.parse("abc = $10".as_bytes());
        assert_eq!(
            err.unwrap_err().message,
            "cannot use names which are valid hexadecimal values such as 'abc'"
        );

        parser = Parser::new();
        err = parser.parse("var =".as_bytes());
        assert_eq!(err.unwrap_err().message, "incomplete assignment");

        parser = Parser::new();
        err = parser.parse("var =   ".as_bytes());
        assert_eq!(err.unwrap_err().message, "incomplete assignment");

        parser = Parser::new();
        err = parser.parse("var =   ; Comment".as_bytes());
        assert_eq!(err.unwrap_err().message, "incomplete assignment");
    }

    // Control statements.

    #[test]
    fn parse_control_no_args() {
        for line in vec![".end", "    .end", " label: .end   ; Comment"].into_iter() {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Control, line, ".end");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn parse_control_one_arg() {
        for line in vec![
            ".hibyte $2000",
            "    .hibyte $2000",
            " label: .hibyte $2000   ; Comment",
            "  .hibyte($2000)",
            "  .hibyte (  $2000  )",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Control, line, ".hibyte");
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2000");
        }
    }

    #[test]
    fn parse_control_multiple_args() {
        for line in vec![
            ".byte $10, $20",
            "    .byte $10, $20",
            " label: .byte $10, $20   ; Comment",
            "  .byte($10, $20)",
            "  .byte (  $10 , $20  )",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Control, line, ".byte");
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 2);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$10");
            assert_node(args.last().unwrap(), NodeType::Literal, line, "$20");
        }
    }

    #[test]
    fn parse_control_id_no_args() {
        for line in vec![
            ".scope Scope",
            "    .scope Scope",
            " label: .scope Scope   ; Comment",
            "  .scope   Scope",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Control, line, ".scope");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Value, line, "Scope");
        }
    }

    #[test]
    fn parse_control_id_one_arg() {
        for line in vec![
            ".macro Macro(arg1)",
            ".macro Macro arg1 ",
            "    .macro   Macro(arg1)",
            " label: .macro Macro(arg1)   ; Comment",
            "  .macro   Macro  ( arg1 )",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Control, line, ".macro");
            assert!(node.right.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Value, line, "Macro");

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
        }
    }

    #[test]
    fn parse_control_id_multiple_args() {
        for line in vec![
            ".macro Macro(arg1, arg2)",
            ".macro Macro arg1, arg2 ",
            "    .macro   Macro(arg1, arg2)",
            " label: .macro Macro(arg1, arg2)   ; Comment",
            "  .macro   Macro  ( arg1 , arg2 )",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Control, line, ".macro");
            assert!(node.right.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Value, line, "Macro");

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 2);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
            assert_node(args.last().unwrap(), NodeType::Value, line, "arg2");
        }
    }

    #[test]
    fn parse_control_bad_number_args() {
        for line in vec![".hibyte", ".hibyte($20, $22)"].into_iter() {
            let mut parser = Parser::new();
            assert_eq!(
                parser.parse(line.as_bytes()).unwrap_err().message,
                "wrong number of arguments for function '.hibyte'"
            );
        }
    }

    #[test]
    fn parse_control_in_instructions() {
        for line in vec!["lda #.hibyte($2010)", "  label:   lda #.hibyte   $2010   "].into_iter() {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(&control, NodeType::Control, line, ".hibyte");
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");
        }
    }

    #[test]
    fn parse_control_in_indirect_x_instructions() {
        for line in vec![
            "lda (#.hibyte($2010), x)",
            " label:   lda (#.hibyte ( $2010 ) , x)",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let ind = node.left.clone().unwrap();
            assert_eq!(ind.node_type, NodeType::Indirection);
            assert!(ind.args.is_none());

            let left = ind.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(&control, NodeType::Control, line, ".hibyte");
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");

            let right = ind.right.clone().unwrap();
            assert_node(&right, NodeType::Value, line, "x");
        }
    }

    #[test]
    fn parse_control_in_indirect_y_instructions() {
        for line in vec![
            "lda (#.hibyte($2010)), y",
            "  label:   lda ( #.hibyte( $2010  ) )  , y",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Instruction, line, "lda");
            assert!(node.args.is_none());

            let ind = node.left.clone().unwrap();
            assert_eq!(ind.node_type, NodeType::Indirection);
            assert!(ind.right.is_none());
            assert!(ind.args.is_none());

            let left = ind.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(&control, NodeType::Control, line, ".hibyte");
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");

            let right = node.right.clone().unwrap();
            assert_node(&right, NodeType::Value, line, "y");
        }
    }

    #[test]
    fn parse_control_in_assignments() {
        for line in vec![
            "lala = #.hibyte($2010)",
            "  lala   =     #.hibyte($2010)",
            "label:  lala   =     #.hibyte($2010)  ; comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Assignment, line, "lala");
            assert!(node.right.is_none());
            assert!(node.args.is_none());

            let left = node.left.clone().unwrap();
            assert_node(&left, NodeType::Literal, line, "#.hibyte");
            assert!(left.right.is_none());
            assert!(left.args.is_none());

            let control = left.left.clone().unwrap();
            assert_node(&control, NodeType::Control, line, ".hibyte");
            assert!(control.left.is_none());
            assert!(control.right.is_none());

            let args = control.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Literal, line, "$2010");
        }
    }

    #[test]
    fn parse_unknown_control() {
        let mut parser = Parser::new();
        assert_eq!(
            parser.parse(".".as_bytes()).unwrap_err().message,
            "unknown function '.'"
        );

        parser = Parser::new();
        assert_eq!(
            parser.parse(".whatever".as_bytes()).unwrap_err().message,
            "unknown function '.whatever'"
        );
    }

    // Macro calls.

    #[test]
    fn parse_macro_call_no_args_variable_lookalike() {
        for line in vec![
            "MACRO_CALL",
            " MACRO_CALL ",
            " label:  MACRO_CALL  ; comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Value, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn parse_macro_call_no_args() {
        for line in vec![
            "MACRO_CALL()",
            " MACRO_CALL() ",
            " MACRO_CALL () ",
            " MACRO_CALL (  ) ",
            " label:  MACRO_CALL ()  ; comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Call, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());
            assert!(node.args.is_none());
        }
    }

    #[test]
    fn parse_macro_call_one_arg() {
        for line in vec![
            "MACRO_CALL(arg1)",
            "MACRO_CALL arg1 ",
            "    MACRO_CALL (arg1)",
            " label: MACRO_CALL( arg1 )   ; Comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Call, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 1);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
        }
    }

    #[test]
    fn parse_macro_call_multiple_args() {
        for line in vec![
            "MACRO_CALL(arg1, arg2)",
            "MACRO_CALL arg1, arg2 ",
            "    MACRO_CALL (arg1,arg2)",
            " label: MACRO_CALL( arg1 , arg2 )   ; Comment",
        ]
        .into_iter()
        {
            let mut parser = Parser::new();
            assert!(parser.parse(line.as_bytes()).is_ok());

            let node = parser.nodes.last().unwrap();
            assert_node(node, NodeType::Call, line, "MACRO_CALL");
            assert!(node.left.is_none());
            assert!(node.right.is_none());

            let args = node.args.clone().unwrap();
            assert_eq!(args.len(), 2);
            assert_node(args.first().unwrap(), NodeType::Value, line, "arg1");
            assert_node(args.last().unwrap(), NodeType::Value, line, "arg2");
        }
    }
}
