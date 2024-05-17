use crate::context::Context;
use crate::errors::ParseError;
use crate::instruction::{
    AddressingMode, Encodable, Generic, Instruction, Literal, Node, PString, Scoped,
};
use crate::opcodes::{INSTRUCTIONS, OPCODES};
use std::collections::hash_map::Entry;
use std::io::{self, BufRead, Read};
use std::ops::Range;

type Result<T> = std::result::Result<T, ParseError>;

pub struct Assembler {
    line: usize,
    column: usize,
    offset_address: usize,
    context: Context,

    // TODO: segments instead
    nodes: Vec<Node>,
}

impl Default for Assembler {
    fn default() -> Self {
        Assembler::new()
    }
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            line: 0,
            column: 0,
            offset_address: 0,
            context: Context::new(),
            nodes: vec![],
        }
    }

    pub fn reset(&mut self) {
        self.line = 0;
        self.column = 0;
        self.offset_address = 0;
        self.nodes = vec![];
        self.context = Context::new();
    }

    pub fn to_nodes(&mut self, reader: impl Read) -> Result<&Vec<Node>> {
        self.from_reader(reader)?;
        self.context.reset();
        self.evaluate()?;

        Ok(&self.nodes)
    }

    pub fn assemble(&mut self, reader: impl Read) -> Result<Vec<&dyn Encodable>> {
        let mut instructions: Vec<&dyn Encodable> = vec![];

        for node in self.to_nodes(reader)? {
            match node {
                Node::Instruction(instr) => instructions.push(instr),
                Node::Literal(lit) => instructions.push(lit),
                _ => {}
            }
        }

        Ok(instructions)
    }

    pub fn disassemble(&mut self, reader: impl Read) -> Result<Vec<&dyn Encodable>> {
        self.from_byte_reader(reader)?;

        let mut instructions: Vec<&dyn Encodable> = vec![];
        for node in &self.nodes {
            match node {
                Node::Instruction(instr) => instructions.push(instr),
                Node::Literal(lit) => instructions.push(lit),
                _ => {}
            }
        }

        Ok(instructions)
    }

    pub fn from_reader<R: Read>(&mut self, reader: R) -> Result<()> {
        for line in io::BufReader::new(reader).lines() {
            // TODO: instead of this, accumulate errors so to give as many
            // errors as possible.
            self.parse_line(line?.as_str())?;
            self.line += 1;
        }
        Ok(())
    }

    pub fn parse_line(&mut self, line: &str) -> Result<()> {
        self.column = 0;

        if !self.skip_whitespace(line) {
            return Ok(());
        }

        match self.parse_identifier(line) {
            Some(identifier) => self.parse_from_identifier(identifier, line),
            None => Ok(()),
        }
    }

    pub fn evaluate(&mut self) -> Result<()> {
        for node in &mut self.nodes {
            match node {
                Node::Instruction(instr) => {
                    Self::update_instruction_with_context(instr, &self.context)?
                }
                Node::Scoped(scope) => {
                    if scope.start {
                        self.context.push(&scope.identifier.value);
                    } else {
                        _ = self.context.pop();
                    }
                }
                Node::Literal(literal) => {
                    Self::update_literal_with_context(literal, &self.context)?
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn update_instruction_with_context(instr: &mut Instruction, context: &Context) -> Result<()> {
        // To keep things simple, we remove out the `implied` case and we parse
        // further with a known `Some` value for the base algorithm implemented
        // in `update_instruction_and_bytes`.
        if instr.left.is_some() {
            Self::update_addressing_and_bytes(instr, context)?;
        } else {
            instr.mode = AddressingMode::Implied;
        }

        // Now that we have the addressing mode and the bytes, we can fill out
        // the rest of it by fetching the values on `INSTRUCTIONS`.
        match INSTRUCTIONS.get(&instr.mnemonic.value.to_lowercase()) {
            Some(entries) => match entries.get(&instr.mode) {
                Some(values) => {
                    instr.cycles = values.cycles;
                    instr.opcode = values.opcode;
                    instr.size = values.size;
                    instr.affected_on_page = values.affected_on_page;
                }
                None => {
                    return Err(instr.mnemonic.parser_error(
                        format!(
                            "bad addressing mode '{}' for the instruction '{}'",
                            &instr.mode, &instr.mnemonic.value
                        )
                        .as_str(),
                    ));
                }
            },
            None => {
                return Err(instr.mnemonic.parser_error(
                    format!("unknown instruction '{}'", &instr.mnemonic.value).as_str(),
                ));
            }
        }

        Ok(())
    }

    fn update_addressing_and_bytes(instr: &mut Instruction, context: &Context) -> Result<()> {
        // `unwrap()` is guaranteed to work by the caller.
        let left = instr.left.as_ref().unwrap();

        // We will first try to check if there's any variable involved on the
        // left arm and replace the string if so. This will greatly simplify
        // things down the line. That being said, there is a special reserved
        // case, which is the implied addressing by using "a". In this case, we
        // want to ensure that we assume an implied addressing and not a
        // variable named "a".
        if left.value.to_lowercase() == "a" {
            instr.mode = AddressingMode::Implied;
        } else {
            let nleft = Self::replace_variable(left, context)?;

            if nleft.value.starts_with('$') {
                // This is an address. At this point we should assume that the
                // left node contains the address itself, and that the right one
                // will contain whether there is indexing.

                let string = nleft.value.chars().as_str();
                instr.bytes = Self::parse_hex_from(string, &nleft, true, false, true)?;

                match &instr.right {
                    Some(xy) => match xy.value.to_lowercase().as_str() {
                        "x" => {
                            if string.len() == 3 {
                                instr.mode = AddressingMode::ZeropageIndexedX;
                            } else {
                                instr.mode = AddressingMode::IndexedX;
                            }
                        }
                        "y" => {
                            if string.len() == 3 {
                                instr.mode = AddressingMode::ZeropageIndexedY;
                            } else {
                                instr.mode = AddressingMode::IndexedY;
                            }
                        }
                        _ => return Err(xy.parser_error("index is neither X nor Y")),
                    },
                    None => {
                        if string.len() == 3 {
                            instr.mode = AddressingMode::RelativeOrZeropage;
                        } else {
                            instr.mode = AddressingMode::Absolute;
                        }
                    }
                }
            } else if nleft.value.starts_with('#') {
                // Immediate addressing in any case: hexadecimal, binary or
                // decimal. Hence, just figure out the character being used and
                // call the right function for it.

                let mut chars = nleft.value.chars();
                chars.next();
                let string = chars.as_str();

                instr.bytes = Self::parse_numeric(string, &nleft, false)?;
                instr.mode = AddressingMode::Immediate;
            } else if nleft.value.starts_with('(') {
                // Indirect addressing. In this case the left arm can be further
                // subdivided. That is, indirect X-indexing is represented like
                // so: `instr ($NN, x)`. Hence, first of all we have to figure
                // out whether there is a subdivision.

                let (left1, oleft2) = Self::split_left_arm(&nleft)?;
                match oleft2 {
                    Some(left2) => {
                        // There is subdivision. Thus, we have to assume
                        // indirect X-indexing, which means that the right arm
                        // should be None and that the right side of the left
                        // node must match the X register. Other than that, the
                        // address being referenced must be zero page.
                        if instr.right.is_some() {
                            return Err(instr.right.as_ref().unwrap().parser_error(
                                "bad indirect mode, expecting an indirect X-indexed addressing mode"
                            ));
                        }
                        if left2.value.to_lowercase() != "x" {
                            return Err(left2.parser_error(
                                "the index in indirect X-indexed addressing must be X",
                            ));
                        }
                        match Self::parse_hex_from(&left1.value, &left1, false, false, true) {
                            Ok(bytes) => instr.bytes = bytes,
                            Err(e) => {
                                let msg = String::from(
                                    "when parsing an instruction with indirect X-indexed addressing: ",
                                ) + &e.message;
                                return Err(left1.parser_error(msg.as_str()));
                            }
                        }
                        instr.mode = AddressingMode::IndirectX;
                    }
                    None => {
                        // There is no subdivision on the left arm. Hence, if
                        // there is something on the right arm then we must
                        // assume indirect Y-index addressing, and if not then
                        // it's indirect addressing with no indices involvved.
                        if instr.right.is_some() {
                            if instr.right.as_ref().unwrap().value.to_lowercase() != "y" {
                                return Err(instr.right.as_ref().unwrap().parser_error(
                                    "the index in indirect Y-indexed addressing must be Y",
                                ));
                            }
                            match Self::parse_hex_from(&left1.value, &left1, false, false, true) {
                                Ok(bytes) => instr.bytes = bytes,
                                Err(e) => {
                                    let msg = String::from(
                                    "when parsing an instruction with indirect Y-indexed addressing: ",
                                ) + &e.message;
                                    return Err(left1.parser_error(msg.as_str()));
                                }
                            }
                            instr.mode = AddressingMode::IndirectY;
                        } else {
                            instr.bytes =
                                Self::parse_hex_from(&left1.value, &left1, true, true, true)?;
                            instr.mode = AddressingMode::Indirect;
                        }
                    }
                }
            } else {
                // At this point all of the syntax cases have been exhausted:
                // the programmer messed up. From this point on we try to figure
                // out how they messed up.

                if nleft.value.starts_with('=') {
                    return Err(instr.mnemonic.parser_error(
                        format!(
                            "cannot use '{}' in an assignment because it's a word reserved for an instruction mnemonic",
                            instr.mnemonic.value
                        ).as_str(),
                    ));
                }
                return Err(instr.mnemonic.parser_error(
                    format!(
                        "unknown addressing mode for instruction '{}'",
                        instr.mnemonic.value
                    )
                    .as_str(),
                ));
            }
        }

        Ok(())
    }

    fn update_literal_with_context(literal: &mut Literal, context: &Context) -> Result<()> {
        // Evaluate any possible variable being used inside of this literal.
        let evaled = Self::replace_variable(&literal.identifier, context)?;

        // Parse the numeric value after a possible variable has been replaced.
        let two_bytes_allowed = literal.size == 2;
        let res = Self::parse_numeric(
            evaled.value.as_str(),
            &literal.identifier,
            two_bytes_allowed,
        );

        // And finally assign the computed bytes.
        match res {
            Ok(bytes) => {
                literal.bytes = bytes;
                Ok(())
            }
            Err(e) => {
                let msg = String::from("when parsing a data literal: ") + &e.message;
                Err(literal.identifier.parser_error(msg.as_str()))
            }
        }
    }

    fn parse_numeric(string: &str, node: &PString, two_bytes_allowed: bool) -> Result<[u8; 2]> {
        if string.starts_with('$') {
            Ok(Self::parse_hex_from(
                string,
                node,
                two_bytes_allowed,
                false,
                true,
            )?)
        } else if string.starts_with('%') {
            Ok([Self::parse_binary_from(string, node)?, 0])
        } else {
            Ok([Self::parse_decimal_from(string, node)?, 0])
        }
    }

    fn split_left_arm(node: &PString) -> Result<(PString, Option<PString>)> {
        let mut chars = node.value.chars();
        chars.next();
        let string = chars.as_str();

        match string.find(|c: char| c == ',') {
            Some(idx) => {
                let left1 = string.get(..idx).unwrap_or("").trim();
                let left2 = string.get(idx + 1..).unwrap_or("").trim();

                Ok((
                    PString {
                        value: left1.to_string(),
                        line: node.line,
                        range: Range {
                            start: node.range.start + 1,
                            end: node.range.start + 1 + left1.len(),
                        },
                    },
                    Some(PString {
                        value: left2.to_string(),
                        line: node.line,
                        range: Range {
                            start: node.range.start + 1 + idx,
                            end: node.range.start + 1 + idx + left2.len(),
                        },
                    }),
                ))
            }
            None => Ok((
                PString {
                    value: string.to_string(),
                    line: node.line,
                    range: Range {
                        start: node.range.start + 1,
                        end: node.range.end,
                    },
                },
                None,
            )),
        }
    }

    fn parse_binary_from(string: &str, node: &PString) -> Result<u8> {
        let mut value = 0;
        let mut shift = 0;

        for c in string.get(1..).unwrap_or("").chars().rev() {
            if c == '1' {
                let val = 1 << shift;
                value += val;
            } else if c != '0' {
                return Err(
                    node.parser_error(format!("bad binary format for '{}'", string).as_str())
                );
            }

            shift += 1;
        }

        if shift < 8 {
            Err(node.parser_error("missing binary digits to get a full byte"))
        } else if shift > 8 {
            Err(node.parser_error("too many binary digits for a single byte"))
        } else {
            Ok(value)
        }
    }

    fn replace_variable(node: &PString, context: &Context) -> Result<PString> {
        match node.value.find(|c: char| c.is_alphabetic() || c == '_') {
            Some(idx) => {
                // Before doing any replacement, let's check the character
                // before the one that was found. In this case, if it was a
                // proper ASCII digit, then it cannot be a variable but it's
                // part of a numeric literal (e.g. '1A'): then just let the
                // different numeric parsing functions do their job.
                if idx > 0 {
                    let prev = node.value.chars().nth(idx - 1).unwrap_or(' ');
                    if prev.is_ascii_digit() {
                        return Ok(node.clone());
                    }
                }

                // The variable might still be before an inner comma (e.g.
                // sta ($20, x)). We will assume that variables can happen
                // only before that.
                let end = node.value.find(',').unwrap_or(node.value.len());
                let mut string = node.value.get(idx..end).unwrap_or("");
                let tail = node.value.get(end..).unwrap_or("");

                // Get the context that might be being referenced.
                let ctxt = match string.find("::") {
                    Some(_) => {
                        let tctxt = string.rsplit_once("::").unwrap_or(("", ""));
                        if tctxt.0.is_empty() {
                            context.current()
                        } else {
                            string = tctxt.1;
                            context.find(tctxt.0)
                        }
                    }
                    None => context.current(),
                };

                match ctxt {
                    Some(hash) => {
                        // If there was a comma before the "variable" (i.e. idx >
                        // end and hence string == ""), or this is just the regular
                        // X or Y index, just return early.
                        match string.to_lowercase().as_str() {
                            "x" | "y" | "" => return Ok(node.clone()),
                            _ => {}
                        }

                        // It's not any of the indices, let's look for a match on
                        // the current scope.
                        match hash.get(string) {
                            Some(var) => {
                                let value = String::from(node.value.get(..idx).unwrap_or(""))
                                    + var.value.as_str();
                                Ok(PString {
                                    value: value.clone() + tail,
                                    line: node.line,
                                    range: Range {
                                        start: node.range.start,
                                        end: node.range.start + value.len(),
                                    },
                                })
                            }
                            None => {
                                // If a variable could not be found, check that
                                // this is not a purely hexadecimal number (e.g.
                                // 'AA'). If that's the case, then just return
                                // its value.
                                if Self::parse_hex_from(string, node, true, false, false).is_ok() {
                                    return Ok(node.clone());
                                }

                                // We've tried hard to not assume the programmer
                                // messing up, but there's no other way around
                                // it: it's an "unknown variable" error.
                                return Err(node.parser_error(
                                    format!("unknown variable '{}'", string).as_str(),
                                ));
                            }
                        }
                    }
                    None => {
                        Err(node.parser_error(format!("unknown scope '{}'", "Global").as_str()))
                    }
                }
            }
            None => Ok(node.clone()),
        }
    }

    // Parse an hexadecimal value from the given `string`. A `node` must also be
    // supplied so a ParseError can be pushed inside of it in case anything goes
    // wrong. Other than that, there are three boolean parameters that have to
    // be passed:
    //   - `two_bytes_allowed`: the value can be 8-bit or 16-bit long.
    //   - `exactly_two_bytes`: the value has to be exactly 16-bit long.
    //   - `char_given`: whether the string starts with a '$' character or not.
    fn parse_hex_from(
        string: &str,
        node: &PString,
        two_bytes_allowed: bool,
        exactly_two_bytes: bool,
        char_given: bool,
    ) -> Result<[u8; 2]> {
        let mut chars = string.chars();
        let len = if char_given {
            chars.next();
            string.len() - 1
        } else {
            string.len()
        };

        match len {
            2 => {
                if exactly_two_bytes {
                    return Err(node.parser_error("expecting a full 16-bit address"));
                }
                let mut i = Self::char_to_hex(node, chars.next())? * 16;
                i += Self::char_to_hex(node, chars.next())?;

                Ok([i, 0])
            }
            4 => {
                if !two_bytes_allowed {
                    return Err(node.parser_error("only one byte of data is allowed here"));
                }

                let mut hi = Self::char_to_hex(node, chars.next())? * 16;
                hi += Self::char_to_hex(node, chars.next())?;

                let mut lo = Self::char_to_hex(node, chars.next())? * 16;
                lo += Self::char_to_hex(node, chars.next())?;

                Ok([lo, hi])
            }
            _ => {
                if two_bytes_allowed {
                    Err(node.parser_error("expecting a number of 2/4 hexadecimal digits"))
                } else if exactly_two_bytes {
                    Err(node.parser_error("expecting a number of 4 hexadecimal digits"))
                } else {
                    Err(node.parser_error("expecting a number of 2 hexadecimal digits"))
                }
            }
        }
    }

    fn char_to_hex(node: &PString, oc: Option<char>) -> Result<u8> {
        match oc {
            Some(c) => match c.to_digit(16) {
                Some(c) => Ok(c as u8),
                None => Err(node.parser_error("could not convert digit to hexadecimal")),
            },
            None => Err(node.parser_error("digit out of bounds")),
        }
    }

    fn parse_decimal_from(string: &str, node: &PString) -> Result<u8> {
        let mut value = 0;
        let mut shift = 1;

        if string.is_empty() {
            return Err(node.parser_error("empty decimal literal"));
        }

        for c in string.chars().rev() {
            if shift > 100 {
                return Err(node.parser_error("decimal value is too big"));
            }
            if c != '0' {
                match c.to_digit(10) {
                    Some(digit) => {
                        value += digit * shift;
                    }
                    None => {
                        return Err(
                            node.parser_error(format!("'{}' is not a decimal value", c).as_str())
                        )
                    }
                }
            }

            shift *= 10;
        }
        if value > 255 {
            return Err(node.parser_error("decimal value is too big"));
        }

        Ok(value as u8)
    }

    // Advances `self.column` until a non-whitespace character is found. Returns
    // false if the line can be skipped entirely, true otherwise.
    fn skip_whitespace(&mut self, line: &str) -> bool {
        for c in line.get(self.column..).unwrap_or("").chars() {
            if !c.is_whitespace() {
                if c == ';' {
                    return false;
                }
                return true;
            }

            self.column += 1;
        }

        true
    }

    // Returns a PString object which holds the information for an identifier.
    //
    // NOTE: this function assumes that `self.column` points to a non-whitespace
    // character.
    fn parse_identifier(&mut self, line: &str) -> Option<PString> {
        let column = self.column;

        // For the general case we just need to iterate until a whitespace
        // character or an inline comment is found. Then our PString object
        // is merely whatever is on the column..self.column range.
        for c in line.get(column..).unwrap_or("").chars() {
            if c.is_whitespace() || c == ';' {
                let range = Range {
                    start: column,
                    end: self.column,
                };

                return Some(PString {
                    value: String::from(line.get(range.clone()).unwrap_or("").trim()),
                    line: self.line,
                    range,
                });
            }

            self.column += 1;
        }

        // Otherwise, we might be at a point whether there is nothing (e.g. an
        // empty line), or the line is merely the identifier (e.g. instruction
        // with implied addressing).
        let range = Range {
            start: column,
            end: line.len(),
        };
        let id = String::from(line.get(range.clone()).unwrap_or("").trim());
        if id.is_empty() {
            None
        } else {
            Some(PString {
                value: id,
                line: self.line,
                range,
            })
        }
    }

    // Given a PString object which acts as the identifier, try to parse the
    // rest of the line depending on whether it's an instruction (e.g. `adc
    // $20`), a control statement (e.g. `.macro whatever`) or a general
    // statement (e.g. `Var = $10`).
    fn parse_from_identifier(&mut self, id: PString, line: &str) -> Result<()> {
        if id.value.starts_with('.') {
            // If the statement starts with a '.', it's guaranteed to be a
            // control statement.
            self.parse_control(id, line)
        } else {
            // Otherwise, we will parse it either as an instruction or a general
            // statement depending on whether the parsed identifier is a valid
            // instruction mnemonic or not.
            match INSTRUCTIONS.get(&id.value) {
                Some(_instr) => self.parse_instruction(id, line),
                None => self.parse_statement(id, line),
            }
        }
    }

    // TODO:
    //   - functions
    //   - macros
    fn parse_control(&mut self, id: PString, line: &str) -> Result<()> {
        match id.value.to_lowercase().as_str() {
            ".scope" => self.parse_scope_definition(&id, line),
            ".endscope" => self.parse_scope_end(&id),
            ".byte" | ".db" => self.parse_literal_bytes(&id, line, false),
            ".word" | ".dw" => self.parse_literal_bytes(&id, line, true),
            _ => {
                return Err(
                    id.parser_error(format!("unknown control statement '{}'", id.value).as_str())
                )
            }
        }
    }

    fn parse_scope_definition(&mut self, id: &PString, line: &str) -> Result<()> {
        self.skip_whitespace(line);

        let identifier = self.fetch_identifier(id, line)?;
        if identifier.is_reserved() {
            return Err(identifier.parser_error(
                format!("cannot use reserved name '{}'", identifier.value).as_str(),
            ));
        }
        self.context.push(&identifier.value);
        self.nodes.push(Node::Scoped(Scoped {
            identifier,
            start: true,
        }));

        Ok(())
    }

    fn parse_scope_end(&mut self, id: &PString) -> Result<()> {
        if !self.context.pop() {
            return Err(id.parser_error("missmatched '.endscope': there is no scope to end"));
        }
        self.nodes.push(Node::Scoped(Scoped {
            identifier: PString::new(),
            start: false,
        }));

        Ok(())
    }

    fn parse_literal_bytes(
        &mut self,
        node: &PString,
        line: &str,
        two_bytes_allowed: bool,
    ) -> Result<()> {
        loop {
            self.skip_whitespace(line);

            match line.chars().nth(self.column) {
                Some(byte) => {
                    let needle = if byte == '\'' {
                        self.column += 1;
                        self.skip_whitespace(line);
                        '\''
                    } else if byte == '"' {
                        self.column += 1;
                        self.skip_whitespace(line);
                        '"'
                    } else {
                        ','
                    };

                    let idx = line
                        .get(self.column..)
                        .unwrap_or("")
                        .find(|c: char| c == needle)
                        .unwrap_or(line.len() - self.column);

                    // If this is the last character, the needle was a quote and
                    // the last char is not the needle, then it means that the
                    // quote was left open. Complain about this as well.
                    if idx == line.len() - self.column {
                        if line.chars().nth(idx).unwrap_or(' ') != needle
                            && (needle == '"' || needle == '\'')
                        {
                            return Err(node.parser_error("non-terminated quote for byte literal"));
                        }
                    }

                    // NOTE: for now we push an incomplete literal. We need the
                    // first pass to fill the context and then a second pass
                    // will evaluate each literal as needed (e.g. replacing
                    // values from variables being used in this literal).
                    let string = line.get(self.column..self.column + idx).unwrap_or(" ");
                    self.nodes.push(Node::Literal(Literal {
                        identifier: PString {
                            value: string.to_owned(),
                            line: self.line,
                            range: Range {
                                start: self.column,
                                end: self.column + idx,
                            },
                        },
                        size: if two_bytes_allowed { 2 } else { 1 },
                        bytes: [0, 0],
                    }));

                    self.column += idx;
                    for c in line.get(self.column..).unwrap_or(" ").chars() {
                        if c == ',' {
                            break;
                        }
                        if c == ';' {
                            return Ok(());
                        }
                        self.column += 1;
                    }
                    self.column += 1;
                    self.skip_whitespace(line);
                }
                None => break,
            };
        }

        Ok(())
    }

    fn fetch_identifier(&mut self, id: &PString, line: &str) -> Result<PString> {
        let idx = line
            .get(self.column..)
            .unwrap_or(" ")
            .find(|c: char| c.is_whitespace());

        match idx {
            Some(offset) => {
                let end = self.column + offset;
                let rest = line.get(end..).unwrap_or("").trim();
                if !rest.is_empty() {
                    if rest.chars().nth(0).unwrap_or(' ') != ';' {
                        return Err(id.parser_error(
                            "there should not be any further content besides the identifier",
                        ));
                    }
                }
                Ok(PString {
                    value: line.get(self.column..end).unwrap_or(" ").trim().to_string(),
                    line: self.line,
                    range: Range {
                        start: self.column,
                        end,
                    },
                })
            }
            None => Ok(PString {
                value: line.get(self.column..).unwrap_or(" ").trim().to_string(),
                line: self.line,
                range: Range {
                    start: self.column,
                    end: line.len(),
                },
            }),
        }
    }

    fn parse_statement(&mut self, id: PString, line: &str) -> Result<()> {
        if line.chars().nth(self.column).unwrap_or(' ') == ':' {
            self.parse_label(id, line)
        } else {
            self.parse_assignment(id, line)
        }
    }

    fn parse_label(&mut self, _id: PString, _line: &str) -> Result<()> {
        // TODO:

        Ok(())
    }

    fn parse_assignment(&mut self, id: PString, line: &str) -> Result<()> {
        // You cannot assign into a name which is reserved.
        if id.is_reserved() {
            return Err(
                id.parser_error(format!("cannot use reserved name '{}'", id.value).as_str())
            );
        }

        // To avoid problems down the line, you cannot assign into names which
        // are proper hexadecimal values.
        if Self::parse_hex_from(&id.value, &id, true, false, false).is_ok() {
            return Err(id.parser_error(
                format!(
                    "cannot use names which are valid hexadecimal values such as '{}'",
                    id.value
                )
                .as_str(),
            ));
        }

        // You cannot assign into scoped names: declare them into their
        // respective scopes instead.
        if id.value.contains("::") {
            return Err(id.parser_error(
                format!(
                    "the name '{}' is scoped: do not declare variables this way",
                    id.value
                )
                .as_str(),
            ));
        }

        // Skip whitespaces and make sure that we have a '=' sign.
        self.skip_whitespace(line);
        if line.chars().nth(self.column).unwrap_or(' ') != '=' {
            return Err(self.parser_error(format!("unknown instruction '{}'", id.value).as_str()));
        }

        // Skip the '=' sign and any possible whitespaces.
        self.column += 1;
        if !self.skip_whitespace(line) {
            return Err(self.parser_error("incomplete assignment"));
        }

        let l = String::from(line.get(self.column..).unwrap_or("").trim());
        if l.is_empty() {
            return Err(self.parser_error("incomplete assignment"));
        }

        // The `Context` struct pretty much guarantees that `current` and
        // `current_mut` will return something, so it's safe to ignore a
        // `None`.
        if let Some(entry) = self.context.current_mut() {
            match entry.entry(id.value.clone()) {
                Entry::Occupied(e) => {
                    return Err(ParseError {
                        line: self.line,
                        message: format!(
                        "variable '{}' is being re-assigned: it was previously defined in line {}",
                        id.value, e.get().clone().line),
                    })
                }
                Entry::Vacant(e) => e.insert(PString {
                    value: l,
                    line: self.line,
                    range: Range {
                        start: id.range.start,
                        end: line.len(),
                    },
                }),
            };
        }

        Ok(())
    }

    fn parse_instruction(&mut self, id: PString, line: &str) -> Result<()> {
        // Parse the instruction into a `Generic` node.
        let node = self.get_generic_instruction_node(id, line)?;

        // Make sure that there is no dangling content.
        for c in line.get(self.column..).unwrap_or("").chars() {
            if c == ';' {
                break;
            }
            if !c.is_whitespace() {
                return Err(self.parser_error("only one statement is allowed per line"));
            }
            self.column += 1;
        }

        // And push a new `Instruction` object based on the parsed node. Note
        // that this instruction will be incomplete: we first need to evaluate
        // variables first in this context to be able to fully parse this
        // object.
        self.push_incomplete_instruction_from(node)
    }

    // Returns a `Generic` node with the contents that can be parsed with the
    // rest of the `line` and assuming that this is an assembly instruction
    // which is identified by `id`.
    fn get_generic_instruction_node(&mut self, id: PString, line: &str) -> Result<Generic> {
        // First of all, make sure that we are at a non-whitespace character.
        self.skip_whitespace(line);

        // Instructions have a character which split the left and the right arms
        // of the instruction. This is the character that we will use in order
        // to stop on the first loop.
        let needle_char = match line.chars().nth(self.column) {
            Some(c) => {
                if c == '(' {
                    ')'
                } else {
                    ','
                }
            }
            None => ',',
        };

        let mut column = self.column;
        let mut left = None;
        let mut right = None;
        let mut found = false;

        // First of the two loops: fetch the left arm. It iterates
        // until the needle_char is found and then initializes the `left`
        // variable with the fetched contents.
        for c in line.get(column..).unwrap_or("").chars() {
            if c == ';' {
                break;
            }
            if c == needle_char {
                self.init_positioned_maybe(&mut left, line, column, self.column);

                for inner in line.get(self.column..).unwrap_or("").chars() {
                    if inner.is_whitespace() || inner == ';' {
                        break;
                    }
                    self.column += 1;
                }

                found = true;
                break;
            }

            self.column += 1;
        }

        // If the previous loop found the needle, the we might have a right arm.
        // Otherwise, if the needle was not found but the end of the line was
        // reached, we might still need to pick up the contents that were not
        // saved in the previous loop.
        if found {
            // Ready the `column` for the right arm.
            self.skip_whitespace(line);
            column = self.column;

            // Second loop: fetch the right arm. This time it iterates until a
            // whitespace character or ';' is found.
            for c in line.get(column..).unwrap_or("").chars() {
                if c == ';' {
                    self.init_positioned_maybe(&mut right, line, column, self.column);
                    break;
                }
                self.column += 1;
            }

            // Similarly to the first loop, if this second one reached the end
            // without finding a whitespace or a ';' character, make sure that this
            // content is not ignored.
            self.init_positioned_maybe(&mut right, line, column, self.column);
        } else {
            self.init_positioned_maybe(&mut left, line, column, self.column);
        }

        Ok(Generic {
            identifier: id,
            left,
            right,
        })
    }

    // Initialize the PString object `p` with the contents of
    // `line.get(left..right)` unless it has already a `Some` value or the
    // fetched contents would result in an empty string.
    fn init_positioned_maybe(
        &mut self,
        p: &mut Option<PString>,
        line: &str,
        left: usize,
        right: usize,
    ) {
        if !p.is_none() || left == right {
            return;
        }

        let string = String::from(line.get(left..right).unwrap_or("").trim());
        if !string.is_empty() {
            *p = Some(PString {
                value: string,
                line: self.line,
                range: Range {
                    start: left,
                    end: right,
                },
            });
        }
    }

    fn push_incomplete_instruction_from(&mut self, node: Generic) -> Result<()> {
        let mut instr = Instruction::from(&node.identifier.value);
        instr.left = node.left;
        instr.right = node.right;

        self.nodes.push(Node::Instruction(instr));
        Ok(())
    }

    fn parser_error(&self, msg: &str) -> ParseError {
        ParseError {
            message: String::from(msg),
            line: self.line,
        }
    }

    pub fn from_byte_reader<R: Read>(&mut self, mut reader: R) -> Result<()> {
        loop {
            let mut buf = [0; 1];
            let n = reader.read(&mut buf)?;
            if n == 0 {
                break;
            }

            match OPCODES.get(&buf[0]) {
                Some(v) => {
                    let mut bs = [0; 2];
                    for i in 0..v.size - 1 {
                        let nn = reader.read(&mut buf)?;
                        if nn == 0 {
                            break;
                        }
                        bs[i as usize] = buf[0];
                    }
                    self.nodes.push(Node::Instruction(Instruction {
                        mnemonic: PString::from(&v.mnemonic),
                        opcode: v.opcode,
                        size: v.size,
                        bytes: bs,
                        left: None,
                        right: None,
                        mode: v.mode.to_owned(),
                        cycles: v.cycles,
                        affected_on_page: v.affected_on_page,
                    }))
                }

                None => {
                    return Err(
                        self.parser_error(format!("unknown byte '0x{:02X}'", buf[0]).as_str())
                    )
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Node::Instruction;

    fn assert_hex(one: &dyn Encodable, expected: &[u8]) {
        assert_eq!(
            one.to_hex(),
            expected
                .iter()
                .map(|x| format!("{:02X}", x))
                .collect::<Vec<_>>()
        );
    }

    fn instruction_test(
        line: &str,
        hex: &[u8],
        cycles: u8,
        affected: bool,
        skip_disassemble: bool,
    ) {
        let mut parser = Assembler::new();
        let res = parser.to_nodes(line.as_bytes());

        if res.is_err() {
            if let Err(e) = res.clone() {
                assert_eq!(
                    e,
                    ParseError {
                        line: 0,
                        message: String::from("")
                    }
                )
            }
        }

        let vec = res.unwrap();
        assert_eq!(vec.len(), 1);

        if let Instruction(instr) = &vec[0] {
            assert_hex(instr, hex);
            assert_eq!(instr.cycles, cycles);
            assert_eq!(instr.affected_on_page, affected);
        } else {
            println!("Not an instruction!");
            assert!(false);
        }

        // Now disassemble.

        if skip_disassemble {
            return;
        }

        parser.reset();
        let dis = parser.disassemble(hex);
        assert!(dis.is_ok());

        let dvec = dis.unwrap();
        assert_eq!(dvec.len(), 1);

        let dinstr = dvec[0];
        assert_eq!(dinstr.to_human(), line);
    }

    fn instruction_err(line: &str, message: &str) {
        let mut parser = Assembler::new();
        let err = parser.assemble(line.as_bytes());

        assert!(err.is_err());
        if let Err(e) = err {
            assert_eq!(e.message, message);
        }
    }

    // Mainly errors.

    #[test]
    fn bad_addressing() {
        instruction_err("unknown #$20", "unknown instruction 'unknown'");
        instruction_err(
            "adc ($2002, x)",
            "when parsing an instruction with indirect X-indexed addressing: only one byte of data is allowed here",
        );
        instruction_err(
            "adc ($2002, x), y",
            "bad indirect mode, expecting an indirect X-indexed addressing mode",
        );
        instruction_err(
            "adc ($2002), y",
            "when parsing an instruction with indirect Y-indexed addressing: only one byte of data is allowed here",
        );
        instruction_err(
            "adc ($20, y)",
            "the index in indirect X-indexed addressing must be X",
        );
        instruction_err(
            "adc ($20), x",
            "the index in indirect Y-indexed addressing must be Y",
        );
        instruction_err("jmp ($20)", "expecting a full 16-bit address");
        instruction_err("adc $20, z", "index is neither X nor Y");
        instruction_err(
            "adc ($2000)",
            "bad addressing mode 'indirect' for the instruction 'adc'",
        );
    }

    #[test]
    fn parse_binary() {
        instruction_err("adc #%", "missing binary digits to get a full byte");
        instruction_err("adc #%0001", "missing binary digits to get a full byte");
        instruction_err("adc #%0001000", "missing binary digits to get a full byte");
        instruction_err(
            "adc #%000100001",
            "too many binary digits for a single byte",
        );
        instruction_test("adc #%10100010", &[0x69, 0xA2], 2, false, true);
    }

    #[test]
    fn parse_hexadecimal() {
        instruction_err("adc $", "expecting a number of 2/4 hexadecimal digits");
        instruction_err("adc #$", "expecting a number of 2 hexadecimal digits");
        instruction_err("adc $AW", "unknown variable 'AW'");
        instruction_test("adc $AA", &[0x65, 0xAA], 3, false, false);
        instruction_test("adc $10", &[0x65, 0x10], 3, false, false);
        instruction_test("adc $10AB", &[0x6D, 0xAB, 0x10], 4, false, false);
    }

    #[test]
    fn parse_decimal() {
        instruction_err("adc #", "empty decimal literal");
        instruction_err("adc #256", "decimal value is too big");
        instruction_err("adc #2000", "decimal value is too big");
        instruction_err("adc #2A", "'A' is not a decimal value");
        instruction_test("adc #1", &[0x69, 0x01], 2, false, true);
    }

    // Individual instructions.

    #[test]
    fn adc() {
        instruction_test("adc #20", &[0x69, 0x14], 2, false, true);
        instruction_test("adc #$20", &[0x69, 0x20], 2, false, false);
        instruction_test("adc $2002", &[0x6D, 0x02, 0x20], 4, false, false);
        instruction_test("adc $20", &[0x65, 0x20], 3, false, false);
        instruction_test("adc $20, x", &[0x75, 0x20], 4, false, false);
        instruction_test("adc $2002, x", &[0x7D, 0x02, 0x20], 4, true, false);
        instruction_test("adc $2002, y", &[0x79, 0x02, 0x20], 4, true, false);
        instruction_test("adc ($20, x)", &[0x61, 0x20], 6, false, false);
        instruction_test("adc ($20), y", &[0x71, 0x20], 5, true, false);
    }

    #[test]
    fn sbc() {
        instruction_test("sbc #$20", &[0xE9, 0x20], 2, false, false);
        instruction_test("sbc $2002", &[0xED, 0x02, 0x20], 4, false, false);
        instruction_test("sbc $20", &[0xE5, 0x20], 3, false, false);
        instruction_test("sbc $20, x", &[0xF5, 0x20], 4, false, false);
        instruction_test("sbc $2002, x", &[0xFD, 0x02, 0x20], 4, true, false);
        instruction_test("sbc $2002, y", &[0xF9, 0x02, 0x20], 4, true, false);
        instruction_test("sbc ($20, x)", &[0xE1, 0x20], 6, false, false);
        instruction_test("sbc ($20), y", &[0xF1, 0x20], 5, true, false);
    }

    #[test]
    fn shift() {
        // asl
        instruction_test("asl", &[0x0A], 2, false, false);
        instruction_test("asl a", &[0x0A], 2, false, true);
        instruction_test("asl $20", &[0x06, 0x20], 5, false, false);
        instruction_test("asl $20, x", &[0x16, 0x20], 6, false, false);
        instruction_test("asl $2002", &[0x0E, 0x02, 0x20], 6, false, false);
        instruction_test("asl $2002, x", &[0x1E, 0x02, 0x20], 7, false, false);

        // lsr
        instruction_test("lsr", &[0x4A], 2, false, false);
        instruction_test("lsr a", &[0x4A], 2, false, true);
        instruction_test("lsr $20", &[0x46, 0x20], 5, false, false);
        instruction_test("lsr $20, x", &[0x56, 0x20], 6, false, false);
        instruction_test("lsr $2002", &[0x4E, 0x02, 0x20], 6, false, false);
        instruction_test("lsr $2002, x", &[0x5E, 0x02, 0x20], 7, false, false);
    }

    #[test]
    fn rotate() {
        // rol
        instruction_test("rol", &[0x2A], 2, false, false);
        instruction_test("rol a", &[0x2A], 2, false, true);
        instruction_test("rol $20", &[0x26, 0x20], 5, false, false);
        instruction_test("rol $20, x", &[0x36, 0x20], 6, false, false);
        instruction_test("rol $2002", &[0x2E, 0x02, 0x20], 6, false, false);
        instruction_test("rol $2002, x", &[0x3E, 0x02, 0x20], 7, false, false);

        // ror
        instruction_test("ror", &[0x6A], 2, false, false);
        instruction_test("ror a", &[0x6A], 2, false, true);
        instruction_test("ror $20", &[0x66, 0x20], 5, false, false);
        instruction_test("ror $20, x", &[0x76, 0x20], 6, false, false);
        instruction_test("ror $2002", &[0x6E, 0x02, 0x20], 6, false, false);
        instruction_test("ror $2002, x", &[0x7E, 0x02, 0x20], 7, false, false);
    }

    #[test]
    fn and() {
        instruction_test("and #$20", &[0x29, 0x20], 2, false, false);
        instruction_test("and $2002", &[0x2D, 0x02, 0x20], 4, false, false);
        instruction_test("and $20", &[0x25, 0x20], 3, false, false);
        instruction_test("and $20, x", &[0x35, 0x20], 4, false, false);
        instruction_test("and $2002, x", &[0x3D, 0x02, 0x20], 4, true, false);
        instruction_test("and $2002, y", &[0x39, 0x02, 0x20], 4, true, false);
        instruction_test("and ($20, x)", &[0x21, 0x20], 6, false, false);
        instruction_test("and ($20), y", &[0x31, 0x20], 5, true, false);
    }

    #[test]
    fn or() {
        // eor
        instruction_test("eor #$20", &[0x49, 0x20], 2, false, false);
        instruction_test("eor $20", &[0x45, 0x20], 3, false, false);
        instruction_test("eor $20, x", &[0x55, 0x20], 4, false, false);
        instruction_test("eor $2002", &[0x4D, 0x02, 0x20], 4, false, false);
        instruction_test("eor $2002, x", &[0x5D, 0x02, 0x20], 4, true, false);
        instruction_test("eor $2002, y", &[0x59, 0x02, 0x20], 4, true, false);
        instruction_test("eor ($20, x)", &[0x41, 0x20], 6, false, false);
        instruction_test("eor ($20), y", &[0x51, 0x20], 5, true, false);

        // ora
        instruction_test("ora #$20", &[0x09, 0x20], 2, false, false);
        instruction_test("ora $20", &[0x05, 0x20], 3, false, false);
        instruction_test("ora $20, x", &[0x15, 0x20], 4, false, false);
        instruction_test("ora $2002", &[0x0D, 0x02, 0x20], 4, false, false);
        instruction_test("ora $2002, x", &[0x1D, 0x02, 0x20], 4, true, false);
        instruction_test("ora $2002, y", &[0x19, 0x02, 0x20], 4, true, false);
        instruction_test("ora ($20, x)", &[0x01, 0x20], 6, false, false);
        instruction_test("ora ($20), y", &[0x11, 0x20], 5, true, false);
    }

    #[test]
    fn jump() {
        instruction_test("jsr $2002", &[0x20, 0x02, 0x20], 6, false, false);

        instruction_test("jmp $2002", &[0x4C, 0x02, 0x20], 3, false, false);
        instruction_test("jmp ($2002)", &[0x6C, 0x02, 0x20], 5, false, false);
    }

    #[test]
    fn inc_dec_instructions() {
        // inc
        instruction_test("inc $10", &[0xE6, 0x10], 5, false, false);
        instruction_test("inc $1000", &[0xEE, 0x00, 0x10], 6, false, false);
        instruction_test("inc $10, x", &[0xF6, 0x10], 6, false, false);
        instruction_test("inc $1000, x", &[0xFE, 0x00, 0x10], 7, false, false);

        instruction_test("inx", &[0xE8], 2, false, false);

        instruction_test("iny", &[0xC8], 2, false, false);

        // dec
        instruction_test("dec $10", &[0xC6, 0x10], 5, false, false);
        instruction_test("dec $1000", &[0xCE, 0x00, 0x10], 6, false, false);
        instruction_test("dec $10, x", &[0xD6, 0x10], 6, false, false);
        instruction_test("dec $1000, x", &[0xDE, 0x00, 0x10], 7, false, false);

        instruction_test("dex", &[0xCA], 2, false, false);

        instruction_test("dey", &[0x88], 2, false, false);
    }

    #[test]
    fn transfer_instructions() {
        instruction_test("tax", &[0xAA], 2, false, false);
        instruction_test("tay", &[0xA8], 2, false, false);
        instruction_test("tsx", &[0xBA], 2, false, false);
        instruction_test("txa", &[0x8A], 2, false, false);
        instruction_test("txs", &[0x9A], 2, false, false);
        instruction_test("tya", &[0x98], 2, false, false);
    }

    #[test]
    fn return_instructions() {
        instruction_test("rti", &[0x40], 6, false, false);
        instruction_test("rts", &[0x60], 6, false, false);
    }

    #[test]
    fn set_clear_instructions() {
        instruction_test("clc", &[0x18], 2, false, false);
        instruction_test("cld", &[0xD8], 2, false, false);
        instruction_test("cli", &[0x58], 2, false, false);
        instruction_test("clv", &[0xB8], 2, false, false);

        instruction_test("sec", &[0x38], 2, false, false);
        instruction_test("sed", &[0xF8], 2, false, false);
        instruction_test("sei", &[0x78], 2, false, false);
    }

    #[test]
    fn push_pull_instructions() {
        instruction_test("pha", &[0x48], 3, false, false);
        instruction_test("php", &[0x08], 3, false, false);
        instruction_test("pla", &[0x68], 4, false, false);
        instruction_test("plp", &[0x28], 4, false, false);
    }

    #[test]
    fn nop_brk() {
        instruction_test("nop", &[0xEA], 2, false, false);
        instruction_test("brk", &[0x00], 7, false, false);
    }

    #[test]
    fn cmp() {
        // cmp
        instruction_test("cmp #$20", &[0xC9, 0x20], 2, false, false);
        instruction_test("cmp $2002", &[0xCD, 0x02, 0x20], 4, false, false);
        instruction_test("cmp $20", &[0xC5, 0x20], 3, false, false);
        instruction_test("cmp $20, x", &[0xD5, 0x20], 4, false, false);
        instruction_test("cmp $2002, x", &[0xDD, 0x02, 0x20], 4, true, false);
        instruction_test("cmp $2002, y", &[0xD9, 0x02, 0x20], 4, true, false);
        instruction_test("cmp ($20, x)", &[0xC1, 0x20], 6, false, false);
        instruction_test("cmp ($20), y", &[0xD1, 0x20], 5, true, false);

        // cpx
        instruction_test("cpx #$20", &[0xE0, 0x20], 2, false, false);
        instruction_test("cpx $2002", &[0xEC, 0x02, 0x20], 4, false, false);
        instruction_test("cpx $20", &[0xE4, 0x20], 3, false, false);

        // cpy
        instruction_test("cpy #$20", &[0xC0, 0x20], 2, false, false);
        instruction_test("cpy $2002", &[0xCC, 0x02, 0x20], 4, false, false);
        instruction_test("cpy $20", &[0xC4, 0x20], 3, false, false);
    }

    #[test]
    fn load() {
        // lda
        instruction_test("lda #$20", &[0xA9, 0x20], 2, false, false);
        instruction_test("lda $20", &[0xA5, 0x20], 3, false, false);
        instruction_test("lda $20, x", &[0xB5, 0x20], 4, false, false);
        instruction_test("lda $2002", &[0xAD, 0x02, 0x20], 4, false, false);
        instruction_test("lda $2002, x", &[0xBD, 0x02, 0x20], 4, true, false);
        instruction_test("lda $2002, y", &[0xB9, 0x02, 0x20], 4, true, false);
        instruction_test("lda ($20, x)", &[0xA1, 0x20], 6, false, false);
        instruction_test("lda ($20), y", &[0xB1, 0x20], 5, true, false);

        // ldx
        instruction_test("ldx #$20", &[0xA2, 0x20], 2, false, false);
        instruction_test("ldx $20", &[0xA6, 0x20], 3, false, false);
        instruction_test("ldx $20, y", &[0xB6, 0x20], 4, false, false);
        instruction_test("ldx $2002", &[0xAE, 0x02, 0x20], 4, false, false);
        instruction_test("ldx $2002, y", &[0xBE, 0x02, 0x20], 4, true, false);

        // ldy
        instruction_test("ldy #$20", &[0xA0, 0x20], 2, false, false);
        instruction_test("ldy $20", &[0xA4, 0x20], 3, false, false);
        instruction_test("ldy $20, x", &[0xB4, 0x20], 4, false, false);
        instruction_test("ldy $2002", &[0xAC, 0x02, 0x20], 4, false, false);
        instruction_test("ldy $2002, x", &[0xBC, 0x02, 0x20], 4, true, false);
    }

    #[test]
    fn store_instructions() {
        //sta
        instruction_test("sta $20", &[0x85, 0x20], 3, false, false);
        instruction_test("sta $20, x", &[0x95, 0x20], 4, false, false);
        instruction_test("sta $2002", &[0x8D, 0x02, 0x20], 4, false, false);
        instruction_test("sta $2002, x", &[0x9D, 0x02, 0x20], 5, false, false);
        instruction_test("sta $2002, y", &[0x99, 0x02, 0x20], 5, false, false);
        instruction_test("sta ($20, x)", &[0x81, 0x20], 6, false, false);
        instruction_test("sta ($20), y", &[0x91, 0x20], 6, false, false);

        // stx
        instruction_test("stx $20", &[0x86, 0x20], 3, false, false);
        instruction_test("stx $20, y", &[0x96, 0x20], 4, false, false);
        instruction_test("stx $2002", &[0x8E, 0x02, 0x20], 4, false, false);

        // sty
        instruction_test("sty $20", &[0x84, 0x20], 3, false, false);
        instruction_test("sty $20, x", &[0x94, 0x20], 4, false, false);
        instruction_test("sty $2002", &[0x8C, 0x02, 0x20], 4, false, false);
    }

    #[test]
    fn bit() {
        instruction_test("bit $10", &[0x24, 0x10], 3, false, false);
        instruction_test("bit $1001", &[0x2C, 0x01, 0x10], 4, false, false);
    }

    // Variables & scopes.

    #[test]
    fn scoped_variable() {
        let mut parser = Assembler::new();
        let res = parser
            .assemble(
                r#"
.scope One   ; This is a comment
  adc #Variable

  Variable = $20
.endscope

.scope Another
  Variable = $40
.endscope

Variable = $30
adc #Variable

adc #One::Variable
adc #Another::Variable
"#
                .as_bytes(),
            )
            .unwrap();

        assert_eq!(res.len(), 4);
        let instrs: Vec<[u8; 2]> = vec![[0x69, 0x20], [0x69, 0x30], [0x69, 0x20], [0x69, 0x40]];

        for i in 0..4 {
            assert_eq!(
                res[i].to_hex(),
                instrs[i]
                    .iter()
                    .map(|x| format!("{:02X}", x))
                    .collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn redefined_variable() {
        let mut parser = Assembler::new();
        let res = parser.assemble(
            r#"
.scope One
  Variable = 1
.endscope

Variable = 1
Yet = 3
Yet = 4
"#
            .as_bytes(),
        );

        assert!(res.is_err());
        if let Err(e) = res {
            assert_eq!(
                e.message,
                "variable 'Yet' is being re-assigned: it was previously defined in line 6"
            );
        }
    }

    #[test]
    fn bad_variable_names() {
        instruction_err("a = 2", "cannot use reserved name 'a'");
        instruction_err("X = 2", "cannot use reserved name 'X'");

        instruction_err(
            "AA = 2",
            "cannot use names which are valid hexadecimal values such as 'AA'",
        );

        instruction_err(
            "Scope::Variable = 2",
            "the name 'Scope::Variable' is scoped: do not declare variables this way",
        );
    }

    #[test]
    fn bad_assignment() {
        instruction_err("Variable =", "incomplete assignment");
        instruction_err("Variable =  ; comment", "incomplete assignment");
        instruction_err("adc = $12", "cannot use 'adc' in an assignment because it's a word reserved for an instruction mnemonic");
    }

    // Literals

    #[test]
    fn byte_literals_errors() {
        instruction_err(
            ".byte $0102",
            "when parsing a data literal: only one byte of data is allowed here",
        );
        instruction_err(".byte '$01", "non-terminated quote for byte literal");
        instruction_err(".byte '$01, $02", "non-terminated quote for byte literal");
    }

    #[test]
    fn byte_literals() {
        let mut asm = Assembler::new();

        let mut res = asm.assemble(".byte $01".as_bytes()).unwrap();
        assert_eq!(res.len(), 1);
        assert_hex(res[0], &[0x01]);

        asm.reset();
        res = asm.assemble(".db $01, $02".as_bytes()).unwrap();
        assert_eq!(res.len(), 2);
        assert_hex(res[0], &[0x01]);
        assert_hex(res[1], &[0x02]);

        asm.reset();
        res = asm
            .assemble(".byte $01, 2, '%00000011', \"$04\"".as_bytes())
            .unwrap();
        assert_eq!(res.len(), 4);
        assert_hex(res[0], &[0x01]);
        assert_hex(res[1], &[0x02]);
        assert_hex(res[2], &[0x03]);
        assert_hex(res[3], &[0x04]);
    }

    #[test]
    fn word_literals() {
        let mut asm = Assembler::new();

        let mut res = asm.assemble(".word $01".as_bytes()).unwrap();
        assert_eq!(res.len(), 1);
        assert_hex(res[0], &[0x01, 0x00]);

        asm.reset();
        res = asm.assemble(".dw $0102, $02".as_bytes()).unwrap();
        assert_eq!(res.len(), 2);
        assert_hex(res[0], &[0x02, 0x01]);
        assert_hex(res[1], &[0x02, 0x00]);

        asm.reset();
        res = asm
            .assemble(".word $0102, $0204, '$0308', \"$0410\"".as_bytes())
            .unwrap();
        assert_eq!(res.len(), 4);
        assert_hex(res[0], &[0x02, 0x01]);
        assert_hex(res[1], &[0x04, 0x02]);
        assert_hex(res[2], &[0x08, 0x03]);
        assert_hex(res[3], &[0x10, 0x04]);
    }

    #[test]
    fn variables_in_literals() {
        let mut asm = Assembler::new();
        let res = asm
            .assemble(
                r#"
.scope One
  Variable = $01
.endscope

Variable = $02
.byte One::Variable, Variable, $03
"#
                .as_bytes(),
            )
            .unwrap();

        assert_eq!(res.len(), 3);
        assert_hex(res[0], &[0x01]);
        assert_hex(res[1], &[0x02]);
        assert_hex(res[2], &[0x03]);
    }
}
