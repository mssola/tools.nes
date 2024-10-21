use crate::context::Context;
use crate::errors::{Error, EvalError};
use crate::mapping::Segment;
use crate::node::{NodeType, PNode, PString};
use crate::opcodes::{AddressingMode, INSTRUCTIONS};
use crate::parser::Parser;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::Read;
use std::ops::Range;

/// A Bundle represents a set of bytes that can be encoded as binary data.
#[derive(Debug, Default, Clone)]
pub struct Bundle {
    /// The bytes which make up any encodable element for the application. The
    /// capacity is of three bytes maximum, but the actual size is encoded in
    /// the `size` property.
    pub bytes: [u8; 3],

    /// The amount of bytes which have actually been set on this bundle.
    pub size: u8,

    /// The address where the given bytes are to be placed on the resulting
    /// binary file.
    pub address: usize,

    /// If this bundle encodes an instruction, the amount of cycles it takes for
    /// the CPU to actually execute it.
    pub cycles: u8,

    /// Whether the cost in cycles is affected when crossing a page boundary.
    pub affected_on_page: bool,
}

#[derive(Clone, PartialEq)]
pub enum LiteralMode {
    Hexadecimal,
    Binary,
    Plain,
}

// TODO: is it really necessary to be this fully fledged?
#[derive(PartialEq)]
pub enum Stage {
    Init,
    Parsing,
    Context,
    Unrolling,
    Bundling,
}

#[derive(Clone, Debug)]
pub struct Macro {
    nodes: Range<usize>,
    args: Vec<PString>,
}

pub struct Assembler {
    context: Context,
    literal_mode: Option<LiteralMode>,
    stage: Stage,
    macros: HashMap<String, Macro>,
    can_bundle: bool,
}

impl Assembler {
    pub fn new(segments: Vec<Segment>) -> Self {
        assert!(!segments.is_empty());

        // TODO
        let mut offsets = HashMap::new();
        for segment in segments {
            offsets.insert(segment.name, 0);
        }

        // TODO
        Self {
            context: Context::new(),
            literal_mode: None,
            stage: Stage::Init,
            macros: HashMap::new(),
            can_bundle: true,
        }
    }

    pub fn assemble(&mut self, reader: impl Read) -> Result<Vec<Bundle>, Vec<Error>> {
        // First of all, parse the input so we get a list of nodes we can work
        // with.
        self.stage = Stage::Parsing;
        let mut parser = Parser::default();
        if let Err(errors) = parser.parse(reader) {
            return Err(errors.iter().map(|e| Error::Parse(e.clone())).collect());
        }

        // Build the context by iterating over the parsed nodes and checking
        // where scopes start/end, evaluating values for variables, labels, etc.
        self.stage = Stage::Context;
        self.eval_context(&parser.nodes)?;

        // TODO: unroll macros, fill out labels, etc.
        self.stage = Stage::Unrolling;

        // Finally convert the relevant nodes into binary bundles which can be
        // used by the caller.
        self.stage = Stage::Bundling;
        self.bundle(&parser.nodes)
    }

    pub fn eval_context(&mut self, nodes: &[PNode]) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();
        let mut current_macro = None;

        for (idx, node) in nodes.iter().enumerate() {
            // TODO: initilize labels on each scope.
            match node.node_type {
                NodeType::Assignment => {
                    // TODO: in fact, we cannot have assignments in many places.
                    if current_macro.is_some() {
                        errors.push(Error::Eval(EvalError {
                            message: "cannot have assignments inside of macro definitions"
                                .to_string(),
                            line: node.value.line,
                        }));
                        continue;
                    }
                    match self.evaluate_node(node.left.as_ref().unwrap()) {
                        Ok(value) => {
                            if let Err(err) = self.context.set_variable(&node.value, &value) {
                                errors.push(Error::Context(err));
                            }
                        }
                        Err(e) => errors.push(Error::Eval(e)),
                    }
                }
                NodeType::Control => {
                    // TODO: prevent nesting of control statements depending on
                    // a definition (e.g. .macro's cannot be nested inside of
                    // another control statement, but .if yes).
                    let id = node.value.value.as_str();

                    if id == ".macro" {
                        // TODO: macros are only on the global scope.
                        //
                        // TODO: boy this is ugly. In fact, this stupid shit if
                        // current_macro might not be relevant anymore.
                        current_macro = Some(&node.left.as_ref().unwrap().value);
                        // TODO: watch out for weird shit on the name of arguments.
                        self.macros
                            .entry(node.left.as_ref().unwrap().value.value.clone())
                            .or_insert(Macro {
                                nodes: Range {
                                    start: idx + 1,
                                    end: idx + 1,
                                },
                                args: node
                                    .args
                                    .clone()
                                    .unwrap_or_default()
                                    .into_iter()
                                    .map(|a| a.value)
                                    .collect::<Vec<_>>(),
                            });
                    } else if id == ".endmacro" {
                        // TODO: if m.nodes.start < idx - 1 => empty macro

                        if let Some(name) = current_macro {
                            self.macros
                                .entry(name.value.clone())
                                .and_modify(|m| m.nodes.end = idx - 1);
                        }
                        current_macro = None;
                    }
                    if let Err(err) = self.context.change_context(node) {
                        // TODO: forbid if inside_macro
                        errors.push(Error::Context(err));
                    }
                }
                _ => {}
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn bundle(&mut self, nodes: &Vec<PNode>) -> Result<Vec<Bundle>, Vec<Error>> {
        let mut bundles = Vec::new();
        let mut errors = Vec::new();

        for node in nodes {
            match node.node_type {
                NodeType::Instruction => {
                    if self.can_bundle {
                        match self.evaluate_node(node) {
                            Ok(bundle) => bundles.push(bundle),
                            Err(e) => errors.push(Error::Eval(e)),
                        }
                    }
                }
                NodeType::Control => {
                    if let Err(e) = self.evaluate_control_statement(node, &mut bundles) {
                        errors.push(Error::Eval(e));
                    }
                }
                NodeType::Value | NodeType::Call => {
                    if let Err(e) = self.bundle_call(node, nodes, &mut bundles) {
                        errors.push(Error::Eval(e));
                    }
                }

                _ => {}
            }
        }

        if errors.is_empty() {
            Ok(bundles)
        } else {
            Err(errors)
        }
    }

    fn bundle_call(
        &mut self,
        node: &PNode,
        nodes: &[PNode],
        bundles: &mut Vec<Bundle>,
    ) -> Result<(), EvalError> {
        // Get the macro object for the given identifier.
        let mcr = self
            .macros
            .get(&node.value.value)
            .ok_or(EvalError {
                line: node.value.line,
                message: format!(
                    "could not find a macro with the name '{}'",
                    node.value.value
                ),
            })?
            .clone();

        // Detect missmatches between the number of arguments provided and the
        // ones defined by the macro.
        let args = node.args.as_ref();
        let nargs = match args {
            Some(v) => v.len(),
            None => 0,
        };
        if mcr.args.len() != nargs {
            return Err(EvalError {
                line: node.value.line,
                message: format!(
                    "wrong number of arguments for '{}': {} required but {} given",
                    node.value.value,
                    mcr.args.len(),
                    nargs
                ),
            });
        }

        // If there are arguments defined by the macro, set their values now.
        if nargs > 0 {
            let mut margs = mcr.args.iter();

            for (idx, arg) in args.unwrap().iter().enumerate() {
                let bundle = self.evaluate_node(arg)?;
                self.context
                    .set_variable(margs.nth(idx).unwrap(), &bundle)?;
            }
        }

        // And now replicate the nodes as contained inside of the macro
        // definition.
        for node in nodes
            .get(mcr.nodes.start..=mcr.nodes.end)
            .unwrap_or_default()
        {
            bundles.push(self.evaluate_node(node)?);
        }

        Ok(())
    }

    fn evaluate_node(&mut self, node: &PNode) -> Result<Bundle, EvalError> {
        match node.node_type {
            NodeType::Instruction => Ok(self.evaluate_instruction(node)?),
            NodeType::Literal => Ok(self.evaluate_literal(node)?),
            NodeType::Control => Ok(self.evaluate_control_expression(node)?),
            NodeType::Value => match self.literal_mode {
                Some(LiteralMode::Hexadecimal) => Ok(self.evaluate_hexadecimal(node)?),
                Some(LiteralMode::Binary) => Ok(self.evaluate_binary(node)?),
                Some(LiteralMode::Plain) => Ok(self.evaluate_decimal(node)?),
                None => {
                    if self.stage == Stage::Context {
                        // If we are just evaluating the context (e.g. parsing a
                        // variable), we'll assume that non-prefixed literals are
                        // just decimal values.
                        Ok(self.evaluate_decimal(node)?)
                    } else if node.value.is_valid_identifier(true).is_err() {
                        // If this is not a valid identifier, just error out.
                        Err(EvalError {
                            message: "no prefix was given to operand".to_string(),
                            line: node.value.line,
                        })
                    } else {
                        // This is actually a valid identifier! Try to fetch the
                        // variable.
                        match self.evaluate_variable(&node.value) {
                            Ok(v) => {
                                self.literal_mode = Some(LiteralMode::Hexadecimal);
                                Ok(v)
                            }
                            Err(err) => Err(EvalError {
                                message: format!(
                                    "no prefix was given to operand and {} either",
                                    err.message
                                ),
                                line: node.value.line,
                            }),
                        }
                    }
                }
            },
            _ => Err(EvalError {
                message: format!("unexpected '{}' expression type", node.node_type),
                line: node.value.line,
            }),
        }
    }

    fn evaluate_hexadecimal(&mut self, node: &PNode) -> Result<Bundle, EvalError> {
        let mut chars = node.value.value.chars();
        let mut bytes = [0, 0, 0];
        let size: u8;

        match node.value.value.len() {
            1 => {
                bytes[0] = self.char_to_hex(chars.next(), node)?;
                size = 1;
            }
            2 => {
                bytes[0] = self.char_to_hex(chars.next(), node)? * 16;
                bytes[0] += self.char_to_hex(chars.next(), node)?;
                size = 1;
            }
            3 => {
                bytes[1] = self.char_to_hex(chars.next(), node)?;
                bytes[0] = self.char_to_hex(chars.next(), node)? * 16;
                bytes[0] += self.char_to_hex(chars.next(), node)?;
                size = 2;
            }
            4 => {
                bytes[1] = self.char_to_hex(chars.next(), node)? * 16;
                bytes[1] += self.char_to_hex(chars.next(), node)?;
                bytes[0] = self.char_to_hex(chars.next(), node)? * 16;
                bytes[0] += self.char_to_hex(chars.next(), node)?;
                size = 2;
            }
            _ => {
                if self.evaluate_variable(&node.value).is_ok() {
                    return Err(EvalError {
                        message: format!(
                            "you cannot use variables like '{}' in hexadecimal literals",
                            node.value.value
                        ),
                        line: node.value.line,
                    });
                }
                return Err(EvalError {
                    message: "expecting a number of 1 to 4 hexadecimal digits".to_string(),
                    line: node.value.line,
                });
            }
        }

        Ok(Bundle {
            bytes,
            size,
            address: 0,
            cycles: 0,
            affected_on_page: false,
        })
    }

    fn evaluate_binary(&mut self, node: &PNode) -> Result<Bundle, EvalError> {
        let string = node.value.value.as_str();
        let mut value = 0;
        let mut shift = 0;

        for c in string.chars().rev() {
            if c == '1' {
                let val = 1 << shift;
                value += val;
            } else if c != '0' {
                if self.evaluate_variable(&node.value).is_ok() {
                    return Err(EvalError {
                        message: format!(
                            "you cannot use variables like '{}' in binary literals",
                            string
                        ),
                        line: node.value.line,
                    });
                }
                return Err(EvalError {
                    message: format!("bad binary format for '{}'", string),
                    line: node.value.line,
                });
            }

            shift += 1;
        }

        match shift.cmp(&8) {
            Ordering::Less => Err(EvalError {
                message: "missing binary digits to get a full byte".to_string(),
                line: node.value.line,
            }),
            Ordering::Greater => Err(EvalError {
                message: "too many binary digits for a single byte".to_string(),
                line: node.value.line,
            }),
            Ordering::Equal => Ok(Bundle {
                bytes: [value as u8, 0, 0],
                size: 1,
                address: 0,
                cycles: 0,
                affected_on_page: false,
            }),
        }
    }

    fn evaluate_decimal(&mut self, node: &PNode) -> Result<Bundle, EvalError> {
        let string = node.value.value.as_str();
        if string.is_empty() {
            return Err(EvalError {
                message: "empty decimal literal".to_string(),
                line: node.value.line,
            });
        }

        let mut value = 0;
        let mut shift = 1;

        for c in string.chars().rev() {
            if shift > 100 {
                return Err(EvalError {
                    message: "decimal value is too big".to_string(),
                    line: node.value.line,
                });
            }
            if c != '0' {
                match c.to_digit(10) {
                    Some(digit) => {
                        value += digit * shift;
                    }
                    None => {
                        if self.stage == Stage::Context {
                            return Err(EvalError {
                                message: format!(
                                    "variables must come from a constant expression, \
                                                  you cannot use other variables such as '{}' \
                                                  in variable definitions",
                                    string
                                ),
                                line: node.value.line,
                            });
                        }
                        match self.evaluate_variable(&node.value) {
                            Ok(v) => return Ok(v),
                            Err(err) => {
                                return Err(EvalError {
                                    message: format!(
                                        "'{}' is not a decimal value and {} either",
                                        c, err.message
                                    ),
                                    line: node.value.line,
                                })
                            }
                        }
                    }
                }
            }

            shift *= 10;
        }
        if value > 255 {
            return Err(EvalError {
                message: "decimal value is too big".to_string(),
                line: node.value.line,
            });
        }

        Ok(Bundle {
            bytes: [value as u8, 0, 0],
            size: 1,
            address: 0,
            cycles: 0,
            affected_on_page: false,
        })
    }

    fn evaluate_literal(&mut self, node: &PNode) -> Result<Bundle, EvalError> {
        // The value of the literal is guaranteed to not be empty by the parser.
        // If that's not the case, then it's a bug.
        let val = node.value.value.as_str();
        assert!(!val.is_empty(), "the value for the literal was empty!");

        // Pick up the left node, which is the node to be further evaluated, and
        // determine the literal mode to be used.
        let left = node.left.as_ref().unwrap();

        let lm;
        if val.starts_with('$') {
            lm = Some(LiteralMode::Hexadecimal);
            if left.node_type == NodeType::Literal {
                return Err(EvalError {
                    message: "literal cannot embed another literal".to_string(),
                    line: node.value.line,
                });
            }
        } else if val.starts_with('%') {
            lm = Some(LiteralMode::Binary);
            if left.node_type == NodeType::Literal {
                return Err(EvalError {
                    message: "literal cannot embed another literal".to_string(),
                    line: node.value.line,
                });
            }
        } else {
            lm = Some(LiteralMode::Plain);
        }

        // And evaluate the left node.
        self.literal_mode = lm.clone();
        let expr = self.evaluate_node(left)?;
        self.literal_mode = lm;

        Ok(expr)
    }

    fn char_to_hex(&mut self, oc: Option<char>, source: &PNode) -> Result<u8, EvalError> {
        match oc {
            Some(c) => match c.to_digit(16) {
                Some(c) => Ok(c as u8),
                None => {
                    if (c.is_alphabetic() || c == '_')
                        && self.evaluate_variable(&source.value).is_ok()
                    {
                        return Err(EvalError {
                            message: format!(
                                "you cannot use variables like '{}' in hexadecimal literals",
                                source.value.value
                            ),
                            line: source.value.line,
                        });
                    }
                    Err(EvalError {
                        message: "could not convert digit to hexadecimal".to_string(),
                        line: source.value.line,
                    })
                }
            },
            None => Err(EvalError {
                message: "digit out of bounds".to_string(),
                line: source.value.line,
            }),
        }
    }

    fn evaluate_control_statement(
        &mut self,
        node: &PNode,
        res: &mut Vec<Bundle>,
    ) -> Result<(), EvalError> {
        let changed;

        // This might just be a statement that changes the context (e.g.
        // ".macro", ".proc", etc.). In this case change the context and leave
        // early.
        (changed, self.can_bundle) = self.context.change_context(node)?;
        if changed {
            return Ok(());
        }

        // Otherwise, check the function that could act as a statement that
        // produces bundles.
        let function = node.value.value.as_str();
        match function {
            ".byte" | ".db" => self.push_evaluated_arguments(node, res, 1),
            ".addr" | ".dw" => self.push_evaluated_arguments(node, res, 2),
            _ => Err(EvalError {
                line: node.value.line,
                message: format!(
                    "cannot handle control statement '{}' in this context",
                    function
                ),
            }),
        }
    }

    fn evaluate_control_expression(&mut self, node: &PNode) -> Result<Bundle, EvalError> {
        let function = node.value.value.as_str();

        match function {
            ".hibyte" => self.evaluate_byte(node, true),
            ".lobyte" => self.evaluate_byte(node, false),
            _ => Err(EvalError {
                line: node.value.line,
                message: format!(
                    "cannot handle control statement '{}' in this context",
                    function
                ),
            }),
        }
    }

    fn evaluate_byte(&mut self, node: &PNode, high: bool) -> Result<Bundle, EvalError> {
        // The parser actually guarantees that the ".hibyte" and ".lobyte"
        // functions have exactly one argument. Hence, if this is not the case,
        // it's fine to let "unwrap" panic: it's a sign that's something is
        // wrong elsewhere.
        let arg = node.args.as_ref().unwrap().first().unwrap();

        // Get the bundle from the argument
        let mut bundle = self.evaluate_node(arg)?;

        // The bundle we got is going to be shuffled if we wanted the high byte.
        // After that, just zero out the rest (not mandatory but let's do it out
        // of consistency) and set the size to just one byte.
        if high {
            bundle.bytes[0] = bundle.bytes[1];
        }
        bundle.bytes[1] = 0x00;
        bundle.bytes[2] = 0x00;
        bundle.size = 1;

        Ok(bundle)
    }

    fn push_evaluated_arguments(
        &mut self,
        node: &PNode,
        res: &mut Vec<Bundle>,
        nbytes: u8,
    ) -> Result<(), EvalError> {
        match &node.args {
            Some(args) => {
                for arg in args {
                    // Evaluate the argument as a node.
                    let mut bundle = self.evaluate_node(arg)?;

                    // If there is a missmatch between the expected number of
                    // bytes and what we got, we might be able to resolve it if
                    // we expected two bytes and only one was received: extend
                    // it by leading zeroes. Otherwise we have to error out:
                    // it's up to the programmer to either call `.hibyte` or
                    // something similar if that's whay they intended.
                    if bundle.size != nbytes {
                        match nbytes {
                            1 => {
                                return Err(EvalError {
                                    line: arg.value.line,
                                    message: "expecting an argument that fits into a byte"
                                        .to_string(),
                                })
                            }
                            2 => {
                                bundle.size = 2;
                                bundle.bytes[1] = 0x00;
                                bundle.bytes[2] = 0x00;
                            }
                            _ => panic!("bad argument when evaluating arguments"),
                        }
                    }
                    res.push(bundle);
                }
            }
            None => {
                return Err(EvalError {
                    line: node.value.line,
                    message: format!(
                        "expecting at least one argument for '{}'",
                        node.value.value.as_str(),
                    ),
                })
            }
        }

        Ok(())
    }

    fn evaluate_variable(&mut self, id: &PString) -> Result<Bundle, EvalError> {
        match self.context.get_variable(id) {
            Ok(value) => Ok(value),
            Err(e) => Err(EvalError {
                message: e.message,
                line: id.line,
            }),
        }
    }

    fn evaluate_instruction(&mut self, node: &PNode) -> Result<Bundle, EvalError> {
        let (mode, mut bundle) = match &node.left {
            Some(_) => self.get_addressing_mode_and_bytes(node)?,
            None => (AddressingMode::Implied, Bundle::default()),
        };

        let mnemonic = node.value.value.to_lowercase();
        match INSTRUCTIONS.get(&mnemonic) {
            Some(entries) => match entries.get(&mode) {
                Some(values) => {
                    bundle.cycles = values.cycles;
                    bundle.size = values.size;
                    bundle.affected_on_page = values.affected_on_page;
                    bundle.bytes[2] = bundle.bytes[1];
                    bundle.bytes[1] = bundle.bytes[0];
                    bundle.bytes[0] = values.opcode.to_le_bytes()[0];
                }
                None => {
                    return Err(EvalError {
                        message: format!(
                            "cannot use {} addressing mode for the instruction '{}'",
                            mode, mnemonic
                        ),
                        line: node.value.line,
                    })
                }
            },
            None => {
                return Err(EvalError {
                    message: format!("unknown instruction {}", mnemonic),
                    line: node.value.line,
                });
            }
        }
        Ok(bundle)
    }

    fn get_addressing_mode_and_bytes(
        &mut self,
        node: &PNode,
    ) -> Result<(AddressingMode, Bundle), EvalError> {
        let left = &node.left;

        if left.as_ref().unwrap().node_type == NodeType::Indirection {
            self.get_from_indirect(node)
        } else if node.right.is_some() {
            self.get_from_indexed(node)
        } else {
            self.get_from_left(left.as_ref().unwrap())
        }
    }

    fn get_from_indirect(&mut self, node: &PNode) -> Result<(AddressingMode, Bundle), EvalError> {
        let left = node.left.as_ref().unwrap();

        match node.right.as_ref() {
            Some(right) => {
                if right.value.value.trim().to_lowercase() == "y" {
                    if left.right.is_some() {
                        return Err(EvalError {
                            message:
                                "it has to be either X addressing or Y addressing, not all at once"
                                    .to_string(),
                            line: node.value.line,
                        });
                    }

                    let val = self.evaluate_node(left.left.as_ref().unwrap())?;
                    if val.size != 1 {
                        return Err(EvalError {
                            message: "address can only be one byte long on indirect Y addressing"
                                .to_string(),
                            line: node.value.line,
                        });
                    }
                    return Ok((AddressingMode::IndirectY, val));
                }
                Err(EvalError {
                    message: "only the Y index is allowed on indirect Y addressing".to_string(),
                    line: node.value.line,
                })
            }
            None => match left.right.as_ref() {
                Some(right) => {
                    if right.value.value.trim().to_lowercase() == "x" {
                        let val = self.evaluate_node(left.left.as_ref().unwrap())?;
                        if val.size != 1 {
                            return Err(EvalError {
                                message:
                                    "address can only be one byte long on indirect X addressing"
                                        .to_string(),
                                line: node.value.line,
                            });
                        }
                        return Ok((AddressingMode::IndirectX, val));
                    }
                    Err(EvalError {
                        message: "only the X index is allowed on indirect X addressing".to_string(),
                        line: node.value.line,
                    })
                }
                None => {
                    let val = self.evaluate_node(left.left.as_ref().unwrap())?;
                    if val.size != 2 {
                        return Err(EvalError {
                            message: "expecting a full 16-bit address".to_string(),
                            line: node.value.line,
                        });
                    }
                    Ok((AddressingMode::Indirect, val))
                }
            },
        }
    }

    fn get_from_indexed(&mut self, node: &PNode) -> Result<(AddressingMode, Bundle), EvalError> {
        // Evaluate the left arm of the instruction.
        let left = node.left.as_ref().unwrap();
        let val = self.evaluate_node(left)?;

        // Ensure that the literal mode for the left arm ensures an address
        // instead of some bogus number.
        if let Some(lm) = &self.literal_mode {
            if *lm != LiteralMode::Hexadecimal {
                return Err(EvalError {
                    message: "indexed addressing only works with addresses".to_string(),
                    line: node.value.line,
                });
            }
        }

        // Check the right arm to know the index being used.
        let right = node.right.as_ref().unwrap();
        match right.value.value.to_lowercase().trim() {
            "x" => {
                if val.size == 1 {
                    Ok((AddressingMode::ZeropageIndexedX, val))
                } else {
                    Ok((AddressingMode::IndexedX, val))
                }
            }
            "y" => {
                if val.size == 1 {
                    Ok((AddressingMode::ZeropageIndexedY, val))
                } else {
                    Ok((AddressingMode::IndexedY, val))
                }
            }
            _ => Err(EvalError {
                message: "can only use X and Y as indices".to_string(),
                line: node.value.line,
            }),
        }
    }

    fn get_from_left(&mut self, left_arm: &PNode) -> Result<(AddressingMode, Bundle), EvalError> {
        if left_arm.value.value.to_lowercase().trim() == "a" {
            return Ok((AddressingMode::Implied, Bundle::default()));
        }

        let val = self.evaluate_node(left_arm)?;
        match self.literal_mode {
            Some(LiteralMode::Hexadecimal) => {
                if val.size == 1 {
                    Ok((AddressingMode::RelativeOrZeropage, val))
                } else {
                    Ok((AddressingMode::Absolute, val))
                }
            }
            Some(LiteralMode::Plain) => {
                if val.size > 1 {
                    Err(EvalError {
                        message: "immediate is too big".to_string(),
                        line: left_arm.value.line,
                    })
                } else {
                    Ok((AddressingMode::Immediate, val))
                }
            }
            _ => Err(EvalError {
                message: "left arm of instruction is neither an address nor an immediate"
                    .to_string(),
                line: left_arm.value.line,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mapping::EMPTY;

    fn assert_instruction(line: &str, hex: &[u8]) {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm.assemble(line.as_bytes()).unwrap();

        assert_eq!(res.len(), 1);

        for i in 0..res[0].size {
            assert_eq!(hex[i as usize], res[0].bytes[i as usize]);
        }
    }

    fn assert_error(line: &str, id: &str, line_num: usize, message: &str) {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm.assemble(line.as_bytes());
        let msg = format!("{} error (line {}): {}.", id, line_num, message);
        assert_eq!(res.unwrap_err().first().unwrap().to_string().as_str(), msg);
    }

    fn assert_eval_error(line: &str, message: &str) {
        assert_error(line, "Evaluation", 1, message);
    }

    fn assert_context_error(line: &str, message: &str, line_num: usize) {
        assert_error(line, "Context", line_num, message);
    }

    // Empty

    #[test]
    fn empty_line() {
        for line in vec!["", "  ", ";; Comment", "  ;; Comment"].into_iter() {
            let mut assembler = Assembler::new(EMPTY.to_vec());
            let bundles = assembler.assemble(line.as_bytes()).unwrap();
            assert!(bundles.is_empty());
        }
    }

    // Literal modes

    #[test]
    fn parse_binary() {
        assert_eval_error("adc #%0001", "missing binary digits to get a full byte");
        assert_eval_error("adc #%0001000", "missing binary digits to get a full byte");
        assert_eval_error(
            "adc #%000100001",
            "too many binary digits for a single byte",
        );
        assert_error(
            r#"
Variable = 42
adc %Variable
"#,
            "Evaluation",
            3,
            "you cannot use variables like 'Variable' in binary literals",
        );
        assert_instruction("adc #%10100010", &[0x69, 0xA2]);
    }

    #[test]
    fn parse_hexadecimal() {
        assert_eval_error(
            "adc #$12345",
            "expecting a number of 1 to 4 hexadecimal digits",
        );
        assert_eval_error("adc $AW", "could not convert digit to hexadecimal");
        assert_error(
            r#"
Variable = 42
adc $Variable
"#,
            "Evaluation",
            3,
            "you cannot use variables like 'Variable' in hexadecimal literals",
        );
        assert_error(
            r#"
Four = 4
adc $Four
"#,
            "Evaluation",
            3,
            "you cannot use variables like 'Four' in hexadecimal literals",
        );
        assert_instruction("adc $AA", &[0x65, 0xAA]);
        assert_instruction("adc $10", &[0x65, 0x10]);
        assert_instruction("adc $10AB", &[0x6D, 0xAB, 0x10]);
    }

    #[test]
    fn parse_decimal() {
        assert_eval_error("adc #256", "decimal value is too big");
        assert_eval_error("adc #2000", "decimal value is too big");
        assert_eval_error(
            "adc #2A",
            "'A' is not a decimal value and could not find variable '2A' in the global scope either",
        );
        assert_instruction("adc #1", &[0x69, 0x01]);
    }

    // Variables

    #[test]
    fn scoped_variable() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
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
            assert_eq!(res[i].size, 2);
            assert_eq!(res[i].bytes[0], instrs[i][0]);
            assert_eq!(res[i].bytes[1], instrs[i][1]);
        }
    }

    #[test]
    fn bare_variables() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
Variable = 4
adc Variable
"#
                .as_bytes(),
            )
            .unwrap();

        assert_eq!(res.len(), 1);

        let instr = res.first().unwrap();
        assert_eq!(instr.size, 2);
        assert_eq!(instr.bytes[0], 0x65);
        assert_eq!(instr.bytes[1], 0x04);
    }

    #[test]
    fn bad_variable_but_valid_identifier_in_instruction() {
        assert_eval_error(
            "adc Variable",
            "no prefix was given to operand and could not find variable 'Variable' in the global scope either",
        );
        assert_eval_error(
            "adc Scoped::Variable",
            "no prefix was given to operand and did not find scope 'Scoped' either",
        );
    }

    #[test]
    fn redefined_variable() {
        assert_context_error(
            r#"
.scope One
  Variable = 1
.endscope

Variable = 1
Yet = 3
Yet = 4
"#,
            "'Yet' already defined in the global scope: you cannot re-assign variables",
            8,
        );
    }

    #[test]
    fn unknown_variables() {
        assert_eval_error(
            "lda #Variable",
            "'e' is not a decimal value and could not find variable \
                      'Variable' in the global scope either",
        );
        assert_eval_error(
            "lda #Scope::Variable",
            "'e' is not a decimal value and did not find scope 'Scope' either",
        );
        assert_error(
            r#"
.scope Scope
.endscope
lda #Scope::Variable
"#,
            "Evaluation",
            4,
            "'e' is not a decimal value and could not find variable 'Variable' in 'Scope' either",
        );
    }

    // Regular instructions

    #[test]
    fn bad_addressing() {
        assert_eval_error(
            "unknown #$20",
            "could not find a macro with the name 'unknown'",
        );
        assert_eval_error(
            "adc ($2002, x)",
            "address can only be one byte long on indirect X addressing",
        );
        assert_eval_error(
            "adc ($2002), y",
            "address can only be one byte long on indirect Y addressing",
        );
        assert_eval_error(
            "adc ($20, y)",
            "only the X index is allowed on indirect X addressing",
        );
        assert_eval_error(
            "adc ($20), x",
            "only the Y index is allowed on indirect Y addressing",
        );
        assert_eval_error("jmp ($20)", "expecting a full 16-bit address");
        assert_eval_error("adc $20, z", "can only use X and Y as indices");
        assert_eval_error(
            "adc ($2000)",
            "cannot use indirect addressing mode for the instruction 'adc'",
        );
        assert_eval_error("lda 12", "no prefix was given to operand")
    }

    #[test]
    fn adc() {
        assert_instruction("adc #20", &[0x69, 0x14]);
        assert_instruction("adc #$20", &[0x69, 0x20]);
        assert_instruction("adc $2002", &[0x6D, 0x02, 0x20]);
        assert_instruction("adc $20", &[0x65, 0x20]);
        assert_instruction("adc $20, x", &[0x75, 0x20]);
        assert_instruction("adc $2002, x", &[0x7D, 0x02, 0x20]);
        assert_instruction("adc $2002, y", &[0x79, 0x02, 0x20]);
        assert_instruction("adc ($20, x)", &[0x61, 0x20]);
        assert_instruction("adc ($20), y", &[0x71, 0x20]);
    }

    #[test]
    fn sbc() {
        assert_instruction("sbc #$20", &[0xE9, 0x20]);
        assert_instruction("sbc $2002", &[0xED, 0x02, 0x20]);
        assert_instruction("sbc $20", &[0xE5, 0x20]);
        assert_instruction("sbc $20, x", &[0xF5, 0x20]);
        assert_instruction("sbc $2002, x", &[0xFD, 0x02, 0x20]);
        assert_instruction("sbc $2002, y", &[0xF9, 0x02, 0x20]);
        assert_instruction("sbc ($20, x)", &[0xE1, 0x20]);
        assert_instruction("sbc ($20), y", &[0xF1, 0x20]);
    }

    #[test]
    fn shift() {
        // asl
        assert_instruction("asl", &[0x0A]);
        assert_instruction("asl a", &[0x0A]);
        assert_instruction("asl $20", &[0x06, 0x20]);
        assert_instruction("asl $20, x", &[0x16, 0x20]);
        assert_instruction("asl $2002", &[0x0E, 0x02, 0x20]);
        assert_instruction("asl $2002, x", &[0x1E, 0x02, 0x20]);

        // lsr
        assert_instruction("lsr", &[0x4A]);
        assert_instruction("lsr a", &[0x4A]);
        assert_instruction("lsr $20", &[0x46, 0x20]);
        assert_instruction("lsr $20, x", &[0x56, 0x20]);
        assert_instruction("lsr $2002", &[0x4E, 0x02, 0x20]);
        assert_instruction("lsr $2002, x", &[0x5E, 0x02, 0x20]);
    }

    #[test]
    fn rotate() {
        // rol
        assert_instruction("rol", &[0x2A]);
        assert_instruction("rol a", &[0x2A]);
        assert_instruction("rol $20", &[0x26, 0x20]);
        assert_instruction("rol $20, x", &[0x36, 0x20]);
        assert_instruction("rol $2002", &[0x2E, 0x02, 0x20]);
        assert_instruction("rol $2002, x", &[0x3E, 0x02, 0x20]);

        // ror
        assert_instruction("ror", &[0x6A]);
        assert_instruction("ror a", &[0x6A]);
        assert_instruction("ror $20", &[0x66, 0x20]);
        assert_instruction("ror $20, x", &[0x76, 0x20]);
        assert_instruction("ror $2002", &[0x6E, 0x02, 0x20]);
        assert_instruction("ror $2002, x", &[0x7E, 0x02, 0x20]);
    }

    #[test]
    fn and() {
        assert_instruction("and #$20", &[0x29, 0x20]);
        assert_instruction("and $2002", &[0x2D, 0x02, 0x20]);
        assert_instruction("and $20", &[0x25, 0x20]);
        assert_instruction("and $20, x", &[0x35, 0x20]);
        assert_instruction("and $2002, x", &[0x3D, 0x02, 0x20]);
        assert_instruction("and $2002, y", &[0x39, 0x02, 0x20]);
        assert_instruction("and ($20, x)", &[0x21, 0x20]);
        assert_instruction("and ($20), y", &[0x31, 0x20]);
    }

    #[test]
    fn or() {
        // eor
        assert_instruction("eor #$20", &[0x49, 0x20]);
        assert_instruction("eor $20", &[0x45, 0x20]);
        assert_instruction("eor $20, x", &[0x55, 0x20]);
        assert_instruction("eor $2002", &[0x4D, 0x02, 0x20]);
        assert_instruction("eor $2002, x", &[0x5D, 0x02, 0x20]);
        assert_instruction("eor $2002, y", &[0x59, 0x02, 0x20]);
        assert_instruction("eor ($20, x)", &[0x41, 0x20]);
        assert_instruction("eor ($20), y", &[0x51, 0x20]);

        // ora
        assert_instruction("ora #$20", &[0x09, 0x20]);
        assert_instruction("ora $20", &[0x05, 0x20]);
        assert_instruction("ora $20, x", &[0x15, 0x20]);
        assert_instruction("ora $2002", &[0x0D, 0x02, 0x20]);
        assert_instruction("ora $2002, x", &[0x1D, 0x02, 0x20]);
        assert_instruction("ora $2002, y", &[0x19, 0x02, 0x20]);
        assert_instruction("ora ($20, x)", &[0x01, 0x20]);
        assert_instruction("ora ($20), y", &[0x11, 0x20]);
    }

    #[test]
    fn load() {
        // lda
        assert_instruction("lda #$20", &[0xA9, 0x20]);
        assert_instruction("lda $20", &[0xA5, 0x20]);
        assert_instruction("lda $20, x", &[0xB5, 0x20]);
        assert_instruction("lda $2002", &[0xAD, 0x02, 0x20]);
        assert_instruction("lda $2002, x", &[0xBD, 0x02, 0x20]);
        assert_instruction("lda $2002, y", &[0xB9, 0x02, 0x20]);
        assert_instruction("lda ($20, x)", &[0xA1, 0x20]);
        assert_instruction("lda ($20), y", &[0xB1, 0x20]);

        // ldx
        assert_instruction("ldx #$20", &[0xA2, 0x20]);
        assert_instruction("ldx $20", &[0xA6, 0x20]);
        assert_instruction("ldx $20, y", &[0xB6, 0x20]);
        assert_instruction("ldx $2002", &[0xAE, 0x02, 0x20]);
        assert_instruction("ldx $2002, y", &[0xBE, 0x02, 0x20]);

        // ldy
        assert_instruction("ldy #$20", &[0xA0, 0x20]);
        assert_instruction("ldy $20", &[0xA4, 0x20]);
        assert_instruction("ldy $20, x", &[0xB4, 0x20]);
        assert_instruction("ldy $2002", &[0xAC, 0x02, 0x20]);
        assert_instruction("ldy $2002, x", &[0xBC, 0x02, 0x20]);
    }

    #[test]
    fn jump() {
        assert_instruction("jsr $2002", &[0x20, 0x02, 0x20]);

        assert_instruction("jmp $2002", &[0x4C, 0x02, 0x20]);
        assert_instruction("jmp ($2002)", &[0x6C, 0x02, 0x20]);
    }

    #[test]
    fn inc_dec_instructions() {
        // inc
        assert_instruction("inc $10", &[0xE6, 0x10]);
        assert_instruction("inc $1000", &[0xEE, 0x00, 0x10]);
        assert_instruction("inc $10, x", &[0xF6, 0x10]);
        assert_instruction("inc $1000, x", &[0xFE, 0x00, 0x10]);

        assert_instruction("inx", &[0xE8]);

        assert_instruction("iny", &[0xC8]);

        // dec
        assert_instruction("dec $10", &[0xC6, 0x10]);
        assert_instruction("dec $1000", &[0xCE, 0x00, 0x10]);
        assert_instruction("dec $10, x", &[0xD6, 0x10]);
        assert_instruction("dec $1000, x", &[0xDE, 0x00, 0x10]);

        assert_instruction("dex", &[0xCA]);

        assert_instruction("dey", &[0x88]);
    }

    #[test]
    fn transfer_instructions() {
        assert_instruction("tax", &[0xAA]);
        assert_instruction("tay", &[0xA8]);
        assert_instruction("tsx", &[0xBA]);
        assert_instruction("txa", &[0x8A]);
        assert_instruction("txs", &[0x9A]);
        assert_instruction("tya", &[0x98]);
    }

    #[test]
    fn return_instructions() {
        assert_instruction("rti", &[0x40]);
        assert_instruction("rts", &[0x60]);
    }

    #[test]
    fn set_clear_instructions() {
        assert_instruction("clc", &[0x18]);
        assert_instruction("cld", &[0xD8]);
        assert_instruction("cli", &[0x58]);
        assert_instruction("clv", &[0xB8]);

        assert_instruction("sec", &[0x38]);
        assert_instruction("sed", &[0xF8]);
        assert_instruction("sei", &[0x78]);
    }

    #[test]
    fn push_pull_instructions() {
        assert_instruction("pha", &[0x48]);
        assert_instruction("php", &[0x08]);
        assert_instruction("pla", &[0x68]);
        assert_instruction("plp", &[0x28]);
    }

    #[test]
    fn nop_brk() {
        assert_instruction("nop", &[0xEA]);
        assert_instruction("brk", &[0x00]);
    }

    #[test]
    fn cmp() {
        // cmp
        assert_instruction("cmp #$20", &[0xC9, 0x20]);
        assert_instruction("cmp $2002", &[0xCD, 0x02, 0x20]);
        assert_instruction("cmp $20", &[0xC5, 0x20]);
        assert_instruction("cmp $20, x", &[0xD5, 0x20]);
        assert_instruction("cmp $2002, x", &[0xDD, 0x02, 0x20]);
        assert_instruction("cmp $2002, y", &[0xD9, 0x02, 0x20]);
        assert_instruction("cmp ($20, x)", &[0xC1, 0x20]);
        assert_instruction("cmp ($20), y", &[0xD1, 0x20]);

        // cpx
        assert_instruction("cpx #$20", &[0xE0, 0x20]);
        assert_instruction("cpx $2002", &[0xEC, 0x02, 0x20]);
        assert_instruction("cpx $20", &[0xE4, 0x20]);

        // cpy
        assert_instruction("cpy #$20", &[0xC0, 0x20]);
        assert_instruction("cpy $2002", &[0xCC, 0x02, 0x20]);
        assert_instruction("cpy $20", &[0xC4, 0x20]);
    }

    #[test]
    fn store_instructions() {
        //sta
        assert_instruction("sta $20", &[0x85, 0x20]);
        assert_instruction("sta $20, x", &[0x95, 0x20]);
        assert_instruction("sta $2002", &[0x8D, 0x02, 0x20]);
        assert_instruction("sta $2002, x", &[0x9D, 0x02, 0x20]);
        assert_instruction("sta $2002, y", &[0x99, 0x02, 0x20]);
        assert_instruction("sta ($20, x)", &[0x81, 0x20]);
        assert_instruction("sta ($20), y", &[0x91, 0x20]);

        // stx
        assert_instruction("stx $20", &[0x86, 0x20]);
        assert_instruction("stx $20, y", &[0x96, 0x20]);
        assert_instruction("stx $2002", &[0x8E, 0x02, 0x20]);

        // sty
        assert_instruction("sty $20", &[0x84, 0x20]);
        assert_instruction("sty $20, x", &[0x94, 0x20]);
        assert_instruction("sty $2002", &[0x8C, 0x02, 0x20]);
    }

    #[test]
    fn bit() {
        assert_instruction("bit $10", &[0x24, 0x10]);
        assert_instruction("bit $1001", &[0x2C, 0x01, 0x10]);
    }

    // Labels & branching
    // TODO

    // Control statements

    #[test]
    fn byte_literals() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
.scope Vars
    Variable = 4
.endscope

.byte #Vars::Variable
.dw $2001, $02
"#
                .as_bytes(),
            )
            .unwrap();

        assert_eq!(res.len(), 3);

        // .byte
        assert_eq!(res[0].bytes[0], 0x04);
        assert_eq!(res[0].bytes[1], 0x00);
        assert_eq!(res[0].size, 1);

        // First .dw argument.
        assert_eq!(res[1].bytes[0], 0x01);
        assert_eq!(res[1].bytes[1], 0x20);
        assert_eq!(res[1].size, 2);

        // Second .dw argument.
        assert_eq!(res[2].bytes[0], 0x02);
        assert_eq!(res[2].bytes[1], 0x00);
        assert_eq!(res[2].size, 2);
    }

    #[test]
    fn hi_lo_byte() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
Var = $2002
lda #.lobyte(Var)
lda #.hibyte(Var)
"#
                .as_bytes(),
            )
            .unwrap();

        assert_eq!(res.len(), 2);
        let instrs: Vec<[u8; 2]> = vec![[0xA9, 0x02], [0xA9, 0x20]];

        for i in 0..2 {
            assert_eq!(res[i].size, 2);
            assert_eq!(res[i].bytes[0], instrs[i][0]);
            assert_eq!(res[i].bytes[1], instrs[i][1]);
        }
    }

    // Macros

    #[test]
    fn macro_no_arguments() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
lda #42

.macro MACRO
    lda #2
.endmacro

lda #1
MACRO
"#
                .as_bytes(),
            )
            .unwrap();

        assert_eq!(res.len(), 3);
        let instrs: Vec<[u8; 2]> = vec![[0xA9, 0x2A], [0xA9, 0x01], [0xA9, 0x02]];

        for i in 0..3 {
            assert_eq!(res[i].size, 2);
            assert_eq!(res[i].bytes[0], instrs[i][0]);
            assert_eq!(res[i].bytes[1], instrs[i][1]);
        }
    }

    #[test]
    fn macro_not_enough_arguments() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
lda #42

.macro MACRO(Var)
    lda #Var
.endmacro

lda #1
MACRO
"#
                .as_bytes(),
            )
            .unwrap_err();

        assert_eq!(
            res.first().unwrap().to_string(),
            "Evaluation error (line 9): wrong number of arguments for 'MACRO': 1 required but 0 given."
        );
    }

    #[test]
    fn macro_too_many_arguments() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
lda #42

.macro MACRO(Var)
    lda #Var
.endmacro

lda #1
MACRO(1, 2)
"#
                .as_bytes(),
            )
            .unwrap_err();

        assert_eq!(
            res.first().unwrap().to_string(),
            "Evaluation error (line 9): wrong number of arguments for 'MACRO': 1 required but 2 given."
        );
    }

    #[test]
    fn macro_with_one_argument() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
lda #42

.macro MACRO(Var)
    lda #Var
.endmacro

lda #1
MACRO(2)
"#
                .as_bytes(),
            )
            .unwrap();

        assert_eq!(res.len(), 3);
        let instrs: Vec<[u8; 2]> = vec![[0xA9, 0x2A], [0xA9, 0x01], [0xA9, 0x02]];

        for i in 0..3 {
            assert_eq!(res[i].size, 2);
            assert_eq!(res[i].bytes[0], instrs[i][0]);
            assert_eq!(res[i].bytes[1], instrs[i][1]);
        }
    }

    #[test]
    fn macro_unknown_arguments() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
lda #42

.macro MACRO(Var)
    lda #Va
.endmacro

lda #1
MACRO(1)
"#
                .as_bytes(),
            )
            .unwrap_err();

        assert_eq!(
            res.first().unwrap().to_string(),
            "Evaluation error (line 5): 'a' is not a decimal value and \
             could not find variable 'Va' in the global scope either."
        );
    }

    #[test]
    fn macro_shadow_argument() {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm
            .assemble(
                r#"
Var = 3
lda #42

.macro MACRO(Var)
    lda #Va
.endmacro

lda #1
MACRO(1)
"#
                .as_bytes(),
            )
            .unwrap_err();

        assert_eq!(
            res.first().unwrap().to_string(),
            "Evaluation error (line 5): 'Var' already defined in the global scope: \
             you cannot re-assign variables."
        );
    }
}
