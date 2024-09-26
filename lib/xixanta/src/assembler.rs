use crate::context::{Context, PValue};
use crate::errors::ParseError;
use crate::instruction::{AddressingMode, Bundle};
use crate::mapping::{Mapping, Segment};
use crate::opcodes::INSTRUCTIONS;
use crate::parser::{NodeType, PNode, Parser};
use std::collections::HashMap;
use std::io::Read;

// TODO: proper AST: WRITE_PPU_DATA from NES is a good example
// TODO: for christ's sake, automated tests!
// TODO: macros are meant to be global!
// TODO: instead of mapping.nodes having a value of vec<node>, the value should be a Context.
// TODO: proc's, labels, macros, and scopes can be merged dramatically.
// TODO: allow pointer arithmetic (e.g. 'adc #List::ptr + 1').
// TODO: more to_owned() stuff, more rustacean way of doing things, more ...
// TODO: warning on empty segments

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralMode {
    Hexadecimal,
    Binary,
    Plain,
}

pub struct Assembler {
    line: usize,
    column: usize,
    context: Context,
    literal_mode: Option<LiteralMode>,
    only_context: bool,
    force_decimal: bool,
    mapping: Mapping,
    offsets: HashMap<String, usize>,
}

// Control statements which end up affecting which context we are in.
const TOUCH_CONTEXT: [&str; 7] = [
    ".scope",
    ".endscope",
    ".proc",
    ".endproc",
    ".macro",
    ".endmacro",
    ".segment",
];

impl Assembler {
    pub fn new(segments: Vec<Segment>) -> Self {
        assert!(segments.len() > 0);

        let mut offsets = HashMap::new();
        for segment in &segments {
            offsets.insert(segment.name.clone(), 0);
        }

        Self {
            line: 0,
            column: 0,
            literal_mode: None,
            only_context: false,
            force_decimal: false,
            context: Context::new(),
            mapping: Mapping::new(segments),
            offsets,
        }
    }

    pub fn reset(&mut self) {
        self.line = 0;
        self.column = 0;
        self.context = Context::new();
        self.mapping.reset();

        self.offsets = HashMap::new();
        for segment in &self.mapping.segments {
            self.offsets.insert(segment.name.clone(), 0);
        }
    }

    pub fn assemble(&mut self, reader: impl Read) -> Result<Vec<Bundle>> {
        let mut res = vec![];

        let mut parser = Parser::new();
        parser.parse(reader)?;

        // println!("{:#?}", parser.nodes);

        // NOTE: first step: unroll macros, update context, set variables.

        self.only_context = true;
        for node in parser.nodes.clone() {
            match node.node_type {
                NodeType::Assignment => {
                    self.evaluate_assignment(node)?;

                    println!("{:#?}", self.context);
                }
                NodeType::Control => {
                    self.evaluate_control(node)?;
                }
                _ => {}
            }
        }
        self.only_context = false;

        // Check for unclosed scope definition.
        if !self.context.is_global() {
            return Err(self.parser_error(
                format!(
                    "definition for '{}' has not been closed",
                    self.context.name()
                )
                .as_str(),
            ));
        }

        // NOTE: second step: let's rock.

        for node in parser.nodes {
            match node.node_type {
                NodeType::Instruction => {
                    res.push(self.evaluate_node(node)?);
                }
                NodeType::Control => {
                    self.evaluate_control(node)?;
                }
                _ => {}
            }
        }

        // NOTE: third step: update addresses of referenced labels.
        // TODO

        // println!("{:#?}", res);
        Ok(res)
    }

    // pub fn disassemble(&mut self, reader: impl Read) -> Result<Vec<&dyn Encodable>> {
    //     self.from_byte_reader(reader)?;

    //     let mut instructions: Vec<&dyn Encodable> = vec![];
    //     for node in self.mapping.current() {
    //         println!("{:#?}", node);
    //         match node {
    //             Node::Instruction(instr) => instructions.push(instr),
    //             Node::Literal(lit) => instructions.push(lit),
    //             _ => {}
    //         }
    //     }

    //     Ok(instructions)
    // }

    fn evaluate_assignment(&mut self, node: Box<PNode>) -> Result<()> {
        if self
            .context
            .current_mut()
            .unwrap()
            .contains_key(&node.value.value)
        {
            return Err(ParseError {
                line: self.line,
                message: format!(
                    "variable '{}' is being re-assigned: it was previously defined in line {}",
                    node.value.value, node.value.line,
                ),
                parse: false,
            });
        }

        if let Some(value_node) = node.left {
            self.force_decimal = true;
            println!("{:#?}", value_node);
            let val = self.evaluate_node(value_node.clone())?;
            println!("{:#?}", val);
            self.force_decimal = false;

            self.context.current_mut().unwrap().insert(
                node.value.value.to_owned(),
                PValue {
                    node: *value_node,
                    value: val,
                    label: false,
                },
            );
        }

        Ok(())
    }

    fn evaluate_node(&mut self, node: Box<PNode>) -> Result<Bundle> {
        match node.node_type {
            NodeType::Control => self.evaluate_control(node),
            NodeType::Literal => self.evaluate_literal(node),
            NodeType::Instruction => self.evaluate_instruction(node),
            NodeType::Value => match self.literal_mode {
                Some(LiteralMode::Hexadecimal) => self.evaluate_hexadecimal(node),
                Some(LiteralMode::Binary) => self.evaluate_binary(node),
                Some(LiteralMode::Plain) => self.evaluate_decimal(node),
                None => {
                    if self.force_decimal {
                        self.evaluate_decimal(node)
                    } else {
                        Err(self.parser_error("no prefix was given to operand"))
                    }
                }
            },
            // TODO
            _ => Ok(Bundle::new()),
        }
    }

    fn evaluate_instruction(&mut self, node: Box<PNode>) -> Result<Bundle> {
        let mnemonic = node.value.value.to_lowercase();

        let (mode, mut bundle) = if node.left.is_some() {
            self.get_addressing_mode_and_bytes(node)?
        } else {
            (AddressingMode::Implied, Bundle::new())
        };

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
                    return Err(self.parser_error(
                        format!(
                            "cannot use {} addressing mode for the instruction '{}'",
                            mode, mnemonic
                        )
                        .as_str(),
                    ))
                }
            },
            None => {
                return Err(self.parser_error(format!("unknown instruction {}", mnemonic).as_str()))
            }
        }
        Ok(bundle)
    }

    fn get_addressing_mode_and_bytes(
        &mut self,
        node: Box<PNode>,
    ) -> Result<(AddressingMode, Bundle)> {
        if node.clone().left.unwrap().node_type == NodeType::Indirection {
            self.get_from_indirect(node)
        } else if node.right.is_some() {
            self.get_from_indexed(node)
        } else {
            self.get_from_left(node)
        }
    }

    fn get_from_indirect(&mut self, node: Box<PNode>) -> Result<(AddressingMode, Bundle)> {
        let left = node.left.unwrap();

        match node.right {
            Some(right) => {
                if right.value.value.trim().to_lowercase() == "y" {
                    if left.right.is_some() {
                        return Err(self.parser_error(
                            "it has to be either X addressing or Y addressing, not all at once",
                        ));
                    }

                    let val = self.evaluate_node(left.left.unwrap())?;
                    if val.size != 1 {
                        return Err(self.parser_error(
                            "address can only be one byte long on indirect Y addressing",
                        ));
                    }
                    return Ok((AddressingMode::IndirectY, val));
                }
                return Err(
                    self.parser_error("only the Y index is allowed on indirect Y addressing")
                );
            }
            None => match left.right {
                Some(right) => {
                    if right.value.value.trim().to_lowercase() == "x" {
                        let val = self.evaluate_node(left.left.unwrap())?;
                        if val.size != 1 {
                            return Err(self.parser_error(
                                "address can only be one byte long on indirect X addressing",
                            ));
                        }
                        return Ok((AddressingMode::IndirectX, val));
                    }
                    return Err(
                        self.parser_error("only the X index is allowed on indirect X addressing")
                    );
                }
                None => {
                    let val = self.evaluate_node(left.left.unwrap())?;
                    if val.size != 2 {
                        return Err(self.parser_error("expecting a full 16-bit address"));
                    }
                    return Ok((AddressingMode::Indirect, val));
                }
            },
        }
    }

    fn get_from_indexed(&mut self, node: Box<PNode>) -> Result<(AddressingMode, Bundle)> {
        self.literal_mode = None; // TODO: needed?
        let val = self.evaluate_node(node.left.unwrap())?;

        if let Some(lm) = &self.literal_mode {
            if *lm != LiteralMode::Hexadecimal {
                return Err(self.parser_error("indexed addressing only works with addresses"));
            }
        }

        match node.right.unwrap().value.value.to_lowercase().trim() {
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
            _ => Err(self.parser_error("can only use X and Y as indices")),
        }
    }

    fn get_from_left(&mut self, node: Box<PNode>) -> Result<(AddressingMode, Bundle)> {
        let left = node.left.unwrap();

        if left.value.value.to_lowercase().trim() == "a" {
            return Ok((AddressingMode::Implied, Bundle::new()));
        }

        self.literal_mode = None; // TODO: needed?
        let val = self.evaluate_node(left)?;

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
                    Err(self.parser_error("immediate is too big"))
                } else {
                    Ok((AddressingMode::Immediate, val))
                }
            }
            _ => {
                Err(self
                    .parser_error("left arm of instruction is neither an address nor an immediate"))
            }
        }
    }

    fn evaluate_control(&mut self, node: Box<PNode>) -> Result<Bundle> {
        let id = node.value.value.to_lowercase();
        let id_str = id.as_str();

        // If we are just dealing with context resolution/assignment and the
        // current control statement does not matter on that regard, just skip
        // it.
        // if self.only_context && !TOUCH_CONTEXT.contains(&id_str) {
        //     return Ok(Bundle::new());
        // }

        match id_str {
            ".hibyte" => self.evaluate_hilo_byte(node.args.unwrap_or(vec![]), true),
            ".lobyte" => self.evaluate_hilo_byte(node.args.unwrap_or(vec![]), false),
            ".scope" => self.evaluate_scope_definition(node),
            ".endscope" => self.evaluate_scope_end(),
            //         ".segment" => self.parse_segment_definition(&id, line),
            //         ".byte" | ".db" => self.parse_literal_bytes(&id, line, false),
            //         ".word" | ".dw" | ".addr" => self.parse_literal_bytes(&id, line, true),
            //         ".proc" => self.parse_proc_definition(&id, line),
            //         ".endproc" => self.parse_proc_end(&id),
            //         ".macro" => self.parse_macro_definition(&id, line),
            //         ".endmacro" => self.parse_macro_end(&id),
            _ => Err(self.parser_error(format!("unknown control statement '{}'", id).as_str())),
        }
    }

    fn evaluate_literal(&mut self, node: Box<PNode>) -> Result<Bundle> {
        let mut prev = None;
        self.literal_mode = None;

        let ret = match node.value.value.chars().nth(0) {
            Some(prefix) => {
                if prefix == '$' {
                    prev = Some(LiteralMode::Hexadecimal);
                } else if prefix == '%' {
                    prev = Some(LiteralMode::Binary);
                } else {
                    prev = Some(LiteralMode::Plain);
                }
                self.literal_mode = prev.clone();
                self.evaluate_node(node.left.unwrap())
            }
            None => Err(self.parser_error("no prefix was given to operand")),
        };

        self.literal_mode = prev;

        ret
    }

    fn evaluate_hexadecimal(&mut self, node: Box<PNode>) -> Result<Bundle> {
        let mut chars = node.value.value.chars();
        let mut bytes = [0, 0, 0];
        let size: u8;

        match node.value.value.len() {
            1 => {
                bytes[0] = self.char_to_hex(chars.next())?;
                size = 1;
            }
            2 => {
                bytes[0] = self.char_to_hex(chars.next())? * 16;
                bytes[0] += self.char_to_hex(chars.next())?;
                size = 1;
            }
            3 => {
                bytes[1] = self.char_to_hex(chars.next())?;
                bytes[0] = self.char_to_hex(chars.next())? * 16;
                bytes[0] += self.char_to_hex(chars.next())?;
                size = 2;
            }
            4 => {
                bytes[1] = self.char_to_hex(chars.next())? * 16;
                bytes[1] += self.char_to_hex(chars.next())?;
                bytes[0] = self.char_to_hex(chars.next())? * 16;
                bytes[0] += self.char_to_hex(chars.next())?;
                size = 2;
            }
            _ => return Err(self.parser_error("expecting a number of 1 to 4 hexadecimal digits")),
        }

        Ok(Bundle {
            bytes,
            size,
            address: 0,
            cycles: 0,
            affected_on_page: false,
        })
    }

    fn char_to_hex(&mut self, oc: Option<char>) -> Result<u8> {
        match oc {
            Some(c) => match c.to_digit(16) {
                Some(c) => Ok(c as u8),
                None => Err(self.parser_error("could not convert digit to hexadecimal")),
            },
            None => Err(self.parser_error("digit out of bounds")),
        }
    }

    fn evaluate_binary(&mut self, node: Box<PNode>) -> Result<Bundle> {
        let string = node.value.value.as_str();
        let mut value = 0;
        let mut shift = 0;

        for c in string.chars().rev() {
            if c == '1' {
                let val = 1 << shift;
                value += val;
            } else if c != '0' {
                return Err(
                    self.parser_error(format!("bad binary format for '{}'", string).as_str())
                );
            }

            shift += 1;
        }

        if shift < 8 {
            Err(self.parser_error("missing binary digits to get a full byte"))
        } else if shift > 8 {
            Err(self.parser_error("too many binary digits for a single byte"))
        } else {
            Ok(Bundle {
                bytes: [value as u8, 0, 0],
                size: 1,
                address: 0,
                cycles: 0,
                affected_on_page: false,
            })
        }
    }

    fn evaluate_decimal(&mut self, node: Box<PNode>) -> Result<Bundle> {
        let string = node.value.value.as_str();
        if string.is_empty() {
            return Err(self.parser_error("empty decimal literal"));
        }

        match self.do_evaluate_decimal(string) {
            Ok(val) => Ok(val),
            Err(e) => {
                if e.parse {
                    Err(e)
                } else {
                    self.fetch_variable(string)
                }
            }
        }
    }

    fn fetch_variable(&mut self, mut string: &str) -> Result<Bundle> {
        // Get the context that might be being referenced.
        let ctxt = match string.find("::") {
            Some(_) => {
                let tctxt = string.rsplit_once("::").unwrap_or(("", ""));
                if tctxt.0.is_empty() {
                    self.context.current()
                } else {
                    string = tctxt.1;
                    self.context.find(tctxt.0)
                }
            }
            None => self.context.current(),
        };

        // println!("{:#?}", self.context);
        // println!("{:#?}", ctxt);

        match ctxt {
            Some(hash) => {
                match hash.get(string) {
                    Some(var) => {
                        // TODO
                        // If this is just a memory address (e.g.
                        // label), then just return it as is.
                        // if var.label {
                        //     return Ok((node.clone(), false));
                        // }
                        Ok(var.value.clone())
                    }
                    None => {
                        Err(self.parser_error(format!("unknown variable '{}'", string).as_str()))
                    }
                }
            }
            None => Err(self.parser_error(format!("unknown scope '{}'", "Global").as_str())),
        }
    }

    fn do_evaluate_decimal(&mut self, string: &str) -> Result<Bundle> {
        let mut value = 0;
        let mut shift = 1;

        if string.is_empty() {
            return Err(self.parser_error("empty decimal literal"));
        }

        for c in string.chars().rev() {
            if shift > 100 {
                return Err(self.parser_error("decimal value is too big"));
            }
            if c != '0' {
                match c.to_digit(10) {
                    Some(digit) => {
                        value += digit * shift;
                    }
                    None => {
                        return Err(ParseError {
                            line: self.line,
                            message: format!("'{}' is not a decimal value", c),
                            parse: false,
                        });
                    }
                }
            }

            shift *= 10;
        }
        if value > 255 {
            return Err(self.parser_error("decimal value is too big"));
        }

        Ok(Bundle {
            bytes: [value as u8, 0, 0],
            size: 1,
            address: 0,
            cycles: 0,
            affected_on_page: false,
        })
    }

    fn evaluate_hilo_byte(&mut self, args: Vec<Box<PNode>>, hi: bool) -> Result<Bundle> {
        if args.len() != 1 {
            return Err(self.parser_error("wrong number of arguments: expecting exactly one"));
        }

        let val = self.evaluate_node(args.first().unwrap().clone())?;
        if val.size < 1 {
            let s = if hi { ".hibyte" } else { ".lobyte" };
            return Err(self.parser_error(format!("empty value for {}", s).as_str()));
        }

        let b = if hi {
            if val.size == 1 {
                val.bytes[0]
            } else {
                val.bytes[1]
            }
        } else {
            val.bytes[0]
        };

        Ok(Bundle {
            bytes: [b, 0, 0],
            size: 1,
            address: 0,
            cycles: 0,
            affected_on_page: false,
        })
    }

    fn evaluate_scope_definition(&mut self, node: Box<PNode>) -> Result<Bundle> {
        println!("{:#?}", node);
        match node.left {
            Some(identifier) => {
                self.context.push(&identifier.value.value);
                // TODO: mapping?

                Ok(Bundle::new())
            }
            None => return Err(self.parser_error("scope definition with no identifier")),
        }
    }

    fn evaluate_scope_end(&mut self) -> Result<Bundle> {
        if !self.context.pop() {
            return Err(self.parser_error("missmatched '.endscope': there is no scope to end"));
        }

        // TODO: mapping?

        Ok(Bundle::new())
    }

    // pub fn assemble(&mut self, reader: impl Read) -> Result<Vec<&dyn Encodable>> {
    //     let mut instructions: Vec<&dyn Encodable> = vec![];

    //     self.assemble_nodes(reader)?;

    //     let mut idx: usize = 0;
    //     for segment in &self.mapping.segments {
    //         let mut size: usize = 0;

    //         while idx < segment.start.into() {
    //             match &segment.fill {
    //                 Some(fill) => instructions.push(fill),
    //                 None => instructions.push(&Fill { value: 0x00 }),
    //             }
    //             idx += 1;
    //         }

    //         for node in &self.mapping.nodes[&segment.name] {
    //             match node {
    //                 Node::Instruction(instr) => {
    //                     instructions.push(instr);
    //                     size += usize::from(instr.size());
    //                 }
    //                 Node::Literal(lit) => {
    //                     instructions.push(lit);
    //                     size += usize::from(lit.size());
    //                 }
    //                 _ => {}
    //             }
    //         }

    //         if size > segment.size {
    //             return Err(ParseError {
    //                 line: 0,
    //                 message: format!(
    //                     "segment '{}' expected a size of '{}' bytes but '{}' bytes were produced instead",
    //                     segment.name, size, segment.size
    //                 ),
    //             });
    //         }
    //         idx += size;
    //         if segment.fill.is_none() {
    //             continue;
    //         }

    //         while size < segment.size {
    //             instructions.push(segment.fill.as_ref().unwrap());
    //             size += 1;
    //             idx += 1;
    //         }
    //     }

    //     Ok(instructions)
    // }

    // pub fn evaluate(&mut self) -> Result<()> {
    //     for segment in &self.mapping.segments {
    //         for node in self.mapping.nodes.get_mut(&segment.name).unwrap() {
    //             match node {
    //                 Node::Instruction(instr) => {
    //                     Self::update_instruction_with_context(instr, &self.context)?;
    //                     instr.address = segment.start;

    //                     self.offsets
    //                         .entry(segment.name.clone())
    //                         .and_modify(|value| {
    //                             instr.address += *value as u16;
    //                             *value += usize::from(instr.size())
    //                         })
    //                         .or_insert(instr.size().into());
    //                 }
    //                 Node::Scoped(scope) => {
    //                     if scope.start {
    //                         self.context.push(&scope.identifier.value);
    //                     } else {
    //                         _ = self.context.pop();
    //                     }
    //                 }
    //                 Node::Literal(literal) => {
    //                     Self::update_literal_with_context(literal, &self.context)?;
    //                     self.offsets
    //                         .entry(segment.name.clone())
    //                         .and_modify(|value| *value += usize::from(literal.size()))
    //                         .or_insert(literal.size().into());
    //                 }
    //                 Node::Label(label) => {
    //                     let address =
    //                         usize::from(segment.start) + self.offsets.get(&segment.name).unwrap();

    //                     self.context
    //                         .current_mut()
    //                         .unwrap()
    //                         .entry(label.value.clone())
    //                         .and_modify(|e| e.value = address);
    //                 }
    //                 _ => {}
    //             }
    //         }
    //     }

    //     Ok(())
    // }

    // // TODO: oh boy...
    // pub fn resolve_labels(&mut self) -> Result<()> {
    //     for segment in &self.mapping.segments {
    //         for node in self.mapping.nodes.get_mut(&segment.name).unwrap() {
    //             match node {
    //                 Node::Instruction(instr) => {
    //                     if !instr.resolved {
    //                         match &instr.left {
    //                             Some(pstring) => {
    //                                 match self.context.current().unwrap().get(&pstring.value) {
    //                                     Some(entry) => {
    //                                         if instr.mode == AddressingMode::Absolute {
    //                                             let bytes = entry.value.to_le_bytes();
    //                                             instr.bytes = [bytes[0], bytes[1]];
    //                                         } else {
    //                                             let diff: isize = entry.value as isize
    //                                                 - (instr.address as isize + 2);
    //                                             if diff < -128 || diff > 127 {
    //                                                 return Err(instr.mnemonic.parser_error(
    //                                                     format!("relative addressing out of range")
    //                                                         .as_str(),
    //                                                 ));
    //                                             }
    //                                             let bytes = diff.to_le_bytes();
    //                                             instr.bytes = [bytes[0], 0];
    //                                         }
    //                                     }
    //                                     None => {
    //                                         return Err(instr.mnemonic.parser_error(
    //                                             format!("label '{}' not found", pstring.value)
    //                                                 .as_str(),
    //                                         ))
    //                                     }
    //                                 }
    //                             }
    //                             None => {
    //                                 return Err(instr.mnemonic.parser_error(
    //                                     format!("there is no label for the given jump instruction")
    //                                         .as_str(),
    //                                 ))
    //                             }
    //                         }
    //                     }
    //                 }
    //                 Node::Literal(literal) => {
    //                     if !literal.resolved {
    //                         match self
    //                             .context
    //                             .current()
    //                             .unwrap()
    //                             .get(&literal.identifier.value)
    //                         {
    //                             Some(entry) => {
    //                                 let bytes = entry.value.to_le_bytes();
    //                                 literal.bytes = [bytes[0], bytes[1]];
    //                             }
    //                             None => {
    //                                 return Err(literal.identifier.parser_error(
    //                                     format!(
    //                                     "'{}' is neither a known variable or label at this scope",
    //                                     literal.identifier.value
    //                                 )
    //                                     .as_str(),
    //                                 ))
    //                             }
    //                         }
    //                     }
    //                 }
    //                 _ => {}
    //             }
    //         }
    //     }

    //     Ok(())
    // }

    // fn update_instruction_with_context(instr: &mut Instruction, context: &Context) -> Result<()> {
    //     // To keep things simple, we remove out the `implied` case and we parse
    //     // further with a known `Some` value for the base algorithm implemented
    //     // in `update_instruction_and_bytes`.
    //     if instr.left.is_some() {
    //         Self::update_addressing_and_bytes(instr, context)?;
    //     } else {
    //         instr.mode = AddressingMode::Implied;
    //     }

    //     // Now that we have the addressing mode and the bytes, we can fill out
    //     // the rest of it by fetching the values on `INSTRUCTIONS`.
    //     match INSTRUCTIONS.get(&instr.mnemonic.value.to_lowercase()) {
    //         Some(entries) => match entries.get(&instr.mode) {
    //             Some(values) => {
    //                 instr.cycles = values.cycles;
    //                 instr.opcode = values.opcode;
    //                 instr.size = values.size;
    //                 instr.affected_on_page = values.affected_on_page;
    //             }
    //             None => {
    //                 return Err(instr.mnemonic.parser_error(
    //                     format!(
    //                         "bad addressing mode '{}' for the instruction '{}'",
    //                         &instr.mode, &instr.mnemonic.value
    //                     )
    //                     .as_str(),
    //                 ));
    //             }
    //         },
    //         None => {
    //             return Err(instr.mnemonic.parser_error(
    //                 format!("unknown instruction '{}'", &instr.mnemonic.value).as_str(),
    //             ));
    //         }
    //     }

    //     Ok(())
    // }

    // fn update_addressing_and_bytes(instr: &mut Instruction, context: &Context) -> Result<()> {
    //     // `unwrap()` is guaranteed to work by the caller.
    //     let left = instr.left.as_ref().unwrap();

    //     // We will first try to check if there's any variable involved on the
    //     // left arm and replace the string if so. This will greatly simplify
    //     // things down the line. That being said, there is a special reserved
    //     // case, which is the implied addressing by using "a". In this case, we
    //     // want to ensure that we assume an implied addressing and not a
    //     // variable named "a".
    //     if left.value.to_lowercase() == "a" {
    //         instr.mode = AddressingMode::Implied;
    //     } else {
    //         let (nleft, resolved) = Self::replace_variable(left, context)?;
    //         // TODO
    //         instr.resolved = resolved;
    //         if !resolved {
    //             if instr.mnemonic.value == "jmp" {
    //                 instr.mode = AddressingMode::Absolute;
    //             } else {
    //                 instr.mode = AddressingMode::RelativeOrZeropage;
    //             }
    //         }

    //         if nleft.value.starts_with('$') {
    //             // This is an address. At this point we should assume that the
    //             // left node contains the address itself, and that the right one
    //             // will contain whether there is indexing.

    //             let string = nleft.value.chars().as_str();
    //             instr.bytes = Self::parse_hex_from(string, &nleft, true, false, true)?;

    //             match &instr.right {
    //                 Some(xy) => match xy.value.to_lowercase().as_str() {
    //                     "x" => {
    //                         if string.len() == 3 {
    //                             instr.mode = AddressingMode::ZeropageIndexedX;
    //                         } else {
    //                             instr.mode = AddressingMode::IndexedX;
    //                         }
    //                     }
    //                     "y" => {
    //                         if string.len() == 3 {
    //                             instr.mode = AddressingMode::ZeropageIndexedY;
    //                         } else {
    //                             instr.mode = AddressingMode::IndexedY;
    //                         }
    //                     }
    //                     _ => return Err(xy.parser_error("index is neither X nor Y")),
    //                 },
    //                 None => {
    //                     if string.len() == 3 {
    //                         instr.mode = AddressingMode::RelativeOrZeropage;
    //                     } else {
    //                         instr.mode = AddressingMode::Absolute;
    //                     }
    //                 }
    //             }
    //         } else if nleft.value.starts_with('#') {
    //             // Immediate addressing in any case: hexadecimal, binary or
    //             // decimal. Hence, just figure out the character being used and
    //             // call the right function for it.

    //             let mut chars = nleft.value.chars();
    //             chars.next();
    //             let string = chars.as_str();

    //             instr.bytes = Self::parse_numeric(string, &nleft, false)?;
    //             instr.mode = AddressingMode::Immediate;
    //         } else if nleft.value.starts_with('(') {
    //             // Indirect addressing. In this case the left arm can be further
    //             // subdivided. That is, indirect X-indexing is represented like
    //             // so: `instr ($NN, x)`. Hence, first of all we have to figure
    //             // out whether there is a subdivision.

    //             let (left1, oleft2) = Self::split_left_arm(&nleft)?;
    //             match oleft2 {
    //                 Some(left2) => {
    //                     // There is subdivision. Thus, we have to assume
    //                     // indirect X-indexing, which means that the right arm
    //                     // should be None and that the right side of the left
    //                     // node must match the X register. Other than that, the
    //                     // address being referenced must be zero page.
    //                     if instr.right.is_some() {
    //                         return Err(instr.right.as_ref().unwrap().parser_error(
    //                             "bad indirect mode, expecting an indirect X-indexed addressing mode"
    //                         ));
    //                     }
    //                     if left2.value.to_lowercase() != "x" {
    //                         return Err(left2.parser_error(
    //                             "the index in indirect X-indexed addressing must be X",
    //                         ));
    //                     }
    //                     match Self::parse_hex_from(&left1.value, &left1, false, false, true) {
    //                         Ok(bytes) => instr.bytes = bytes,
    //                         Err(e) => {
    //                             let msg = String::from(
    //                                 "when parsing an instruction with indirect X-indexed addressing: ",
    //                             ) + &e.message;
    //                             return Err(left1.parser_error(msg.as_str()));
    //                         }
    //                     }
    //                     instr.mode = AddressingMode::IndirectX;
    //                 }
    //                 None => {
    //                     // There is no subdivision on the left arm. Hence, if
    //                     // there is something on the right arm then we must
    //                     // assume indirect Y-index addressing, and if not then
    //                     // it's indirect addressing with no indices involvved.
    //                     if instr.right.is_some() {
    //                         if instr.right.as_ref().unwrap().value.to_lowercase() != "y" {
    //                             return Err(instr.right.as_ref().unwrap().parser_error(
    //                                 "the index in indirect Y-indexed addressing must be Y",
    //                             ));
    //                         }
    //                         match Self::parse_hex_from(&left1.value, &left1, false, false, true) {
    //                             Ok(bytes) => instr.bytes = bytes,
    //                             Err(e) => {
    //                                 let msg = String::from(
    //                                 "when parsing an instruction with indirect Y-indexed addressing: ",
    //                             ) + &e.message;
    //                                 return Err(left1.parser_error(msg.as_str()));
    //                             }
    //                         }
    //                         instr.mode = AddressingMode::IndirectY;
    //                     } else {
    //                         instr.bytes =
    //                             Self::parse_hex_from(&left1.value, &left1, true, true, true)?;
    //                         instr.mode = AddressingMode::Indirect;
    //                     }
    //                 }
    //             }
    //         } else {
    //             // At this point all of the syntax cases have been exhausted:
    //             // the programmer messed up. From this point on we try to figure
    //             // out how they messed up.

    //             if nleft.value.starts_with('=') {
    //                 return Err(instr.mnemonic.parser_error(
    //                     format!(
    //                         "cannot use '{}' in an assignment because it's a word reserved for an instruction mnemonic",
    //                         instr.mnemonic.value
    //                     ).as_str(),
    //                 ));
    //             }
    //             // TODO:
    //             // instr.mode = AddressingMode::Absolute;
    //             // return Err(instr.mnemonic.parser_error(
    //             //     format!(
    //             //         "unknown addressing mode for instruction '{}'",
    //             //         instr.mnemonic.value
    //             //     )
    //             //     .as_str(),
    //             // ));
    //         }
    //     }

    //     Ok(())
    // }

    // fn update_literal_with_context(literal: &mut Literal, context: &Context) -> Result<()> {
    //     // If it has already been set, skip it.
    //     // TODO: add a proper `is_set` thingie to it instead of this hack.
    //     if literal.bytes[0] != 0 || literal.bytes[1] != 0 {
    //         return Ok(());
    //     }

    //     // Evaluate any possible variable being used inside of this literal.
    //     let (evaled, resolved) = Self::replace_variable(&literal.identifier, context)?;

    //     // It may happen that the literal is just a label that is to be resolved
    //     // in the future. If so, let's leave early.
    //     literal.resolved = resolved;
    //     if !resolved {
    //         return Ok(());
    //     }

    //     // Parse the numeric value after a possible variable has been replaced.
    //     let two_bytes_allowed = literal.size == 2;
    //     let res = Self::parse_numeric(
    //         evaled.value.as_str(),
    //         &literal.identifier,
    //         two_bytes_allowed,
    //     );

    //     // And finally assign the computed bytes.
    //     match res {
    //         Ok(bytes) => {
    //             literal.bytes = bytes;
    //             Ok(())
    //         }
    //         Err(e) => {
    //             let msg = String::from("when parsing a data literal: ") + &e.message;
    //             Err(literal.identifier.parser_error(msg.as_str()))
    //         }
    //     }
    // }

    // fn parse_numeric(string: &str, node: &PString, two_bytes_allowed: bool) -> Result<[u8; 2]> {
    //     if string.starts_with('$') {
    //         Ok(Self::parse_hex_from(
    //             string,
    //             node,
    //             two_bytes_allowed,
    //             false,
    //             true,
    //         )?)
    //     } else if string.starts_with('%') {
    //         Ok([Self::parse_binary_from(string, node)?, 0])
    //     } else {
    //         Ok([Self::parse_decimal_from(string, node)?, 0])
    //     }
    // }

    // fn split_left_arm(node: &PString) -> Result<(PString, Option<PString>)> {
    //     let mut chars = node.value.chars();
    //     chars.next();
    //     let string = chars.as_str();

    //     match string.find(|c: char| c == ',') {
    //         Some(idx) => {
    //             let left1 = string.get(..idx).unwrap_or("").trim();
    //             let left2 = string.get(idx + 1..).unwrap_or("").trim();

    //             Ok((
    //                 PString {
    //                     value: left1.to_string(),
    //                     line: node.line,
    //                     range: Range {
    //                         start: node.range.start + 1,
    //                         end: node.range.start + 1 + left1.len(),
    //                     },
    //                 },
    //                 Some(PString {
    //                     value: left2.to_string(),
    //                     line: node.line,
    //                     range: Range {
    //                         start: node.range.start + 1 + idx,
    //                         end: node.range.start + 1 + idx + left2.len(),
    //                     },
    //                 }),
    //             ))
    //         }
    //         None => Ok((
    //             PString {
    //                 value: string.to_string(),
    //                 line: node.line,
    //                 range: Range {
    //                     start: node.range.start + 1,
    //                     end: node.range.end,
    //                 },
    //             },
    //             None,
    //         )),
    //     }
    // }

    // fn parse_binary_from(string: &str, node: &PString) -> Result<u8> {
    //     let mut value = 0;
    //     let mut shift = 0;

    //     for c in string.get(1..).unwrap_or("").chars().rev() {
    //         if c == '1' {
    //             let val = 1 << shift;
    //             value += val;
    //         } else if c != '0' {
    //             return Err(
    //                 node.parser_error(format!("bad binary format for '{}'", string).as_str())
    //             );
    //         }

    //         shift += 1;
    //     }

    //     if shift < 8 {
    //         Err(node.parser_error("missing binary digits to get a full byte"))
    //     } else if shift > 8 {
    //         Err(node.parser_error("too many binary digits for a single byte"))
    //     } else {
    //         Ok(value)
    //     }
    // }

    // // TODO: returns if resolved
    // fn replace_variable(node: &PString, context: &Context) -> Result<(PString, bool)> {
    //     match node
    //         .value
    //         .find(|c: char| c.is_alphabetic() || c == '_' || c == '@')
    //     {
    //         Some(idx) => {
    //             // Before doing any replacement, let's check the character
    //             // before the one that was found. In this case, if it was a
    //             // proper ASCII digit, then it cannot be a variable but it's
    //             // part of a numeric literal (e.g. '1A'): then just let the
    //             // different numeric parsing functions do their job.
    //             if idx > 0 {
    //                 let prev = node.value.chars().nth(idx - 1).unwrap_or(' ');
    //                 if prev.is_ascii_digit() {
    //                     return Ok((node.clone(), true));
    //                 }
    //             }

    //             // The variable might still be before an inner comma (e.g.
    //             // sta ($20, x)). We will assume that variables can happen
    //             // only before that.
    //             let end = node.value.find(',').unwrap_or(node.value.len());
    //             let mut string = node.value.get(idx..end).unwrap_or("");
    //             let tail = node.value.get(end..).unwrap_or("");

    //             // Get the context that might be being referenced.
    //             let ctxt = match string.find("::") {
    //                 Some(_) => {
    //                     let tctxt = string.rsplit_once("::").unwrap_or(("", ""));
    //                     if tctxt.0.is_empty() {
    //                         context.current()
    //                     } else {
    //                         string = tctxt.1;
    //                         context.find(tctxt.0)
    //                     }
    //                 }
    //                 None => context.current(),
    //             };

    //             match ctxt {
    //                 Some(hash) => {
    //                     // If there was a comma before the "variable" (i.e. idx >
    //                     // end and hence string == ""), or this is just the regular
    //                     // X or Y index, just return early.
    //                     match string.to_lowercase().as_str() {
    //                         "x" | "y" | "" => return Ok((node.clone(), true)),
    //                         _ => {}
    //                     }

    //                     // It's not any of the indices, let's look for a match on
    //                     // the current scope.
    //                     match hash.get(string) {
    //                         Some(var) => {
    //                             // If this is just a memory address (e.g.
    //                             // label), then just return it as is.
    //                             if var.label {
    //                                 return Ok((node.clone(), false));
    //                             }

    //                             let value = String::from(node.value.get(..idx).unwrap_or(""))
    //                                 + var.node.value.as_str();
    //                             Ok((
    //                                 PString {
    //                                     value: value.clone() + tail,
    //                                     line: node.line,
    //                                     range: Range {
    //                                         start: node.range.start,
    //                                         end: node.range.start + value.len(),
    //                                     },
    //                                 },
    //                                 true,
    //                             ))
    //                         }
    //                         None => {
    //                             // If a variable could not be found, check that
    //                             // this is not a purely hexadecimal number (e.g.
    //                             // 'AA'). If that's the case, then just return
    //                             // its value.
    //                             if Self::parse_hex_from(string, node, true, false, false).is_ok() {
    //                                 return Ok((node.clone(), true));
    //                             }

    //                             // We've tried hard to not assume the programmer
    //                             // messing up, but there's no other way around
    //                             // it: it's an "unknown variable" error.
    //                             return Err(node.parser_error(
    //                                 format!("unknown variable '{}'", string).as_str(),
    //                             ));
    //                         }
    //                     }
    //                 }
    //                 None => {
    //                     Err(node.parser_error(format!("unknown scope '{}'", "Global").as_str()))
    //                 }
    //             }
    //         }
    //         None => Ok((node.clone(), true)),
    //     }
    // }

    // fn parse_macro_definition(&mut self, id: &PString, line: &str) -> Result<()> {
    //     self.skip_whitespace(line);

    //     let identifier = self.fetch_identifier(id, line)?;
    //     if identifier.is_reserved() {
    //         return Err(identifier.parser_error(
    //             format!(
    //                 "cannot use reserved name '{}' for proc name",
    //                 identifier.value
    //             )
    //             .as_str(),
    //         ));
    //     }

    //     self.mapping.current_macro = Some(identifier.value.clone());
    //     self.mapping.macros.entry(identifier.value).or_default();
    //     Ok(())
    // }

    // fn parse_macro_end(&mut self, id: &PString) -> Result<()> {
    //     match self.mapping.current_macro {
    //         Some(_) => self.mapping.current_macro = None,
    //         None => {
    //             return Err(id.parser_error(
    //                 format!("bad `.endmacro`: we are not inside of a macro definition").as_str(),
    //             ))
    //         }
    //     }

    //     Ok(())
    // }

    // fn parse_proc_definition(&mut self, id: &PString, line: &str) -> Result<()> {
    //     self.skip_whitespace(line);

    //     let identifier = self.fetch_identifier(id, line)?;
    //     if identifier.is_reserved() {
    //         return Err(identifier.parser_error(
    //             format!(
    //                 "cannot use reserved name '{}' for proc name",
    //                 identifier.value
    //             )
    //             .as_str(),
    //         ));
    //     }

    //     // Insert the given identifier into the context.
    //     if let Some(entry) = self.context.current_mut() {
    //         match entry.entry(identifier.value.clone()) {
    //             Entry::Occupied(e) => {
    //                 return Err(ParseError {
    //                     line: self.line,
    //                     message: format!(
    //                     "proc '{}' already exists for this context: it was previously defined in line {}",
    //                     id.value, e.get().node.line),
    //                 })
    //             }
    //             Entry::Vacant(e) => e.insert(PValue {
    //                 node: PString {
    //                 value: identifier.value.clone(),
    //                 line: self.line,
    //                 range: Range {
    //                     start: id.range.start,
    //                     end: id.range.end,
    //                 },
    //                 },
    //                 value: 0,
    //                 label: true,
    //             }),
    //         };
    //     }

    //     // And add the node so it's picked up later.
    //     self.mapping.push(Node::Label(Label {
    //         value: identifier.value.to_string(),
    //     }));

    //     // TODO: lol
    //     self.context.push_stack(&identifier.value);

    //     self.mapping.push(Node::Scoped(Scoped {
    //         identifier: identifier.clone(),
    //         start: true,
    //     }));

    //     Ok(())
    // }

    // fn parse_proc_end(&mut self, id: &PString) -> Result<()> {
    //     if !self.context.pop() {
    //         return Err(id.parser_error("missmatched '.endproc': there is no proc to end"));
    //     }
    //     self.mapping.push(Node::Scoped(Scoped {
    //         identifier: PString::new(),
    //         start: false,
    //     }));

    //     Ok(())
    // }

    // fn parse_segment_definition(&mut self, id: &PString, line: &str) -> Result<()> {
    //     self.skip_whitespace(line);

    //     let identifier = self.fetch_possibly_quoted_identifier(id, line)?;
    //     self.mapping.switch(&identifier)?;

    //     Ok(())
    // }

    // fn parse_scope_definition(&mut self, id: &PString, line: &str) -> Result<()> {
    //     self.skip_whitespace(line);

    //     let identifier = self.fetch_identifier(id, line)?;
    //     if identifier.is_reserved() {
    //         return Err(identifier.parser_error(
    //             format!("cannot use reserved name '{}'", identifier.value).as_str(),
    //         ));
    //     }
    //     self.context.push(&identifier.value);
    //     self.mapping.push(Node::Scoped(Scoped {
    //         identifier,
    //         start: true,
    //     }));

    //     Ok(())
    // }

    // fn parse_scope_end(&mut self, id: &PString) -> Result<()> {
    //     if !self.context.pop() {
    //         return Err(id.parser_error("missmatched '.endscope': there is no scope to end"));
    //     }
    //     self.mapping.push(Node::Scoped(Scoped {
    //         identifier: PString::new(),
    //         start: false,
    //     }));

    //     Ok(())
    // }

    // fn parse_literal_bytes(
    //     &mut self,
    //     node: &PString,
    //     line: &str,
    //     two_bytes_allowed: bool,
    // ) -> Result<()> {
    //     loop {
    //         self.skip_whitespace(line);

    //         match line.chars().nth(self.column) {
    //             Some(byte) => {
    //                 let needle = if byte == '\'' {
    //                     self.column += 1;
    //                     self.skip_whitespace(line);
    //                     '\''
    //                 } else if byte == '"' {
    //                     self.column += 1;
    //                     self.skip_whitespace(line);
    //                     '"'
    //                 } else {
    //                     ','
    //                 };

    //                 // Find the index of the needle. If it cannot be found, try
    //                 // to find the first whitespace (e.g. to ditch out inline
    //                 // comments or other artifacts). If neither of these are
    //                 // found, it will simply return the end of the string.
    //                 //
    //                 // TODO: instead of ditching out what's right of the first
    //                 // whitespace, try to error out on weird scenarios.
    //                 let needle_idx = line
    //                     .get(self.column..)
    //                     .unwrap_or("")
    //                     .find(|c: char| c == needle);
    //                 let idx = match needle_idx {
    //                     Some(v) => v,
    //                     None => line
    //                         .get(self.column..)
    //                         .unwrap_or("")
    //                         .find(|c: char| c.is_whitespace())
    //                         .unwrap_or(line.len() - self.column),
    //                 };

    //                 // If this is the last character, the needle was a quote and
    //                 // the last char is not the needle, then it means that the
    //                 // quote was left open. Complain about this as well.
    //                 if idx == line.len() - self.column {
    //                     if line.chars().nth(idx).unwrap_or(' ') != needle
    //                         && (needle == '"' || needle == '\'')
    //                     {
    //                         return Err(node.parser_error("non-terminated quote for byte literal"));
    //                     }
    //                 }

    //                 // Now we have our string. Before pushing it, though, there
    //                 // is a special case for alphabetic literals that need to be
    //                 // translated.
    //                 let string = line.get(self.column..self.column + idx).unwrap_or(" ");
    //                 let mut bytes: [u8; 2] = [0, 0];
    //                 if string.len() == 1 && string.chars().nth(0).unwrap().is_ascii_alphabetic() {
    //                     let v = Vec::from(string);
    //                     bytes[0] = v[0];
    //                 }

    //                 // NOTE: for now we push an incomplete literal. We need the
    //                 // first pass to fill the context and then a second pass
    //                 // will evaluate each literal as needed (e.g. replacing
    //                 // values from variables being used in this literal).
    //                 self.mapping.push(Node::Literal(Literal {
    //                     identifier: PString {
    //                         value: string.to_owned(),
    //                         line: self.line,
    //                         range: Range {
    //                             start: self.column,
    //                             end: self.column + idx,
    //                         },
    //                     },
    //                     size: if two_bytes_allowed { 2 } else { 1 },
    //                     bytes,
    //                     resolved: true,
    //                 }));

    //                 self.column += idx;
    //                 for c in line.get(self.column..).unwrap_or(" ").chars() {
    //                     if c == ',' {
    //                         break;
    //                     }
    //                     if c == ';' {
    //                         return Ok(());
    //                     }
    //                     self.column += 1;
    //                 }
    //                 self.column += 1;
    //                 self.skip_whitespace(line);
    //             }
    //             None => break,
    //         };
    //     }

    //     Ok(())
    // }

    // fn fetch_identifier(&mut self, id: &PString, line: &str) -> Result<PString> {
    //     let idx = line
    //         .get(self.column..)
    //         .unwrap_or(" ")
    //         .find(|c: char| c.is_whitespace());

    //     match idx {
    //         Some(offset) => {
    //             let end = self.column + offset;
    //             let rest = line.get(end..).unwrap_or("").trim();
    //             if !rest.is_empty() {
    //                 if rest.chars().nth(0).unwrap_or(' ') != ';' {
    //                     return Err(id.parser_error(
    //                         "there should not be any further content besides the identifier",
    //                     ));
    //                 }
    //             }
    //             Ok(PString {
    //                 value: line.get(self.column..end).unwrap_or(" ").trim().to_string(),
    //                 line: self.line,
    //                 range: Range {
    //                     start: self.column,
    //                     end,
    //                 },
    //             })
    //         }
    //         None => Ok(PString {
    //             value: line.get(self.column..).unwrap_or(" ").trim().to_string(),
    //             line: self.line,
    //             range: Range {
    //                 start: self.column,
    //                 end: line.len(),
    //             },
    //         }),
    //     }
    // }

    // fn fetch_possibly_quoted_identifier(&mut self, id: &PString, line: &str) -> Result<PString> {
    //     let mut identifier = self.fetch_identifier(id, line)?;

    //     if identifier.value.starts_with('\'') || identifier.value.starts_with('`') {
    //         return Err(id.parser_error("use double quotes for the segment identifier instead"));
    //     } else if identifier.value.starts_with('"') {
    //         identifier.value = match identifier
    //             .value
    //             .get(1..(identifier.range.end - identifier.range.start - 1))
    //         {
    //             Some(v) => v.to_string(),
    //             None => return Err(id.parser_error("could not fetch quoted identifier")),
    //         };
    //         if identifier.value.contains('"') {
    //             return Err(id.parser_error("do not use double quotes inside of the identifier"));
    //         }
    //         identifier.range.start += 1;
    //         identifier.range.end -= 1;
    //     }

    //     Ok(identifier)
    // }

    // fn parse_label(&mut self, id: PString, _line: &str) -> Result<()> {
    //     let name = &id.value.as_str()[..id.value.len() - 1].to_string();

    //     // Forbid weird scenarios.
    //     if name.contains("::") {
    //         return Err(id.parser_error(
    //             format!(
    //                 "the label '{}' is scoped: do not declare variables this way",
    //                 id.value
    //             )
    //             .as_str(),
    //         ));
    //     }

    //     // Insert the given label into the context.
    //     if let Some(entry) = self.context.current_mut() {
    //         match entry.entry(name.clone()) {
    //             Entry::Occupied(e) => {
    //                 return Err(ParseError {
    //                     line: self.line,
    //                     message: format!(
    //                     "label '{}' already exists for this context: it was previously defined in line {}",
    //                     id.value, e.get().node.line),
    //                 })
    //             }
    //             Entry::Vacant(e) => e.insert(PValue {
    //                 node: PString {
    //                 value: name.clone(),
    //                 line: self.line,
    //                 range: Range {
    //                     start: id.range.start,
    //                     end: id.range.end,
    //                 },
    //                 },
    //                 value: 0,
    //                 label: true,
    //             }),
    //         };
    //     }

    //     // And add the node so it's picked up later.
    //     self.mapping.push(Node::Label(Label {
    //         value: name.to_string(),
    //     }));
    //     Ok(())
    // }

    fn parser_error(&self, msg: &str) -> ParseError {
        ParseError {
            message: String::from(msg),
            line: self.line,
            parse: true,
        }
    }

    // fn from_byte_reader<R: Read>(&mut self, mut reader: R) -> Result<()> {
    //     loop {
    //         let mut buf = [0; 1];
    //         let n = reader.read(&mut buf)?;
    //         if n == 0 {
    //             break;
    //         }

    //         match OPCODES.get(&buf[0]) {
    //             Some(v) => {
    //                 let mut bs = [0; 2];
    //                 for i in 0..v.size - 1 {
    //                     let nn = reader.read(&mut buf)?;
    //                     if nn == 0 {
    //                         break;
    //                     }
    //                     bs[i as usize] = buf[0];
    //                 }
    //                 self.mapping.push(Node::Instruction(Instruction {
    //                     mnemonic: PString::from(&v.mnemonic),
    //                     opcode: v.opcode,
    //                     size: v.size,
    //                     bytes: bs,
    //                     left: None,
    //                     right: None,
    //                     mode: v.mode.to_owned(),
    //                     cycles: v.cycles,
    //                     affected_on_page: v.affected_on_page,
    //                     address: 0, // TODO
    //                     resolved: true,
    //                 }))
    //             }

    //             None => {
    //                 return Err(
    //                     self.parser_error(format!("unknown byte '0x{:02X}'", buf[0]).as_str())
    //                 )
    //             }
    //         }
    //     }

    //     Ok(())
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mapping::EMPTY;

    fn instruction_test(line: &str, hex: &[u8], skip_disassemble: bool) {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let res = asm.assemble(line.as_bytes()).unwrap();

        assert_eq!(res.len(), 1);

        for i in 0..res[0].size {
            assert_eq!(hex[i as usize], res[0].bytes[i as usize]);
        }

        if skip_disassemble {
            return;
        }
        // TODO
    }

    fn instruction_err(line: &str, message: &str) {
        let mut asm = Assembler::new(EMPTY.to_vec());
        let err = asm.assemble(line.as_bytes());

        assert!(err.is_err());
        if let Err(e) = err {
            assert_eq!(e.message, message);
        }
    }

    #[test]
    fn bad_addressing() {
        instruction_err("unknown #$20", "unknown instruction 'unknown'");
        instruction_err(
            "adc ($2002, x)",
            "address can only be one byte long on indirect X addressing",
        );
        instruction_err(
            "adc ($20, x), y",
            "it has to be either X addressing or Y addressing, not all at once",
        );
        instruction_err(
            "adc ($2002), y",
            "address can only be one byte long on indirect Y addressing",
        );
        instruction_err(
            "adc ($20, y)",
            "only the X index is allowed on indirect X addressing",
        );
        instruction_err(
            "adc ($20), x",
            "only the Y index is allowed on indirect Y addressing",
        );
        instruction_err("jmp ($20)", "expecting a full 16-bit address");
        instruction_err("adc $20, z", "can only use X and Y as indices");
        instruction_err(
            "adc ($2000)",
            "cannot use indirect addressing mode for the instruction 'adc'",
        );
        instruction_err("lda 12", "no prefix was given to operand")
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
        instruction_test("adc #%10100010", &[0x69, 0xA2], true);
    }

    #[test]
    fn parse_hexadecimal() {
        instruction_err("adc $", "expecting a number of 1 to 4 hexadecimal digits");
        // TODO: see comment on literal_mode being a stack.
        instruction_err("adc #$", "expecting a number of 1 to 4 hexadecimal digits");
        instruction_err("adc $AW", "could not convert digit to hexadecimal");
        instruction_test("adc $AA", &[0x65, 0xAA], false);
        instruction_test("adc $10", &[0x65, 0x10], false);
        instruction_test("adc $10AB", &[0x6D, 0xAB, 0x10], false);
    }

    #[test]
    fn parse_decimal() {
        instruction_err("adc #", "empty decimal literal");
        instruction_err("adc #256", "decimal value is too big");
        instruction_err("adc #2000", "decimal value is too big");
        instruction_err("adc #2A", "unknown variable '2A'"); // TODO: not sure about this
        instruction_test("adc #1", &[0x69, 0x01], true);
    }

    // Individual instructions.

    #[test]
    fn adc() {
        instruction_test("adc #20", &[0x69, 0x14], true);
        instruction_test("adc #$20", &[0x69, 0x20], false);
        instruction_test("adc $2002", &[0x6D, 0x02, 0x20], false);
        instruction_test("adc $20", &[0x65, 0x20], false);
        instruction_test("adc $20, x", &[0x75, 0x20], false);
        instruction_test("adc $2002, x", &[0x7D, 0x02, 0x20], false);
        instruction_test("adc $2002, y", &[0x79, 0x02, 0x20], false);
        instruction_test("adc ($20, x)", &[0x61, 0x20], false);
        instruction_test("adc ($20), y", &[0x71, 0x20], false);
    }

    #[test]
    fn sbc() {
        instruction_test("sbc #$20", &[0xE9, 0x20], false);
        instruction_test("sbc $2002", &[0xED, 0x02, 0x20], false);
        instruction_test("sbc $20", &[0xE5, 0x20], false);
        instruction_test("sbc $20, x", &[0xF5, 0x20], false);
        instruction_test("sbc $2002, x", &[0xFD, 0x02, 0x20], false);
        instruction_test("sbc $2002, y", &[0xF9, 0x02, 0x20], false);
        instruction_test("sbc ($20, x)", &[0xE1, 0x20], false);
        instruction_test("sbc ($20), y", &[0xF1, 0x20], false);
    }

    #[test]
    fn shift() {
        // asl
        instruction_test("asl", &[0x0A], false);
        instruction_test("asl a", &[0x0A], true);
        instruction_test("asl $20", &[0x06, 0x20], false);
        instruction_test("asl $20, x", &[0x16, 0x20], false);
        instruction_test("asl $2002", &[0x0E, 0x02, 0x20], false);
        instruction_test("asl $2002, x", &[0x1E, 0x02, 0x20], false);

        // lsr
        instruction_test("lsr", &[0x4A], false);
        instruction_test("lsr a", &[0x4A], true);
        instruction_test("lsr $20", &[0x46, 0x20], false);
        instruction_test("lsr $20, x", &[0x56, 0x20], false);
        instruction_test("lsr $2002", &[0x4E, 0x02, 0x20], false);
        instruction_test("lsr $2002, x", &[0x5E, 0x02, 0x20], false);
    }

    #[test]
    fn rotate() {
        // rol
        instruction_test("rol", &[0x2A], false);
        instruction_test("rol a", &[0x2A], true);
        instruction_test("rol $20", &[0x26, 0x20], false);
        instruction_test("rol $20, x", &[0x36, 0x20], false);
        instruction_test("rol $2002", &[0x2E, 0x02, 0x20], false);
        instruction_test("rol $2002, x", &[0x3E, 0x02, 0x20], false);

        // ror
        instruction_test("ror", &[0x6A], false);
        instruction_test("ror a", &[0x6A], true);
        instruction_test("ror $20", &[0x66, 0x20], false);
        instruction_test("ror $20, x", &[0x76, 0x20], false);
        instruction_test("ror $2002", &[0x6E, 0x02, 0x20], false);
        instruction_test("ror $2002, x", &[0x7E, 0x02, 0x20], false);
    }

    #[test]
    fn and() {
        instruction_test("and #$20", &[0x29, 0x20], false);
        instruction_test("and $2002", &[0x2D, 0x02, 0x20], false);
        instruction_test("and $20", &[0x25, 0x20], false);
        instruction_test("and $20, x", &[0x35, 0x20], false);
        instruction_test("and $2002, x", &[0x3D, 0x02, 0x20], false);
        instruction_test("and $2002, y", &[0x39, 0x02, 0x20], false);
        instruction_test("and ($20, x)", &[0x21, 0x20], false);
        instruction_test("and ($20), y", &[0x31, 0x20], false);
    }

    #[test]
    fn or() {
        // eor
        instruction_test("eor #$20", &[0x49, 0x20], false);
        instruction_test("eor $20", &[0x45, 0x20], false);
        instruction_test("eor $20, x", &[0x55, 0x20], false);
        instruction_test("eor $2002", &[0x4D, 0x02, 0x20], false);
        instruction_test("eor $2002, x", &[0x5D, 0x02, 0x20], false);
        instruction_test("eor $2002, y", &[0x59, 0x02, 0x20], false);
        instruction_test("eor ($20, x)", &[0x41, 0x20], false);
        instruction_test("eor ($20), y", &[0x51, 0x20], false);

        // ora
        instruction_test("ora #$20", &[0x09, 0x20], false);
        instruction_test("ora $20", &[0x05, 0x20], false);
        instruction_test("ora $20, x", &[0x15, 0x20], false);
        instruction_test("ora $2002", &[0x0D, 0x02, 0x20], false);
        instruction_test("ora $2002, x", &[0x1D, 0x02, 0x20], false);
        instruction_test("ora $2002, y", &[0x19, 0x02, 0x20], false);
        instruction_test("ora ($20, x)", &[0x01, 0x20], false);
        instruction_test("ora ($20), y", &[0x11, 0x20], false);
    }

    #[test]
    fn load() {
        // lda
        instruction_test("lda #$20", &[0xA9, 0x20], false);
        instruction_test("lda $20", &[0xA5, 0x20], false);
        instruction_test("lda $20, x", &[0xB5, 0x20], false);
        instruction_test("lda $2002", &[0xAD, 0x02, 0x20], false);
        instruction_test("lda $2002, x", &[0xBD, 0x02, 0x20], false);
        instruction_test("lda $2002, y", &[0xB9, 0x02, 0x20], false);
        instruction_test("lda ($20, x)", &[0xA1, 0x20], false);
        instruction_test("lda ($20), y", &[0xB1, 0x20], false);

        // ldx
        instruction_test("ldx #$20", &[0xA2, 0x20], false);
        instruction_test("ldx $20", &[0xA6, 0x20], false);
        instruction_test("ldx $20, y", &[0xB6, 0x20], false);
        instruction_test("ldx $2002", &[0xAE, 0x02, 0x20], false);
        instruction_test("ldx $2002, y", &[0xBE, 0x02, 0x20], false);

        // ldy
        instruction_test("ldy #$20", &[0xA0, 0x20], false);
        instruction_test("ldy $20", &[0xA4, 0x20], false);
        instruction_test("ldy $20, x", &[0xB4, 0x20], false);
        instruction_test("ldy $2002", &[0xAC, 0x02, 0x20], false);
        instruction_test("ldy $2002, x", &[0xBC, 0x02, 0x20], false);
    }

    #[test]
    fn jump() {
        instruction_test("jsr $2002", &[0x20, 0x02, 0x20], false);

        instruction_test("jmp $2002", &[0x4C, 0x02, 0x20], false);
        instruction_test("jmp ($2002)", &[0x6C, 0x02, 0x20], false);
    }

    #[test]
    fn inc_dec_instructions() {
        // inc
        instruction_test("inc $10", &[0xE6, 0x10], false);
        instruction_test("inc $1000", &[0xEE, 0x00, 0x10], false);
        instruction_test("inc $10, x", &[0xF6, 0x10], false);
        instruction_test("inc $1000, x", &[0xFE, 0x00, 0x10], false);

        instruction_test("inx", &[0xE8], false);

        instruction_test("iny", &[0xC8], false);

        // dec
        instruction_test("dec $10", &[0xC6, 0x10], false);
        instruction_test("dec $1000", &[0xCE, 0x00, 0x10], false);
        instruction_test("dec $10, x", &[0xD6, 0x10], false);
        instruction_test("dec $1000, x", &[0xDE, 0x00, 0x10], false);

        instruction_test("dex", &[0xCA], false);

        instruction_test("dey", &[0x88], false);
    }

    #[test]
    fn transfer_instructions() {
        instruction_test("tax", &[0xAA], false);
        instruction_test("tay", &[0xA8], false);
        instruction_test("tsx", &[0xBA], false);
        instruction_test("txa", &[0x8A], false);
        instruction_test("txs", &[0x9A], false);
        instruction_test("tya", &[0x98], false);
    }

    #[test]
    fn return_instructions() {
        instruction_test("rti", &[0x40], false);
        instruction_test("rts", &[0x60], false);
    }

    #[test]
    fn set_clear_instructions() {
        instruction_test("clc", &[0x18], false);
        instruction_test("cld", &[0xD8], false);
        instruction_test("cli", &[0x58], false);
        instruction_test("clv", &[0xB8], false);

        instruction_test("sec", &[0x38], false);
        instruction_test("sed", &[0xF8], false);
        instruction_test("sei", &[0x78], false);
    }

    #[test]
    fn push_pull_instructions() {
        instruction_test("pha", &[0x48], false);
        instruction_test("php", &[0x08], false);
        instruction_test("pla", &[0x68], false);
        instruction_test("plp", &[0x28], false);
    }

    #[test]
    fn nop_brk() {
        instruction_test("nop", &[0xEA], false);
        instruction_test("brk", &[0x00], false);
    }

    #[test]
    fn cmp() {
        // cmp
        instruction_test("cmp #$20", &[0xC9, 0x20], false);
        instruction_test("cmp $2002", &[0xCD, 0x02, 0x20], false);
        instruction_test("cmp $20", &[0xC5, 0x20], false);
        instruction_test("cmp $20, x", &[0xD5, 0x20], false);
        instruction_test("cmp $2002, x", &[0xDD, 0x02, 0x20], false);
        instruction_test("cmp $2002, y", &[0xD9, 0x02, 0x20], false);
        instruction_test("cmp ($20, x)", &[0xC1, 0x20], false);
        instruction_test("cmp ($20), y", &[0xD1, 0x20], false);

        // cpx
        instruction_test("cpx #$20", &[0xE0, 0x20], false);
        instruction_test("cpx $2002", &[0xEC, 0x02, 0x20], false);
        instruction_test("cpx $20", &[0xE4, 0x20], false);

        // cpy
        instruction_test("cpy #$20", &[0xC0, 0x20], false);
        instruction_test("cpy $2002", &[0xCC, 0x02, 0x20], false);
        instruction_test("cpy $20", &[0xC4, 0x20], false);
    }

    #[test]
    fn store_instructions() {
        //sta
        instruction_test("sta $20", &[0x85, 0x20], false);
        instruction_test("sta $20, x", &[0x95, 0x20], false);
        instruction_test("sta $2002", &[0x8D, 0x02, 0x20], false);
        instruction_test("sta $2002, x", &[0x9D, 0x02, 0x20], false);
        instruction_test("sta $2002, y", &[0x99, 0x02, 0x20], false);
        instruction_test("sta ($20, x)", &[0x81, 0x20], false);
        instruction_test("sta ($20), y", &[0x91, 0x20], false);

        // stx
        instruction_test("stx $20", &[0x86, 0x20], false);
        instruction_test("stx $20, y", &[0x96, 0x20], false);
        instruction_test("stx $2002", &[0x8E, 0x02, 0x20], false);

        // sty
        instruction_test("sty $20", &[0x84, 0x20], false);
        instruction_test("sty $20, x", &[0x94, 0x20], false);
        instruction_test("sty $2002", &[0x8C, 0x02, 0x20], false);
    }

    #[test]
    fn bit() {
        instruction_test("bit $10", &[0x24, 0x10], false);
        instruction_test("bit $1001", &[0x2C, 0x01, 0x10], false);
    }

    // Variables & scopes.

    #[test]
    fn using_variables() {
        // TODO
        // todo!()
    }

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
    fn redefined_variable() {
        let mut parser = Assembler::new(EMPTY.to_vec());
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
                "variable 'Yet' is being re-assigned: it was previously defined in line 7"
            );
        }
    }

    #[test]
    fn bad_assignment() {
        instruction_err("Variable =", "incomplete assignment");
        instruction_err("Variable =  ; comment", "incomplete assignment");
    }
}

//     // Literals

//     #[test]
//     fn byte_literals_errors() {
//         // TODO
//         // instruction_err(
//         //     ".byte $0102",
//         //     "when parsing a data literal: only one byte of data is allowed here",
//         // );
//         // instruction_err(".byte '$01", "non-terminated quote for byte literal");
//         // instruction_err(".byte '$01, $02", "non-terminated quote for byte literal");
//     }

//     #[test]
//     fn byte_literals() {
//         let mut asm = Assembler::new(EMPTY.to_vec());

//         let mut res = asm.assemble(".byte $01".as_bytes()).unwrap();
//         assert_eq!(res.len(), 1);
//         assert_hex(res[0], &[0x01]);

//         asm.reset();
//         res = asm.assemble(".db $01, $02".as_bytes()).unwrap();
//         assert_eq!(res.len(), 2);
//         assert_hex(res[0], &[0x01]);
//         assert_hex(res[1], &[0x02]);

//         asm.reset();
//         res = asm
//             .assemble(".byte $01, 2, '%00000011', \"$04\"".as_bytes())
//             .unwrap();
//         assert_eq!(res.len(), 4);
//         assert_hex(res[0], &[0x01]);
//         assert_hex(res[1], &[0x02]);
//         assert_hex(res[2], &[0x03]);
//         assert_hex(res[3], &[0x04]);
//     }

//     #[test]
//     fn word_literals() {
//         let mut asm = Assembler::new(EMPTY.to_vec());

//         let mut res = asm.assemble(".word $01".as_bytes()).unwrap();
//         assert_eq!(res.len(), 1);
//         assert_hex(res[0], &[0x01, 0x00]);

//         asm.reset();
//         res = asm.assemble(".dw $0102, $02".as_bytes()).unwrap();
//         assert_eq!(res.len(), 2);
//         assert_hex(res[0], &[0x02, 0x01]);
//         assert_hex(res[1], &[0x02, 0x00]);

//         asm.reset();
//         res = asm
//             .assemble(".word $0102, $0204, '$0308', \"$0410\"".as_bytes())
//             .unwrap();
//         assert_eq!(res.len(), 4);
//         assert_hex(res[0], &[0x02, 0x01]);
//         assert_hex(res[1], &[0x04, 0x02]);
//         assert_hex(res[2], &[0x08, 0x03]);
//         assert_hex(res[3], &[0x10, 0x04]);
//     }

//     #[test]
//     fn variables_in_literals() {
//         let mut asm = Assembler::new(EMPTY.to_vec());
//         let res = asm
//             .assemble(
//                 r#"
// .scope One
//   Variable = $01
// .endscope

// Variable = $02
// .byte One::Variable, Variable, $03
// "#
//                 .as_bytes(),
//             )
//             .unwrap();

//         assert_eq!(res.len(), 3);
//         assert_hex(res[0], &[0x01]);
//         assert_hex(res[1], &[0x02]);
//         assert_hex(res[2], &[0x03]);
//     }
// }
