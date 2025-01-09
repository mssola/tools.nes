use crate::mapping::{get_mapping_configuration, Mapping};
use crate::node::{ControlType, NodeType, OperationType, PNode};
use crate::object::{Bundle, Context, Object, ObjectType};
use crate::opcodes::{AddressingMode, INSTRUCTIONS};
use crate::parser::Parser;
use crate::Error;
use crate::SourceInfo;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::ops::Neg;

/// The mode in which a literal is expressed.
#[derive(Clone, PartialEq)]
pub enum LiteralMode {
    /// An 8/16 bit hexadecimal value.
    Hexadecimal,

    /// A byte expressed in binary format.
    Binary,

    /// A byte expressed in plain decimal format.
    Plain,
}

/// The different stages that the assembler goes through and which are relevant
/// for the process.
#[derive(PartialEq)]
pub enum Stage {
    /// The context is still building up (i.e. we don't have all the variable
    /// values, labels and their addresses yet).
    Context,

    /// An initial context is there, and we can already consume most bundles. We
    /// only need to leave some of them as pending if they require a value that
    /// is relative to the end size of segments.
    Bundling,

    /// We have most bundles, and we already know the size for all segments.
    /// Hence, we can resolve (crunch) the nodes that were pending to be
    /// bundled.
    Crunching,
}

#[derive(Clone, Debug)]
pub struct PendingNode {
    mapping: usize,
    segment: usize,
    context: String,
    bundle_index: usize,
    node: PNode,
    labels_seen: usize,
}

pub struct Assembler<'a> {
    context: Context,
    literal_mode: Option<LiteralMode>,
    stage: Stage,
    macros: HashMap<String, &'a PNode>,
    mappings: Vec<Mapping>,
    current_mapping: usize,
    current_segment: usize,
    pending: Vec<PendingNode>,
    labels_seen: usize,

    // Number of macro statements seen on a given iteration. Note that the
    // parser guarantees that macros are defined well (no unclosed macros nor
    // too many .endmacro's).
    macros_seen: usize,

    // Same as macros_seen but for .proc's.
    procs_seen: usize,

    // Same as macros_seen but for .repeat's.
    repeats_seen: usize,

    // Warnings that have accumulated over the run.
    warnings: Vec<Error>,

    // TODO
    sources: Vec<SourceInfo>,
}

#[derive(Debug)]
pub struct AssemblerResult {
    pub bundles: Vec<Bundle>,
    pub errors: Vec<Error>,
    pub warnings: Vec<Error>,
}

/// Read the contents from the `reader` as a source file and produce a list of
/// bundles that can be formatted as binary data. You also need to pass
/// information of the source by means of `source`, as otherwise control
/// statements like ".import" or ".incbin" wouldn't know how to resolve relative
/// paths. You can specify the mapper to be used as an identifier in `mapping`,
/// which will be handled via `get_mapping_configuration`.
pub fn assemble(reader: impl Read, mapping: &str, source: SourceInfo) -> AssemblerResult {
    let config = match get_mapping_configuration(mapping) {
        Ok(config) => config,
        Err(e) => {
            return AssemblerResult {
                bundles: vec![],
                errors: vec![Error {
                    global: true,
                    line: 0,
                    message: e,
                    source,
                }],
                warnings: vec![],
            };
        }
    };

    assemble_with_mapping(reader, config, source)
}

/// Read the contents from the `reader` as a source file and produce a list of
/// bundles that can be formatted as binary data. You also need to pass
/// information of the source by means of `source`, as otherwise control
/// statements like ".import" or ".incbin" wouldn't know how to resolve relative
/// paths. You also need to provide the `mapping` as handled internally. If you
/// are unsure how to use it, just call `assemble`.
pub fn assemble_with_mapping(
    reader: impl Read,
    mapping: Vec<Mapping>,
    source: SourceInfo,
) -> AssemblerResult {
    let mut asm = Assembler::new(mapping);

    // First of all, parse the input so we get a list of nodes we can work
    // with.
    let mut parser = Parser::default();
    if let Err(errors) = parser.parse(reader, source) {
        return AssemblerResult {
            bundles: vec![],
            errors,
            warnings: asm.warnings,
        };
    }

    let nodes = parser.nodes();
    asm.sources = parser.sources;

    // Build the context by iterating over the parsed nodes and checking
    // where scopes start/end, evaluating values for variables, labels, etc.
    if let Err(errors) = asm.eval_context(&nodes) {
        return AssemblerResult {
            bundles: vec![],
            errors,
            warnings: asm.warnings,
        };
    }

    // Convert the relevant nodes into binary bundles which can be used by
    // the caller. This is done for most nodes, even if some of them will
    // have to be marked as pending, since they depend on knowing the exact
    // size for a given segment.
    if let Err(errors) = asm.bundle(&nodes) {
        return AssemblerResult {
            bundles: vec![],
            errors,
            warnings: asm.warnings,
        };
    }

    // Now we know how much each segment spans, and we can resolve (crunch)
    // the nodes marked as pending.
    if let Err(errors) = asm.crunch() {
        return AssemblerResult {
            bundles: vec![],
            errors,
            warnings: asm.warnings,
        };
    }

    // All set, fill the vector of bundles to be returned.
    match asm.fill() {
        Ok(bundles) => AssemblerResult {
            bundles,
            errors: vec![],
            warnings: asm.warnings,
        },
        Err(errors) => AssemblerResult {
            bundles: vec![],
            errors,
            warnings: asm.warnings,
        },
    }
}

impl<'a> Assembler<'a> {
    pub fn new(mappings: Vec<Mapping>) -> Self {
        Self {
            context: Context::new(),
            literal_mode: None,
            stage: Stage::Context,
            macros: HashMap::new(),
            mappings,
            current_mapping: 0,
            current_segment: 0,
            pending: vec![],
            labels_seen: 0,
            macros_seen: 0,
            procs_seen: 0,
            repeats_seen: 0,
            warnings: vec![],
            sources: vec![],
        }
    }

    // Define a new variable by taking the given `id`. This variable will only
    // be created if `id` is not empty. The function will error out if the given
    // name is already taken.
    fn define_variable(&mut self, node: &PNode) -> Result<(), Error> {
        if node.value.is_empty() {
            return Ok(());
        }

        if let Err(message) = self.context.set_variable(
            &node.value,
            &Object::new(
                self.current_mapping,
                self.current_segment,
                ObjectType::Address,
            ),
            false,
        ) {
            return Err(Error {
                message,
                line: node.value.line,
                global: false,
                source: self.source_for(node),
            });
        }

        Ok(())
    }

    fn eval_context(&mut self, nodes: &'a [PNode]) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();

        for node in nodes {
            match &node.node_type {
                // At this stage we only define the label into the current
                // context so it's known. The actual value cannot be computed
                // right now as we don't know the segment size where it belongs
                // yet.
                NodeType::Label => {
                    // There's no good reason to declare a named label inside of
                    // a macro. If that's the case, just error out.
                    if (self.macros_seen > 0 || self.repeats_seen > 0) && !node.value.is_empty() {
                        errors.push(Error {
                            line: node.value.line,
                            message: format!(
                                "using a named label ('{}') inside of a macro/repeat definition",
                                node.value.value
                            ),
                            source: self.source_for(node),
                            global: false,
                        });
                        continue;
                    }
                    if let Err(err) = self.define_variable(node) {
                        errors.push(err);
                    }
                }
                NodeType::Assignment => {
                    if self.macros_seen > 0 || self.repeats_seen > 0 {
                        errors.push(Error {
                            message: "cannot have assignments inside of macro/repeat definitions"
                                .to_string(),
                            line: node.value.line,
                            source: self.source_for(node),
                            global: false,
                        });
                        continue;
                    }
                    self.literal_mode = None;
                    match self.evaluate_node(node.left.as_ref().unwrap()) {
                        Ok(value) => {
                            if let Err(err) = self.context.set_variable(
                                &node.value,
                                &Object {
                                    bundle: value,
                                    mapping: self.current_mapping,
                                    segment: self.current_segment,
                                    object_type: ObjectType::Value,
                                },
                                false,
                            ) {
                                errors.push(Error {
                                    message: err,
                                    line: node.value.line,
                                    global: false,
                                    source: self.source_for(node),
                                });
                            }
                        }
                        Err(e) => errors.push(e),
                    }
                }
                NodeType::Control(control_type) => {
                    if !self.context.is_global() && control_type.must_be_global() {
                        errors.push(Error {
                            message: format!("{} must be on the global scope", control_type),
                            line: node.value.line,
                            global: false,
                            source: self.source_for(node),
                        });
                        continue;
                    }

                    match control_type {
                        ControlType::StartMacro => {
                            // NOTE: macros inside of macros cannot happen
                            // because of the previous check of them needing to
                            // be on the global scope. Hence, we don't have to
                            // do a similar check as for .proc's or .scope's on
                            // illegal definitions.
                            self.macros_seen += 1;

                            // Insert a reference to this node so it can be
                            // unrolled whenever we have to perform a macro
                            // call.
                            self.macros
                                .entry(node.left.as_ref().unwrap().value.value.clone())
                                .or_insert(node);
                        }
                        ControlType::EndMacro => {
                            if self.macros_seen > 0 {
                                self.macros_seen -= 1;
                            }
                        }
                        // Same as NodeType::Label.
                        ControlType::StartProc => {
                            if self.macros_seen > 0 || self.procs_seen > 0 || self.repeats_seen > 0
                            {
                                errors.push(Error {
                                    message: "you cannot call '.proc' in this context".to_string(),
                                    line: node.value.line,
                                    global: false,
                                    source: self.source_for(node),
                                });
                                continue;
                            }

                            self.procs_seen += 1;
                            let proc_name = &node.left.as_ref().unwrap();
                            if let Err(err) = self.define_variable(proc_name) {
                                errors.push(err);
                            }
                        }
                        ControlType::EndProc => {
                            // Same case as with macros.
                            if self.procs_seen > 0 {
                                self.procs_seen -= 1;
                            }
                        }
                        ControlType::StartScope => {
                            if self.macros_seen > 0 || self.procs_seen > 0 || self.repeats_seen > 0
                            {
                                errors.push(Error {
                                    message: "you cannot call '.scope' in this context".to_string(),
                                    line: node.value.line,
                                    source: self.source_for(node),
                                    global: false,
                                });
                                continue;
                            }
                        }
                        ControlType::StartRepeat => {
                            self.repeats_seen += 1;
                        }
                        ControlType::EndRepeat => {
                            if self.repeats_seen > 0 {
                                self.repeats_seen -= 1;
                            }
                        }
                        _ => {}
                    }

                    // If this control statement implies a context change, do it
                    // now.
                    if let Err(message) = self.context.change_context(node) {
                        errors.push(Error {
                            message,
                            line: node.value.line,
                            global: false,
                            source: self.source_for(node),
                        });
                    }

                    // If this control statement actually has a body, go inside
                    // of it.
                    if control_type.has_body() {
                        let inner = &node.right.as_ref().unwrap().args.as_ref().unwrap();
                        self.eval_context(inner)?;
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

    // Apply the current segment offset to the label identified by `id` unless
    // it's empty (i.e. anonymous label). In either case, the computed label
    // will be pushed into the context's list of known labels with the current
    // segment offset.
    fn apply_segment_offset_to_label(&mut self, node: &PNode) -> Result<(), Error> {
        let segment = &self.mappings[self.current_mapping].segments[self.current_segment];
        let value = segment.offset.to_le_bytes();
        let object = Object {
            bundle: Bundle {
                bytes: [value[0], value[1], value[2]],
                size: 2,
                address: 0,
                cycles: 0,
                affected_on_page: false,
                resolved: false,
                negative: false,
            },
            mapping: self.current_mapping,
            segment: self.current_segment,
            object_type: ObjectType::Address,
        };

        if !node.value.is_empty() {
            if let Err(message) = self.context.set_variable(&node.value, &object, true) {
                return Err(Error {
                    message,
                    line: node.value.line,
                    global: false,
                    source: self.source_for(node),
                });
            }
        }
        self.context.add_label(&object);

        Ok(())
    }

    fn bundle(&mut self, nodes: &'a [PNode]) -> Result<(), Vec<Error>> {
        let mut errors = Vec::new();

        self.stage = Stage::Bundling;

        for node in nodes {
            match &node.node_type {
                // Initialize the label to the offset address of the current
                // segment. Note that this is only the offset from the beginning
                // of the offset, the effective address will only be available
                // after calling `Context::get_variable`
                NodeType::Label => {
                    if let Err(e) = self.apply_segment_offset_to_label(node) {
                        errors.push(e);
                    }
                }
                // Same as with labels but with the addition that ".proc"
                // introduces a new context. Hence, first act as a label, and
                // then open up its inner context.
                NodeType::Control(ControlType::StartProc) => {
                    let proc_name = &node.left.as_ref().unwrap();
                    if let Err(e) = self.apply_segment_offset_to_label(proc_name) {
                        errors.push(e);
                    }
                    if let Err(message) = self.context.change_context(node) {
                        errors.push(Error {
                            message,
                            line: node.value.line,
                            global: false,
                            source: self.source_for(node),
                        });
                    }

                    // And now go inside of its body if it exists (note that its
                    // existence might not be guaranteed if the parser gave an
                    // error on this block).
                    // if node.right.as_ref().is_some() {
                    let args = &node.right.as_ref().unwrap().args.as_ref().unwrap();
                    if args.is_empty() {
                        self.warnings.push(Error {
                            line: node.value.line,
                            message: format!("empty .proc '{}'", proc_name.value.value),
                            source: self.source_for(node),
                            global: false,
                        });
                    } else {
                        self.bundle(args)?;
                    }
                }
                NodeType::Instruction => {
                    self.literal_mode = None;
                    match self.evaluate_node(node) {
                        Ok(bundle) => {
                            if let Err(e) = self.push_bundle(bundle, node) {
                                errors.push(e);
                            }
                        }
                        Err(e) => errors.push(e),
                    }
                }
                NodeType::Control(control_type) => {
                    if let Err(e) = self.evaluate_control_statement(node) {
                        errors.push(e);
                    }

                    // On control statements which modify the context, there are
                    // some further evaluating to do.
                    match control_type {
                        ControlType::StartRepeat => {
                            self.evaluate_repeat_statement(node)?;
                        }
                        ControlType::StartScope => {
                            // If this is the start of a .scope statement, then go
                            // inside of its body too if it exists (note that its
                            // existence might not be guaranteed if the parser gave an
                            // error on this block). Note that we do that too for
                            // .proc's in its specialized branch, and we don't want to
                            // do it for macros as they will be evaluated on a per call
                            // basis.
                            if node.right.as_ref().is_some() {
                                let scope_name = &node.left.as_ref().unwrap().value;
                                let args = &node.right.as_ref().unwrap().args.as_ref().unwrap();
                                if args.is_empty() {
                                    self.warnings.push(Error {
                                        line: node.value.line,
                                        message: format!("empty .scope '{}'", scope_name.value),
                                        source: self.source_for(node),
                                        global: false,
                                    });
                                } else {
                                    self.bundle(args)?;
                                }
                            }
                        }
                        _ => {}
                    }
                }
                NodeType::Value | NodeType::Call => {
                    if let Err(mut ers) = self.bundle_call(node) {
                        errors.append(&mut ers);
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

    fn crunch(&mut self) -> Result<(), Vec<Error>> {
        let mut errors = vec![];

        self.stage = Stage::Crunching;

        for pn in self.pending.clone() {
            self.labels_seen = pn.labels_seen;
            self.context.force_context_switch(&pn.context);

            self.literal_mode = None;
            match self.evaluate_node(&pn.node) {
                Ok(mut bundle) => {
                    let current = &self.mappings[pn.mapping].segments[pn.segment];
                    bundle.address = current.bundles[pn.bundle_index].address;

                    if pn.node.is_branch() {
                        bundle.resolved = true;
                        if let Err(e) = self.to_relative_address(&pn.node, &mut bundle) {
                            errors.push(e);
                        }
                    }

                    let current_mut = &mut self.mappings[pn.mapping].segments[pn.segment];
                    current_mut.bundles[pn.bundle_index].bytes = bundle.bytes;
                }
                Err(e) => errors.push(e),
            }

            self.context.force_context_pop();
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn fill(&mut self) -> Result<Vec<Bundle>, Vec<Error>> {
        let mut errors = vec![];

        // Validate the mappings that have been evaluated before spitting it
        // out.
        if let Err(e) = crate::mapping::validate(&self.mappings) {
            return Err(vec![Error {
                line: 0,
                global: true,
                message: e,
                source: self.sources[0].clone(),
            }]);
        }

        let mut res = vec![];

        for mapping in &mut self.mappings {
            for segment in mapping.segments.iter_mut() {
                if segment.is_empty() {
                    self.warnings.push(Error {
                        line: 0,
                        message: format!("segment '{}' is empty", segment.name),
                        source: self.sources[0].clone(),
                        global: true,
                    });
                }
                res.append(&mut segment.bundles);
            }

            let mut diff = mapping.size as isize - mapping.offset as isize;
            if diff < 0 {
                errors.push(Error{
                        line: 0,
                        message: format!(
                            "exceeding segment size for '{}'; expecting {} bytes and {} bytes have already been seen",
                            mapping.name, mapping.size, mapping.offset,
                        ),
                        source: self.sources[0].clone(),
                        global: false,
                });
            }

            if let Some(fill) = mapping.fill {
                while diff > 0 {
                    res.push(Bundle::fill(fill));
                    diff -= 1;
                }
            }
        }

        if errors.is_empty() {
            Ok(res)
        } else {
            Err(errors)
        }
    }

    // Consume a node which contains a macro call by pushing its bundles now.
    fn bundle_call(&mut self, node: &PNode) -> Result<(), Vec<Error>> {
        // Get the macro we are trying to reproduce.
        let mcr = *self.macros.get(&node.value.value).ok_or(Error {
            line: node.value.line,
            message: format!(
                "could not find a macro with the name '{}'",
                node.value.value
            ),
            source: self.source_for(node),
            global: false,
        })?;

        // Detect missmatches between the number of arguments provided and the
        // ones defined by the macro.
        let given_args = node.args.as_ref().unwrap_or(&vec![]).len();
        let macro_args = mcr.args.as_ref().unwrap_or(&vec![]).len();
        if macro_args != given_args {
            return Err(Error {
                line: node.value.line,
                message: format!(
                    "wrong number of arguments for '{}': {} required but {} given",
                    node.value.value, macro_args, given_args,
                ),
                source: self.source_for(node),
                global: false,
            }
            .into());
        }

        // If there are arguments defined by the macro, set their values now.
        if given_args > 0 {
            let mut margs = mcr.args.as_ref().unwrap().iter();

            for arg in node.args.as_ref().unwrap().iter() {
                let obj = Object {
                    bundle: self.evaluate_node(arg)?,
                    mapping: self.current_mapping,
                    segment: self.current_segment,
                    object_type: ObjectType::Value,
                };

                // Note that we overwrite the variable value from previous
                // calls, just in case a macro is applied multiple times and we
                // need to get the latest value.
                if let Err(message) =
                    self.context
                        .set_variable(&margs.next().unwrap().value, &obj, true)
                {
                    return Err(Error {
                        line: node.value.line,
                        message,
                        source: self.source_for(node),
                        global: false,
                    }
                    .into());
                }
            }
        }

        // And now replicate the nodes as contained inside of the macro
        // definition. In order to handle inner statements from macros such as
        // anonymous labels and stuff like that, we simply call again
        // `Assembler::bundle`. Note that the macro could have been malformed
        // (i.e. there was an error during parser time), or it could be empty
        // altogether. Just issue a warning on the latter case.
        let inner = &mcr.right.as_ref().unwrap().args.as_ref().unwrap();
        if inner.is_empty() {
            self.warnings.push(Error {
                line: node.value.line,
                message: format!("trying to apply empty macro '{}'", node.value.value),
                source: self.source_for(node),
                global: false,
            });
        } else {
            self.bundle(inner)?;
        }
        Ok(())
    }

    fn push_bundle(&mut self, mut bundle: Bundle, node: &PNode) -> Result<(), Error> {
        let current = &mut self.mappings[self.current_mapping];
        bundle.address = current.start as usize + current.offset;
        current.offset += bundle.size as usize;
        current.segments[self.current_segment].offset += bundle.size as usize;

        if !bundle.resolved {
            self.pending.push(PendingNode {
                mapping: self.current_mapping,
                segment: self.current_segment,
                context: self.context.name().to_string(),
                bundle_index: current.segments[self.current_segment].bundles.len(),
                node: node.to_owned(),
                labels_seen: self.context.labels_seen(),
            });
        }
        current.segments[self.current_segment].bundles.push(bundle);

        Ok(())
    }

    fn evaluate_node(&mut self, node: &PNode) -> Result<Bundle, Error> {
        match &node.node_type {
            NodeType::Instruction => Ok(self.evaluate_instruction(node)?),
            NodeType::Literal => Ok(self.evaluate_literal(node)?),
            NodeType::Control(_) => Ok(self.evaluate_control_expression(node)?),
            NodeType::Operation(op) => Ok(self.evaluate_operation(node, op)?),
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
                    } else if node.value.is_anonymous_relative_reference() {
                        Ok(self.evaluate_anonymous_relative_reference(node)?)
                    } else if node.value.is_valid_identifier(true).is_err() {
                        // If this is not a valid identifier, just error out.
                        Err(Error {
                            message: "no prefix was given to operand".to_string(),
                            line: node.value.line,
                            source: self.source_for(node),
                            global: false,
                        })
                    } else {
                        // This is actually a valid identifier! Try to fetch the
                        // variable.
                        match self.evaluate_variable(node) {
                            Ok(v) => {
                                self.literal_mode = Some(LiteralMode::Hexadecimal);
                                Ok(v)
                            }
                            Err(err) => Err(Error {
                                message: format!(
                                    "no prefix was given to operand and {} either",
                                    err.message
                                ),
                                line: node.value.line,
                                source: self.source_for(node),
                                global: false,
                            }),
                        }
                    }
                }
            },
            _ => Err(Error {
                message: format!("unexpected '{}' expression type", node.node_type),
                line: node.value.line,
                source: self.source_for(node),
                global: false,
            }),
        }
    }

    // Evaluate the `node` by performing the operation described in
    // `operation_type` onto its arms.
    fn evaluate_operation(
        &mut self,
        node: &PNode,
        operation_type: &OperationType,
    ) -> Result<Bundle, Error> {
        let mut right = self.evaluate_node(node.right.as_ref().unwrap())?;
        let rval = right.value();

        let res: isize = match operation_type {
            OperationType::UnaryPositive => {
                right.negative = false;
                rval.abs()
            }
            OperationType::UnaryNegative => {
                right.negative = true;
                rval.neg()
            }
            OperationType::BitwiseNot => !rval,
            OperationType::LoByte => {
                let r = (rval as u16).to_le_bytes();
                right.bytes[0] = r[0];
                right.bytes[1] = 0;
                right.size = 1;
                return Ok(right);
            }
            OperationType::HiByte => {
                let r = (rval as u16).to_le_bytes();
                right.bytes[0] = r[1];
                right.bytes[1] = 0;
                right.size = 1;
                return Ok(right);
            }
            OperationType::Add => {
                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval + rval
            }
            OperationType::Sub => {
                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval - rval
            }
            OperationType::Mul => {
                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval * rval
            }
            OperationType::Div => {
                if rval == 0 {
                    return Err(Error {
                        line: node.value.line,
                        global: false,
                        source: self.source_for(node),
                        message: "attempting to divide by zero".to_string(),
                    });
                }
                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval / rval
            }
            OperationType::And => {
                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval & rval
            }
            OperationType::Or => {
                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval | rval
            }
            OperationType::Xor => {
                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval ^ rval
            }
            OperationType::Lshift => {
                if rval > 16 {
                    return Err(Error {
                        line: node.value.line,
                        global: false,
                        source: self.source_for(node),
                        message: "shift operator too big".to_string(),
                    });
                }

                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval << rval
            }
            OperationType::Rshift => {
                if rval > 16 {
                    return Err(Error {
                        line: node.value.line,
                        global: false,
                        source: self.source_for(node),
                        message: "shift operator too big".to_string(),
                    });
                }

                let lval = self.evaluate_node(node.left.as_ref().unwrap())?.value();
                lval >> rval
            }
        };

        // Prevent overflows.
        if res > i16::MAX.into() || res < i16::MIN.into() {
            return Err(Error {
                line: node.value.line,
                global: false,
                source: self.source_for(node),
                message: "performing the operation would overflow a 16-bit integer".to_string(),
            });
        }

        // Set the computes bytes to right since that's the node in common
        // across all operations and return it.
        let byte_result = (res as u16).to_le_bytes();
        right.bytes[0] = byte_result[0];
        right.bytes[1] = byte_result[1];

        Ok(right)
    }

    fn evaluate_anonymous_relative_reference(&mut self, node: &PNode) -> Result<Bundle, Error> {
        self.literal_mode = Some(LiteralMode::Plain);

        match &self.stage {
            Stage::Bundling => Ok(Bundle {
                bytes: [0, 0, 0],
                size: 2,
                address: 0,
                cycles: 0,
                affected_on_page: false,
                resolved: false,
                negative: false,
            }),
            Stage::Crunching => {
                match self.context.get_relative_label(
                    node.value.to_isize(),
                    self.labels_seen,
                    &self.mappings,
                ) {
                    Ok(object) => Ok(object.bundle),
                    Err(message) => Err(Error {
                        line: node.value.line,
                        message,
                        source: self.source_for(node),
                        global: false,
                    }),
                }
            }
            _ => panic!("unexpected evaluation of relative reference"),
        }
    }

    fn evaluate_hexadecimal(&mut self, node: &PNode) -> Result<Bundle, Error> {
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
                if self.evaluate_variable(node).is_ok() {
                    return Err(Error {
                        message: format!(
                            "you cannot use variables like '{}' in hexadecimal literals",
                            node.value.value
                        ),
                        source: self.source_for(node),
                        line: node.value.line,
                        global: false,
                    });
                }
                return Err(Error {
                    message: "expecting a number of 1 to 4 hexadecimal digits".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                });
            }
        }

        Ok(Bundle {
            bytes,
            size,
            address: 0,
            cycles: 0,
            affected_on_page: false,
            resolved: true,
            negative: false,
        })
    }

    fn evaluate_binary(&mut self, node: &PNode) -> Result<Bundle, Error> {
        let string = node.value.value.as_str();
        let mut value = 0;

        // We are strict with the definition of binary values to avoid gotchas.
        // Force the literal to be exactly 8 digits wide. If that's not the
        // case, do not even try evaluating it.
        match string.len().cmp(&8) {
            Ordering::Less => {
                return Err(Error {
                    message: "missing binary digits to get a full byte".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                })
            }
            Ordering::Greater => {
                return Err(Error {
                    message: "too many binary digits for a single byte".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                })
            }
            _ => {}
        }

        for (shift, c) in string.chars().rev().enumerate() {
            if c == '1' {
                let val = 1 << shift;
                value += val;
            } else if c != '0' {
                if self.evaluate_variable(node).is_ok() {
                    return Err(Error {
                        message: format!(
                            "you cannot use variables like '{}' in binary literals",
                            string
                        ),
                        line: node.value.line,
                        source: self.source_for(node),
                        global: false,
                    });
                }
                return Err(Error {
                    message: format!("bad binary format for '{}'", string),
                    line: node.value.line,
                    global: false,
                    source: self.source_for(node),
                });
            }
        }

        Ok(Bundle {
            bytes: [value as u8, 0, 0],
            size: 1,
            address: 0,
            cycles: 0,
            affected_on_page: false,
            resolved: true,
            negative: false,
        })
    }

    fn evaluate_decimal(&mut self, node: &PNode) -> Result<Bundle, Error> {
        let string = node.value.value.as_str();
        if string.is_empty() {
            return Err(Error {
                message: "empty decimal literal".to_string(),
                line: node.value.line,
                source: self.source_for(node),
                global: false,
            });
        }

        let mut value = 0;
        let mut shift = 1;

        for c in string.chars().rev() {
            if shift > 100 {
                return Err(Error {
                    message: "decimal value is too big".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                });
            }
            if c != '0' {
                match c.to_digit(10) {
                    Some(digit) => {
                        value += digit * shift;
                    }
                    None => {
                        if self.stage == Stage::Context {
                            return Err(Error {
                                message: format!(
                                    "variables must come from a constant expression, \
                                                  you cannot use other variables such as '{}' \
                                                  in variable definitions",
                                    string
                                ),
                                source: self.source_for(node),
                                line: node.value.line,
                                global: false,
                            });
                        }
                        match self.evaluate_variable(node) {
                            Ok(v) => return Ok(v),
                            Err(err) => {
                                return Err(Error {
                                    message: format!(
                                        "'{}' is not a decimal value and {} either",
                                        c, err.message
                                    ),
                                    source: self.source_for(node),
                                    line: node.value.line,
                                    global: false,
                                });
                            }
                        }
                    }
                }
            }

            shift *= 10;
        }
        if value > 255 {
            return Err(Error {
                message: "decimal value is too big".to_string(),
                line: node.value.line,
                source: self.source_for(node),
                global: false,
            });
        }

        Ok(Bundle {
            bytes: [value as u8, 0, 0],
            size: 1,
            address: 0,
            cycles: 0,
            affected_on_page: false,
            resolved: true,
            negative: false,
        })
    }

    fn evaluate_literal(&mut self, node: &PNode) -> Result<Bundle, Error> {
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
                return Err(Error {
                    message: "literal cannot embed another literal".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                });
            }
        } else if val.starts_with('%') {
            lm = Some(LiteralMode::Binary);
            if left.node_type == NodeType::Literal {
                return Err(Error {
                    message: "literal cannot embed another literal".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
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

    fn char_to_hex(&mut self, oc: Option<char>, source: &PNode) -> Result<u8, Error> {
        match oc {
            Some(c) => match c.to_digit(16) {
                Some(c) => Ok(c as u8),
                None => {
                    if (c.is_alphabetic() || c == '_') && self.evaluate_variable(source).is_ok() {
                        return Err(Error {
                            message: format!(
                                "you cannot use variables like '{}' in hexadecimal literals",
                                source.value.value
                            ),
                            line: source.value.line,
                            source: self.source_for(source),
                            global: false,
                        });
                    }
                    Err(Error {
                        message: "could not convert digit to hexadecimal".to_string(),
                        line: source.value.line,
                        source: self.source_for(source),
                        global: false,
                    })
                }
            },
            None => Err(Error {
                message: "digit out of bounds".to_string(),
                line: source.value.line,
                source: self.source_for(source),
                global: false,
            }),
        }
    }

    fn evaluate_control_statement(&mut self, node: &PNode) -> Result<(), Error> {
        // This might just be a statement that changes the context (e.g.
        // ".macro", ".proc", etc.). In this case change the context and leave
        // early.
        match self.context.change_context(node) {
            Ok(changed) => {
                if changed {
                    return Ok(());
                }
            }
            Err(message) => {
                return Err(Error {
                    message,
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                });
            }
        }

        // Otherwise, check the function that could act as a statement that
        // produces bundles.
        match node.node_type {
            NodeType::Control(ControlType::Byte) => self.push_evaluated_arguments(node, 1),
            NodeType::Control(ControlType::Addr) | NodeType::Control(ControlType::Word) => {
                self.push_evaluated_arguments(node, 2)
            }
            NodeType::Control(ControlType::Segment) => self.switch_to_segment(node),
            NodeType::Control(ControlType::IncBin) => {
                self.incbin(node.args.as_ref().unwrap().first().unwrap())
            }
            NodeType::Control(ControlType::IncludeSource) => Ok(()),
            _ => Err(Error {
                line: node.value.line,
                message: format!(
                    "cannot handle control statement '{}' in this context",
                    node.value.value
                ),
                source: self.source_for(node),
                global: false,
            }),
        }
    }

    // Push as many bundles as bytes are in the given file path. If there is any
    // issue with reading the given file, or the parameter is given in a weird
    // format, it will error out.
    fn incbin(&mut self, node: &PNode) -> Result<(), Error> {
        let value = &node.value.value;

        // Validate the path literal.
        if value.len() < 3 || !value.starts_with('"') || !value.ends_with('"') {
            return Err(Error {
                line: node.value.line,
                message: format!(
                    "path has to be written inside of double quotes ('{}' given instead)",
                    value,
                ),
                source: self.source_for(node),
                global: false,
            });
        }

        // The '.incbin' control assumes that paths are relative to the
        // directory of the current file. Hence, in order to make subsequent
        // `File` operations work in this way, set the current directory now.
        match &self.sources.get(node.source) {
            Some(source) => {
                if let Err(e) = std::env::set_current_dir(&source.directory) {
                    return Err(Error {
                        line: node.value.line,
                        message: format!("could not move to the directory of '{}': {}", value, e),
                        source: self.source_for(node),
                        global: false,
                    });
                }
            }
            None => panic!("mismatch on the node source"),
        }

        // Fetch the actual path.
        let path = &value[1..value.len() - 1].trim();
        let file = match File::open(path) {
            Ok(f) => f,
            Err(e) => {
                return Err(Error {
                    global: false,
                    line: node.value.line,
                    source: self.source_for(node),
                    message: format!("could not include binary data: {}", e),
                })
            }
        };

        // Ensure that the included binary data is within reason.
        match file.metadata() {
            Ok(metadata) => {
                // Note that we cannot assume that it's always going to be
                // included in some specific mapping type (e.g. CHR-ROM vs
                // CHR-RAM). Hence, let's force that nothing above 512KB can be
                // included at face value. If this really surpasses the actual
                // limit on where it's included, then it's going to show up at a
                // later check.
                if metadata.len() > 512 * 1024 {
                    return Err(Error {
                        global: false,
                        line: node.value.line,
                        source: self.source_for(node),
                        message: format!("file '{}' is too big", path),
                    });
                } else if metadata.len() == 0 {
                    return Err(Error {
                        global: false,
                        line: node.value.line,
                        source: self.source_for(node),
                        message: format!("trying to include an empty file ('{}')", path),
                    });
                }
            }
            Err(e) => {
                return Err(Error {
                    global: false,
                    line: node.value.line,
                    source: self.source_for(node),
                    message: format!("could not include binary data: {}", e),
                })
            }
        }

        // And finally just push each byte from the given file as a fill bundle.
        for byte in file.bytes() {
            match byte {
                Ok(b) => self.push_bundle(Bundle::fill(b), node)?,
                Err(_) => break,
            }
        }
        Ok(())
    }

    // Evaluate the given node assuming it's a .repeat statement and push all
    // the requested bundles from it..
    fn evaluate_repeat_statement(&mut self, node: &'a PNode) -> Result<(), Vec<Error>> {
        // First fetch the number of times the code block must be repeated.
        let args = node.args.as_ref().unwrap();
        let first = &args.first().unwrap().value.value;
        let repeats = match first.parse::<usize>() {
            Ok(n) => {
                if n < 2 {
                    return Err(Error {
                        global: false,
                        line: node.value.line,
                        source: self.source_for(node),
                        message: "pointless .repeat statement".to_string(),
                    }
                    .into());
                } else if n > 255 {
                    return Err(Error {
                        global: false,
                        line: node.value.line,
                        source: self.source_for(node),
                        message: "the number of iterations has to fit in a single byte".to_string(),
                    }
                    .into());
                }
                n
            }
            Err(_) => {
                return Err(Error {
                    global: false,
                    line: node.value.line,
                    message: format!(
                        "first argument must be an integer, '{}' found instead",
                        first
                    ),
                    source: self.source_for(node),
                }
                .into())
            }
        };

        // The code that is to be repeated.
        let code = &node.right.as_ref().unwrap().args.as_ref().unwrap();
        if code.is_empty() {
            self.warnings.push(Error {
                line: node.value.line,
                message: "empty .repeat statement".to_string(),
                source: self.source_for(node),
                global: false,
            });
            return Ok(());
        }

        // Perform the action.
        for i in 0..repeats {
            // If an index was given, set it now as a .repeat variable with the
            // loop index.
            if args.len() == 2 {
                if let Err(e) = self.context.set_variable(
                    &args.last().unwrap().value,
                    &Object {
                        bundle: Bundle::fill(i as u8),
                        mapping: self.current_mapping,
                        segment: self.current_segment,
                        object_type: ObjectType::Value,
                    },
                    true,
                ) {
                    return Err(Error {
                        line: node.value.line,
                        message: e,
                        source: self.source_for(node),
                        global: false,
                    }
                    .into());
                }
            }

            // And push all the bundles from the inner code.
            self.bundle(code)?;
        }

        Ok(())
    }

    fn evaluate_control_expression(&mut self, node: &PNode) -> Result<Bundle, Error> {
        match node.node_type {
            NodeType::Control(ControlType::Hibyte) => self.evaluate_byte(node, true),
            NodeType::Control(ControlType::Lobyte) => self.evaluate_byte(node, false),
            _ => Err(Error {
                line: node.value.line,
                message: format!(
                    "cannot handle control statement '{}' as an expression in this context",
                    node.value.value
                ),
                source: self.source_for(node),
                global: false,
            }),
        }
    }

    fn evaluate_byte(&mut self, node: &PNode, high: bool) -> Result<Bundle, Error> {
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

    fn push_evaluated_arguments(&mut self, node: &PNode, nbytes: u8) -> Result<(), Error> {
        match &node.args {
            Some(args) => {
                for arg in args {
                    // Evaluate the argument as a node.
                    self.literal_mode = None;
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
                                return Err(Error {
                                    line: arg.value.line,
                                    message: "expecting an argument that fits into a byte"
                                        .to_string(),
                                    source: self.source_for(node),
                                    global: false,
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
                    self.push_bundle(bundle, arg)?;
                }
            }
            None => {
                return Err(Error {
                    line: node.value.line,
                    message: format!(
                        "expecting at least one argument for '{}'",
                        node.value.value.as_str(),
                    ),
                    source: self.source_for(node),
                    global: false,
                })
            }
        }

        Ok(())
    }

    fn switch_to_segment(&mut self, node: &PNode) -> Result<(), Error> {
        // First of all, fetch the argument for the ".segment" statement and
        // validate that it has some basic format. Note that the existence of
        // exactly one argument is guaranteed by the parser and, thus,
        // `unwrap()` calls are not dangerous in this context.
        let arg = &node.args.as_ref().unwrap().first().unwrap();
        let val = &arg.value.value;
        if val.len() < 3 || !val.starts_with('"') || !val.ends_with('"') {
            return Err(Error {
                line: node.value.line,
                message: format!(
                    "segment declaration has to be written inside of double quotes ('{}' given instead)",
                    val,
                ),
                source: self.source_for(node),
                global: false,
            });
        }

        // Validate the segment name.
        let name = &val[1..val.len() - 1];
        if name
            .chars()
            .any(|ch| !(ch.is_ascii_alphanumeric() || ch == '_' || ch == '-'))
        {
            return Err(Error {
                line: node.value.line,
                message: "segment name contains bad characters".to_string(),
                source: self.source_for(node),
                global: false,
            });
        }

        // Find the segment being referenced and update the
        // `self.current_segment` accordingly.
        let mut found = false;
        for (mapping_idx, mapping) in self.mappings.iter().enumerate() {
            for (segment_idx, segment) in mapping.segments.iter().enumerate() {
                if segment.name == name {
                    self.current_mapping = mapping_idx;
                    self.current_segment = segment_idx;
                    found = true;
                    break;
                }
            }
        }
        if !found {
            return Err(Error {
                line: node.value.line,
                message: format!("unknown segment '{}'", name),
                source: self.source_for(node),
                global: false,
            });
        }
        Ok(())
    }

    fn evaluate_variable(&mut self, node: &PNode) -> Result<Bundle, Error> {
        match self.context.get_variable(&node.value, &self.mappings) {
            Ok(value) => Ok(value.bundle),
            Err(e) => Err(Error {
                message: e,
                line: node.value.line,
                source: self.source_for(node),
                global: false,
            }),
        }
    }

    fn evaluate_instruction(&mut self, node: &PNode) -> Result<Bundle, Error> {
        self.literal_mode = None;

        let (mode, mut bundle) = match &node.left {
            Some(_) => self.get_addressing_mode_and_bytes(node)?,
            None => (AddressingMode::Implied, Bundle::new(true)),
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
                    return Err(Error {
                        message: format!(
                            "cannot use {} addressing mode for the instruction '{}'",
                            mode, mnemonic
                        ),
                        source: self.source_for(node),
                        line: node.value.line,
                        global: false,
                    });
                }
            },
            None => {
                return Err(Error {
                    message: format!("unknown instruction {}", mnemonic),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                });
            }
        }
        Ok(bundle)
    }

    fn get_addressing_mode_and_bytes(
        &mut self,
        node: &PNode,
    ) -> Result<(AddressingMode, Bundle), Error> {
        let left = &node.left;

        if left.as_ref().unwrap().node_type == NodeType::Indirection {
            self.get_from_indirect(node)
        } else if node.right.is_some() {
            self.get_from_indexed(node)
        } else {
            self.get_from_left(node, left.as_ref().unwrap())
        }
    }

    fn get_from_indirect(&mut self, node: &PNode) -> Result<(AddressingMode, Bundle), Error> {
        let left = node.left.as_ref().unwrap();

        match node.right.as_ref() {
            Some(right) => {
                if right.value.value.trim().to_lowercase() == "y" {
                    if left.right.is_some() {
                        return Err(Error {
                            message:
                                "it has to be either X addressing or Y addressing, not all at once"
                                    .to_string(),
                            line: node.value.line,
                            source: self.source_for(node),
                            global: false,
                        });
                    }

                    let val = self.evaluate_node(left.left.as_ref().unwrap())?;
                    if val.size != 1 {
                        return Err(Error {
                            message: "address can only be one byte long on indirect Y addressing"
                                .to_string(),
                            line: node.value.line,
                            source: self.source_for(node),
                            global: false,
                        });
                    }
                    return Ok((AddressingMode::IndirectY, val));
                }
                Err(Error {
                    message: "only the Y index is allowed on indirect Y addressing".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                })
            }
            None => match left.right.as_ref() {
                Some(right) => {
                    if right.value.value.trim().to_lowercase() == "x" {
                        let val = self.evaluate_node(left.left.as_ref().unwrap())?;
                        if val.size != 1 {
                            return Err(Error {
                                message:
                                    "address can only be one byte long on indirect X addressing"
                                        .to_string(),
                                line: node.value.line,
                                source: self.source_for(node),
                                global: false,
                            });
                        }
                        return Ok((AddressingMode::IndirectX, val));
                    }
                    Err(Error {
                        message: "only the X index is allowed on indirect X addressing".to_string(),
                        line: node.value.line,
                        source: self.source_for(node),
                        global: false,
                    })
                }
                None => {
                    let val = self.evaluate_node(left.left.as_ref().unwrap())?;
                    if val.size != 2 {
                        return Err(Error {
                            message: "expecting a full 16-bit address".to_string(),
                            line: node.value.line,
                            source: self.source_for(node),
                            global: false,
                        });
                    }
                    Ok((AddressingMode::Indirect, val))
                }
            },
        }
    }

    fn get_from_indexed(&mut self, node: &PNode) -> Result<(AddressingMode, Bundle), Error> {
        // Evaluate the left arm of the instruction.
        let left = node.left.as_ref().unwrap();
        let mut val = self.evaluate_node(left)?;

        // Ensure that the literal mode for the left arm ensures an address
        // instead of some bogus number.
        if let Some(lm) = &self.literal_mode {
            if *lm != LiteralMode::Hexadecimal {
                return Err(Error {
                    message: "indexed addressing only works with addresses".to_string(),
                    line: node.value.line,
                    source: self.source_for(node),
                    global: false,
                });
            }
        }

        // Check the right arm to know the index being used.
        let right = node.right.as_ref().unwrap();
        match right.value.value.to_lowercase().trim() {
            "x" => {
                // If the size == 2 but we can fit it on a single byte (i.e.
                // because the second byte is just 0x00), then just "compress"
                // this instruction. Note that this is only valid if the value
                // is fully well-known (i.e. it's not yet to be resolved). When
                // the value is not yet resolved, it usually revolves around an
                // address being referenced, which is never on the zeropage
                // section, so it wouldn't fit on a single byte anyways.
                if val.size == 1 || (val.resolved && val.bytes[1] == 0x00) {
                    // If despite all of the above the parsed instruction +
                    // zeropage indexing is actually not valid but the absolute
                    // indexing is, then convert this instruction to absolute
                    // indexing.
                    let mnemonic = node.value.value.to_lowercase();
                    if let Some(entries) = INSTRUCTIONS.get(&mnemonic) {
                        if entries.get(&AddressingMode::ZeropageIndexedX).is_none()
                            && entries.get(&AddressingMode::IndexedX).is_some()
                        {
                            val.size = 2;
                            return Ok((AddressingMode::IndexedX, val));
                        }
                    }

                    // Re-inforce the optimization when val.size == 2 by forcing
                    // the size to 1.
                    val.size = 1;
                    Ok((AddressingMode::ZeropageIndexedX, val))
                } else {
                    Ok((AddressingMode::IndexedX, val))
                }
            }
            "y" => {
                // Same optimization as with the "x" case.
                if val.size == 1 || (val.resolved && val.bytes[1] == 0x00) {
                    // Similar to the case on "x" indexing.
                    let mnemonic = node.value.value.to_lowercase();
                    if let Some(entries) = INSTRUCTIONS.get(&mnemonic) {
                        if entries.get(&AddressingMode::ZeropageIndexedY).is_none()
                            && entries.get(&AddressingMode::IndexedY).is_some()
                        {
                            val.size = 2;
                            return Ok((AddressingMode::IndexedY, val));
                        }
                    }

                    val.size = 1;
                    Ok((AddressingMode::ZeropageIndexedY, val))
                } else {
                    Ok((AddressingMode::IndexedY, val))
                }
            }
            _ => Err(Error {
                message: "can only use X and Y as indices".to_string(),
                line: node.value.line,
                source: self.source_for(node),
                global: false,
            }),
        }
    }

    fn get_from_left(
        &mut self,
        base: &PNode,
        left_arm: &PNode,
    ) -> Result<(AddressingMode, Bundle), Error> {
        if left_arm.value.value.to_lowercase().trim() == "a" {
            return Ok((AddressingMode::Implied, Bundle::new(true)));
        }

        let mut val = self.evaluate_node(left_arm)?;
        match self.literal_mode {
            Some(LiteralMode::Hexadecimal) => {
                // As for checking the most significant byte, it's the same
                // optimization as with absolute to zeropage indexed addressing.
                if base.is_branch()
                    || val.size == 1
                    || (val.bytes[1] == 0x00 && !matches!(base.value.value.as_str(), "jmp" | "jsr"))
                {
                    val.size = 1;
                    Ok((AddressingMode::RelativeOrZeropage, val))
                } else {
                    Ok((AddressingMode::Absolute, val))
                }
            }
            Some(LiteralMode::Plain) => {
                if base.is_branch() {
                    Ok((AddressingMode::RelativeOrZeropage, val))
                } else if val.size > 1 {
                    match base.value.value.as_str() {
                        "jmp" | "jsr" => Ok((AddressingMode::Absolute, val)),
                        _ => Err(Error {
                            message: "immediate is too big".to_string(),
                            line: left_arm.value.line,
                            source: self.source_for(base),
                            global: false,
                        }),
                    }
                } else {
                    Ok((AddressingMode::Immediate, val))
                }
            }
            _ => Err(Error {
                message: "left arm of instruction is neither an address nor an immediate"
                    .to_string(),
                line: left_arm.value.line,
                source: self.source_for(base),
                global: false,
            }),
        }
    }

    fn to_relative_address(&self, node: &PNode, bundle: &mut Bundle) -> Result<(), Error> {
        if !bundle.resolved {
            return Ok(());
        }

        let next = (bundle.address + 2) as u16;
        let target = u16::from_le_bytes([bundle.bytes[1], bundle.bytes[2]]);

        let byte = if target < next {
            let diff = target as i16 - next as i16;
            if diff < -128 {
                return Err(Error {
                    line: node.value.line,
                    message: "you cannot branch to this location: it's too far away".to_string(),
                    source: self.source_for(node),
                    global: false,
                });
            }
            diff.to_le_bytes()[0]
        } else {
            let diff = target - next;
            if diff > 127 {
                return Err(Error {
                    line: node.value.line,
                    message: "you cannot branch to this location: it's too far away".to_string(),
                    source: self.source_for(node),
                    global: false,
                });
            }
            diff.to_le_bytes()[0]
        };

        bundle.bytes[1] = byte;
        bundle.bytes[2] = 0;
        bundle.size = 2;

        Ok(())
    }

    // Builds a SourceInfo object based on the given node.
    fn source_for(&self, node: &PNode) -> SourceInfo {
        self.sources
            .get(node.source)
            .unwrap_or(&SourceInfo {
                directory: self.sources[0].directory.clone(),
                name: self.sources[0].name.clone(),
            })
            .clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mapping::{get_mapping_configuration, SectionType, Segment};

    fn one_two() -> Vec<Mapping> {
        vec![
            Mapping {
                name: String::from("HEADER"),
                start: 0x0000,
                size: 0x0010,
                offset: 0,
                fill: Some(0x00),
                section_type: SectionType::Header,
                segments: vec![Segment {
                    name: String::from("HEADER"),
                    len: 0,
                    offset: 0,
                    bundles: vec![],
                }],
            },
            Mapping {
                name: String::from("ROM0"),
                start: 0x8000,
                size: 0x8000,
                offset: 0,
                fill: None,
                section_type: SectionType::PrgRom,
                segments: vec![
                    Segment {
                        name: String::from("ONE"),
                        len: 0,
                        offset: 0,
                        bundles: vec![],
                    },
                    Segment {
                        name: String::from("TWO"),
                        len: 0,
                        offset: 0,
                        bundles: vec![],
                    },
                ],
            },
        ]
    }

    fn empty() -> Vec<Mapping> {
        get_mapping_configuration("empty").unwrap()
    }

    fn minimal_header() -> &'static str {
        // NES\0
        // 1 * 8KB of PRG ROM
        // No CHR ROM
        r#".segment "HEADER"
.byte 'N', 'E', 'S', $1A, $01, $00
.segment "CODE"
"#
    }

    // Just run `assemble_with_mapping` by assuming `minimal_header()` and an
    // empty mapper.
    fn just_assemble(line: &str) -> AssemblerResult {
        // The line cannot be given as is, but we have to prepend the header or
        // the assembler will freak out.
        let real_line = minimal_header().to_string() + line;

        assemble_with_mapping(real_line.as_bytes(), empty(), SourceInfo::default())
    }

    // Like `just_assemble` but it only returns bundles passed the header.
    fn just_bundles(line: &str) -> Vec<Bundle> {
        let res = just_assemble(line);

        assert_eq!(res.errors.len(), 0);
        res.bundles[0x10..].to_vec()
    }

    // Given an instruction in `line`, assert that the resulting bundle matches
    // the given `hex` values.
    fn assert_instruction(line: &str, hex: &[u8]) {
        let res = just_assemble(line);

        assert!(res.warnings.is_empty());
        assert!(res.errors.is_empty());

        assert_eq!(res.bundles.len(), 0x11);

        let bundles = &res.bundles[0x10..];
        for i in 0..bundles[0].size {
            assert_eq!(hex[i as usize], bundles[0].bytes[i as usize]);
        }
    }

    // Given a code in `line`, assemble it and assert that an error with the
    // given `message` is reported. The line number `line_num` should also
    // appear if `global` is set to false. Note that the line number doesn't
    // have to account for the inserted header.
    fn assert_error(line: &str, line_num: usize, global: bool, message: &str) {
        let res = just_assemble(line);
        let msg = if global {
            message.to_string()
        } else {
            // +3 to account for the header.
            format!("{} (line {})", message, line_num + 3)
        };

        assert!(res.bundles.is_empty());
        assert!(!res.errors.is_empty());
        assert_eq!(res.errors[0].to_string().as_str(), msg);
    }

    // Empty

    #[test]
    fn empty_line() {
        for line in vec!["", "  ", ";; Comment", "  ;; Comment"].into_iter() {
            let res = just_assemble(line);

            assert!(res.errors.is_empty());
            assert!(res.bundles[0x10..].is_empty());

            // Warning for empty CODE segment.
            assert_eq!(res.warnings.len(), 1);
            assert_eq!(res.warnings[0].to_string(), "segment 'CODE' is empty");
        }
    }

    // Literal modes

    #[test]
    fn parse_binary() {
        assert_error(
            "adc #%0001",
            1,
            false,
            "missing binary digits to get a full byte",
        );
        assert_error(
            "adc #%0001000",
            1,
            false,
            "missing binary digits to get a full byte",
        );
        assert_error(
            "adc #%000100001",
            1,
            false,
            "too many binary digits for a single byte",
        );
        assert_error(
            r#"
            Variable = 42
            adc %Variable
            "#,
            3,
            false,
            "you cannot use variables like 'Variable' in binary literals",
        );
        assert_instruction("adc #%10100010", &[0x69, 0xA2]);
    }

    #[test]
    fn parse_hexadecimal() {
        assert_error(
            "adc #$12345",
            1,
            false,
            "expecting a number of 1 to 4 hexadecimal digits",
        );
        assert_error(
            "adc $AW",
            1,
            false,
            "could not convert digit to hexadecimal",
        );
        assert_error(
            r#"
    Variable = 42
    adc $Variable
    "#,
            3,
            false,
            "you cannot use variables like 'Variable' in hexadecimal literals",
        );
        assert_error(
            r#"
    Four = 4
    adc $Four
    "#,
            3,
            false,
            "you cannot use variables like 'Four' in hexadecimal literals",
        );
        assert_instruction("adc $AA", &[0x65, 0xAA]);
        assert_instruction("adc $10", &[0x65, 0x10]);
        assert_instruction("adc $10AB", &[0x6D, 0xAB, 0x10]);
    }

    #[test]
    fn parse_decimal() {
        assert_error("adc #256", 1, false, "decimal value is too big");
        assert_error("adc #2000", 1, false, "decimal value is too big");
        assert_error(
                "adc #2A",
                1, false,
                "'A' is not a decimal value and could not find variable '2A' in the global scope either",
            );
        assert_instruction("adc #1", &[0x69, 0x01]);
    }

    // Variables

    #[test]
    fn scoped_variable() {
        let res = just_bundles(
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
    "#,
        );

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
        let res = just_bundles(
            r#"
    Variable = 4
    adc Variable
    "#,
        );

        assert_eq!(res.len(), 1);

        let instr = res.first().unwrap();
        assert_eq!(instr.size, 2);
        assert_eq!(instr.bytes[0], 0x65);
        assert_eq!(instr.bytes[1], 0x04);
    }

    #[test]
    fn reference_outer_variables() {
        let res = just_bundles(
            r#"
    foo:
      rts

    .proc inner
        jsr foo
    .endproc
    "#,
        );

        assert_eq!(res.len(), 2);

        // foo: rts
        assert_eq!(res[0].size, 1);
        assert_eq!(res[0].bytes[0], 0x60);

        // inner: jsr foo
        assert_eq!(res[1].size, 3);
        assert_eq!(res[1].bytes[0], 0x20);
        assert_eq!(res[1].bytes[1], 0x00);
        assert_eq!(res[1].bytes[2], 0x80);
    }

    #[test]
    fn bad_variable_but_valid_identifier_in_instruction() {
        assert_error(
                "adc Variable",
                1, false,
                "no prefix was given to operand and could not find variable 'Variable' in the global scope either",
            );
        assert_error(
            "adc Scoped::Variable",
            1,
            false,
            "no prefix was given to operand and did not find scope 'Scoped' either",
        );
    }

    #[test]
    fn redefined_variable() {
        assert_error(
            r#"
    .scope One
      Variable = 1
    .endscope

    Variable = 1
    Yet = 3
    Yet = 4
    "#,
            8,
            false,
            "'Yet' already defined in the global scope: you cannot re-assign names",
        );
    }

    #[test]
    fn unknown_variables() {
        assert_error(
            "lda #Variable",
            1,
            false,
            "'e' is not a decimal value and could not find variable \
                          'Variable' in the global scope either",
        );
        assert_error(
            "lda #Scope::Variable",
            1,
            false,
            "'e' is not a decimal value and did not find scope 'Scope' either",
        );
        assert_error(
            r#"
    .scope Scope
    .endscope
    lda #Scope::Variable
    "#,
            4,
            false,
            "'e' is not a decimal value and could not find variable 'Variable' in 'Scope' either",
        );
    }

    #[test]
    fn operations_with_variables() {
        let res = just_bundles(
            r#"
    Value = 4
    ldx #(2 + Value)
    ldx #(2 - Value)
    ldx #(2 * -Value)
    ldx #(Value / 2)
    ldx #(Value << 2)
    ldx #~Value
    "#,
        );

        assert_eq!(res.len(), 6);

        let expected = [
            [0x06, 0x00],
            [0xFE, 0xFF],
            [0xF8, 0xFF],
            [0x02, 0x00],
            [0x10, 0x00],
            [0xFB, 0xFF],
        ];
        for (idx, node) in res.iter().enumerate() {
            assert_eq!(node.size, 2);
            assert_eq!(node.bytes[0], 0xA2);
            assert_eq!(node.bytes[1], expected[idx][0]);
            assert_eq!(node.bytes[2], expected[idx][1]);
        }
    }

    #[test]
    fn signed_with_variables() {
        let res = just_bundles(
            r#"
    Value = -2
    ldx #Value
    ldx #+Value
    "#,
        );

        assert_eq!(res.len(), 2);

        let neg = res.first().unwrap();
        assert_eq!(neg.size, 2);
        assert_eq!(neg.bytes[0], 0xA2);
        assert_eq!(neg.bytes[1], 0xFE);
        assert_eq!(neg.bytes[2], 0xFF);

        let pos = res.last().unwrap();
        assert_eq!(pos.size, 2);
        assert_eq!(pos.bytes[0], 0xA2);
        assert_eq!(pos.bytes[1], 0x02);
        assert_eq!(pos.bytes[2], 0x00);
    }

    #[test]
    fn divide_by_zero() {
        assert_error(
            r#"Value = 0
    ldx #(2 / Value)
"#,
            2,
            false,
            "attempting to divide by zero",
        );
    }

    #[test]
    fn bad_shift() {
        assert_error(
            r#"Value = #$11
    ldx #(2 << Value)
    "#,
            2,
            false,
            "shift operator too big",
        );
    }

    #[test]
    fn reset_literal_mode_on_assignment() {
        let res = just_bundles(
            r#"var1 = $31
var2 = 20
cpx #(4 * var2)"#,
        );

        assert_eq!(res.len(), 1);
        assert_eq!(res[0].bytes[0], 0xE0);
        assert_eq!(res[0].bytes[1], 0x50);
        assert_eq!(res[0].bytes[2], 0x00);
    }

    // Regular instructions

    #[test]
    fn bad_addressing() {
        assert_error(
            "unknown #$20",
            1,
            false,
            "could not find a macro with the name 'unknown'",
        );
        assert_error(
            "adc ($2002, x)",
            1,
            false,
            "address can only be one byte long on indirect X addressing",
        );
        assert_error(
            "adc ($2002), y",
            1,
            false,
            "address can only be one byte long on indirect Y addressing",
        );
        assert_error(
            "adc ($20, y)",
            1,
            false,
            "only the X index is allowed on indirect X addressing",
        );
        assert_error(
            "adc ($20), x",
            1,
            false,
            "only the Y index is allowed on indirect Y addressing",
        );
        assert_error("jmp ($20)", 1, false, "expecting a full 16-bit address");
        assert_error("adc $20, z", 1, false, "can only use X and Y as indices");
        assert_error(
            "adc ($2000)",
            1,
            false,
            "cannot use indirect addressing mode for the instruction 'adc'",
        );
        assert_error("lda 12", 1, false, "no prefix was given to operand")
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

        // Expand zeropage-looking into absolute index.
        assert_instruction("lda $42, y", &[0xB9, 0x42, 0x00]);

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

        // Expand zeropage-looking into absolute index.
        assert_instruction("sta $42, y", &[0x99, 0x42, 0x00]);

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

    #[test]
    fn same_segment_labels() {
        let res = just_bundles(
            r#"
    nop
    @hello:
        jmp @hello
        jmp @end
    @end:
        nop
    "#,
        );

        assert_eq!(res.len(), 4);

        // jmp @hello
        assert_eq!(res[1].size, 3);
        assert_eq!(res[1].bytes[0], 0x4C);
        assert_eq!(res[1].bytes[1], 0x01);
        assert_eq!(res[1].bytes[2], 0x80);

        // jmp @end
        assert_eq!(res[2].size, 3);
        assert_eq!(res[2].bytes[0], 0x4C);
        assert_eq!(res[2].bytes[1], 0x07);
        assert_eq!(res[2].bytes[2], 0x80);
    }

    #[test]
    fn anonymous_relative_jumps() {
        let res = just_bundles(
            r#"
    nop
    :
        nop
    @hello:
        jmp :--
        jmp :+
        jmp @hello
        jmp :+++
    @end:
        nop
    :
        nop
    : nop
    "#,
        );

        assert_eq!(res.len(), 9);

        // First two nop's
        assert_eq!(res[0].size, 1);
        assert_eq!(res[0].bytes[0], 0xEA);
        assert_eq!(res[1].size, 1);
        assert_eq!(res[1].bytes[0], 0xEA);

        // jmp :--
        assert_eq!(res[2].size, 3);
        assert_eq!(res[2].bytes[0], 0x4C);
        assert_eq!(res[2].bytes[1], 0x01);
        assert_eq!(res[2].bytes[2], 0x80);

        // jmp :+
        assert_eq!(res[3].size, 3);
        assert_eq!(res[3].bytes[0], 0x4C);
        assert_eq!(res[3].bytes[1], 0x0E);
        assert_eq!(res[3].bytes[2], 0x80);

        // jmp @hello
        assert_eq!(res[4].size, 3);
        assert_eq!(res[4].bytes[0], 0x4C);
        assert_eq!(res[4].bytes[1], 0x02);
        assert_eq!(res[4].bytes[2], 0x80);

        // jmp :+++
        assert_eq!(res[5].size, 3);
        assert_eq!(res[5].bytes[0], 0x4C);
        assert_eq!(res[5].bytes[1], 0x10);
        assert_eq!(res[5].bytes[2], 0x80);

        // Three last nop's.
        assert_eq!(res[6].size, 1);
        assert_eq!(res[6].bytes[0], 0xEA);
        assert_eq!(res[7].size, 1);
        assert_eq!(res[7].bytes[0], 0xEA);
        assert_eq!(res[8].size, 1);
        assert_eq!(res[8].bytes[0], 0xEA);
    }

    #[test]
    fn anonymous_relative_branches() {
        let res = just_bundles(
            r#"
    nop
    :
        nop
    @hello:
        beq :--
        beq :+
        beq @hello
        beq :+++
    @end:
        nop
    :
        nop
    : nop
    "#,
        );

        assert_eq!(res.len(), 9);

        // First two nop's
        assert_eq!(res[0].size, 1);
        assert_eq!(res[0].bytes[0], 0xEA);
        assert_eq!(res[1].size, 1);
        assert_eq!(res[1].bytes[0], 0xEA);

        // beq :--
        assert_eq!(res[2].size, 2);
        assert_eq!(res[2].bytes[0], 0xF0);
        assert_eq!(res[2].bytes[1], 0xFD);

        // beq :+
        assert_eq!(res[3].size, 2);
        assert_eq!(res[3].bytes[0], 0xF0);
        assert_eq!(res[3].bytes[1], 0x04);

        // beq @hello
        assert_eq!(res[4].size, 2);
        assert_eq!(res[4].bytes[0], 0xF0);
        assert_eq!(res[4].bytes[1], 0xFA);

        // beq :+++
        assert_eq!(res[5].size, 2);
        assert_eq!(res[5].bytes[0], 0xF0);
        assert_eq!(res[5].bytes[1], 0x02);

        // Three last nop's.
        assert_eq!(res[6].size, 1);
        assert_eq!(res[6].bytes[0], 0xEA);
        assert_eq!(res[7].size, 1);
        assert_eq!(res[7].bytes[0], 0xEA);
        assert_eq!(res[8].size, 1);
        assert_eq!(res[8].bytes[0], 0xEA);
    }

    #[test]
    fn conditional_branch_to_labels() {
        let res = just_bundles(
            r#"
    nop
    @hello:
        beq @hello
        beq @end
    @end:
        nop
    "#,
        );

        assert_eq!(res.len(), 4);

        // beq @hello
        assert_eq!(res[1].size, 2);
        assert_eq!(res[1].bytes[0], 0xF0);
        assert_eq!(res[1].bytes[1], 0xFE);

        // beq @end
        assert_eq!(res[2].size, 2);
        assert_eq!(res[2].bytes[0], 0xF0);
        assert_eq!(res[2].bytes[1], 0x00);
    }

    #[test]
    fn jsr_to_proc() {
        let res = just_bundles(
            r#"nop
    .proc hello
      nop
      rts
    .endproc
      jsr hello
    "#,
        );

        assert_eq!(res.len(), 4);

        // First two nop's + rts
        assert_eq!(res[0].size, 1);
        assert_eq!(res[0].bytes[0], 0xEA);
        assert_eq!(res[1].size, 1);
        assert_eq!(res[1].bytes[0], 0xEA);
        assert_eq!(res[2].size, 1);
        assert_eq!(res[2].bytes[0], 0x60);

        // jsr hello
        assert_eq!(res[3].size, 3);
        assert_eq!(res[3].bytes[0], 0x20);
        assert_eq!(res[3].bytes[1], 0x01);
        assert_eq!(res[3].bytes[2], 0x80);
    }

    #[test]
    fn label_in_instruction_addressing() {
        let res = just_bundles(
            r#"
      ldx #0
    @load_palettes_loop:
      lda palettes, x
    palettes:
      .byte $0F, $12, $22, $32
    "#,
        );

        assert_instruction("ldx #0", &res[0].bytes);
        assert_instruction("lda $8005, x", &res[1].bytes);
        assert_eq!(&res[2].bytes, &[0x0F, 0x00, 0x00]);
        assert_eq!(&res[3].bytes, &[0x12, 0x00, 0x00]);
        assert_eq!(&res[4].bytes, &[0x22, 0x00, 0x00]);
        assert_eq!(&res[5].bytes, &[0x32, 0x00, 0x00]);
    }

    #[test]
    fn full_to_zeropage_optimization() {
        assert_instruction("sta $0020", &[0x85, 0x20]);
        assert_instruction("sty $021, x", &[0x94, 0x21]);
        assert_instruction("sta $002, x", &[0x95, 0x02]);
        assert_instruction("stx $020, y", &[0x96, 0x20]);
    }

    // Control statements

    #[test]
    fn byte_literals() {
        let res = just_bundles(
            r#"
    .scope Vars
        Variable = 4
    .endscope

    .byte #Vars::Variable
    .dw $2001, $02
    "#,
        );

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
        let res = just_bundles(
            r#"
    Var = $2002
    lda #.lobyte(Var)
    lda #<Var
    lda #.hibyte(Var)
    lda #>Var
    "#,
        );

        assert_eq!(res.len(), 4);
        let instrs: Vec<[u8; 2]> = vec![[0xA9, 0x02], [0xA9, 0x02], [0xA9, 0x20], [0xA9, 0x20]];

        for i in 0..4 {
            assert_eq!(res[i].size, 2);
            assert_eq!(res[i].bytes[0], instrs[i][0]);
            assert_eq!(res[i].bytes[1], instrs[i][1]);
        }
    }

    #[test]
    fn warnings_on_empty_proc_scope_macro() {
        let res = just_assemble(
            r#".proc Proc
    .endproc

    .scope Scope
    .endscope

    .macro MACRO
    .endmacro

    MACRO
    "#,
        );

        let warnings = res.warnings;
        assert_eq!(warnings.len(), 4);

        assert_eq!(warnings[0].to_string(), "empty .proc 'Proc' (line 4)");
        assert_eq!(warnings[1].to_string(), "empty .scope 'Scope' (line 7)");
        assert_eq!(
            warnings[2].to_string(),
            "trying to apply empty macro 'MACRO' (line 13)"
        );
        assert_eq!(warnings[3].to_string(), "segment 'CODE' is empty");
    }

    // Macros

    #[test]
    fn macro_no_arguments() {
        let res = just_bundles(
            r#"
    lda #42

    .macro MACRO
        lda #2
    .endmacro

    lda #1
    MACRO
    "#,
        );

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
        assert_error(
            r#"
    lda #42

    .macro MACRO(Var)
        lda #Var
    .endmacro

    lda #1
    MACRO
    "#,
            9,
            false,
            "wrong number of arguments for 'MACRO': 1 required but 0 given",
        );
    }

    #[test]
    fn macro_too_many_arguments() {
        assert_error(
            r#"
    lda #42

    .macro MACRO(Var)
        lda #Var
    .endmacro

    lda #1
    MACRO(1, 2)
    "#,
            9,
            false,
            "wrong number of arguments for 'MACRO': 1 required but 2 given",
        );
    }

    #[test]
    fn macro_with_one_argument() {
        let res = just_bundles(
            r#"
    lda #42

    .macro MACRO(Var)
        lda #Var
    .endmacro

    lda #1
    MACRO(2)
    "#,
        );

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
        assert_error(
            r#"
    lda #42

    .macro MACRO(Var)
        lda #Va
    .endmacro

    lda #1
    MACRO(1)
    "#,
            5,
            false,
            "'a' is not a decimal value and \
                 could not find variable 'Va' in the global scope either",
        );
    }

    #[test]
    fn macro_multiple_arguments() {
        let res = just_bundles(
            r#"
    .macro WRITE_PPU_DATA address, value
        bit $2002                   ; PPUSTATUS
        lda #.HIBYTE(address)
        sta $2006                   ; PPUADDR
        lda #.LOBYTE(address)
        sta $2006                   ; PPUADDR
        lda #value
        sta $2007                   ; PPUDATA
    .endmacro

    WRITE_PPU_DATA $20B9, $04
    "#,
        );

        assert_eq!(res.len(), 7);

        // bit $2002
        assert_eq!(res[0].size, 3);
        assert_eq!(res[0].bytes[0], 0x2C);
        assert_eq!(res[0].bytes[1], 0x02);
        assert_eq!(res[0].bytes[2], 0x20);

        // lda #.HIBYTE(address)
        assert_eq!(res[1].size, 2);
        assert_eq!(res[1].bytes[0], 0xA9);
        assert_eq!(res[1].bytes[1], 0x20);

        // sta $2006
        assert_eq!(res[2].size, 3);
        assert_eq!(res[2].bytes[0], 0x8D);
        assert_eq!(res[2].bytes[1], 0x06);
        assert_eq!(res[2].bytes[2], 0x20);

        // lda #.LOBYTE(address)
        assert_eq!(res[3].size, 2);
        assert_eq!(res[3].bytes[0], 0xA9);
        assert_eq!(res[3].bytes[1], 0xB9);

        // sta $2006
        assert_eq!(res[4].size, 3);
        assert_eq!(res[4].bytes[0], 0x8D);
        assert_eq!(res[4].bytes[1], 0x06);
        assert_eq!(res[4].bytes[2], 0x20);

        // lda #value
        assert_eq!(res[5].size, 2);
        assert_eq!(res[5].bytes[0], 0xA9);
        assert_eq!(res[5].bytes[1], 0x04);

        // sta $2007
        assert_eq!(res[6].size, 3);
        assert_eq!(res[6].bytes[0], 0x8D);
        assert_eq!(res[6].bytes[1], 0x07);
        assert_eq!(res[6].bytes[2], 0x20);
    }

    #[test]
    fn start_macro_inside_of_scope() {
        assert_error(
            r#".scope Some
    .macro WRITE_PPU_DATA
        bit $2002
    .endmacro
    .endscope
    "#,
            2,
            false,
            ".macro must be on the global scope",
        );
    }

    #[test]
    fn macro_call_inside_of_proc() {
        let res = just_bundles(
            r#".macro MACRO
    nop
    .endmacro

    .proc Foo
      MACRO
    .endproc
    "#,
        );

        assert_eq!(res.len(), 1);

        assert_eq!(res[0].size, 1);
        assert_eq!(res[0].bytes[0], 0xEA);
        assert_eq!(res[0].bytes[1], 0x00);
        assert_eq!(res[0].bytes[2], 0x00);
    }

    #[test]
    fn bad_scope_definition_inside_of_proc() {
        assert_error(
            r#".proc Hey
    .scope Something
    .endscope
    .endproc
    "#,
            2,
            false,
            "you cannot call '.scope' in this context",
        );
    }

    #[test]
    fn bad_scope_definition_inside_of_macro() {
        assert_error(
            r#".macro Hey
    .scope Something
    .endscope
    .endmacro
    "#,
            2,
            false,
            "you cannot call '.scope' in this context",
        );
    }

    #[test]
    fn bad_proc_definition_inside_of_proc() {
        assert_error(
            r#".proc Hey
    .proc Something
    .endproc
    .endproc
    "#,
            2,
            false,
            "you cannot call '.proc' in this context",
        );
    }

    #[test]
    fn bad_proc_definition_inside_of_macro() {
        assert_error(
            r#".macro Hey
    .proc Something
    .endproc
    .endmacro
    "#,
            2,
            false,
            "you cannot call '.proc' in this context",
        );
    }

    #[test]
    fn error_on_named_label_inside_macro() {
        assert_error(
            r#".macro MACRO
    @label:
      jmp @label
    .endmacro
    "#,
            2,
            false,
            "using a named label ('@label') inside of a macro/repeat definition",
        );
    }

    #[test]
    fn jumps_inside_of_macros() {
        let res = just_bundles(
            r#".macro MACRO address, value
        lda #.HIBYTE(address)
        lda #.LOBYTE(address)
    :
        lda #value
        beq :-
    .endmacro

    MACRO $20B9, $04
    "#,
        );

        assert_eq!(res.len(), 4);

        // lda #.HIBYTE(address)
        assert_eq!(res[0].size, 2);
        assert_eq!(res[0].bytes[0], 0xA9);
        assert_eq!(res[0].bytes[1], 0x20);

        // lda #.LOBYTE(address)
        assert_eq!(res[1].size, 2);
        assert_eq!(res[1].bytes[0], 0xA9);
        assert_eq!(res[1].bytes[1], 0xB9);

        // lda #value
        assert_eq!(res[2].size, 2);
        assert_eq!(res[2].bytes[0], 0xA9);
        assert_eq!(res[2].bytes[1], 0x04);

        // beq :-
        assert_eq!(res[3].size, 2);
        assert_eq!(res[3].bytes[0], 0xF0);
        assert_eq!(res[3].bytes[1], 0xFC);
    }

    // .repeat

    #[test]
    fn code_gets_repeated() {
        let res = just_bundles(
            r#".repeat 3
nop
.endrepeat
"#,
        );

        assert_eq!(res.len(), 3);

        for it in res.iter().take(2) {
            assert_eq!(it.size, 1);
            assert_eq!(it.bytes[0], 0xEA);
            assert_eq!(it.bytes[1], 0x00);
            assert_eq!(it.bytes[2], 0x00);
        }
    }

    #[test]
    fn variable_in_repeat() {
        let res = just_bundles(
            r#".repeat 2, I
.repeat 2, J
lda #I
ldx #J
.endrepeat
.endrepeat"#,
        );

        assert_eq!(res.len(), 8);

        // 00
        assert_eq!(res[0].bytes[0], 0xA9);
        assert_eq!(res[0].bytes[1], 0x00);
        assert_eq!(res[1].bytes[0], 0xA2);
        assert_eq!(res[1].bytes[1], 0x00);

        // 01
        assert_eq!(res[2].bytes[0], 0xA9);
        assert_eq!(res[2].bytes[1], 0x00);
        assert_eq!(res[3].bytes[0], 0xA2);
        assert_eq!(res[3].bytes[1], 0x01);

        // 10
        assert_eq!(res[4].bytes[0], 0xA9);
        assert_eq!(res[4].bytes[1], 0x01);
        assert_eq!(res[5].bytes[0], 0xA2);
        assert_eq!(res[5].bytes[1], 0x00);

        // 11
        assert_eq!(res[6].bytes[0], 0xA9);
        assert_eq!(res[6].bytes[1], 0x01);
        assert_eq!(res[7].bytes[0], 0xA2);
        assert_eq!(res[7].bytes[1], 0x01);
    }

    #[test]
    fn error_on_invalid_repeat() {
        assert_error(
            ".repeat 1\n.endrepeat",
            1,
            false,
            "pointless .repeat statement",
        );
        assert_error(
            ".repeat a\n.endrepeat",
            1,
            false,
            "first argument must be an integer, 'a' found instead",
        );
        assert_error(
            ".repeat 256\n.endrepeat",
            1,
            false,
            "the number of iterations has to fit in a single byte",
        );
    }

    #[test]
    fn warning_on_empty_repeat() {
        let res = just_assemble(".repeat 2\n.endrepeat");

        assert!(res.bundles[0x10..].is_empty());
        assert!(res.errors.is_empty());

        assert_eq!(res.warnings.len(), 2); // empty code and the one we are testing.
        assert_eq!(
            res.warnings.first().unwrap().to_string(),
            "empty .repeat statement (line 4)"
        );
    }

    #[test]
    fn custom_human_message_on_unknown_variable_in_repeat() {
        assert_error(
            r#".repeat 2
lda #Variable
.endrepeat
"#,
            2,
            false,
            "'e' is not a decimal value and could not find variable 'Variable' \
             in the current scope either",
        );
    }

    // Segments

    #[test]
    fn error_on_unknown_segment() {
        assert_error(
            r#"
    .segment "THREE"

    .segment "TWO"
    nop'
    "#,
            2,
            false,
            "unknown segment 'THREE'",
        );
    }

    #[test]
    fn warning_on_empty_segment() {
        let res = assemble_with_mapping(
            r#"
.segment "HEADER"
.byte 'N', 'E', 'S', $1A, $01, $00

    .segment "ONE"

    .segment "TWO"
    nop
    "#
            .as_bytes(),
            one_two().to_vec(),
            SourceInfo::default(),
        );

        assert_eq!(res.bundles.len(), 0x11);

        let warnings = res.warnings;
        assert_eq!(warnings.len(), 1);
        assert_eq!(
            warnings.first().unwrap().to_string(),
            "segment 'ONE' is empty"
        );
    }

    #[test]
    fn jmp_and_beq_inside_segment() {
        let res = assemble_with_mapping(
            r#".segment "HEADER"
.byte 'N', 'E', 'S', $1A, $01, $00

    .segment "ONE"
    nop

    .segment "TWO"
    nop
    :
        nop
    @hello:
        beq :--
        beq :+
        jmp @hello
        beq @hello
        beq :+++
        beq @end
    @end:
        nop
    :
        nop
    : nop
    "#
            .as_bytes(),
            one_two().to_vec(),
            SourceInfo::default(),
        );

        let bundles = &res.bundles[0x11..];

        // First two nop's
        assert_eq!(bundles[0].size, 1);
        assert_eq!(bundles[0].bytes[0], 0xEA);
        assert_eq!(bundles[1].size, 1);
        assert_eq!(bundles[1].bytes[0], 0xEA);

        // beq :--
        assert_eq!(bundles[2].size, 2);
        assert_eq!(bundles[2].bytes[0], 0xF0);
        assert_eq!(bundles[2].bytes[1], 0xFD);

        // beq :+
        assert_eq!(bundles[3].size, 2);
        assert_eq!(bundles[3].bytes[0], 0xF0);
        assert_eq!(bundles[3].bytes[1], 0x09);

        // jmp @hello
        assert_eq!(bundles[4].size, 3);
        assert_eq!(bundles[4].bytes[0], 0x4C);
        assert_eq!(bundles[4].bytes[1], 0x03);
        assert_eq!(bundles[4].bytes[2], 0x80);

        // beq @hello
        assert_eq!(bundles[5].size, 2);
        assert_eq!(bundles[5].bytes[0], 0xF0);
        assert_eq!(bundles[5].bytes[1], 0xF7);

        // beq :+++
        assert_eq!(bundles[6].size, 2);
        assert_eq!(bundles[6].bytes[0], 0xF0);
        assert_eq!(bundles[6].bytes[1], 0x04);

        // beq @end
        assert_eq!(bundles[7].size, 2);
        assert_eq!(bundles[7].bytes[0], 0xF0);
        assert_eq!(bundles[7].bytes[1], 0x00);

        // Three last nop's.
        assert_eq!(bundles[8].size, 1);
        assert_eq!(bundles[8].bytes[0], 0xEA);
        assert_eq!(bundles[9].size, 1);
        assert_eq!(bundles[9].bytes[0], 0xEA);
        assert_eq!(bundles[10].size, 1);
        assert_eq!(bundles[10].bytes[0], 0xEA);
    }

    #[test]
    fn jmp_on_different_segments_intertwined() {
        let res = assemble_with_mapping(
            r#".segment "HEADER"
.byte 'N', 'E', 'S', $1A, $01, $00

    .segment "ONE"
    lala:
        rts
        .byte $F3

    .segment "TWO"
    code:
        jsr lala
        jsr code
        rts

    .segment "ONE"
        jsr code
    "#
            .as_bytes(),
            one_two().to_vec(),
            SourceInfo::default(),
        );

        let bundles = &res.bundles[0x12..]; // Ignoring HEADER + first two ONE

        // "jsr code" from ONE (notice that it's intertwined!)
        assert_eq!(bundles[0].size, 3);
        assert_eq!(bundles[0].bytes[0], 0x20);
        assert_eq!(bundles[0].bytes[1], 0x05);
        assert_eq!(bundles[0].bytes[2], 0x80);

        // "jsr lala" from TWO
        assert_eq!(bundles[1].size, 3);
        assert_eq!(bundles[1].bytes[0], 0x20);
        assert_eq!(bundles[1].bytes[1], 0x00);
        assert_eq!(bundles[1].bytes[2], 0x80);

        // "jsr code" from TWO (again, notice that it's intertwined)
        assert_eq!(bundles[2].size, 3);
        assert_eq!(bundles[2].bytes[0], 0x20);
        assert_eq!(bundles[2].bytes[1], 0x05);
        assert_eq!(bundles[2].bytes[2], 0x80);
    }

    #[test]
    fn jumps_and_labels_inside_proc() {
        let res = assemble_with_mapping(
            r#".segment "HEADER"
.byte 'N', 'E', 'S', $1A, $01, $00

.segment "ONE"
    nop
    .segment "TWO"
    .proc Foo
        lda #$10
    :
        beq :-
        jmp @label
        nop
    @label:
        rts
    .endproc

    jsr Foo
    "#
            .as_bytes(),
            one_two().to_vec(),
            SourceInfo::default(),
        );

        let bundles = &res.bundles[0x11..]; // Ignoring HEADER + first nop

        assert_eq!(bundles.len(), 6);

        // lda #$10
        assert_eq!(bundles[0].size, 2);
        assert_eq!(bundles[0].bytes[0], 0xA9);
        assert_eq!(bundles[0].bytes[1], 0x10);

        // beq :-
        assert_eq!(bundles[1].size, 2);
        assert_eq!(bundles[1].bytes[0], 0xF0);
        assert_eq!(bundles[1].bytes[1], 0xFE);

        // jmp @label
        assert_eq!(bundles[2].size, 3);
        assert_eq!(bundles[2].bytes[0], 0x4C);
        assert_eq!(bundles[2].bytes[1], 0x09);
        assert_eq!(bundles[2].bytes[2], 0x80);

        // nop
        assert_eq!(bundles[3].size, 1);
        assert_eq!(bundles[3].bytes[0], 0xEA);

        // rts
        assert_eq!(bundles[4].size, 1);
        assert_eq!(bundles[4].bytes[0], 0x60);

        // jsr Foo
        assert_eq!(bundles[5].size, 3);
        assert_eq!(bundles[5].bytes[0], 0x20);
        assert_eq!(bundles[5].bytes[1], 0x01);
        assert_eq!(bundles[5].bytes[2], 0x80);
    }

    #[test]
    fn proc_reference_another_segment() {
        let res = assemble_with_mapping(
            r#".segment "HEADER"
.byte 'N', 'E', 'S', $1A, $01, $00

    .segment "ONE"
    .addr code

    .segment "TWO"
    nop
    code:
        rts
    "#
            .as_bytes(),
            one_two().to_vec(),
            SourceInfo::default(),
        );

        let bundles = &res.bundles[0x10..];

        assert_eq!(bundles[0].size, 2);
        assert_eq!(bundles[0].bytes[0], 0x03);
        assert_eq!(bundles[0].bytes[1], 0x80);
    }

    #[test]
    fn cannot_fit_address_in_one_byte() {
        let res = assemble_with_mapping(
            r#".segment "HEADER"
.byte 'N', 'E', 'S', $1A, $01, $00

.segment "ONE"
        .byte code

        .segment "TWO"
        nop
        code:
            rts
        "#
            .as_bytes(),
            one_two().to_vec(),
            SourceInfo::default(),
        );

        assert_eq!(
            res.errors[0].to_string(),
            "expecting an argument that fits into a byte (line 5)"
        );
    }

    #[test]
    fn cannot_switch_to_segment_inside_of_scope() {
        assert_error(
            r#"
        .scope Vars
        .segment "CODE"
        nop
        .endscope
        "#,
            3,
            false,
            ".segment must be on the global scope",
        );
    }

    // TODO: jmp/beq outside of allocated PRG ROM
}
