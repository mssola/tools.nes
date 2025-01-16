use std::fmt;

/// A Positioned String. That is, a String which also has information on the
/// line number and the column range.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct PString {
    /// The actual value.
    pub value: String,

    /// Line number where it has been found.
    pub line: usize,

    /// The start column where it has been found.
    pub start: usize,

    /// The end column where it has been found.
    pub end: usize,
}

impl PString {
    /// Returns true if the string has either an empty value or an empty range.
    pub fn is_empty(&self) -> bool {
        self.value.is_empty() || (self.start == self.end)
    }

    /// Returns an empty tuple if the string contains a valid identifier, or a
    /// String containing the error otherwise. If `allow_scoped` is set to true,
    /// then identifiers with the "::" operator in it will be considered valid.
    pub fn is_valid_identifier(&self, allow_scoped: bool) -> Result<(), String> {
        if self.value.trim().is_empty() {
            return Err(String::from("empty identifier"));
        }

        // You cannot assign into a name which is reserved.
        if matches!(self.value.to_lowercase().as_str(), "x" | "y" | "a") {
            return Err(format!("cannot use reserved name '{}'", self.value));
        }

        // You cannot assign into scoped names: declare them into their
        // respective scopes instead.
        if !allow_scoped && self.value.contains("::") {
            return Err(format!(
                "the name '{}' is scoped: do not declare things this way",
                self.value
            ));
        }

        // Let's gather info from the variable name which is relevant to later
        // checks.
        let mut alpha_seen = false;
        let mut valid_hex = matches!(self.value.len(), 1..=4);
        for c in self.value.to_lowercase().chars() {
            if c == '_' {
                valid_hex = false;
            } else if c.is_alphabetic() {
                alpha_seen = true;
                if c > 'f' && c <= 'z' {
                    valid_hex = false;
                }
            }
        }

        // We need at least one alphabetic character. Otherwise it might be
        // confusing with numbers.
        if !alpha_seen {
            return Err(format!(
                "name '{}' requires at least one alphabetic character",
                self.value
            ));
        }

        // To avoid problems down the line, you cannot assign into names which
        // are proper hexadecimal values.
        if valid_hex {
            return Err(format!(
                "cannot use names which are valid hexadecimal values such as '{}'",
                self.value
            ));
        }

        Ok(())
    }

    /// Returns true if this is an anonymous relative reference (i.e. the ':+'
    /// in something like "beq :+").
    pub fn is_anonymous_relative_reference(&self) -> bool {
        let mut it = self.value.chars();

        // An anonymous relative reference must start with ':'. If that's not
        // the case, return early.
        if it.next().unwrap_or(' ') != ':' {
            return false;
        }

        // Get the first character of the reference. The rest of the string must
        // be the same as this character.
        let init = it.next().unwrap_or(' ');
        if init != '+' && init != '-' {
            return false;
        }

        // The rest of the string should match the initial 'c' character (i.e.
        // either '+' or '-', but never mixed in).
        it.all(|current| init == current)
    }

    /// Returns the isize value that can be computed assuming that this is an
    /// anonymous relative reference. References to a previous label will have a
    /// negative value, while references to next labels have a positive one.
    pub fn to_isize(&self) -> isize {
        let c = self.value.chars().nth(1).unwrap_or(' ');

        // As stated from the documentation, this *has to be* a valid reference.
        assert!(c == '+' || c == '-', "bad relative reference");

        let res = self.value.chars().filter(|x| *x == c).count() as isize;
        if c == '-' {
            return -res;
        }
        res
    }
}

/// The type of control function being used. Use this enum in order to detect
/// which control function was detected instead of the node value.
#[derive(Debug, Clone, PartialEq)]
pub enum ControlType {
    Hibyte,
    Lobyte,
    StartMacro,
    EndMacro,
    StartProc,
    EndProc,
    StartScope,
    EndScope,
    Segment,
    Byte,
    Word,
    Addr,
    IncBin,
    StartRepeat,
    EndRepeat,
    IncludeSource,
    ReserveMemory,
    Asciiz,
}

impl fmt::Display for ControlType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ControlType::Hibyte => write!(f, ".hibyte"),
            ControlType::Lobyte => write!(f, ".lobyte"),
            ControlType::StartMacro => write!(f, ".macro"),
            ControlType::EndMacro => write!(f, ".endmacro"),
            ControlType::StartProc => write!(f, ".proc"),
            ControlType::EndProc => write!(f, ".endproc"),
            ControlType::StartScope => write!(f, ".scope"),
            ControlType::EndScope => write!(f, ".endscope"),
            ControlType::Segment => write!(f, ".segment"),
            ControlType::Byte => write!(f, ".byte/.db"),
            ControlType::Word => write!(f, ".word/.dw"),
            ControlType::Addr => write!(f, ".addr"),
            ControlType::IncBin => write!(f, ".incbin"),
            ControlType::StartRepeat => write!(f, ".repeat"),
            ControlType::EndRepeat => write!(f, ".endrepeat"),
            ControlType::IncludeSource => write!(f, ".include"),
            ControlType::ReserveMemory => write!(f, ".res"),
            ControlType::Asciiz => write!(f, ".asciiz"),
        }
    }
}

impl ControlType {
    /// Returns true if the type of control statement requires it to be in the
    /// global scope.
    pub fn must_be_global(&self) -> bool {
        matches!(self, ControlType::StartMacro | ControlType::Segment)
    }

    /// Returns true if the control type guarantees that a body is going to be
    /// present under an inner node.
    pub fn has_body(&self) -> bool {
        matches!(
            self,
            ControlType::StartMacro | Self::StartProc | Self::StartScope
        )
    }
}

/// The type of operation being used.
#[derive(Debug, Clone, PartialEq)]
pub enum OperationType {
    Add,
    Sub,
    Mul,
    Div,
    Lshift,
    Rshift,
    And,
    LogicalAnd,
    Or,
    LogicalOr,
    Xor,
    UnaryPositive,
    UnaryNegative,
    BitwiseNot,
    LogicalNot,
    LoByte,
    HiByte,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
}

/// The PNode type.
#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    /// A general/abstract value. This can either be a variable, or a macro call
    /// with no arguments. This is, thus, to be determined by the assembler
    /// after a proper context has been set.
    Value,

    /// An instruction. The value of the PNode has the instruction mnemonic. The
    /// left node is the "left arm", which is whatever is left of the comma from
    /// expressions like "lda Left, x", or an Indirection on indirect addressing
    /// mode (e.g. "lda (Indirect, x)"). The righ node contains the "right arm",
    /// which basically contains the index on an indexed addressing mode.
    Instruction,

    /// An Indirection node, which might have two arms (e.g. "lda (Left,
    /// Right)"), or just a left one (e.g. "lda (Left), y")
    Indirection,

    /// An assignment holds the name of the variable on the `value` and the
    /// actual expression that initializes it on the `left`. The rest is None.
    Assignment,

    /// A control statement (e.g. ".proc foo"). The `value` string contains the
    /// name of the function as it was provided (use the `ControlType` value of
    /// the enum to detect which function was exactly provided), the `left` an
    /// optional identifier (e.g. the "foo" on ".proc foo"), and the `args`
    /// contain any possible arguments that have been passed to this control
    /// statement. The right arm might contain the body of the control statement
    /// if it has some (e.g. the body inside of a .macro declaration).
    Control(ControlType),

    /// The body of a control statement. The only relevant info here is `args`,
    /// which contain the instructions of the body.
    ControlBody,

    /// A literal expression, that is, something that starts with '#', '%' or
    /// '$'. The `left` node contains the inner expression.
    Literal,

    /// A label statement, which only sets the `value`, the name of the label.
    Label,

    /// A macro call. Note that a Value might also encode this, but when a Call
    /// has been detected, then there is no doubt on it.
    Call,

    /// A operation expression. If the operation has two sides, then the left
    /// and right arms contain the operands, otherwise only the right one.
    Operation(OperationType),
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NodeType::Value => write!(f, "value"),
            NodeType::Instruction => write!(f, "instruction"),
            NodeType::Indirection => write!(f, "indirection"),
            NodeType::Assignment => write!(f, "assignment"),
            NodeType::Control(control_type) => write!(f, "control function ({})", control_type),
            NodeType::ControlBody => write!(f, "control function body"),
            NodeType::Literal => write!(f, "literal"),
            NodeType::Label => write!(f, "label"),
            NodeType::Call => write!(f, "call"),
            NodeType::Operation(op) => match op {
                OperationType::Add => write!(f, "addition"),
                OperationType::Sub => write!(f, "subtraction"),
                OperationType::Mul => write!(f, "multiplication"),
                OperationType::Div => write!(f, "division"),
                OperationType::Lshift => write!(f, "left shift"),
                OperationType::Rshift => write!(f, "right shift"),
                OperationType::And => write!(f, "bitwise and"),
                OperationType::LogicalAnd => write!(f, "logical and"),
                OperationType::Or => write!(f, "bitwise or"),
                OperationType::LogicalOr => write!(f, "logical or"),
                OperationType::Xor => write!(f, "bitwise xor"),
                OperationType::UnaryPositive => write!(f, "unary positive"),
                OperationType::UnaryNegative => write!(f, "unary negative"),
                OperationType::BitwiseNot => write!(f, "bitwise not"),
                OperationType::LogicalNot => write!(f, "logical not"),
                OperationType::LoByte => write!(f, "low byte"),
                OperationType::HiByte => write!(f, "high byte"),
                OperationType::Equal => write!(f, "equal"),
                OperationType::NotEqual => write!(f, "not equal"),
                OperationType::LessEqual => write!(f, "less or equal"),
                OperationType::GreaterEqual => write!(f, "greater or equal"),
                OperationType::Less => write!(f, "less"),
                OperationType::Greater => write!(f, "greater"),
            },
        }
    }
}

impl NodeType {
    /// Returns the NodeType that closes the current one if any.
    pub fn closing_type(&self) -> Option<NodeType> {
        match self {
            NodeType::Control(ControlType::StartMacro) => {
                Some(NodeType::Control(ControlType::EndMacro))
            }
            NodeType::Control(ControlType::StartProc) => {
                Some(NodeType::Control(ControlType::EndProc))
            }
            NodeType::Control(ControlType::StartScope) => {
                Some(NodeType::Control(ControlType::EndScope))
            }
            NodeType::Control(ControlType::StartRepeat) => {
                Some(NodeType::Control(ControlType::EndRepeat))
            }
            _ => None,
        }
    }
}

/// A Positioned Node. This is a node on a tree which holds a PString as a
/// value. The node type determines the actual representation of the value and
/// both childs (see the `NodeType` enum). Moreover, out of convenience, a node
/// also holds an optional list of arguments, which simplifies arrangements such
/// as the `Call` or the `Control` node types.
#[derive(Debug, Clone)]
pub struct PNode {
    /// The type of the PNode.
    pub node_type: NodeType,

    /// A Positioned String acting as the value. Check with the `NodeType`
    /// documentation to check when it makes sense to use it.
    pub value: PString,

    /// The left child of the node. Check with the `NodeType` documentation to
    /// check when it makes sense to use it.
    pub left: Option<Box<PNode>>,

    /// The right child of the node. Check with the `NodeType` documentation to
    /// check when it makes sense to use it.
    pub right: Option<Box<PNode>>,

    /// Convenience list used by some node types in order to express a list of
    /// optional arguments.
    pub args: Option<Vec<PNode>>,

    /// Index on the list of SourceInfo's maintained by both the parser and the
    /// assembler.
    pub source: usize,
}

/// Whether there is a body for a given node and whether it starts or ends it.
#[derive(Debug)]
pub enum NodeBodyType {
    None,
    Starts,
    Ends,
}

impl PNode {
    /// Returns true if the current node represents a branching instruction.
    pub fn is_branch(&self) -> bool {
        if self.node_type != NodeType::Instruction {
            return false;
        }

        let opcode = self.value.value.to_lowercase();
        matches!(
            opcode.as_str(),
            "bcc" | "bcs" | "beq" | "bmi" | "bne" | "bpl" | "bvc" | "bvs"
        )
    }

    /// Returns whether the node describes a starting/ending statement or none
    /// of them.
    pub fn body_type(&self) -> NodeBodyType {
        match self.node_type {
            NodeType::Control(ControlType::StartMacro)
            | NodeType::Control(ControlType::StartProc)
            | NodeType::Control(ControlType::StartScope)
            | NodeType::Control(ControlType::StartRepeat) => NodeBodyType::Starts,
            NodeType::Control(ControlType::EndMacro)
            | NodeType::Control(ControlType::EndProc)
            | NodeType::Control(ControlType::EndScope)
            | NodeType::Control(ControlType::EndRepeat) => NodeBodyType::Ends,
            _ => NodeBodyType::None,
        }
    }
}
