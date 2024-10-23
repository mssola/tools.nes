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
    /// name of the function, the `left` an optional identifier (e.g. the "foo"
    /// on ".proc foo"), and the `args` contain any possible arguments that have
    /// been passed to this control statement.
    Control,

    /// A literal expression, that is, something that starts with '#', '%' or
    /// '$'. The `left` node contains the inner expression.
    Literal,

    /// A label statement, which only sets the `value`, the name of the label.
    Label,

    /// A macro call. Note that a Value might also encode this, but when a Call
    /// has been detected, then there is no doubt on it.
    Call,
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NodeType::Value => write!(f, "value"),
            NodeType::Instruction => write!(f, "instruction"),
            NodeType::Indirection => write!(f, "indirection"),
            NodeType::Assignment => write!(f, "assignment"),
            NodeType::Control => write!(f, "control function"),
            NodeType::Literal => write!(f, "literal"),
            NodeType::Label => write!(f, "label"),
            NodeType::Call => write!(f, "call"),
        }
    }
}

/// A Position Node. This is a node on a binary tree which holds a PString as a
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
}
