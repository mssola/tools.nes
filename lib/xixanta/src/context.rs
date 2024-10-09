use crate::assembler::Bundle;
use crate::errors::{ContextError, ContextErrorReason};
use crate::node::{PNode, PString};
use crate::opcodes::CONTROL_FUNCTIONS;
use std::collections::HashMap;

// The name of the global context as used internally..
const GLOBAL_CONTEXT: &str = "Global";

/// Context holds information about the different scopes being defined, the
/// current scope, and has a map of all the variables defined for each scope.
#[derive(Debug)]
pub struct Context {
    stack: Vec<String>,
    map: HashMap<String, HashMap<String, Bundle>>,
}

impl Context {
    /// Returns a new empty context.
    pub fn new() -> Self {
        Context {
            stack: vec![],
            map: HashMap::from([(String::from(GLOBAL_CONTEXT), HashMap::new())]),
        }
    }

    /// Returns the value of the variable represented by the given `id`. Note
    /// that this `id` can be scoped or not, and this function will try to pick
    /// the variable from the right scope.
    pub fn get_variable(&self, id: &PString) -> Result<Bundle, ContextError> {
        // First of all, figure out the name of the scope and the real name of
        // the variable. If this was not scoped at all (None case when trying to
        // rsplit by the "::" operator), then we assume it's a global variable.
        let (scope_name, var_name) = match id.value.rsplit_once("::") {
            Some((scope, name)) => (scope, name),
            None => (self.name(), id.value.as_str()),
        };

        // And with that, the only thing left is to find the scope and the
        // variable in it.
        match self.map.get(scope_name) {
            Some(scope) => match scope.get(var_name) {
                Some(var) => Ok(var.clone()),
                None => Err(ContextError {
                    message: format!(
                        "could not find variable '{}' in {}",
                        var_name,
                        self.to_human_with(scope_name)
                    ),
                    line: id.line,
                    reason: ContextErrorReason::UnknownVariable,
                }),
            },
            None => Err(ContextError {
                message: format!("did not find scope '{}'", scope_name),
                line: id.line,
                reason: ContextErrorReason::BadScope,
            }),
        }
    }

    /// Sets a value for a new variable defined in the assignment `node`.
    pub fn set_variable(&mut self, id: &PString, bundle: &Bundle) -> Result<(), ContextError> {
        let scope_name = self.name().to_string();
        let scope = self.map.get_mut(&scope_name).unwrap();

        match scope.get_mut(&id.value) {
            Some(_) => {
                return Err(ContextError {
                    message: format!(
                        "'{}' already defined in {}: you cannot re-assign variables",
                        id.value,
                        self.to_human()
                    ),
                    line: id.line,
                    reason: ContextErrorReason::Redefinition,
                })
            }
            None => {
                self.map.insert(
                    scope_name,
                    HashMap::from([(id.value.clone(), bundle.to_owned())]),
                );
            }
        }

        Ok(())
    }

    /// Change the current context given a `node`.
    pub fn change_context(&mut self, node: &PNode) -> Result<(), ContextError> {
        // The parser already guarantees that the control node is
        // from a function that we already know, so calling `unwrap`
        // is not dangerous.
        let control = CONTROL_FUNCTIONS
            .get(&node.value.value.to_lowercase())
            .unwrap();

        // If the control function does not touch the context, leave early.
        if !control.touches_context {
            return Ok(());
        }

        // And push/pop the context depending on the control being used.
        match node.value.value.as_str() {
            ".macro" | ".proc" | ".scope" => self.context_push(&node.left.clone().unwrap()),
            ".endmacro" | ".endproc" | ".endscope" => self.context_pop(&node.value)?,
            _ => {}
        }

        Ok(())
    }

    // Pushes a new context given a `node`, which holds the identifier of the
    // new scope.
    fn context_push(&mut self, id: &PNode) {
        let name = match self.stack.last() {
            Some(n) => format!("{}::{}", n, id.value.value),
            None => id.value.value.clone(),
        };

        // Actually push the name to the stack and initialize it on the variable
        // map.
        self.stack.push(name.clone());
        self.map.entry(name).or_default();
    }

    // Pops out the latest context that was pushed.
    fn context_pop(&mut self, id: &PString) -> Result<(), ContextError> {
        if self.stack.is_empty() {
            return Err(ContextError {
                message: format!("missplaced '{}' statement", id.value),
                reason: ContextErrorReason::BadScope,
                line: id.line,
            });
        }

        self.stack.truncate(self.stack.len() - 1);
        Ok(())
    }

    // Returns the name of the current context.
    fn name(&self) -> &str {
        match self.stack.last() {
            Some(name) => name,
            None => GLOBAL_CONTEXT,
        }
    }

    // Returns a human-readable string representing the current context.
    fn to_human(&self) -> String {
        match self.stack.last() {
            Some(n) => format!("'{}'", n),
            None => "the global scope".to_string(),
        }
    }

    // Returns a human-readable string representing the given context.
    fn to_human_with(&self, name: &str) -> String {
        if name == GLOBAL_CONTEXT {
            "the global scope".to_string()
        } else {
            format!("'{}'", name)
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
