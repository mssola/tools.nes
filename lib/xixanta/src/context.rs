use crate::assembler::Bundle;
use crate::errors::{ContextError, ContextErrorReason};
use crate::node::{ControlType, NodeType, PNode, PString};
use crate::opcodes::CONTROL_FUNCTIONS;
use std::collections::HashMap;

// The name of the global context as used internally.
const GLOBAL_CONTEXT: &str = "Global";

/// Context holds information about the different scopes being defined, the
/// current scope, and has a map of all the variables defined for each scope.
#[derive(Debug)]
pub struct Context {
    /// Tracks the contexts that we have entered at any given point. The last
    /// element is the actual context, and it will be empty if we are in the
    /// global context.
    stack: Vec<String>,

    /// Map of variables for any given context. The key is the name of the
    /// context, and the value is another map. This inner map has the variable
    /// name as the key, and the Bundle as a value.
    map: HashMap<String, HashMap<String, Bundle>>,

    /// Map of labels for any given context. Note that this only keeps track of
    /// the amount of labels that have been defined, which might include
    /// anonymous positions. This is primarily used on relative addressing where
    /// the name of the label might not be provided (e.g. anonymous relative
    /// reference).
    labels: HashMap<String, Vec<Bundle>>,
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    /// Returns a new empty context.
    pub fn new() -> Self {
        Context {
            stack: vec![],
            map: HashMap::from([(String::from(GLOBAL_CONTEXT), HashMap::new())]),
            labels: HashMap::from([(String::from(GLOBAL_CONTEXT), vec![])]),
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

    /// Sets a value for a variable defined in the assignment `node`. If
    /// `overwrite` is set to true, then this value will be set even if the
    /// variable already existed, otherwise it will return a ContextError
    pub fn set_variable(
        &mut self,
        id: &PString,
        bundle: &Bundle,
        overwrite: bool,
    ) -> Result<(), ContextError> {
        let scope_name = self.name().to_string();
        let scope = self.map.get_mut(&scope_name).unwrap();

        match scope.get_mut(&id.value) {
            Some(sc) => {
                if !overwrite {
                    return Err(ContextError {
                        message: format!(
                            "'{}' already defined in {}: you cannot re-assign variables",
                            id.value,
                            self.to_human()
                        ),
                        line: id.line,
                        reason: ContextErrorReason::Redefinition,
                    });
                }
                *sc = bundle.clone();
            }
            None => {
                scope.insert(id.value.clone(), bundle.to_owned());
            }
        }

        Ok(())
    }

    /// Add a new label that has the value as given by the `bundle` parameter.
    /// The actual name of the label does not matter since that is already
    /// referenced as a "variable". This function needs to be called whenever we
    /// are sure that we have the proper address for a label and that we should
    /// track it in order for relative addressing to work.
    pub fn add_label(&mut self, bundle: &Bundle) {
        let scope_name = self.name().to_string();
        let scope = self.labels.get_mut(&scope_name).unwrap();

        // println!("PUSHING: {:#?}", bundle);

        scope.push(bundle.clone());
    }

    /// Change the current context given a `node`. Returns a tuple which states:
    ///   0. Whether the context has changed.
    ///   1. Whether a caller can bundle nodes safely.
    pub fn change_context(&mut self, node: &PNode) -> Result<(bool, bool), ContextError> {
        // The parser already guarantees that the control node is
        // from a function that we already know, so calling `unwrap`
        // is not dangerous.
        let control = CONTROL_FUNCTIONS
            .get(&node.value.value.to_lowercase())
            .unwrap();

        // If the control function does not touch the context, leave early.
        if !control.touches_context {
            return Ok((false, true));
        }

        // And push/pop the context depending on the control being used.
        match node.node_type {
            NodeType::Control(ControlType::StartMacro) => {
                self.context_push(&node.left.clone().unwrap());
                Ok((true, false))
            }
            NodeType::Control(ControlType::StartProc)
            | NodeType::Control(ControlType::StartScope) => {
                self.context_push(&node.left.clone().unwrap());
                Ok((true, true))
            }
            NodeType::Control(ControlType::EndMacro)
            | NodeType::Control(ControlType::EndProc)
            | NodeType::Control(ControlType::EndScope) => {
                self.context_pop(&node.value)?;
                Ok((true, true))
            }
            _ => Ok((false, true)),
        }
    }

    /// Change the current context to the given one identified by `name`,
    /// disregarding any check. This is to be used when switching a context to
    /// set a very specific value for that context. You should call
    /// `force_context_pop` immediately.
    pub fn force_context_switch(&mut self, name: &String) {
        self.stack.push(name.to_owned());
    }

    /// Remove the last context being used if any. In contrast with
    /// `context_pop`, this one does not error out, but does nothing in case we
    /// are in the global context. This is to be used in conjunction with
    /// `force_context_switch`.
    pub fn force_context_pop(&mut self) {
        if !self.stack.is_empty() {
            self.stack.truncate(self.stack.len() - 1);
        }
    }

    /// Returns the amount of labels that have been submitted so far for the
    /// current scope.
    pub fn labels_seen(&self) -> usize {
        let scope_name = self.name().to_string();

        match self.labels.get(&scope_name) {
            Some(labels) => labels.len(),
            None => 0,
        }
    }

    /// Returns the bundle which is representative for the label being
    /// referenced in a relative way. The relation is given on the `rel`
    /// parameter, with a value between -4 or +4 where negative values represent
    /// previous labels and positive values next ones (e.g. -2 means "2 labels
    /// before"). This is in relation to `labels_seen`, which states how many
    /// labels have been seen by the caller at this point.
    pub fn get_relative_label(
        &self,
        rel: isize,
        labels_seen: usize,
    ) -> Result<Bundle, ContextError> {
        // Bound check: the given 'rel' parameter has a proper value.
        assert!(
            rel < 5 && rel > -5 && rel != 0,
            "bad parameter for relative label"
        );

        // Bound check: you cannot reference a past label that doesn't exist.
        // This is the programmer's to blame, not on us, so don't assert.
        if labels_seen == 0 && rel < 0 {
            return Err(ContextError {
                line: 0,
                message: "cannot reference an unknown previous label".to_string(),
                reason: ContextErrorReason::Label,
            });
        }

        // Get the labels as referenced in the current context, and also the
        // index that we will be using.
        let scope_name = self.name().to_string();
        let labels = self.labels.get(&scope_name).unwrap();
        let idx = if rel > 0 {
            labels_seen as isize + rel - 1
        } else {
            labels_seen as isize + rel
        };

        // Bound check: is the programmer referencing an "out of bounds" label?
        // If so then it's a mistake on their part.
        if idx < 0 || idx >= labels.len() as isize {
            return Err(ContextError {
                line: 0,
                message: "cannot reference bogus label (out of bounds)".to_string(),
                reason: ContextErrorReason::Label,
            });
        }

        // Everything should be fine from here on, simply return the bundle that
        // was being referenced.
        Ok(labels[idx as usize].clone())
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
        self.map.entry(name.clone()).or_default();
        self.labels.entry(name).or_default();
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

    /// Returns the name of the current context.
    pub fn name(&self) -> &str {
        match self.stack.last() {
            Some(name) => name,
            None => GLOBAL_CONTEXT,
        }
    }

    /// Returns true if we are in the global scope.
    pub fn is_global(&self) -> bool {
        self.stack.is_empty()
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
