#[macro_use]
extern crate lazy_static;

// TODO: be more mindful on what's exported outside.

pub mod assembler;
mod context;
mod errors;
pub mod instruction;
mod opcodes;
