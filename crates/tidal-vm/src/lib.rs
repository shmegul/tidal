//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

pub mod bytecode;
pub mod codegen;

pub use bytecode::{Bytecode, Instr};
pub use codegen::{CompileOutcome, compile};
