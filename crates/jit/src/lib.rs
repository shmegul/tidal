//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use tidal_ast::Program;

#[derive(Debug, Clone)]
pub struct NativeModule;

#[derive(Debug, Clone)]
pub enum JitOutcome {
    Native(NativeModule),
    Unsupported,
}

pub fn compile(_program: &Program) -> JitOutcome {
    // JIT compiler scaffold: not yet implemented, return Unsupported to fall back to VM/AST
    JitOutcome::Unsupported
}
