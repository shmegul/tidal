//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use tidal_ast::{Expr, Stmt};
use tidal_errors::Result;

use super::Env;
use crate::value::Value;

#[derive(Debug, Clone)]
pub enum OptimizedOp {
    AlwaysTrueThen(Vec<Stmt>),
    AlwaysFalseThen(Vec<Stmt>),
    Interpret(Stmt),
}

impl Env {
    pub fn should_optimize(&self, iteration_count: usize) -> bool {
        const OPTIMIZATION_THRESHOLD: usize = 1000;
        iteration_count >= OPTIMIZATION_THRESHOLD
    }

    pub fn detect_patterns(&self, stmts: &[Stmt]) -> Vec<OptimizedOp> {
        let mut optimized = Vec::with_capacity(stmts.len());
        for s in stmts {
            if let Some(op) = self.match_known_pattern(s) {
                optimized.push(op);
            } else {
                optimized.push(OptimizedOp::Interpret(s.clone()));
            }
        }
        optimized
    }

    pub fn match_known_pattern(&self, stmt: &Stmt) -> Option<OptimizedOp> {
        match stmt {
            Stmt::ExprStmt(Expr::If {
                branches,
                else_branch: _,
            }) => {
                if let Some((Expr::Binary(l, tidal_ast::ops::Op::Eq, r), then_block)) =
                    branches.first()
                {
                    // Recognize i == i -> true
                    if let (Expr::Ident(a), Expr::Ident(b)) = (&**l, &**r) {
                        if a == b {
                            return Some(OptimizedOp::AlwaysTrueThen(then_block.clone()));
                        }
                    }
                }
                // Fallback
                None
            }
            _ => None,
        }
    }

    pub fn exec_optimized_op(&mut self, op: &OptimizedOp) -> Result<(Value, bool)> {
        match op {
            OptimizedOp::AlwaysTrueThen(stmts) => {
                // Execute then-block and return last value (not printed by default)
                let v = self.eval_block_ref(stmts)?;
                Ok((v, false))
            }
            OptimizedOp::AlwaysFalseThen(_stmts) => Ok((Value::Int(0), false)),
            OptimizedOp::Interpret(stmt) => self.exec_stmt_ref(stmt),
        }
    }
}
