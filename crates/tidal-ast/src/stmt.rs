//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use crate::{expr::Expr, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        mutable: bool,
        name: String,
        ty: Option<Type>,
        expr: Expr,
    },
    AddAssign {
        name: String,
        expr: Expr,
    },
    SubAssign {
        name: String,
        expr: Expr,
    },
    MulAssign {
        name: String,
        expr: Expr,
    },
    DivAssign {
        name: String,
        expr: Expr,
    },
    RemAssign {
        name: String,
        expr: Expr,
    },
    AssignIndex {
        name: String,
        index: usize,
        expr: Expr,
    },
    For {
        mutable: bool,
        name: String,
        iter: Expr,
        body: Vec<Stmt>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Loop {
        body: Vec<Stmt>,
    },
    Break,
    Continue,
    ExprStmt(Expr),
}
