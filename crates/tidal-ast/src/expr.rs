//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use crate::{ops::Op, stmt::Stmt, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(std::ffi::c_int),
    Bool(bool),
    String(std::ffi::CString),
    Float(std::ffi::c_float),
    Double(std::ffi::c_double),
    Char(std::ffi::c_char),
    Ident(String),
    Binary(Box<Expr>, Op, Box<Expr>),
    BitNot(Box<Expr>),
    Not(Box<Expr>),
    Block(Vec<Stmt>),
    If {
        branches: Vec<(Expr, Vec<Stmt>)>,
        else_branch: Option<Vec<Stmt>>,
    },
    // Type cast: left expression casted to target type
    Cast(Box<Expr>, Type),
    // Array and tuple literals
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    // Postfix index access: base.<index>
    Index(Box<Expr>, usize),
    // Range operator for iterators: a..b
    Range(Box<Expr>, Box<Expr>),
}
