//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    String,
    Bool,
    Float,
    Double,
    Char,
    
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    BigInt,
    
    UnInt,
    UnInt8,
    UnInt16,
    UnInt32,
    UnInt64,
    UnInt128,
    UnBigInt,
    
    Range,
    Usize,
    Isize,
    
    Array(Box<Type>, usize),
    Tuple(Vec<Type>),
}
