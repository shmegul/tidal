//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use std::fmt;

use std::rc::Rc;
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    String(Rc<String>),
    Float(f32),
    Double(f64),
    Char(char),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Range(i64, i64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Float(x) => write!(f, "{}", x),
            Value::Double(x) => write!(f, "{}", x),
            Value::Char(c) => write!(f, "{}", c),
            Value::Array(items) => {
                write!(f, "[")?;
                let mut first = true;
                for v in items {
                    if !first {
                        write!(f, ", ")?;
                    } else {
                        first = false;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Tuple(items) => {
                write!(f, "(")?;
                let mut first = true;
                for v in items {
                    if !first {
                        write!(f, ", ")?;
                    } else {
                        first = false;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Value::Range(a, b) => write!(f, "{}..{}", a, b),
        }
    }
}
