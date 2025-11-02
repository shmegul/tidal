//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use std::fmt::{self, Display};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    Lex,
    Parse,
    Runtime,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub message: String,
}

impl Error {
    pub fn lex(msg: impl Into<String>) -> Self {
        Self {
            kind: ErrorKind::Lex,
            message: msg.into(),
        }
    }
    pub fn parse(msg: impl Into<String>) -> Self {
        Self {
            kind: ErrorKind::Parse,
            message: msg.into(),
        }
    }
    pub fn runtime(msg: impl Into<String>) -> Self {
        Self {
            kind: ErrorKind::Runtime,
            message: msg.into(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {}",
            match self.kind {
                ErrorKind::Lex => "LexError",
                ErrorKind::Parse => "ParseError",
                ErrorKind::Runtime => "RuntimeError",
            },
            self.message
        )
    }
}

impl std::error::Error for Error {}
