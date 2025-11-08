//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

pub mod lexer;
pub mod token;

pub use lexer::lex;
pub use token::{Token, TokenKind};
