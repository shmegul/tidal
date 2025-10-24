//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

pub mod token;
pub mod lexer;

pub use token::{Token, TokenKind};
pub use lexer::lex;
