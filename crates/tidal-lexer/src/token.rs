//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident(String),
    IntNumber(i64),
    FloatNumber(f64),
    StrLiteral(String),
    BoolLiteral(bool),
    CharLiteral(char),
    Mut,
    // Keywords
    If,
    Elsif,
    Else,
    End,
    For,
    In,
    While,
    Do,
    Loop,
    Break,
    Continue,
    // Logical keywords
    And,
    Or,
    Not,
    // Types
    IntType,
    StringType,
    BoolType,
    FloatType,
    DoubleType,
    CharType,
    Int8Type,
    Int16Type,
    Int32Type,
    Int64Type,
    Int128Type,
    BigIntType,
    UnIntType,
    UnInt8Type,
    UnInt16Type,
    UnInt32Type,
    UnInt64Type,
    UnInt128Type,
    UnBigIntType,
    RangeType,
    UsizeType,
    IsizeType,
    // Punctuation/operators
    Colon,
    Equal,
    EqEq,
    NotEqual,    // !=
    Less,        // <
    Greater,     // >
    LessEqual,   // <=
    GreaterEqual,// >=
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    // bitwise and power
    Ampersand,   // &
    Pipe,        // |
    Caret,       // ^
    Tilde,       // ~ (bitwise not)
    Power,       // **
    // shifts (<<, >>) and optional = variants
    SSLeft,
    SSRight,
    SSLeftEqual,
    SSRightEqual,
    // single-char punctuation for grouping/collections/indexing
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Dot,
    DoubleDot,
    // keyword operator
    As,
    Semicolon,
    Newline,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}
