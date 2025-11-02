//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

#![allow(clippy::too_many_arguments, clippy::if_same_then_else)]
use crate::token::{Token, TokenKind};
use tidal_errors::{Error, Result};

#[inline(always)]
fn push(tokens: &mut Vec<Token>, kind: TokenKind, pos: usize, line: usize, col: usize) {
    tokens.push(Token {
        kind,
        pos,
        line,
        col,
    });
}

#[inline(always)]
fn push_with_optional_eq(
    tokens: &mut Vec<Token>,
    bytes: &[u8],
    i: &mut usize,
    line: usize,
    col: &mut usize,
    pos: usize,
    kind_without_eq: TokenKind,
    kind_with_eq: TokenKind,
) {
    if *i + 1 < bytes.len() && bytes[*i + 1] == b'=' {
        push(tokens, kind_with_eq, pos, line, *col);
        *i += 2;
        *col += 2;
    } else {
        push(tokens, kind_without_eq, pos, line, *col);
        *i += 1;
        *col += 1;
    }
}

#[inline(always)]
fn push_double_with_optional_eq(
    tokens: &mut Vec<Token>,
    bytes: &[u8],
    i: &mut usize,
    line: usize,
    col: &mut usize,
    pos: usize,
    first: u8,
    second: u8,
    kind_without_eq: TokenKind,
    kind_with_eq: TokenKind,
) -> bool {
    if *i + 1 < bytes.len() && bytes[*i] == first && bytes[*i + 1] == second {
        if *i + 2 < bytes.len() && bytes[*i + 2] == b'=' {
            push(tokens, kind_with_eq, pos, line, *col);
            *i += 3;
            *col += 3;
        } else {
            push(tokens, kind_without_eq, pos, line, *col);
            *i += 2;
            *col += 2;
        }
        true
    } else {
        false
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut i = 0usize;
    let bytes = input.as_bytes();
    let mut line = 1usize;
    let mut col = 1usize;

    while i < bytes.len() {
        let c = bytes[i] as char;
        let pos = i;
        match c {
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            }
            '\n' => {
                push(&mut tokens, TokenKind::Newline, pos, line, col);
                i += 1;
                line += 1;
                col = 1;
            }
            ';' => {
                push(&mut tokens, TokenKind::Semicolon, pos, line, col);
                i += 1;
                col += 1;
            }
            ':' => {
                push(&mut tokens, TokenKind::Colon, pos, line, col);
                i += 1;
                col += 1;
            }
            '(' => {
                push(&mut tokens, TokenKind::LParen, pos, line, col);
                i += 1;
                col += 1;
            }
            ')' => {
                push(&mut tokens, TokenKind::RParen, pos, line, col);
                i += 1;
                col += 1;
            }
            '[' => {
                push(&mut tokens, TokenKind::LBracket, pos, line, col);
                i += 1;
                col += 1;
            }
            ']' => {
                push(&mut tokens, TokenKind::RBracket, pos, line, col);
                i += 1;
                col += 1;
            }
            ',' => {
                push(&mut tokens, TokenKind::Comma, pos, line, col);
                i += 1;
                col += 1;
            }
            '.' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'.' {
                    push(&mut tokens, TokenKind::DoubleDot, pos, line, col);
                    i += 2;
                    col += 2;
                } else {
                    push(&mut tokens, TokenKind::Dot, pos, line, col);
                    i += 1;
                    col += 1;
                }
            }
            '=' => {
                push_with_optional_eq(
                    &mut tokens,
                    bytes,
                    &mut i,
                    line,
                    &mut col,
                    pos,
                    TokenKind::Equal,
                    TokenKind::EqEq,
                );
            }
            '+' => {
                push_with_optional_eq(
                    &mut tokens,
                    bytes,
                    &mut i,
                    line,
                    &mut col,
                    pos,
                    TokenKind::Plus,
                    TokenKind::PlusEqual,
                );
            }
            '-' => {
                push_with_optional_eq(
                    &mut tokens,
                    bytes,
                    &mut i,
                    line,
                    &mut col,
                    pos,
                    TokenKind::Minus,
                    TokenKind::MinusEqual,
                );
            }
            '*' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    push(&mut tokens, TokenKind::Power, pos, line, col);
                    i += 2;
                    col += 2;
                } else {
                    push_with_optional_eq(
                        &mut tokens,
                        bytes,
                        &mut i,
                        line,
                        &mut col,
                        pos,
                        TokenKind::Star,
                        TokenKind::StarEqual,
                    );
                }
            }
            '/' => {
                push_with_optional_eq(
                    &mut tokens,
                    bytes,
                    &mut i,
                    line,
                    &mut col,
                    pos,
                    TokenKind::Slash,
                    TokenKind::SlashEqual,
                );
            }
            '%' => {
                push_with_optional_eq(
                    &mut tokens,
                    bytes,
                    &mut i,
                    line,
                    &mut col,
                    pos,
                    TokenKind::Percent,
                    TokenKind::PercentEqual,
                );
            }
            '"' => {
                i += 1;
                col += 1;
                let mut s = String::new();
                let mut closed = false;
                while i < bytes.len() {
                    let ch = bytes[i] as char;
                    if ch == '"' {
                        i += 1;
                        col += 1;
                        closed = true;
                        break;
                    }
                    if ch == '\\' {
                        if i + 1 >= bytes.len() {
                            break;
                        }
                        let esc = bytes[i + 1] as char;
                        match esc {
                            '"' => {
                                s.push('"');
                                i += 2;
                                col += 2;
                            }
                            '\\' => {
                                s.push('\\');
                                i += 2;
                                col += 2;
                            }
                            'n' => {
                                s.push('\n');
                                i += 2;
                                col += 2;
                            }
                            't' => {
                                s.push('\t');
                                i += 2;
                                col += 2;
                            }
                            _ => {
                                return Err(Error::lex("invalid escape in string"));
                            }
                        }
                    } else {
                        s.push(ch);
                        i += 1;
                        col += 1;
                    }
                }
                if !closed {
                    return Err(Error::lex("unterminated string literal"));
                }
                push(&mut tokens, TokenKind::StrLiteral(s), pos, line, col);
            }
            '\'' => {
                i += 1;
                col += 1;
                if i >= bytes.len() {
                    return Err(Error::lex("unterminated char literal"));
                }
                let ch = bytes[i] as char;
                let c_val = if ch == '\\' {
                    if i + 1 >= bytes.len() {
                        return Err(Error::lex("unterminated char escape"));
                    }
                    let esc = bytes[i + 1] as char;
                    i += 2;
                    col += 2;
                    match esc {
                        '\'' => '\'',
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        _ => {
                            return Err(Error::lex("invalid escape in char"));
                        }
                    }
                } else {
                    i += 1;
                    col += 1;
                    ch
                };
                if i >= bytes.len() || bytes[i] as char != '\'' {
                    return Err(Error::lex("unterminated char literal"));
                }
                i += 1;
                col += 1;
                push(&mut tokens, TokenKind::CharLiteral(c_val), pos, line, col);
            }
            '<' => {
                // '<<' '<<=' or '<' '<='
                if push_double_with_optional_eq(
                    &mut tokens,
                    bytes,
                    &mut i,
                    line,
                    &mut col,
                    pos,
                    b'<',
                    b'<',
                    TokenKind::SSLeft,
                    TokenKind::SSLeftEqual,
                ) {
                    // done
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    push(&mut tokens, TokenKind::LessEqual, pos, line, col);
                    i += 2;
                    col += 2;
                } else {
                    push(&mut tokens, TokenKind::Less, pos, line, col);
                    i += 1;
                    col += 1;
                }
            }
            '>' => {
                // '>>' '>>=' or '>' '>='
                if push_double_with_optional_eq(
                    &mut tokens,
                    bytes,
                    &mut i,
                    line,
                    &mut col,
                    pos,
                    b'>',
                    b'>',
                    TokenKind::SSRight,
                    TokenKind::SSRightEqual,
                ) {
                    // done
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    push(&mut tokens, TokenKind::GreaterEqual, pos, line, col);
                    i += 2;
                    col += 2;
                } else {
                    push(&mut tokens, TokenKind::Greater, pos, line, col);
                    i += 1;
                    col += 1;
                }
            }
            '&' => {
                push(&mut tokens, TokenKind::Ampersand, pos, line, col);
                i += 1;
                col += 1;
            }
            '|' => {
                push(&mut tokens, TokenKind::Pipe, pos, line, col);
                i += 1;
                col += 1;
            }
            '^' => {
                push(&mut tokens, TokenKind::Caret, pos, line, col);
                i += 1;
                col += 1;
            }
            '~' => {
                push(&mut tokens, TokenKind::Tilde, pos, line, col);
                i += 1;
                col += 1;
            }
            '!' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    push(&mut tokens, TokenKind::NotEqual, pos, line, col);
                    i += 2;
                    col += 2;
                } else {
                    return Err(Error::lex("unexpected character '!'"));
                }
            }
            '#' => {
                // comment: single-line or multiline #* *#
                if i + 1 < bytes.len() && bytes[i + 1] as char == '*' {
                    // multiline #* ... *#
                    i += 2;
                    col += 2;
                    let mut closed = false;
                    while i + 1 < bytes.len() {
                        if bytes[i] as char == '*' && bytes[i + 1] as char == '#' {
                            i += 2;
                            col += 2;
                            closed = true;
                            break;
                        }
                        if bytes[i] as char == '\n' {
                            line += 1;
                            col = 1;
                            i += 1;
                            continue;
                        }
                        i += 1;
                        col += 1;
                    }
                    if !closed {
                        return Err(Error::lex("Unterminated multi-line comment (#* *#)"));
                    }
                } else {
                    // single-line # ... end
                    while i < bytes.len() && bytes[i] as char != '\n' {
                        i += 1;
                        col += 1;
                    }
                }
            }
            c if c.is_ascii_digit() => {
                let start_col = col;
                // Special prefixes: binary 0b / 0B
                if bytes[i] == b'0'
                    && i + 1 < bytes.len()
                    && (bytes[i + 1] == b'b' || bytes[i + 1] == b'B')
                {
                    let mut j = i + 2;
                    let mut val: i64 = 0;
                    let mut saw_digit = false;
                    while j < bytes.len() {
                        let ch = bytes[j] as char;
                        match ch {
                            '0' => {
                                val = val.saturating_mul(2);
                                saw_digit = true;
                                j += 1;
                            }
                            '1' => {
                                val = val.saturating_mul(2).saturating_add(1);
                                saw_digit = true;
                                j += 1;
                            }
                            '_' => {
                                j += 1;
                            }
                            _ => break,
                        }
                    }
                    if !saw_digit {
                        return Err(Error::lex("invalid binary literal"));
                    }
                    push(&mut tokens, TokenKind::IntNumber(val), pos, line, start_col);
                    col += j - i;
                    i = j;
                } else {
                    // integer part with numeric separators '_'
                    let mut val: i64 = 0;
                    let mut j = i;
                    while j < bytes.len() {
                        let ch = bytes[j] as char;
                        if ch.is_ascii_digit() {
                            val = val
                                .saturating_mul(10)
                                .saturating_add((ch as u8 - b'0') as i64);
                            j += 1;
                        } else if ch == '_' {
                            // skip separators
                            j += 1;
                        } else {
                            break;
                        }
                    }

                    // Detect fractional and/or exponent part.
                    let mut k = j;
                    let mut is_float = false;

                    // Fractional part: '.' followed by digit
                    if k + 1 < bytes.len()
                        && (bytes[k] as char) == '.'
                        && (bytes[k + 1] as char).is_ascii_digit()
                    {
                        is_float = true;
                        k += 1; // skip '.'
                        while k < bytes.len() {
                            let ch = bytes[k] as char;
                            if ch.is_ascii_digit() {
                                k += 1;
                            } else if ch == '_' {
                                k += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    // Exponent part: 'e' or 'E' with optional sign, requires at least one digit
                    if k < bytes.len() && ((bytes[k] as char) == 'e' || (bytes[k] as char) == 'E') {
                        is_float = true;
                        k += 1; // consume 'e'/'E'
                        if k < bytes.len()
                            && ((bytes[k] as char) == '+' || (bytes[k] as char) == '-')
                        {
                            k += 1;
                        }
                        // must have at least one digit in exponent
                        if !(k < bytes.len() && (bytes[k] as char).is_ascii_digit()) {
                            return Err(Error::lex("invalid float literal exponent"));
                        }
                        while k < bytes.len() {
                            let ch = bytes[k] as char;
                            if ch.is_ascii_digit() || ch == '_' {
                                k += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    if is_float {
                        // parse as float from i..k, ignoring '_'
                        let mut s = String::new();
                        let mut t = i;
                        while t < k {
                            let ch = bytes[t] as char;
                            if ch != '_' {
                                s.push(ch);
                            }
                            t += 1;
                        }
                        let f: f64 = s.parse().map_err(|_| Error::lex("invalid float literal"))?;
                        push(&mut tokens, TokenKind::FloatNumber(f), pos, line, start_col);
                        col += k - i;
                        i = k;
                    } else {
                        // integer
                        push(&mut tokens, TokenKind::IntNumber(val), pos, line, start_col);
                        col += j - i;
                        i = j;
                    }
                }
            }
            c if is_ident_start(c) => {
                let start_col = col;
                let mut s = String::new();
                s.push(c);
                i += 1;
                col += 1;
                while i < bytes.len() {
                    let ch = bytes[i] as char;
                    if is_ident_continue(ch) {
                        s.push(ch);
                        i += 1;
                        col += 1;
                    } else {
                        break;
                    }
                }
                let kind = match s.as_str() {
                    "mut" => TokenKind::Mut,
                    "if" => TokenKind::If,
                    "elsif" => TokenKind::Elsif,
                    "else" => TokenKind::Else,
                    "end" => TokenKind::End,
                    "for" => TokenKind::For,
                    "in" => TokenKind::In,
                    "while" => TokenKind::While,
                    "do" => TokenKind::Do,
                    "loop" => TokenKind::Loop,
                    "break" => TokenKind::Break,
                    "continue" => TokenKind::Continue,
                    // types
                    "Int" => TokenKind::IntType,
                    "String" => TokenKind::StringType,
                    "Bool" => TokenKind::BoolType,
                    "Float" => TokenKind::FloatType,
                    "Double" => TokenKind::DoubleType,
                    "Char" => TokenKind::CharType,
                    "Int8" => TokenKind::Int8Type,
                    "Int16" => TokenKind::Int16Type,
                    "Int32" => TokenKind::Int32Type,
                    "Int64" => TokenKind::Int64Type,
                    "Int128" => TokenKind::Int128Type,
                    "BigInt" => TokenKind::BigIntType,
                    "UnInt" => TokenKind::UnIntType,
                    "UnInt8" => TokenKind::UnInt8Type,
                    "UnInt16" => TokenKind::UnInt16Type,
                    "UnInt32" => TokenKind::UnInt32Type,
                    "UnInt64" => TokenKind::UnInt64Type,
                    "UnInt128" => TokenKind::UnInt128Type,
                    "UnBigInt" => TokenKind::UnBigIntType,
                    "Iterator" => TokenKind::RangeType,
                    "Usize" => TokenKind::UsizeType,
                    "Isize" => TokenKind::IsizeType,
                    // logical operators (word form)
                    "and" => TokenKind::And,
                    "or" => TokenKind::Or,
                    "not" => TokenKind::Not,
                    // keyword operator
                    "as" => TokenKind::As,
                    // bool literals
                    "true" => TokenKind::BoolLiteral(true),
                    "false" => TokenKind::BoolLiteral(false),
                    _ => TokenKind::Ident(s),
                };
                push(&mut tokens, kind, pos, line, start_col);
            }
            _ => {
                return Err(Error::lex(format!("unexpected character '{}'", c)));
            }
        }
    }
    tokens.push(Token {
        kind: TokenKind::Eof,
        pos: i,
        line,
        col,
    });
    Ok(tokens)
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}
fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
