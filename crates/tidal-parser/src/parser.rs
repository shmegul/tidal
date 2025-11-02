//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

#![allow(
    clippy::while_let_loop,
    clippy::collapsible_if,
    clippy::collapsible_else_if,
    clippy::manual_repeat_n,
    clippy::redundant_closure
)]
use tidal_ast::{Expr, Op, Program, Stmt, Type};
use tidal_errors::{Error, Result};
use tidal_lexer::{Token, TokenKind, lex};

pub fn parse_program(input: &str) -> Result<Program> {
    let tokens = lex(input)?;
    let mut p = Parser { tokens, idx: 0 };
    let program = p.parse_program()?;
    crate::type_checker::typecheck_program(&program)?;
    Ok(program)
}

struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    fn cur(&self) -> &TokenKind {
        &self.tokens[self.idx].kind
    }
    fn cur_token(&self) -> &Token {
        &self.tokens[self.idx]
    }
    fn bump(&mut self) {
        if self.idx < self.tokens.len() - 1 {
            self.idx += 1;
        }
    }
    fn peek(&self) -> &TokenKind {
        if self.idx + 1 < self.tokens.len() {
            &self.tokens[self.idx + 1].kind
        } else {
            &self.tokens[self.idx].kind
        }
    }

    fn eat_newlines_and_semicolons(&mut self) {
        loop {
            match self.cur() {
                TokenKind::Newline | TokenKind::Semicolon => self.bump(),
                _ => break,
            }
        }
    }

    fn parse_program(&mut self) -> Result<Program> {
        let mut stmts = Vec::new();
        self.eat_newlines_and_semicolons();
        while !matches!(self.cur(), TokenKind::Eof) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            let mut had_sep = false;
            loop {
                match self.cur() {
                    TokenKind::Semicolon | TokenKind::Newline => {
                        had_sep = true;
                        self.bump();
                    }
                    _ => break,
                }
            }
            if !had_sep {
                if self.starts_statement_or_expr(self.cur()) {
                    return Err(Error::parse("Missing ';' or newline between statements"));
                }
            }
            self.eat_newlines_and_semicolons();
        }
        Ok(Program { stmts })
    }

    fn starts_statement_or_expr(&self, tk: &TokenKind) -> bool {
        matches!(
            tk,
            TokenKind::Ident(_)
                | TokenKind::Mut
                | TokenKind::If
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Loop
                | TokenKind::IntNumber(_)
                | TokenKind::FloatNumber(_)
                | TokenKind::StrLiteral(_)
                | TokenKind::BoolLiteral(_)
                | TokenKind::CharLiteral(_)
                | TokenKind::Tilde
                | TokenKind::Not
                | TokenKind::Minus
                | TokenKind::LBracket
                | TokenKind::LParen
                | TokenKind::Break
                | TokenKind::Continue
        )
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        // Forms:
        // - mut IDENT ( : Type )? (= expr)?  // if no expr but type given, default-init
        // - IDENT ( : Type )? (= expr)?      // declaration (shadowing)
        // - IDENT op= expr                   // compound assignment (+= -= *= /=)
        // - for name in expr [do]? <block> end
        // - while expr [do]? <block> end
        // - loop [do]? <block> end
        // - expr                             // expression statement
        match self.cur() {
            TokenKind::Mut => {
                self.bump();
                let name = self.expect_ident()?;
                let ty = if self.eat(TokenKind::Colon) {
                    Some(self.expect_type()?)
                } else {
                    None
                };
                if self.eat(TokenKind::Equal) {
                    let expr = self.parse_expr()?;
                    Ok(Stmt::Let {
                        mutable: true,
                        name,
                        ty,
                        expr,
                    })
                } else if let Some(t) = ty.clone() {
                    // default initialize based on type
                    let expr = Self::default_expr_for_type(&t);
                    Ok(Stmt::Let {
                        mutable: true,
                        name,
                        ty: Some(t),
                        expr,
                    })
                } else {
                    Err(self.unexpected("'=' or ': Type' for declaration"))
                }
            }
            TokenKind::For => {
                self.bump();
                // Optional 'mut' before loop variable name
                let mutable = if matches!(self.cur(), TokenKind::Mut) {
                    self.bump();
                    true
                } else {
                    false
                };
                let name = self.expect_ident()?;
                // expect 'in'
                if !self.eat(TokenKind::In) {
                    return Err(self.unexpected("'in'"));
                }
                let iter = self.parse_expr()?;
                // optional 'do' or require newline/semicolon handled by block parser
                if self.eat(TokenKind::Do) { /* consume do */
                } else {
                    self.eat_newlines_and_semicolons();
                }
                let body = self.parse_block_until(&[TokenKind::End])?;
                self.expect(TokenKind::End)?;
                Ok(Stmt::For {
                    mutable,
                    name,
                    iter,
                    body,
                })
            }
            TokenKind::While => {
                self.bump();
                let cond = self.parse_expr()?;
                if self.eat(TokenKind::Do) { /* consume do */
                } else {
                    self.eat_newlines_and_semicolons();
                }
                let body = self.parse_block_until(&[TokenKind::End])?;
                self.expect(TokenKind::End)?;
                Ok(Stmt::While { cond, body })
            }
            TokenKind::Loop => {
                self.bump();
                if self.eat(TokenKind::Do) { /* consume do */
                } else {
                    self.eat_newlines_and_semicolons();
                }
                let body = self.parse_block_until(&[TokenKind::End])?;
                self.expect(TokenKind::End)?;
                Ok(Stmt::Loop { body })
            }
            TokenKind::Break => {
                self.bump();
                Ok(Stmt::Break)
            }
            TokenKind::Continue => {
                self.bump();
                Ok(Stmt::Continue)
            }
            TokenKind::Ident(name) => {
                // lookahead for declaration/compound assignment directly after identifier
                if matches!(
                    self.peek(),
                    TokenKind::PlusEqual
                        | TokenKind::MinusEqual
                        | TokenKind::StarEqual
                        | TokenKind::SlashEqual
                        | TokenKind::PercentEqual
                        | TokenKind::Colon
                        | TokenKind::Equal
                ) {
                    let name = name.clone();
                    self.bump();
                    if self.eat(TokenKind::PlusEqual) {
                        let expr = self.parse_expr()?;
                        Ok(Stmt::AddAssign { name, expr })
                    } else if self.eat(TokenKind::MinusEqual) {
                        let expr = self.parse_expr()?;
                        Ok(Stmt::SubAssign { name, expr })
                    } else if self.eat(TokenKind::StarEqual) {
                        let expr = self.parse_expr()?;
                        Ok(Stmt::MulAssign { name, expr })
                    } else if self.eat(TokenKind::SlashEqual) {
                        let expr = self.parse_expr()?;
                        Ok(Stmt::DivAssign { name, expr })
                    } else if self.eat(TokenKind::PercentEqual) {
                        let expr = self.parse_expr()?;
                        Ok(Stmt::RemAssign { name, expr })
                    } else {
                        let ty = if self.eat(TokenKind::Colon) {
                            Some(self.expect_type()?)
                        } else {
                            None
                        };
                        if self.eat(TokenKind::Equal) {
                            let expr = self.parse_expr()?;
                            Ok(Stmt::Let {
                                mutable: false,
                                name,
                                ty,
                                expr,
                            })
                        } else if let Some(t) = ty.clone() {
                            let expr = Self::default_expr_for_type(&t);
                            Ok(Stmt::Let {
                                mutable: false,
                                name,
                                ty: Some(t),
                                expr,
                            })
                        } else {
                            Err(self.unexpected("'=' or ': Type' for declaration"))
                        }
                    }
                } else if matches!(self.peek(), TokenKind::Dot) {
                    // Potential element assignment like: name . <int> = expr
                    // Parse full left-hand side as an expression (will stop before '=')
                    let lhs = self.parse_expr()?;
                    if self.eat(TokenKind::Equal) {
                        let rhs = self.parse_expr()?;
                        match lhs {
                            Expr::Index(base, idx) => match *base {
                                Expr::Ident(nm) => Ok(Stmt::AssignIndex {
                                    name: nm,
                                    index: idx,
                                    expr: rhs,
                                }),
                                _ => Err(self.unexpected(
                                    "assignable left-hand side (variable array/tuple element)",
                                )),
                            },
                            _ => Err(self.unexpected("index assignment target")),
                        }
                    } else {
                        Ok(Stmt::ExprStmt(lhs))
                    }
                } else {
                    // expression statement starting with ident
                    let expr = self.parse_expr()?;
                    Ok(Stmt::ExprStmt(expr))
                }
            }
            _ => {
                // anything else, try as expression statement
                let expr = self.parse_expr()?;
                Ok(Stmt::ExprStmt(expr))
            }
        }
    }

    // Precedence (low -> high): or > and > == != > < >= <= > | > ^ > & > << >> > + - > * / > ** > unary (not ~) > primary
    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr> {
        let mut left = self.parse_and()?;
        loop {
            match self.cur() {
                TokenKind::Or => {
                    self.bump();
                    let right = self.parse_and()?;
                    left = Expr::Binary(Box::new(left), Op::Or, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr> {
        let mut left = self.parse_equality()?;
        loop {
            match self.cur() {
                TokenKind::And => {
                    self.bump();
                    let right = self.parse_equality()?;
                    left = Expr::Binary(Box::new(left), Op::And, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr> {
        let mut left = self.parse_comparison()?;
        loop {
            match self.cur() {
                TokenKind::EqEq => {
                    self.bump();
                    let right = self.parse_comparison()?;
                    left = Expr::Binary(Box::new(left), Op::Eq, Box::new(right));
                }
                TokenKind::NotEqual => {
                    self.bump();
                    let right = self.parse_comparison()?;
                    left = Expr::Binary(Box::new(left), Op::Ne, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr> {
        let mut left = self.parse_bit_or()?;
        loop {
            match self.cur() {
                TokenKind::Less => {
                    self.bump();
                    let right = self.parse_bit_or()?;
                    left = Expr::Binary(Box::new(left), Op::Lt, Box::new(right));
                }
                TokenKind::Greater => {
                    self.bump();
                    let right = self.parse_bit_or()?;
                    left = Expr::Binary(Box::new(left), Op::Gt, Box::new(right));
                }
                TokenKind::LessEqual => {
                    self.bump();
                    let right = self.parse_bit_or()?;
                    left = Expr::Binary(Box::new(left), Op::Le, Box::new(right));
                }
                TokenKind::GreaterEqual => {
                    self.bump();
                    let right = self.parse_bit_or()?;
                    left = Expr::Binary(Box::new(left), Op::Ge, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_bit_or(&mut self) -> Result<Expr> {
        let mut left = self.parse_bit_xor()?;
        loop {
            match self.cur() {
                TokenKind::Pipe => {
                    self.bump();
                    let right = self.parse_bit_xor()?;
                    left = Expr::Binary(Box::new(left), Op::BitOr, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_bit_xor(&mut self) -> Result<Expr> {
        let mut left = self.parse_bit_and()?;
        loop {
            match self.cur() {
                TokenKind::Caret => {
                    self.bump();
                    let right = self.parse_bit_and()?;
                    left = Expr::Binary(Box::new(left), Op::BitXor, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_bit_and(&mut self) -> Result<Expr> {
        let mut left = self.parse_shift()?;
        loop {
            match self.cur() {
                TokenKind::Ampersand => {
                    self.bump();
                    let right = self.parse_shift()?;
                    left = Expr::Binary(Box::new(left), Op::BitAnd, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_shift(&mut self) -> Result<Expr> {
        let mut left = self.parse_add()?;
        loop {
            match self.cur() {
                TokenKind::SSLeft => {
                    self.bump();
                    let right = self.parse_add()?;
                    left = Expr::Binary(Box::new(left), Op::Shl, Box::new(right));
                }
                TokenKind::SSRight => {
                    self.bump();
                    let right = self.parse_add()?;
                    left = Expr::Binary(Box::new(left), Op::Shr, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_add(&mut self) -> Result<Expr> {
        let mut left = self.parse_mul()?;
        loop {
            match self.cur() {
                TokenKind::Plus => {
                    self.bump();
                    let right = self.parse_mul()?;
                    left = Expr::Binary(Box::new(left), Op::Add, Box::new(right));
                }
                TokenKind::Minus => {
                    self.bump();
                    let right = self.parse_mul()?;
                    left = Expr::Binary(Box::new(left), Op::Sub, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_mul(&mut self) -> Result<Expr> {
        let mut left = self.parse_power()?;
        loop {
            match self.cur() {
                TokenKind::Star => {
                    self.bump();
                    let right = self.parse_power()?;
                    left = Expr::Binary(Box::new(left), Op::Mul, Box::new(right));
                }
                TokenKind::Slash => {
                    self.bump();
                    let right = self.parse_power()?;
                    left = Expr::Binary(Box::new(left), Op::Div, Box::new(right));
                }
                TokenKind::Percent => {
                    self.bump();
                    let right = self.parse_power()?;
                    left = Expr::Binary(Box::new(left), Op::Rem, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(left)
    }

    fn parse_power(&mut self) -> Result<Expr> {
        // right-associative: a ** b ** c == a ** (b ** c)
        let mut left = self.parse_postfix()?;
        while matches!(self.cur(), TokenKind::Power) {
            self.bump();
            let right = self.parse_power()?;
            left = Expr::Binary(Box::new(left), Op::Pow, Box::new(right));
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        match self.cur() {
            TokenKind::Tilde => {
                self.bump();
                let expr = self.parse_unary()?;
                Ok(Expr::BitNot(Box::new(expr)))
            }
            TokenKind::Not => {
                self.bump();
                let expr = self.parse_unary()?;
                Ok(Expr::Not(Box::new(expr)))
            }
            TokenKind::Minus => {
                // Unary minus: desugar to 0 - expr (keeps precedence with other unary ops)
                self.bump();
                let expr = self.parse_unary()?;
                let zero: std::ffi::c_int = 0 as std::ffi::c_int;
                Ok(Expr::Binary(
                    Box::new(Expr::Int(zero)),
                    Op::Sub,
                    Box::new(expr),
                ))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_cast(&mut self) -> Result<Expr> {
        // highest precedence: after unary, before power multiplication
        let mut expr = self.parse_unary()?;
        loop {
            if self.eat(TokenKind::As) {
                let ty = self.expect_type()?;
                expr = Expr::Cast(Box::new(expr), ty);
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut expr = self.parse_cast()?;
        // Range operator '..' (right-binding to next higher-precedence expr)
        if matches!(self.cur(), TokenKind::DoubleDot) {
            self.bump();
            let right = self.parse_cast()?;
            expr = Expr::Range(Box::new(expr), Box::new(right));
        }
        loop {
            match self.cur() {
                TokenKind::Dot => {
                    self.bump();
                    // expect an integer literal as index
                    match self.cur() {
                        TokenKind::IntNumber(n) => {
                            let idx = *n;
                            if idx < 0 {
                                return Err(self.unexpected("non-negative index"));
                            }
                            self.bump();
                            expr = Expr::Index(Box::new(expr), idx as usize);
                        }
                        _ => return Err(self.unexpected("integer index after '.'")),
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        match self.cur() {
            TokenKind::IntNumber(n) => {
                let n = *n;
                self.bump();
                // cast to C-compatible int
                let cn: std::ffi::c_int = n as std::ffi::c_int;
                Ok(Expr::Int(cn))
            }
            TokenKind::FloatNumber(f) => {
                let f = *f;
                self.bump();
                Ok(Expr::Double(f as std::ffi::c_double))
            }
            TokenKind::StrLiteral(s) => {
                let s = s.clone();
                self.bump();
                // Convert to CString; reject interior NULs
                match std::ffi::CString::new(s) {
                    Ok(cs) => Ok(Expr::String(cs)),
                    Err(_) => Err(self.unexpected("string literal cannot contain NUL (\0) byte")),
                }
            }
            TokenKind::BoolLiteral(b) => {
                let b = *b;
                self.bump();
                Ok(Expr::Bool(b))
            }
            TokenKind::CharLiteral(c) => {
                let c = *c;
                self.bump();
                let code = c as u32;
                if code > 0xFF {
                    return Err(self.unexpected("char literal must fit in 8-bit C char"));
                }
                let cc: std::ffi::c_char = (code as u8) as std::ffi::c_char;
                Ok(Expr::Char(cc))
            }
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.bump();
                Ok(Expr::Ident(name))
            }
            TokenKind::If => self.parse_if_expr(),
            TokenKind::LBracket => {
                // array literal: [expr, expr, ...]
                self.bump();
                let mut elems = Vec::new();
                if !matches!(self.cur(), TokenKind::RBracket) {
                    loop {
                        let e = self.parse_expr()?;
                        elems.push(e);
                        if self.eat(TokenKind::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(TokenKind::RBracket)?;
                Ok(Expr::Array(elems))
            }
            TokenKind::LParen => {
                // tuple literal: (expr, ...)
                self.bump();
                let mut elems = Vec::new();
                if !matches!(self.cur(), TokenKind::RParen) {
                    loop {
                        let e = self.parse_expr()?;
                        elems.push(e);
                        if self.eat(TokenKind::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Tuple(elems))
            }
            _ => Err(self.unexpected("expression")),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr> {
        // already at 'if'
        self.bump();
        let cond = self.parse_expr()?;
        // require newline or semicolon before block
        self.eat_newlines_and_semicolons();
        let mut branches: Vec<(Expr, Vec<Stmt>)> = Vec::new();
        // parse then-branch block
        let then_block =
            self.parse_block_until(&[TokenKind::Elsif, TokenKind::Else, TokenKind::End])?;
        branches.push((cond, then_block));
        let mut else_branch: Option<Vec<Stmt>> = None;
        loop {
            match self.cur() {
                TokenKind::Elsif => {
                    self.bump();
                    let c = self.parse_expr()?;
                    self.eat_newlines_and_semicolons();
                    let blk = self.parse_block_until(&[
                        TokenKind::Elsif,
                        TokenKind::Else,
                        TokenKind::End,
                    ])?;
                    branches.push((c, blk));
                }
                TokenKind::Else => {
                    self.bump();
                    self.eat_newlines_and_semicolons();
                    let blk = self.parse_block_until(&[TokenKind::End])?;
                    else_branch = Some(blk);
                }
                _ => break,
            }
        }
        // expect end
        self.expect(TokenKind::End)?;
        Ok(Expr::If {
            branches,
            else_branch,
        })
    }

    fn parse_block_until(&mut self, terminators: &[TokenKind]) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        self.eat_newlines_and_semicolons();
        loop {
            // stop if current token matches any terminator
            let mut is_term = false;
            for t in terminators {
                if std::mem::discriminant(self.cur()) == std::mem::discriminant(t) {
                    is_term = true;
                    break;
                }
            }
            if is_term {
                break;
            }
            if matches!(self.cur(), TokenKind::Eof) {
                return Err(self.unexpected("block end"));
            }
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.eat_newlines_and_semicolons();
        }
        Ok(stmts)
    }

    fn expect(&mut self, expected: TokenKind) -> Result<()> {
        if std::mem::discriminant(self.cur()) == std::mem::discriminant(&expected) {
            self.bump();
            Ok(())
        } else {
            Err(self.unexpected(&format!("'{:?}'", expected)))
        }
    }

    fn eat(&mut self, expected: TokenKind) -> bool {
        if std::mem::discriminant(self.cur()) == std::mem::discriminant(&expected) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect_ident(&mut self) -> Result<String> {
        match self.cur() {
            TokenKind::Ident(name) => {
                let n = name.clone();
                self.bump();
                Ok(n)
            }
            _ => Err(self.unexpected("identifier")),
        }
    }

    fn default_expr_for_type(ty: &Type) -> Expr {
        match ty {
            Type::Int => Expr::Int(0 as std::ffi::c_int),
            Type::Int8 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::Int8),
            Type::Int16 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::Int16),
            Type::Int32 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::Int32),
            Type::Int64 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::Int64),
            Type::Int128 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::Int128),
            Type::BigInt => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::BigInt),
            Type::UnInt => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::UnInt),
            Type::UnInt8 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::UnInt8),
            Type::UnInt16 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::UnInt16),
            Type::UnInt32 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::UnInt32),
            Type::UnInt64 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::UnInt64),
            Type::UnInt128 => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::UnInt128),
            Type::UnBigInt => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::UnBigInt),
            Type::Usize => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::Usize),
            Type::Isize => Expr::Cast(Box::new(Expr::Int(0 as std::ffi::c_int)), Type::Isize),
            Type::Float => Expr::Float(0.0 as std::ffi::c_float),
            Type::Double => Expr::Double(0.0 as std::ffi::c_double),
            Type::Bool => Expr::Bool(false),
            Type::String => Expr::String(std::ffi::CString::new("").unwrap()),
            Type::Char => Expr::Char(0u8 as std::ffi::c_char),
            Type::Range => Expr::Range(
                Box::new(Expr::Int(0 as std::ffi::c_int)),
                Box::new(Expr::Int(0 as std::ffi::c_int)),
            ),
            Type::Array(elem_ty, n) => {
                let elem = Self::default_expr_for_type(elem_ty);
                Expr::Array(std::iter::repeat(elem).take(*n).collect())
            }
            Type::Tuple(tys) => {
                let elems = tys.iter().map(|t| Self::default_expr_for_type(t)).collect();
                Expr::Tuple(elems)
            }
        }
    }

    fn expect_type(&mut self) -> Result<Type> {
        // structured types: [Type, Int] or (Type, ...)
        match self.cur() {
            TokenKind::LBracket => {
                self.bump();
                let elem_ty = self.expect_type()?;
                // accept comma between type and length per spec
                if !self.eat(TokenKind::Comma) {
                    return Err(self.unexpected(", between type and length in array type"));
                }
                let len = match self.cur() {
                    TokenKind::IntNumber(n) => {
                        let v = *n;
                        if v < 0 {
                            return Err(self.unexpected("non-negative length"));
                        }
                        self.bump();
                        v as usize
                    }
                    _ => return Err(self.unexpected("array length integer")),
                };
                self.expect(TokenKind::RBracket)?;
                Ok(Type::Array(Box::new(elem_ty), len))
            }
            TokenKind::LParen => {
                self.bump();
                let mut tys = Vec::new();
                if !matches!(self.cur(), TokenKind::RParen) {
                    loop {
                        let t = self.expect_type()?;
                        tys.push(t);
                        if self.eat(TokenKind::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(TokenKind::RParen)?;
                Ok(Type::Tuple(tys))
            }
            _ => {
                let ty = match self.cur() {
                    TokenKind::IntType => Type::Int,
                    TokenKind::StringType => Type::String,
                    TokenKind::BoolType => Type::Bool,
                    TokenKind::FloatType => Type::Float,
                    TokenKind::DoubleType => Type::Double,
                    TokenKind::CharType => Type::Char,
                    TokenKind::Int8Type => Type::Int8,
                    TokenKind::Int16Type => Type::Int16,
                    TokenKind::Int32Type => Type::Int32,
                    TokenKind::Int64Type => Type::Int64,
                    TokenKind::Int128Type => Type::Int128,
                    TokenKind::BigIntType => Type::BigInt,
                    TokenKind::UnIntType => Type::UnInt,
                    TokenKind::UnInt8Type => Type::UnInt8,
                    TokenKind::UnInt16Type => Type::UnInt16,
                    TokenKind::UnInt32Type => Type::UnInt32,
                    TokenKind::UnInt64Type => Type::UnInt64,
                    TokenKind::UnInt128Type => Type::UnInt128,
                    TokenKind::UnBigIntType => Type::UnBigInt,
                    TokenKind::RangeType => Type::Range,
                    TokenKind::UsizeType => Type::Usize,
                    TokenKind::IsizeType => Type::Isize,
                    _ => {
                        return Err(self.unexpected("type"));
                    }
                };
                self.bump();
                Ok(ty)
            }
        }
    }

    fn unexpected(&self, expected: &str) -> Error {
        let t = self.cur_token();
        Error::parse(format!(
            "Unexpected token {:?} at {}:{}, expected {}",
            t.kind, t.line, t.col, expected
        ))
    }
}
