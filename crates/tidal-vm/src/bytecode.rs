//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use std::rc::Rc;
use std::fmt;

use tidal_ast::Type;

#[derive(Debug, Clone)]
pub enum Instr {
    // stack operations for literals and locals
    LoadInt(i64),
    LoadBool(bool),
    LoadConstStr(usize), // index into constant pool (strings)
    LoadLocal(usize),
    StoreLocal(usize),
    StoreConst(usize, i64), // write immediate Int into local without stack traffic
    // arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // comparisons
    CmpEq,
    CmpNe,
    CmpLt,
    CmpGt,
    CmpLe,
    CmpGe,
    // control flow
    Jump(usize),
    JumpIfFalse(usize),
    // casts (subset)
    CastTo(Type),
    // make an iterator from two ints on stack (left..right)
    Range,
    // variable declarations bound to local slot (shadowing allowed by reusing slot)
    LetLocal { slot: usize, mutable: bool }, // pop value, set local, and (once) insert into Env
    // for-loop: expects top of stack to be Iterator; sets up loop, executes body, repeats
    // body_len is number of instructions forming the loop body
    ForStart { slot: usize, mutable: bool, body_len: usize },
    ForEnd,
    // loop control
    Break,
    Continue,
    // expression statement boundary (to decide what to show)
    ExprYield,
    // pop value (cleanup)
    Pop,
    // Scopes
    ScopeEnter,
    ScopeExit,
    // Indexing
    IndexLoad(usize), // expects base value on stack top; loads base.idx
    IndexStore(usize), // expects rhs on stack, and base in a local slot indicated? Simplified: not used yet
    // Super-instructions
    IncLocal(usize), // increment local slot by 1 (no stack traffic)
}

#[derive(Debug, Clone)]
pub struct Bytecode {
    pub code: Vec<Instr>,
    pub consts: Vec<Rc<String>>,     // string pool
    pub names: Vec<Rc<String>>,      // name pool
    pub slot_name_idx: Vec<usize>,   // slot -> name index
    pub has_static_types: bool,       // parser type checker validated types; VM can skip dynamic type checks
}

impl Instr {
    fn fmt_op(&self) -> String {
        match self {
            Instr::LoadInt(n) => format!("LoadInt {}", n),
            Instr::LoadBool(b) => format!("LoadBool {}", b),
            Instr::LoadConstStr(i) => format!("LoadConstStr #{}", i),
            Instr::LoadLocal(s) => format!("LoadLocal ${}", s),
            Instr::StoreLocal(s) => format!("StoreLocal ${}", s),
            Instr::StoreConst(s, n) => format!("StoreConst ${}, {}", s, n),
            Instr::Add => "Add".into(),
            Instr::Sub => "Sub".into(),
            Instr::Mul => "Mul".into(),
            Instr::Div => "Div".into(),
            Instr::Rem => "Rem".into(),
            Instr::CmpEq => "CmpEq".into(),
            Instr::CmpNe => "CmpNe".into(),
            Instr::CmpLt => "CmpLt".into(),
            Instr::CmpGt => "CmpGt".into(),
            Instr::CmpLe => "CmpLe".into(),
            Instr::CmpGe => "CmpGe".into(),
            Instr::Jump(t) => format!("Jump {}", t),
            Instr::JumpIfFalse(t) => format!("JumpIfFalse {}", t),
            Instr::CastTo(ty) => format!("CastTo {:?}", ty),
            Instr::Range => "Range".into(),
            Instr::LetLocal { slot, mutable } => format!("LetLocal ${}{}", slot, if *mutable { " mut" } else { "" }),
            Instr::ForStart { slot, mutable, body_len } => format!("ForStart ${}{} body_len={} ", slot, if *mutable { " mut" } else { "" }, body_len),
            Instr::ForEnd => "ForEnd".into(),
            Instr::Break => "Break".into(),
            Instr::Continue => "Continue".into(),
            Instr::ExprYield => "ExprYield".into(),
            Instr::Pop => "Pop".into(),
            Instr::IndexLoad(i) => format!("IndexLoad {}", i),
            Instr::IndexStore(i) => format!("IndexStore {}", i),
            Instr::ScopeEnter => "ScopeEnter".into(),
            Instr::ScopeExit => "ScopeExit".into(),
            Instr::IncLocal(s) => format!("IncLocal ${}", s),
        }
    }
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ";; CONSTS ({}):", self.consts.len())?;
        for (i, s) in self.consts.iter().enumerate() {
            writeln!(f, "  [{}] \"{}\"", i, s)?;
        }
        writeln!(f, ";; NAMES ({}):", self.names.len())?;
        for (i, n) in self.names.iter().enumerate() {
            writeln!(f, "  [{}] {}", i, n)?;
        }
        writeln!(f, ";; SLOTS ({}):", self.slot_name_idx.len())?;
        for (i, idx) in self.slot_name_idx.iter().enumerate() {
            let nm = self.names.get(*idx).map(|s| s.as_str()).unwrap_or("<anon>");
            writeln!(f, "  ${}: {}(#{} )", i, nm, idx)?;
        }
        writeln!(f, ";; CODE ({}):", self.code.len())?;
        for (i, instr) in self.code.iter().enumerate() {
            writeln!(f, "  {:04}: {}", i, instr.fmt_op())?;
        }
        Ok(())
    }
}
