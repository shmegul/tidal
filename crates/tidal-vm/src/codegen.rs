//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

#![allow(clippy::collapsible_if, clippy::collapsible_match, clippy::ptr_arg, clippy::single_match, clippy::too_many_arguments, clippy::needless_return)]
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use tidal_ast::{Expr, Program, Stmt, Type};

use crate::bytecode::{Bytecode, Instr};

#[derive(Debug)]
pub enum CompileOutcome {
    Bytecode(Bytecode),
    Unsupported,
}

pub fn compile(program: &Program) -> CompileOutcome {
    // Pools and symbol table
    let mut consts: Vec<Rc<String>> = Vec::new();
    let mut names: Vec<Rc<String>> = Vec::new();
    let mut name_to_idx: HashMap<Rc<String>, usize> = HashMap::new();
    let mut slots: Vec<usize> = Vec::new();
    let mut slot_of: HashMap<Rc<String>, usize> = HashMap::new();
    let mut out: Vec<Instr> = Vec::new();

    // --- Helper functions ---
    fn ensure_name(
        nm: &str,
        names: &mut Vec<Rc<String>>,
        name_to_idx: &mut HashMap<Rc<String>, usize>,
    ) -> Rc<String> {
        let key = Rc::new(nm.to_string());
        if let Some(&idx) = name_to_idx.get(&key) {
            return names[idx].clone();
        }
        let idx = names.len();
        names.push(key.clone());
        name_to_idx.insert(key.clone(), idx);
        key
    }

    fn ensure_slot(
        nm: &str,
        slots: &mut Vec<usize>,
        slot_of: &mut HashMap<Rc<String>, usize>,
        names: &mut Vec<Rc<String>>,
        name_to_idx: &mut HashMap<Rc<String>, usize>,
    ) -> usize {
        for (rc, &s) in slot_of.iter() {
            if rc.as_str() == nm {
                return s;
            }
        }
        let name_rc = ensure_name(nm, names, name_to_idx);
        let s = slots.len();
        slots.push(name_to_idx[&name_rc]);
        slot_of.insert(name_rc, s);
        s
    }

    fn is_int_like_ty(t: &Type) -> bool {
        matches!(t,
            Type::Int | Type::Int8 | Type::Int16 | Type::Int32 | Type::Int64 | Type::Int128 | Type::BigInt |
            Type::UnInt | Type::UnInt8 | Type::UnInt16 | Type::UnInt32 | Type::UnInt64 | Type::UnInt128 | Type::UnBigInt |
            Type::Usize | Type::Isize)
    }

    fn cstring_to_string(cs: &std::ffi::CString) -> String {
        match cs.to_str() { Ok(st) => st.to_owned(), Err(_) => cs.to_string_lossy().into_owned() }
    }

    fn parse_i64_ignoring_underscores(s: &str) -> Option<i64> {
        let mut buf = String::with_capacity(s.len());
        for ch in s.chars() { if ch != '_' { buf.push(ch); } }
        buf.parse::<i64>().ok()
    }

    // --- Expression compilation ---
    fn compile_expr_inner(
        expr: &Expr,
        out: &mut Vec<Instr>,
        slots: &mut Vec<usize>,
        slot_of: &mut HashMap<Rc<String>, usize>,
        names: &mut Vec<Rc<String>>,
        name_to_idx: &mut HashMap<Rc<String>, usize>,
        consts: &mut Vec<Rc<String>>,
    ) -> Result<(), ()> {
        match expr {
            Expr::Int(n) => { out.push(Instr::LoadInt(*n as i64)); Ok(()) }
            Expr::String(cs) => {
                let s = cstring_to_string(cs);
                let idx = consts.iter().position(|rc| rc.as_str() == s)
                    .unwrap_or_else(|| { consts.push(Rc::new(s.clone())); consts.len()-1 });
                out.push(Instr::LoadConstStr(idx));
                Ok(())
            }
            Expr::Bool(b) => { out.push(Instr::LoadBool(*b)); Ok(()) }
            Expr::Float(f) => { out.push(Instr::LoadInt(*f as i64)); Ok(()) }
            Expr::Double(f) => { out.push(Instr::LoadInt(*f as i64)); Ok(()) }
            Expr::Ident(name) => {
                // Try to find an existing slot; if not found, conservatively create one.
                // This avoids Unsupported due to compile-time ordering or shadowing differences.
                let maybe_slot = slot_of.iter().find(|(k, _)| k.as_str() == name).map(|(_, &s)| s);
                let slot = match maybe_slot {
                    Some(s) => s,
                    None => {
                        // Create a slot for this name so VM has a place to load from; assumes runtime Env provides binding.
                        ensure_slot(name, slots, slot_of, names, name_to_idx)
                    }
                };
                out.push(Instr::LoadLocal(slot));
                Ok(())
            }
            Expr::Binary(l, op, r) => {
                use tidal_ast::ops::Op;
                // Constant folding for simple literals and self-equality
                match (&**l, &**r, op) {
                    (Expr::Int(a), Expr::Int(b), Op::Add) => { out.push(Instr::LoadInt((*a as i64) + (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Sub) => { out.push(Instr::LoadInt((*a as i64) - (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Mul) => { out.push(Instr::LoadInt((*a as i64) * (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Div) => {
                        if *b != 0 { out.push(Instr::LoadInt((*a as i64) / (*b as i64))); return Ok(()); }
                    }
                    (Expr::Int(a), Expr::Int(b), Op::Rem) => {
                        if *b != 0 { out.push(Instr::LoadInt((*a as i64) % (*b as i64))); return Ok(()); }
                    }
                    (Expr::Int(a), Expr::Int(b), Op::Eq) => { out.push(Instr::LoadBool((*a as i64) == (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Ne) => { out.push(Instr::LoadBool((*a as i64) != (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Lt) => { out.push(Instr::LoadBool((*a as i64) <  (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Gt) => { out.push(Instr::LoadBool((*a as i64) >  (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Le) => { out.push(Instr::LoadBool((*a as i64) <= (*b as i64))); return Ok(()); }
                    (Expr::Int(a), Expr::Int(b), Op::Ge) => { out.push(Instr::LoadBool((*a as i64) >= (*b as i64))); return Ok(()); }
                    (Expr::Bool(a), Expr::Bool(b), Op::Eq) => { out.push(Instr::LoadBool(*a == *b)); return Ok(()); }
                    (Expr::Bool(a), Expr::Bool(b), Op::Ne) => { out.push(Instr::LoadBool(*a != *b)); return Ok(()); }
                    (Expr::Ident(a), Expr::Ident(b), Op::Eq) => { out.push(Instr::LoadBool(a == b)); return Ok(()); }
                    (Expr::Ident(a), Expr::Ident(b), Op::Ne) => { out.push(Instr::LoadBool(a != b)); return Ok(()); }
                    _ => {}
                }
                // Algebraic identities to shrink code
                match op {
                    Op::Add => {
                        if matches!(&**r, Expr::Int(b) if *b == 0) {
                            compile_expr_inner(l, out, slots, slot_of, names, name_to_idx, consts)?; return Ok(());
                        }
                        if matches!(&**l, Expr::Int(a) if *a == 0) {
                            compile_expr_inner(r, out, slots, slot_of, names, name_to_idx, consts)?; return Ok(());
                        }
                    }
                    Op::Mul => {
                        if matches!(&**l, Expr::Int(a) if *a == 0) || matches!(&**r, Expr::Int(b) if *b == 0) {
                            out.push(Instr::LoadInt(0)); return Ok(());
                        }
                        if matches!(&**r, Expr::Int(b) if *b == 1) {
                            compile_expr_inner(l, out, slots, slot_of, names, name_to_idx, consts)?; return Ok(());
                        }
                        if matches!(&**l, Expr::Int(a) if *a == 1) {
                            compile_expr_inner(r, out, slots, slot_of, names, name_to_idx, consts)?; return Ok(());
                        }
                    }
                    Op::Div => {
                        if matches!(&**r, Expr::Int(b) if *b == 1) {
                            compile_expr_inner(l, out, slots, slot_of, names, name_to_idx, consts)?; return Ok(());
                        }
                    }
                    Op::Rem => {
                        if matches!(&**r, Expr::Int(b) if *b == 1) {
                            out.push(Instr::LoadInt(0)); return Ok(());
                        }
                    }
                    _ => {}
                }
                match op {
                    Op::And => {
                        // Short-circuit AND
                        compile_expr_inner(l, out, slots, slot_of, names, name_to_idx, consts)?;
                        let jf1_pos = out.len();
                        out.push(Instr::JumpIfFalse(usize::MAX));
                        compile_expr_inner(r, out, slots, slot_of, names, name_to_idx, consts)?;
                        let jf2_pos = out.len();
                        out.push(Instr::JumpIfFalse(usize::MAX));
                        out.push(Instr::LoadBool(true));
                        let jend_pos = out.len();
                        out.push(Instr::Jump(usize::MAX));
                        let false_pos = out.len();
                        out.push(Instr::LoadBool(false));
                        let end_pos = out.len();
                        if let Instr::JumpIfFalse(t) = &mut out[jf1_pos] { *t = false_pos; } else { return Err(()); }
                        if let Instr::JumpIfFalse(t) = &mut out[jf2_pos] { *t = false_pos; } else { return Err(()); }
                        if let Instr::Jump(t) = &mut out[jend_pos] { *t = end_pos; } else { return Err(()); }
                        Ok(())
                    }
                    Op::Or => {
                        // Short-circuit OR
                        compile_expr_inner(l, out, slots, slot_of, names, name_to_idx, consts)?;
                        let jf_left_pos = out.len();
                        out.push(Instr::JumpIfFalse(usize::MAX));
                        // left is true => result true
                        out.push(Instr::LoadBool(true));
                        let jend1_pos = out.len();
                        out.push(Instr::Jump(usize::MAX));
                        let eval_right_pos = out.len();
                        compile_expr_inner(r, out, slots, slot_of, names, name_to_idx, consts)?;
                        let jf_right_pos = out.len();
                        out.push(Instr::JumpIfFalse(usize::MAX));
                        out.push(Instr::LoadBool(true));
                        let jend2_pos = out.len();
                        out.push(Instr::Jump(usize::MAX));
                        let false_pos = out.len();
                        out.push(Instr::LoadBool(false));
                        let end_pos = out.len();
                        if let Instr::JumpIfFalse(t) = &mut out[jf_left_pos] { *t = eval_right_pos; } else { return Err(()); }
                        if let Instr::JumpIfFalse(t) = &mut out[jf_right_pos] { *t = false_pos; } else { return Err(()); }
                        if let Instr::Jump(t) = &mut out[jend1_pos] { *t = end_pos; } else { return Err(()); }
                        if let Instr::Jump(t) = &mut out[jend2_pos] { *t = end_pos; } else { return Err(()); }
                        Ok(())
                    }
                    _ => {
                        compile_expr_inner(l, out, slots, slot_of, names, name_to_idx, consts)?;
                        compile_expr_inner(r, out, slots, slot_of, names, name_to_idx, consts)?;
                        out.push(match op {
                            Op::Add => Instr::Add,
                            Op::Sub => Instr::Sub,
                            Op::Mul => Instr::Mul,
                            Op::Div => Instr::Div,
                            Op::Rem => Instr::Rem,
                            Op::Eq => Instr::CmpEq,
                            Op::Ne => Instr::CmpNe,
                            Op::Lt => Instr::CmpLt,
                            Op::Gt => Instr::CmpGt,
                            Op::Le => Instr::CmpLe,
                            Op::Ge => Instr::CmpGe,
                            _ => return Err(()),
                        });
                        Ok(())
                    }
                }
            }
            Expr::Cast(e, ty) => {
                if let Expr::String(cs) = &**e {
                    if is_int_like_ty(ty) {
                        let s = cstring_to_string(cs);
                        if let Some(n) = parse_i64_ignoring_underscores(&s) {
                            out.push(Instr::LoadInt(n));
                            return Ok(());
                        }
                    }
                }
                compile_expr_inner(e, out, slots, slot_of, names, name_to_idx, consts)?;
                out.push(Instr::CastTo(ty.clone()));
                Ok(())
            }
            Expr::Range(a, b) => {
                compile_expr_inner(a, out, slots, slot_of, names, name_to_idx, consts)?;
                compile_expr_inner(b, out, slots, slot_of, names, name_to_idx, consts)?;
                out.push(Instr::Range);
                Ok(())
            }
            Expr::If { branches, else_branch } => {
                // Generate if/elsif/else without extra scopes; minimal jumps.
                let base = out.len();
                let mut tmp: Vec<Instr> = Vec::new();
                // For backpatching
                struct BrPatch { jfalse_pos: usize, jend_pos: Option<usize> }
                let mut patches: Vec<BrPatch> = Vec::new();
                let mut branch_starts: Vec<usize> = Vec::new();
                // Compile branches
                for (idx, (cond, blk)) in branches.iter().enumerate() {
                    branch_starts.push(tmp.len());
                    compile_expr_inner(cond, &mut tmp, slots, slot_of, names, name_to_idx, consts)?;
                    let jfalse_pos = tmp.len();
                    tmp.push(Instr::JumpIfFalse(0)); // target to be patched to next branch/else
                    // then-block
                    for s in blk.iter() {
                        compile_stmt_inner(s, &mut tmp, slots, slot_of, names, name_to_idx, consts)?;
                    }
                    // If there is another branch or else, we need a jump over it.
                    let need_jump_to_end = idx + 1 < branches.len() || else_branch.is_some();
                    let jend_pos = if need_jump_to_end { let p = tmp.len(); tmp.push(Instr::Jump(0)); Some(p) } else { None };
                    patches.push(BrPatch { jfalse_pos, jend_pos });
                }
                // Else branch (optional)
                let else_start = tmp.len();
                if let Some(blk) = else_branch.as_ref() {
                    for s in blk.iter() {
                        compile_stmt_inner(s, &mut tmp, slots, slot_of, names, name_to_idx, consts)?;
                    }
                }
                let end_idx = tmp.len();
                // Backpatch JumpIfFalse to point to next branch (or else), and Jump to end if present
                for (i, patch) in patches.iter().enumerate() {
                    let next_start_rel = if i + 1 < branch_starts.len() { branch_starts[i + 1] } else { else_start };
                    if let Some(instr) = tmp.get_mut(patch.jfalse_pos) {
                        if let Instr::JumpIfFalse(t) = instr { *t = base + next_start_rel; }
                    }
                    if let Some(jend_pos) = patch.jend_pos {
                        if let Some(instr) = tmp.get_mut(jend_pos) {
                            if let Instr::Jump(t) = instr { *t = base + end_idx; }
                        }
                    }
                }
                out.extend(tmp);
                Ok(())
            }
            _ => Err(()),
        }
    }

    // --- Hoisting helpers ---
    // Recursively collect names that are assigned anywhere within the provided statements.
    fn collect_assigned_names(body: &Vec<Stmt>) -> HashSet<String> {
        fn visit_stmts(stmts: &[Stmt], set: &mut HashSet<String>) {
            for s in stmts {
                match s {
                    Stmt::Let { name, .. } => { set.insert(name.clone()); }
                    Stmt::AddAssign { name, .. }
                    | Stmt::SubAssign { name, .. }
                    | Stmt::MulAssign { name, .. }
                    | Stmt::DivAssign { name, .. }
                    | Stmt::RemAssign { name, .. } => { set.insert(name.clone()); }
                    Stmt::AssignIndex { name, .. } => { set.insert(name.clone()); }
                    Stmt::For { body, .. }
                    | Stmt::While { body, .. }
                    | Stmt::Loop { body, .. } => { visit_stmts(body, set); }
                    Stmt::ExprStmt(Expr::If { branches, else_branch }) => {
                        for (_cond, blk) in branches { visit_stmts(blk, set); }
                        if let Some(blk) = else_branch { visit_stmts(blk, set); }
                    }
                    _ => {}
                }
            }
        }
        let mut set = HashSet::new();
        visit_stmts(body, &mut set);
        set
    }
    // Variant that works on a slice of the body (to exclude hoisted prefix)
    fn collect_assigned_names_from(body: &[Stmt]) -> HashSet<String> {
        fn visit_stmts(stmts: &[Stmt], set: &mut HashSet<String>) {
            for s in stmts {
                match s {
                    Stmt::Let { name, .. } => { set.insert(name.clone()); }
                    Stmt::AddAssign { name, .. }
                    | Stmt::SubAssign { name, .. }
                    | Stmt::MulAssign { name, .. }
                    | Stmt::DivAssign { name, .. }
                    | Stmt::RemAssign { name, .. } => { set.insert(name.clone()); }
                    Stmt::AssignIndex { name, .. } => { set.insert(name.clone()); }
                    Stmt::For { body, .. }
                    | Stmt::While { body, .. }
                    | Stmt::Loop { body, .. } => { visit_stmts(body, set); }
                    Stmt::ExprStmt(Expr::If { branches, else_branch }) => {
                        for (_cond, blk) in branches { visit_stmts(blk, set); }
                        if let Some(blk) = else_branch { visit_stmts(blk, set); }
                    }
                    _ => {}
                }
            }
        }
        let mut set = HashSet::new();
        visit_stmts(body, &mut set);
        set
    }
    // Detect whether a given name is used as a loop variable in any nested for inside `body`
    fn name_is_loop_var_in_nested_for(body: &[Stmt], nm: &str) -> bool {
        for s in body {
            match s {
                Stmt::For { name, body: inner_body, .. } => {
                    if name == nm { return true; }
                    if name_is_loop_var_in_nested_for(inner_body, nm) { return true; }
                }
                _ => {}
            }
        }
        false
    }
    fn expr_is_invariant(expr: &Expr, loop_var: &str, assigned: &HashSet<String>) -> bool {
        match expr {
            Expr::Ident(nm) => nm != loop_var && !assigned.contains(nm),
            Expr::Int(_) | Expr::Bool(_) | Expr::String(_) | Expr::Float(_) | Expr::Double(_) | Expr::Char(_) => true,
            Expr::Binary(l, _op, r) => expr_is_invariant(l, loop_var, assigned) && expr_is_invariant(r, loop_var, assigned),
            Expr::BitNot(e) | Expr::Not(e) | Expr::Cast(e, _) => expr_is_invariant(e, loop_var, assigned),
            Expr::Array(items) | Expr::Tuple(items) => items.iter().all(|e| expr_is_invariant(e, loop_var, assigned)),
            Expr::Index(base, _) => expr_is_invariant(base, loop_var, assigned),
            Expr::Range(a, b) => expr_is_invariant(a, loop_var, assigned) && expr_is_invariant(b, loop_var, assigned),
            Expr::Block(_) | Expr::If { .. } => false,
        }
    }

    // --- Statement compilation ---
    fn compile_stmt_inner(
        stmt: &Stmt,
        out: &mut Vec<Instr>,
        slots: &mut Vec<usize>,
        slot_of: &mut HashMap<Rc<String>, usize>,
        names: &mut Vec<Rc<String>>,
        name_to_idx: &mut HashMap<Rc<String>, usize>,
        consts: &mut Vec<Rc<String>>,
    ) -> Result<(), ()> {
        compile_stmt_with_overrides(stmt, out, slots, slot_of, names, name_to_idx, consts, None)
    }

    fn compile_stmt_with_overrides(
        stmt: &Stmt,
        out: &mut Vec<Instr>,
        slots: &mut Vec<usize>,
        slot_of: &mut HashMap<Rc<String>, usize>,
        names: &mut Vec<Rc<String>>, 
        name_to_idx: &mut HashMap<Rc<String>, usize>,
        consts: &mut Vec<Rc<String>>, 
        store_overrides: Option<&HashSet<String>>,
    ) -> Result<(), ()> {
        // Helper: If-expression compilation that respects store_overrides for branch bodies
        fn compile_if_expr_with_overrides(
            expr: &Expr,
            out: &mut Vec<Instr>,
            slots: &mut Vec<usize>,
            slot_of: &mut HashMap<Rc<String>, usize>,
            names: &mut Vec<Rc<String>>, 
            name_to_idx: &mut HashMap<Rc<String>, usize>,
            consts: &mut Vec<Rc<String>>, 
            store_overrides: Option<&HashSet<String>>,
        ) -> Result<(), ()> {
            if let Expr::If { branches, else_branch } = expr {
                let base = out.len();
                let mut tmp: Vec<Instr> = Vec::new();
                struct BrPatch { jfalse_pos: usize, jend_pos: Option<usize> }
                let mut patches: Vec<BrPatch> = Vec::new();
                let mut branch_starts: Vec<usize> = Vec::new();
                for (idx, (cond, blk)) in branches.iter().enumerate() {
                    branch_starts.push(tmp.len());
                    compile_expr_inner(cond, &mut tmp, slots, slot_of, names, name_to_idx, consts)?;
                    let jfalse_pos = tmp.len();
                    tmp.push(Instr::JumpIfFalse(0));
                    for s in blk.iter() {
                        compile_stmt_with_overrides(s, &mut tmp, slots, slot_of, names, name_to_idx, consts, store_overrides)?;
                    }
                    let need_jump_to_end = idx + 1 < branches.len() || else_branch.is_some();
                    let jend_pos = if need_jump_to_end { let p = tmp.len(); tmp.push(Instr::Jump(0)); Some(p) } else { None };
                    patches.push(BrPatch { jfalse_pos, jend_pos });
                }
                let else_start = tmp.len();
                if let Some(blk) = else_branch.as_ref() {
                    for s in blk.iter() {
                        compile_stmt_with_overrides(s, &mut tmp, slots, slot_of, names, name_to_idx, consts, store_overrides)?;
                    }
                }
                let end_idx = tmp.len();
                for (i, patch) in patches.iter().enumerate() {
                    let next_start_rel = if i + 1 < branch_starts.len() { branch_starts[i + 1] } else { else_start };
                    if let Some(instr) = tmp.get_mut(patch.jfalse_pos) {
                        if let Instr::JumpIfFalse(t) = instr { *t = base + next_start_rel; }
                    }
                    if let Some(jend_pos) = patch.jend_pos {
                        if let Some(instr) = tmp.get_mut(jend_pos) {
                            if let Instr::Jump(t) = instr { *t = base + end_idx; }
                        }
                    }
                }
                out.extend(tmp);
                Ok(())
            } else {
                // Fallback to generic path
                compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)
            }
        }

        match stmt {
            Stmt::Let { mutable, name, ty: _, expr } => {
                // Decide if this Let should be treated as a mutation (due to hoisting overrides)
                let force_store = store_overrides.map(|ov| ov.contains(name)).unwrap_or(false);
                let slot = ensure_slot(name, slots, slot_of, names, name_to_idx);

                if force_store {
                    // In override mode, this is a reset/update of an existing slot, not a declaration.
                    // Super-instruction: detect name = name + 1 or 1 + name
                    if let Expr::Binary(l, op, r) = expr {
                        use tidal_ast::ops::Op;
                        if matches!(op, Op::Add) {
                            let is_inc = match (&**l, &**r) {
                                (Expr::Ident(id), Expr::Int(n)) if id == name && *n == 1 => true,
                                (Expr::Int(n), Expr::Ident(id)) if id == name && *n == 1 => true,
                                _ => false,
                            };
                            if is_inc {
                                out.push(Instr::IncLocal(slot));
                                return Ok(());
                            }
                        }
                    }
                    // Constant store fast path
                    if let Expr::Int(n) = expr {
                        out.push(Instr::StoreConst(slot, *n as i64));
                        return Ok(());
                    }
                    // General store path
                    compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                    out.push(Instr::StoreLocal(slot));
                    return Ok(());
                } else {
                    // Normal declaration: always use LetLocal, even if a slot with this name already exists (shadowing).
                    // Do not emit StoreLocal/StoreConst here.
                    compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                    out.push(Instr::LetLocal { slot, mutable: *mutable });
                    return Ok(());
                }
            }
            Stmt::AddAssign { name, expr } => {
                let slot = slot_of.iter().find(|(k, _)| k.as_str() == name).map(|(_, &s)| s).ok_or(())?;
                // Super-instruction: count += 1
                if let Expr::Int(n) = expr { if *n == 1 { out.push(Instr::IncLocal(slot)); return Ok(()); } }
                out.push(Instr::LoadLocal(slot));
                compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                out.push(Instr::Add);
                out.push(Instr::StoreLocal(slot));
                Ok(())
            }
            Stmt::SubAssign { name, expr } => {
                let slot = slot_of.iter().find(|(k, _)| k.as_str() == name).map(|(_, &s)| s).ok_or(())?;
                out.push(Instr::LoadLocal(slot));
                compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                out.push(Instr::Sub);
                out.push(Instr::StoreLocal(slot));
                Ok(())
            }
            Stmt::MulAssign { name, expr } => {
                let slot = slot_of.iter().find(|(k, _)| k.as_str() == name).map(|(_, &s)| s).ok_or(())?;
                out.push(Instr::LoadLocal(slot));
                compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                out.push(Instr::Mul);
                out.push(Instr::StoreLocal(slot));
                Ok(())
            }
            Stmt::DivAssign { name, expr } => {
                let slot = slot_of.iter().find(|(k, _)| k.as_str() == name).map(|(_, &s)| s).ok_or(())?;
                out.push(Instr::LoadLocal(slot));
                compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                out.push(Instr::Div);
                out.push(Instr::StoreLocal(slot));
                Ok(())
            }
            Stmt::RemAssign { name, expr } => {
                let slot = slot_of.iter().find(|(k, _)| k.as_str() == name).map(|(_, &s)| s).ok_or(())?;
                out.push(Instr::LoadLocal(slot));
                compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                out.push(Instr::Rem);
                out.push(Instr::StoreLocal(slot));
                Ok(())
            }
            Stmt::ExprStmt(expr) => {
                // Compile a bare expression used as a statement. For If, the expression yields no
                // stack value; do not emit Pop. For other expressions, emit Pop to clean up.
                match expr {
                    Expr::If { .. } => {
                        compile_if_expr_with_overrides(expr, out, slots, slot_of, names, name_to_idx, consts, store_overrides)?;
                        // no Pop
                    }
                    _ => {
                        compile_expr_inner(expr, out, slots, slot_of, names, name_to_idx, consts)?;
                        out.push(Instr::Pop);
                    }
                }
                Ok(())
            }
            Stmt::Break => { out.push(Instr::Break); Ok(()) }
            Stmt::Continue => { out.push(Instr::Continue); Ok(()) }
            Stmt::For { mutable, name, iter, body } => {
                if !matches!(iter, Expr::Range(_, _)) { return Err(()); }

                // Hoist leading invariant lets as declarations before the loop;
                // inside the loop, reassign via StoreLocal ONLY when necessary to preserve semantics.
                let mut hoisted_names: HashSet<String> = HashSet::new();
                let mut predeclare: Vec<(String, bool, &Expr)> = Vec::new(); // (name, mutable, expr)
                // Additionally, for immutable invariant lets, precompute their RHS once into a temp,
                // but keep the original Let inside the loop (assigned from the temp) to preserve immutability.
                let mut immut_value_temps: Vec<(String, String, &Expr)> = Vec::new(); // (orig_name, temp_name, expr)
                let mut prefix_len: usize = 0;
                // Track names hoisted earlier in this prefix to prevent hoisting dependent lets (e.g., r using i)
                let mut hoisted_prefix_names: HashSet<String> = HashSet::new();
                for stmt in body.iter() {
                    if let Stmt::Let { mutable: m, name: n, ty: _, expr: e } = stmt {
                        // Conservative: only hoist if RHS is invariant relative to loop var
                        let assigned = collect_assigned_names(body);
                        // Additionally, do not hoist if RHS references any name already hoisted in this prefix
                        fn expr_refs_any(expr: &Expr, names: &HashSet<String>) -> bool {
                            match expr {
                                Expr::Ident(s) => names.contains(s),
                                Expr::Binary(l, _op, r) => expr_refs_any(l, names) || expr_refs_any(r, names),
                                Expr::BitNot(e) | Expr::Not(e) | Expr::Cast(e, _) => expr_refs_any(e, names),
                                Expr::Array(items) | Expr::Tuple(items) => items.iter().any(|e| expr_refs_any(e, names)),
                                Expr::Index(base, _) => expr_refs_any(base, names),
                                Expr::Range(a, b) => expr_refs_any(a, names) || expr_refs_any(b, names),
                                Expr::Block(_) | Expr::If { .. } | Expr::Int(_) | Expr::Bool(_) | Expr::String(_) | Expr::Float(_) | Expr::Double(_) | Expr::Char(_) => false,
                            }
                        }
                        if expr_is_invariant(e, name.as_str(), &assigned) && !expr_refs_any(e, &hoisted_prefix_names) {
                            if *m {
                                // Mutable: real hoist of binding
                                hoisted_names.insert(n.clone());
                                predeclare.push((n.clone(), *m, e));
                                hoisted_prefix_names.insert(n.clone());
                            } else {
                                // Immutable: precompute value into a temp, keep per-iteration LetLocal
                                let temp_name = format!("__hval_{}", n);
                                immut_value_temps.push((n.clone(), temp_name, e));
                                hoisted_prefix_names.insert(n.clone());
                            }
                            prefix_len += 1;
                            continue;
                        }
                    }
                    break;
                }

                // Determine which hoisted names actually need per-iteration reset
                let rest = &body[prefix_len..];
                let assigned_in_rest = collect_assigned_names_from(rest);
                let mut reset_each_iter: HashSet<String> = HashSet::new();
                for (nm, _m, e) in predeclare.iter() {
                    let assigned_later = assigned_in_rest.contains(nm);
                    let is_loop_idx_in_inner = name_is_loop_var_in_nested_for(rest, nm);
                    let rhs_is_literal = matches!(e, Expr::Int(_) | Expr::Bool(_) | Expr::String(_) | Expr::Float(_) | Expr::Double(_) | Expr::Char(_));
                    // Reset if it can change during the loop (explicit assignment) or RHS is not a pure literal and may depend on external state
                    // Do NOT reset if it's an inner loop index or a loop-constant literal with no later assignments
                    let need_reset = is_loop_idx_in_inner || assigned_later || !rhs_is_literal;
                    if need_reset { reset_each_iter.insert(nm.clone()); }
                }

                // Emit pre-declarations once (for truly hoisted bindings)
                for (nm, m, e) in predeclare.iter() {
                    compile_expr_inner(e, out, slots, slot_of, names, name_to_idx, consts)?;
                    let slot = ensure_slot(nm, slots, slot_of, names, name_to_idx);
                    out.push(Instr::LetLocal { slot, mutable: *m });
                }
                // Emit precomputed immutable temps once
                for (_orig, temp, e) in immut_value_temps.iter() {
                    compile_expr_inner(e, out, slots, slot_of, names, name_to_idx, consts)?;
                    let temp_slot = ensure_slot(temp, slots, slot_of, names, name_to_idx);
                    out.push(Instr::LetLocal { slot: temp_slot, mutable: false });
                }

                // Compile iterator after any preheader work
                compile_expr_inner(iter, out, slots, slot_of, names, name_to_idx, consts)?;

                // Build loop body with overrides and peephole for remainder
                let mut body_code: Vec<Instr> = Vec::new();
                // Map for quick lookup of immutable temp by original name
                let mut immut_temp_lookup: HashMap<String, String> = HashMap::new();
                for (orig, temp, _e) in immut_value_temps.iter() { immut_temp_lookup.insert(orig.clone(), temp.clone()); }
                // Track temporary aliasing of names to slots inside the loop body
                let mut alias_saves: Vec<(Rc<String>, Option<usize>)> = Vec::new();
                // Patches for jumps generated while building body_code (relative to body_code start)
                let mut body_jump_patches: Vec<(usize, usize)> = Vec::new(); // (pos, target_rel)
                let mut i = 0usize;
                while i < body.len() {
                    // Skip hoisted declarations, but still assign their value each iteration via StoreLocal
                    if let Stmt::Let { mutable: _, name: n, ty: _, expr: e } = &body[i] {
                        if hoisted_names.contains(n) {
                            // Only reset per iteration if required for correctness
                            if reset_each_iter.contains(n) {
                                let slot = ensure_slot(n, slots, slot_of, names, name_to_idx);
                                if let Expr::Int(k) = e {
                                    body_code.push(Instr::StoreConst(slot, *k as i64));
                                } else {
                                    compile_expr_inner(e, &mut body_code, slots, slot_of, names, name_to_idx, consts)?;
                                    body_code.push(Instr::StoreLocal(slot));
                                }
                            }
                            i += 1;
                            continue;
                        }
                    }

                    // Peephole: let tmp = a % b; if tmp == 0 { then... }
                    if i + 1 < body.len() {
                        if let Stmt::Let { mutable: _, name: rem_name, ty: _, expr: rem_expr } = &body[i] {
                            if let Expr::Binary(l, op, r) = rem_expr {
                                if matches!(op, tidal_ast::ops::Op::Rem) {
                                    if let Stmt::ExprStmt(Expr::If { branches, else_branch }) = &body[i+1] {
                                        if else_branch.is_none() {
                                            if let Some((cond, then_blk)) = branches.first() {
                                                if let Expr::Binary(cl2, cop, cr2) = cond {
                                                    if matches!(cop, tidal_ast::ops::Op::Eq) {
                                                        if matches!(&**cl2, Expr::Ident(nm) if *nm == *rem_name) && matches!(&**cr2, Expr::Int(z) if (*z as i64) == 0) {
                                                            // Emit stack-based compare without local tmp
                                                            compile_expr_inner(l, &mut body_code, slots, slot_of, names, name_to_idx, consts)?;
                                                            compile_expr_inner(r, &mut body_code, slots, slot_of, names, name_to_idx, consts)?;
                                                            body_code.push(Instr::Rem);
                                                            body_code.push(Instr::LoadInt(0));
                                                            body_code.push(Instr::CmpEq);
                                                            let jfalse_pos = body_code.len();
                                                            body_code.push(Instr::JumpIfFalse(0));
                                                            // then block
                                                            for s in then_blk.iter() {
                                                                compile_stmt_with_overrides(s, &mut body_code, slots, slot_of, names, name_to_idx, consts, Some(&hoisted_names))?;
                                                            }
                                                            // record JumpIfFalse patch to end (relative to body start)
                                                            let end_rel = body_code.len();
                                                            body_jump_patches.push((jfalse_pos, end_rel));
                                                            i += 2;
                                                            continue;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Handle immutable hoisted-let using precomputed temp: alias name to temp slot for this loop body.
                    if let Stmt::Let { mutable: _m, name: n, ty: _, expr: _e } = &body[i] {
                        if let Some(temp_name) = immut_temp_lookup.get(n) {
                            let temp_slot = ensure_slot(temp_name, slots, slot_of, names, name_to_idx);
                            // Ensure the original name exists in the names table to maintain slot/name listings
                            let name_rc = ensure_name(n, names, name_to_idx);
                            let prev = slot_of.get(&name_rc).cloned();
                            alias_saves.push((name_rc.clone(), prev));
                            slot_of.insert(name_rc, temp_slot);
                            // Do NOT emit any instructions; we simply reuse the temp slot for all uses of `n`.
                            i += 1;
                            continue;
                        }
                    }
                    // Default compilation
                    compile_stmt_with_overrides(&body[i], &mut body_code, slots, slot_of, names, name_to_idx, consts, Some(&hoisted_names))?;
                    i += 1;
                }

                // Compute absolute base for loop body: immediately after ForStart that we'll emit next
                let slot = ensure_slot(name, slots, slot_of, names, name_to_idx);
                let body_len = body_code.len();
                let base_abs = out.len() + 1; // ForStart will be at out.len(), body starts at +1

                // Adjust any jumps inside body_code that were patched relative to body start (including peephole and nested ifs)
                for instr in body_code.iter_mut() {
                    match instr {
                        Instr::Jump(t) | Instr::JumpIfFalse(t) => { *t += base_abs; }
                        _ => {}
                    }
                }
                // Apply recorded JumpIfFalse patches from peephole (convert to absolute now)
                for (pos, target_rel) in body_jump_patches.into_iter() {
                    if let Some(instr) = body_code.get_mut(pos) {
                        match instr {
                            Instr::JumpIfFalse(t) => { *t = base_abs + target_rel; }
                            _ => {}
                        }
                    }
                }

                // Restore any aliasing we installed for immutable hoisted lets
                for (nm_rc, prev) in alias_saves.into_iter() {
                    match prev {
                        Some(s) => { slot_of.insert(nm_rc, s); }
                        None => { slot_of.retain(|k, _| !Rc::ptr_eq(k, &nm_rc)); }
                    }
                }

                // Emit loop without extra ScopeEnter/ScopeExit; ForStart/ForEnd bound the scope.
                out.push(Instr::ForStart { slot, mutable: *mutable, body_len });
                out.extend(body_code);
                out.push(Instr::ForEnd);
                Ok(())
            }
            _ => Err(()),
        }
    }

    // --- Compile program ---
    for stmt in &program.stmts {
        if compile_stmt_inner(stmt, &mut out, &mut slots, &mut slot_of, &mut names, &mut name_to_idx, &mut consts).is_err() {
            return CompileOutcome::Unsupported;
        }
    }

    let bc = Bytecode { code: out, consts, names, slot_name_idx: slots, has_static_types: true };
    CompileOutcome::Bytecode(bc)
}
