//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

#![allow(
    dead_code,
    clippy::collapsible_if,
    clippy::collapsible_else_if,
    clippy::borrow_deref_ref,
    clippy::ptr_arg,
    clippy::get_first,
    clippy::only_used_in_recursion,
    clippy::unnecessary_cast,
    clippy::assign_op_pattern
)]
use crate::value::Value;
use std::collections::HashMap;
use tidal_ast::{Expr, Op, Program, Stmt, Type};
use tidal_errors::{Error, Result};

#[derive(Debug, Clone)]
pub struct Binding {
    pub mutable: bool,
    pub ty: Type,
    pub value: Value,
}

#[derive(Debug, Default)]
pub struct Env {
    // Single scope sufficient for REPL with shadowing by re-inserting
    map: HashMap<String, Binding>,
    // Whether to echo inner statement values while executing blocks/loops (REPL-only)
    echo_inner: bool,
    // Loop control state
    loop_depth: usize,
    break_now: bool,
    continue_now: bool,
    // Per-iteration cache for immutable bindings to avoid repeated name lookups in tight loops
    loop_cache: HashMap<String, Value>,
    loop_cache_active: bool,
    // Per-iteration cache for derived casts of immutable identifiers to avoid repeated parsing/conversion
    // Nested map: name -> (target type tag -> converted Value)
    cast_cache: HashMap<String, HashMap<u8, Value>>,
    cast_cache_active: bool,
}

impl Env {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            echo_inner: false,
            loop_depth: 0,
            break_now: false,
            continue_now: false,
            loop_cache: HashMap::new(),
            loop_cache_active: false,
            cast_cache: HashMap::new(),
            cast_cache_active: false,
        }
    }

    pub fn set_echo_inner(&mut self, on: bool) {
        self.echo_inner = on;
    }

    /// Should VM export locals to Env? We use REPL's echo_inner flag as a proxy: true in REPL, false in CLI.
    pub fn should_export_vm_locals(&self) -> bool {
        self.echo_inner
    }

    #[inline]
    fn loop_iter_begin(&mut self) {
        self.loop_cache.clear();
        self.cast_cache.clear();
        self.loop_cache_active = true;
        self.cast_cache_active = true;
    }

    #[inline]
    fn loop_iter_end(&mut self) {
        self.loop_cache_active = false;
        self.cast_cache_active = false;
    }

    #[inline]
    fn loop_cache_insert_if_active(&mut self, name: &str, binding: &Binding) {
        if self.loop_cache_active && !binding.mutable {
            self.loop_cache
                .insert(name.to_string(), binding.value.clone());
        }
    }

    /// Executes a program and returns the last produced value and whether it should be printed.
    pub fn exec_program(&mut self, program: Program) -> Result<Option<(Value, bool)>> {
        // 1) Try JIT first (if supported), then VM, then fall back to AST interpreter.
        match tidal_jit::compile(&program) {
            tidal_jit::JitOutcome::Native(_module) => {
                // JIT backend is scaffolded; for now, fall back to VM execution.
            }
            tidal_jit::JitOutcome::Unsupported => {}
        }
        // 2) Try VM for supported subset; otherwise AST evaluator.
        match crate::vm::compile(&program) {
            crate::vm::CompileOutcome::Bytecode(code) => crate::vm::run(self, &code),
            crate::vm::CompileOutcome::Unsupported => {
                panic!(
                    "VM Unsupported: expand VM coverage or avoid unsupported constructs. No AST fallback as requested."
                );
            }
        }
    }

    fn exec_stmt(&mut self, stmt: Stmt) -> Result<(Value, bool)> {
        match stmt {
            Stmt::Let {
                mutable,
                name,
                ty,
                expr,
            } => {
                let val = self.eval_expr(expr)?;
                // If an explicit type annotation is provided, enforce it.
                // Otherwise, infer type from the value to allow shadowing with different types.
                let final_ty = match ty {
                    Some(t) => {
                        // Assume static typing has validated this at compile time; skip runtime check.
                        t
                    }
                    None => self.infer_type(&val),
                };
                // Shadowing: always insert new binding
                let binding = Binding {
                    mutable,
                    ty: final_ty.clone(),
                    value: val.clone(),
                };
                // If inside a loop iteration, cache immutable lets for fast access in this iteration
                self.loop_cache_insert_if_active(&name, &binding);
                self.map.insert(name, binding);
                Ok((val, false)) // do not print let bindings in REPL
            }
            Stmt::AddAssign { name, expr } => {
                let v = self.apply_compound(name, expr, Op::Add)?;
                Ok((v, true))
            }
            Stmt::SubAssign { name, expr } => {
                let v = self.apply_compound(name, expr, Op::Sub)?;
                Ok((v, true))
            }
            Stmt::MulAssign { name, expr } => {
                let v = self.apply_compound(name, expr, Op::Mul)?;
                Ok((v, true))
            }
            Stmt::DivAssign { name, expr } => {
                let v = self.apply_compound(name, expr, Op::Div)?;
                Ok((v, true))
            }
            Stmt::RemAssign { name, expr } => {
                let v = self.apply_compound(name, expr, Op::Rem)?;
                Ok((v, true))
            }
            Stmt::ExprStmt(expr) => {
                // Special case: top-level if-expression without matching branch should be ignored (Rust-like)
                if let Expr::If {
                    branches,
                    else_branch,
                } = expr.clone()
                {
                    match self.eval_if_optional(branches, else_branch)? {
                        Some(v) => Ok((v, true)),
                        None => Ok((Value::Int(0), false)), // no output, no error
                    }
                } else {
                    let v = self.eval_expr(expr)?;
                    Ok((v, true))
                }
            }
            Stmt::Break => {
                if self.loop_depth == 0 {
                    return Err(Error::runtime(
                        "E1001: 'break' used outside of loop. Hint: place 'break' inside for/while/loop.",
                    ));
                }
                self.break_now = true;
                Ok((Value::Int(0), false))
            }
            Stmt::Continue => {
                if self.loop_depth == 0 {
                    return Err(Error::runtime(
                        "E1002: 'continue' used outside of loop. Hint: place 'continue' inside for/while/loop.",
                    ));
                }
                self.continue_now = true;
                Ok((Value::Int(0), false))
            }
            Stmt::AssignIndex { name, index, expr } => {
                let rhs = self.eval_expr(expr)?;
                let b = self
                    .map
                    .get_mut(&name)
                    .ok_or_else(|| Error::runtime(format!("Unknown variable '{}'", name)))?;
                // Only arrays and tuples are supported for index assignment
                // Enforce mutability on the binding to avoid tracking inner mut flags
                if !b.mutable {
                    return Err(Error::runtime("Cannot assign to immutable object"));
                }
                let res = match &mut b.value {
                    Value::Array(items) => {
                        if index >= items.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        // Assign without changing array shape
                        items[index] = rhs.clone();
                        (rhs, true)
                    }
                    Value::Tuple(items) => {
                        if index >= items.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        // Assign into tuple element position (tuple remains same arity)
                        items[index] = rhs.clone();
                        (rhs, true)
                    }
                    _ => {
                        return Err(Error::runtime(
                            "Index assignment is only supported for arrays and tuples",
                        ));
                    }
                };
                Ok(res)
            }
            Stmt::For {
                mutable,
                name,
                iter,
                body,
            } => {
                let iterable = self.eval_expr(iter)?;
                let mut last: Option<Value> = None;
                // Precompute simple optimized ops for the loop body
                let optimized_ops = self.detect_patterns(&body);
                self.loop_depth = self.loop_depth.saturating_add(1);
                match iterable {
                    Value::Array(items) => {
                        'outer_array: for it in items {
                            // begin per-iteration cache
                            self.loop_iter_begin();
                            let ty = self.infer_type(&it);
                            let binding = Binding {
                                mutable,
                                ty,
                                value: it.clone(),
                            };
                            self.loop_cache_insert_if_active(&name, &binding);
                            self.map.insert(name.clone(), binding);
                            let mut last_in_body: Option<Value> = None;
                            for op in &optimized_ops {
                                let (v, show) = self.exec_optimized_op(op)?;
                                if self.echo_inner && show {
                                    println!("{}", v);
                                }
                                last_in_body = Some(v);
                                if self.break_now {
                                    self.break_now = false; // break out of this loop entirely
                                    self.loop_iter_end();
                                    break 'outer_array;
                                }
                                if self.continue_now {
                                    self.continue_now = false;
                                    break;
                                }
                            }
                            if let Some(v) = last_in_body {
                                last = Some(v);
                            }
                            // end per-iteration cache
                            self.loop_iter_end();
                        }
                    }
                    Value::Range(start, end) => {
                        let step: i64 = if start <= end { 1 } else { -1 };
                        let mut i = start;
                        'outer_iter: loop {
                            if step > 0 {
                                if i > end {
                                    break;
                                }
                            } else {
                                if i < end {
                                    break;
                                }
                            }
                            // begin per-iteration cache
                            self.loop_iter_begin();
                            let it = Value::Int(i);
                            let ty = self.infer_type(&it);
                            let binding = Binding {
                                mutable,
                                ty,
                                value: it,
                            };
                            self.loop_cache_insert_if_active(&name, &binding);
                            self.map.insert(name.clone(), binding);
                            let mut last_in_body: Option<Value> = None;
                            for op in &optimized_ops {
                                let (v, show) = self.exec_optimized_op(op)?;
                                if self.echo_inner && show {
                                    println!("{}", v);
                                }
                                last_in_body = Some(v);
                                if self.break_now {
                                    self.break_now = false;
                                    self.loop_iter_end();
                                    break 'outer_iter;
                                }
                                if self.continue_now {
                                    self.continue_now = false;
                                    break;
                                }
                            }
                            if let Some(v) = last_in_body {
                                last = Some(v);
                            }
                            // end per-iteration cache
                            self.loop_iter_end();
                            i = i.saturating_add(step);
                        }
                    }
                    other => {
                        self.loop_depth = self.loop_depth.saturating_sub(1);
                        return Err(Error::runtime(format!(
                            "'for' can iterate over Array or Iterator, got {}",
                            other
                        )));
                    }
                }
                self.loop_depth = self.loop_depth.saturating_sub(1);
                Ok((last.unwrap_or(Value::Int(0)), false))
            }
            Stmt::While { cond, body } => {
                let mut last: Option<Value> = None;
                self.loop_depth = self.loop_depth.saturating_add(1);
                'outer_while: loop {
                    let cv = self.eval_expr(cond.clone())?;
                    match cv {
                        Value::Bool(true) => {
                            // begin per-iteration cache for while-body execution
                            self.loop_iter_begin();
                            let mut last_in_body: Option<Value> = None;
                            for s in &body {
                                let (v, show) = self.exec_stmt(s.clone())?;
                                if self.echo_inner && show {
                                    println!("{}", v);
                                }
                                last_in_body = Some(v);
                                if self.break_now {
                                    self.break_now = false;
                                    self.loop_iter_end();
                                    break 'outer_while;
                                }
                                if self.continue_now {
                                    self.continue_now = false;
                                    break;
                                }
                            }
                            if let Some(v) = last_in_body {
                                last = Some(v);
                            }
                            // end per-iteration cache
                            self.loop_iter_end();
                        }
                        Value::Bool(false) => break,
                        _ => {
                            self.loop_depth = self.loop_depth.saturating_sub(1);
                            return Err(Error::runtime("While condition must be Bool"));
                        }
                    }
                }
                self.loop_depth = self.loop_depth.saturating_sub(1);
                Ok((last.unwrap_or(Value::Int(0)), false))
            }
            Stmt::Loop { body } => {
                self.loop_depth = self.loop_depth.saturating_add(1);
                'outer_loop: loop {
                    self.loop_iter_begin();
                    let mut _last_in_body: Option<Value> = None;
                    for s in &body {
                        let (v, show) = self.exec_stmt(s.clone())?;
                        if self.echo_inner && show {
                            println!("{}", v);
                        }
                        _last_in_body = Some(v);
                        if self.break_now {
                            self.break_now = false;
                            self.loop_iter_end();
                            break 'outer_loop;
                        }
                        if self.continue_now {
                            self.continue_now = false;
                            break;
                        }
                    }
                    self.loop_iter_end();
                }
                self.loop_depth = self.loop_depth.saturating_sub(1);
                Ok((Value::Int(0), false))
            }
        }
    }

    fn apply_compound(&mut self, name: String, expr: Expr, op: Op) -> Result<Value> {
        let rhs = self.eval_expr(expr)?;
        let b = self
            .map
            .get_mut(&name)
            .ok_or_else(|| Error::runtime(format!("Unknown variable '{}'", name)))?;
        if !b.mutable {
            return Err(Error::runtime(format!(
                "Cannot assign to immutable variable '{}'",
                name
            )));
        }
        match (&mut b.value, rhs) {
            (Value::Int(l), Value::Int(r)) => {
                match op {
                    Op::Add => {
                        *l = l.saturating_add(r);
                    }
                    Op::Sub => {
                        *l = l.saturating_sub(r);
                    }
                    Op::Mul => {
                        *l = l.saturating_mul(r);
                    }
                    Op::Div => {
                        if r == 0 {
                            return Err(Error::runtime("Division by zero"));
                        }
                        *l = *l / r;
                    }
                    Op::Rem => {
                        if r == 0 {
                            return Err(Error::runtime("Division by zero"));
                        }
                        *l = *l % r;
                    }
                    _ => {}
                }
                Ok(Value::Int(*l))
            }
            _ => Err(Error::runtime(
                "Unsupported compound assignment for given types",
            )),
        }
    }

    fn eval_block(&mut self, stmts: Vec<Stmt>) -> Result<Value> {
        let mut last: Option<Value> = None;
        for s in stmts {
            let (v, _show) = self.exec_stmt(s)?;
            last = Some(v);
        }
        last.ok_or_else(|| Error::runtime("Empty block has no value"))
    }

    fn eval_expr(&mut self, expr: Expr) -> Result<Value> {
        // Thin wrapper to reuse borrowing evaluator to minimize cloning
        self.eval_expr_ref(&expr)
    }

    fn eval_expr_ref(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Int(n) => Ok(Value::Int((*n) as i64)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::String(s) => match s.to_str() {
                Ok(st) => Ok(Value::String(std::rc::Rc::new(st.to_owned()))),
                Err(_) => Ok(Value::String(std::rc::Rc::new(
                    s.to_string_lossy().into_owned(),
                ))),
            },
            Expr::Float(x) => Ok(Value::Float(*x as f32)),
            Expr::Double(x) => Ok(Value::Double(*x as f64)),
            Expr::Char(c) => {
                let byte = (*c as i32 & 0xFF) as u8;
                let ch = std::char::from_u32(byte as u32).unwrap_or('\u{0}');
                Ok(Value::Char(ch))
            }
            Expr::Ident(name) => {
                // Fast path: if inside loop and immutable value cached, use it
                if self.loop_cache_active {
                    if let Some(v) = self.loop_cache.get(name) {
                        return Ok(v.clone());
                    }
                }
                // Fallback to global map lookup
                let b = self
                    .map
                    .get(name)
                    .ok_or_else(|| Error::runtime(format!("Unknown variable '{}'", name)))?;
                // If inside loop and binding is immutable, cache it for subsequent hits this iteration
                if self.loop_cache_active && !b.mutable {
                    self.loop_cache.insert(name.clone(), b.value.clone());
                }
                Ok(b.value.clone())
            }
            Expr::Range(a, b) => {
                let va = self.eval_expr_ref(&*a)?;
                let vb = self.eval_expr_ref(&*b)?;
                match (va, vb) {
                    (Value::Int(sa), Value::Int(eb)) => Ok(Value::Range(sa, eb)),
                    _ => Err(Error::runtime("Range '..' bounds must be Int")),
                }
            }
            Expr::Array(items) => {
                let mut vals = Vec::with_capacity(items.len());
                for e in items {
                    vals.push(self.eval_expr_ref(e)?);
                }
                // Arrays are homogeneous by static typing; skip dynamic homogeneity checks for performance.
                Ok(Value::Array(vals))
            }
            Expr::Tuple(items) => {
                let mut vals = Vec::with_capacity(items.len());
                for e in items {
                    vals.push(self.eval_expr_ref(e)?);
                }
                Ok(Value::Tuple(vals))
            }
            Expr::Index(base, idx) => {
                let v = self.eval_expr_ref(&*base)?;
                match v {
                    Value::Array(arr) => {
                        if *idx >= arr.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        Ok(arr[*idx].clone())
                    }
                    Value::Tuple(tup) => {
                        if *idx >= tup.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        Ok(tup[*idx].clone())
                    }
                    _ => Err(Error::runtime(
                        "Indexing is supported only for arrays and tuples",
                    )),
                }
            }
            Expr::Binary(l, op, r) => {
                // Handle logical short-circuit for 'and'/'or'
                if *op == Op::And {
                    let lv = self.eval_expr_ref(&*l)?;
                    return match lv {
                        Value::Bool(false) => Ok(Value::Bool(false)),
                        Value::Bool(true) => {
                            let rv = self.eval_expr_ref(&*r)?;
                            match rv {
                                Value::Bool(b) => Ok(Value::Bool(b)),
                                _ => Err(Error::runtime("Logical 'and' expects Bool operands")),
                            }
                        }
                        _ => Err(Error::runtime("Logical 'and' expects Bool operands")),
                    };
                } else if *op == Op::Or {
                    let lv = self.eval_expr_ref(&*l)?;
                    return match lv {
                        Value::Bool(true) => Ok(Value::Bool(true)),
                        Value::Bool(false) => {
                            let rv = self.eval_expr_ref(&*r)?;
                            match rv {
                                Value::Bool(b) => Ok(Value::Bool(b)),
                                _ => Err(Error::runtime("Logical 'or' expects Bool operands")),
                            }
                        }
                        _ => Err(Error::runtime("Logical 'or' expects Bool operands")),
                    };
                }
                let lv = self.eval_expr_ref(&*l)?;
                let rv = self.eval_expr_ref(&*r)?;
                match (lv, rv, op.clone()) {
                    (Value::Int(a), Value::Int(b), Op::Add) => Ok(Value::Int(a.saturating_add(b))),
                    (Value::Int(a), Value::Int(b), Op::Sub) => Ok(Value::Int(a.saturating_sub(b))),
                    (Value::Int(a), Value::Int(b), Op::Mul) => Ok(Value::Int(a.saturating_mul(b))),
                    (Value::Int(a), Value::Int(b), Op::Div) => {
                        if b == 0 {
                            Err(Error::runtime("Division by zero"))
                        } else {
                            Ok(Value::Int(a / b))
                        }
                    }
                    (Value::Int(a), Value::Int(b), Op::Rem) => {
                        if b == 0 {
                            Err(Error::runtime("Division by zero"))
                        } else {
                            Ok(Value::Int(a % b))
                        }
                    }
                    (Value::Int(a), Value::Int(b), Op::Pow) => {
                        if b < 0 {
                            return Err(Error::runtime("Negative exponent not supported for Int"));
                        }
                        let mut res: i64 = 1;
                        let mut base = a;
                        let mut exp = b as u64;
                        // fast pow by squaring with saturating mul
                        while exp > 0 {
                            if (exp & 1) == 1 {
                                res = res.saturating_mul(base);
                            }
                            exp >>= 1;
                            if exp > 0 {
                                base = base.saturating_mul(base);
                            }
                        }
                        Ok(Value::Int(res))
                    }
                    (Value::Int(a), Value::Int(b), Op::Shl) => {
                        let s = if b < 0 {
                            return Err(Error::runtime("Negative shift"));
                        } else {
                            (b as u64).min(63) as u32
                        };
                        Ok(Value::Int(a.wrapping_shl(s)))
                    }
                    (Value::Int(a), Value::Int(b), Op::Shr) => {
                        let s = if b < 0 {
                            return Err(Error::runtime("Negative shift"));
                        } else {
                            (b as u64).min(63) as u32
                        };
                        Ok(Value::Int(a >> s))
                    }
                    (Value::Int(a), Value::Int(b), Op::BitAnd) => Ok(Value::Int(a & b)),
                    (Value::Int(a), Value::Int(b), Op::BitOr) => Ok(Value::Int(a | b)),
                    (Value::Int(a), Value::Int(b), Op::BitXor) => Ok(Value::Int(a ^ b)),
                    (Value::Int(a), Value::Int(b), Op::Eq) => Ok(Value::Bool(a == b)),
                    (Value::Int(a), Value::Int(b), Op::Ne) => Ok(Value::Bool(a != b)),
                    (Value::Int(a), Value::Int(b), Op::Lt) => Ok(Value::Bool(a < b)),
                    (Value::Int(a), Value::Int(b), Op::Gt) => Ok(Value::Bool(a > b)),
                    (Value::Int(a), Value::Int(b), Op::Le) => Ok(Value::Bool(a <= b)),
                    (Value::Int(a), Value::Int(b), Op::Ge) => Ok(Value::Bool(a >= b)),

                    (Value::Bool(a), Value::Bool(b), Op::Eq) => Ok(Value::Bool(a == b)),
                    (Value::Bool(a), Value::Bool(b), Op::Ne) => Ok(Value::Bool(a != b)),

                    (Value::String(a), Value::String(b), Op::Eq) => Ok(Value::Bool(a == b)),
                    (Value::String(a), Value::String(b), Op::Ne) => Ok(Value::Bool(a != b)),

                    // Float arithmetic and comparisons
                    (Value::Float(a), Value::Float(b), Op::Add) => Ok(Value::Float(a + b)),
                    (Value::Float(a), Value::Float(b), Op::Sub) => Ok(Value::Float(a - b)),
                    (Value::Float(a), Value::Float(b), Op::Mul) => Ok(Value::Float(a * b)),
                    (Value::Float(a), Value::Float(b), Op::Div) => Ok(Value::Float(a / b)),
                    (Value::Float(a), Value::Float(b), Op::Rem) => {
                        if b == 0.0 {
                            Err(Error::runtime("Division by zero"))
                        } else {
                            Ok(Value::Float(a % b))
                        }
                    }
                    (Value::Float(a), Value::Float(b), Op::Eq) => Ok(Value::Bool(a == b)),
                    (Value::Float(a), Value::Float(b), Op::Ne) => Ok(Value::Bool(a != b)),
                    (Value::Float(a), Value::Float(b), Op::Lt) => Ok(Value::Bool(a < b)),
                    (Value::Float(a), Value::Float(b), Op::Gt) => Ok(Value::Bool(a > b)),
                    (Value::Float(a), Value::Float(b), Op::Le) => Ok(Value::Bool(a <= b)),
                    (Value::Float(a), Value::Float(b), Op::Ge) => Ok(Value::Bool(a >= b)),

                    // Double arithmetic and comparisons
                    (Value::Double(a), Value::Double(b), Op::Add) => Ok(Value::Double(a + b)),
                    (Value::Double(a), Value::Double(b), Op::Sub) => Ok(Value::Double(a - b)),
                    (Value::Double(a), Value::Double(b), Op::Mul) => Ok(Value::Double(a * b)),
                    (Value::Double(a), Value::Double(b), Op::Div) => Ok(Value::Double(a / b)),
                    (Value::Double(a), Value::Double(b), Op::Rem) => {
                        if b == 0.0 {
                            Err(Error::runtime("Division by zero"))
                        } else {
                            Ok(Value::Double(a % b))
                        }
                    }
                    (Value::Double(a), Value::Double(b), Op::Eq) => Ok(Value::Bool(a == b)),
                    (Value::Double(a), Value::Double(b), Op::Ne) => Ok(Value::Bool(a != b)),
                    (Value::Double(a), Value::Double(b), Op::Lt) => Ok(Value::Bool(a < b)),
                    (Value::Double(a), Value::Double(b), Op::Gt) => Ok(Value::Bool(a > b)),
                    (Value::Double(a), Value::Double(b), Op::Le) => Ok(Value::Bool(a <= b)),
                    (Value::Double(a), Value::Double(b), Op::Ge) => Ok(Value::Bool(a >= b)),

                    // Char equality
                    (Value::Char(a), Value::Char(b), Op::Eq) => Ok(Value::Bool(a == b)),
                    (Value::Char(a), Value::Char(b), Op::Ne) => Ok(Value::Bool(a != b)),

                    _ => Err(Error::runtime(
                        "Unsupported binary operation for given types",
                    )),
                }
            }
            Expr::BitNot(e) => {
                let v = self.eval_expr_ref(&*e)?;
                match v {
                    Value::Int(a) => Ok(Value::Int(!a)),
                    _ => Err(Error::runtime("Bitwise not '~' is only supported for Int")),
                }
            }
            Expr::Not(e) => {
                let v = self.eval_expr_ref(&*e)?;
                match v {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    _ => Err(Error::runtime("Logical 'not' expects Bool operand")),
                }
            }
            Expr::Block(stmts) => self.eval_block_ref(stmts),
            Expr::If {
                branches,
                else_branch,
            } => {
                // Fast paths on first branch
                if let Some((cond0, blk0)) = branches.get(0) {
                    match cond0 {
                        Expr::Bool(true) => {
                            return self.eval_block_ref(blk0);
                        }
                        Expr::Bool(false) => { /* skip */ }
                        Expr::Binary(l, Op::Eq, r) => {
                            if let (Expr::Ident(a), Expr::Ident(b)) = (&**l, &**r) {
                                if a == b {
                                    return self.eval_block_ref(blk0);
                                }
                            }
                            if let (Expr::Int(a), Expr::Int(b)) = (&**l, &**r) {
                                if a == b {
                                    return self.eval_block_ref(blk0);
                                }
                            }
                        }
                        _ => {}
                    }
                }
                // Default evaluation over borrowed branches
                for (cond, blk) in branches {
                    let cv = self.eval_expr_ref(cond)?;
                    match cv {
                        Value::Bool(true) => {
                            return self.eval_block_ref(blk);
                        }
                        Value::Bool(false) => {}
                        _ => return Err(Error::runtime("If condition must be Bool")),
                    }
                }
                if let Some(blk) = else_branch {
                    self.eval_block_ref(blk)
                } else {
                    Err(Error::runtime("No matching if branch and no else"))
                }
            }
            Expr::Cast(e, ty) => {
                // Fast path: cache casts of immutable identifiers during loops
                if let Expr::Ident(name) = &**e {
                    if self.cast_cache_active {
                        if let Some(b) = self.map.get(name) {
                            if !b.mutable {
                                if let Some(tag) = Self::ty_tag(ty) {
                                    if let Some(inner) = self.cast_cache.get(name) {
                                        if let Some(v) = inner.get(&tag) {
                                            return Ok(v.clone());
                                        }
                                    }
                                    let src_val = b.value.clone();
                                    let converted = self.convert_value_to(src_val, ty)?;
                                    self.cast_cache
                                        .entry(name.clone())
                                        .or_default()
                                        .insert(tag, converted.clone());
                                    return Ok(converted);
                                }
                            }
                        }
                    }
                }
                // Fallback: evaluate expression and convert
                let v = self.eval_expr_ref(&*e)?;
                let converted = self.convert_value_to(v, ty)?;
                Ok(converted)
            }
        }
    }

    // Simple runtime tag for Value variants used to enforce homogenous arrays
    fn value_tag(v: &Value) -> u8 {
        match v {
            Value::Int(_) => 0,
            Value::Bool(_) => 1,
            Value::String(_) => 2,
            Value::Float(_) => 3,
            Value::Double(_) => 4,
            Value::Array(_) => 5,
            Value::Tuple(_) => 6,
            Value::Char(_) => 7,
            Value::Range(_, _) => 8,
        }
    }

    #[inline]
    fn ty_tag(t: &Type) -> Option<u8> {
        match t {
            Type::Int
            | Type::Int8
            | Type::Int16
            | Type::Int32
            | Type::Int64
            | Type::Int128
            | Type::BigInt
            | Type::UnInt
            | Type::UnInt8
            | Type::UnInt16
            | Type::UnInt32
            | Type::UnInt64
            | Type::UnInt128
            | Type::UnBigInt
            | Type::Usize
            | Type::Isize => Some(0),
            Type::Float => Some(3),
            Type::Double => Some(4),
            Type::Bool => Some(1),
            Type::String => Some(2),
            Type::Char => Some(7),
            _ => None,
        }
    }

    fn check_type(&self, ty: &Type, val: &Value) -> Result<()> {
        match (ty, val) {
            // Group all integer-like annotations to accept runtime Int
            (Type::Int, Value::Int(_))
            | (Type::Int8, Value::Int(_))
            | (Type::Int16, Value::Int(_))
            | (Type::Int32, Value::Int(_))
            | (Type::Int64, Value::Int(_))
            | (Type::Int128, Value::Int(_))
            | (Type::BigInt, Value::Int(_))
            | (Type::UnInt, Value::Int(_))
            | (Type::UnInt8, Value::Int(_))
            | (Type::UnInt16, Value::Int(_))
            | (Type::UnInt32, Value::Int(_))
            | (Type::UnInt64, Value::Int(_))
            | (Type::UnInt128, Value::Int(_))
            | (Type::UnBigInt, Value::Int(_))
            | (Type::Usize, Value::Int(_))
            | (Type::Isize, Value::Int(_)) => Ok(()),
            (Type::Bool, Value::Bool(_)) => Ok(()),
            (Type::String, Value::String(_)) => Ok(()),
            (Type::Float, Value::Float(_)) => Ok(()),
            (Type::Float, Value::Double(_)) => Ok(()), // accept Double for Float annotation
            (Type::Double, Value::Double(_)) => Ok(()),
            (Type::Double, Value::Float(_)) => Ok(()), // accept Float for Double annotation
            (Type::Char, Value::Char(_)) => Ok(()),
            (Type::Array(elem_ty, n), Value::Array(items)) => {
                if *n != items.len() {
                    return Err(Error::runtime("Array length mismatch"));
                }
                for it in items {
                    self.check_type(elem_ty, it)?;
                }
                Ok(())
            }
            (Type::Tuple(tys), Value::Tuple(items)) => {
                if tys.len() != items.len() {
                    return Err(Error::runtime("Tuple arity mismatch"));
                }
                for (t, v) in tys.iter().zip(items.iter()) {
                    self.check_type(t, v)?;
                }
                Ok(())
            }
            (Type::Range, Value::Range(_, _)) => Ok(()),
            // Fallback: if annotation provided but value not matching, error
            _ => Err(Error::runtime("Type mismatch")),
        }
    }
}

impl Env {
    pub fn infer_type_public(&self, val: &Value) -> Type {
        self.infer_type(val)
    }
    fn infer_type(&self, val: &Value) -> Type {
        match val {
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
            Value::String(_) => Type::String,
            Value::Float(_) => Type::Float,
            Value::Double(_) => Type::Double,
            Value::Char(_) => Type::Char,
            Value::Range(_, _) => Type::Range,
            Value::Array(items) => {
                let len = items.len();
                // Arrays are homogeneous; element type inferred from first element, default Int for empty
                let elem_ty = if let Some(first) = items.first() {
                    self.infer_type(first)
                } else {
                    Type::Int
                };
                Type::Array(Box::new(elem_ty), len)
            }
            Value::Tuple(items) => {
                let tys = items.iter().map(|v| self.infer_type(v)).collect();
                Type::Tuple(tys)
            }
        }
    }
}

impl Env {
    #[inline(always)]
    fn fast_parse_i64(s: &str) -> Option<i64> {
        if s.is_empty() {
            return None;
        }
        let bytes = s.as_bytes();
        let mut i = 0usize;
        let mut neg = false;
        match bytes[0] as char {
            '-' => {
                neg = true;
                i = 1;
            }
            '+' => {
                i = 1;
            }
            _ => {}
        }
        if i >= bytes.len() {
            return None;
        }
        let mut val: i64 = 0;
        let mut any = false;
        while i < bytes.len() {
            let b = bytes[i];
            if b == b'_' {
                i += 1;
                continue;
            }
            if !b.is_ascii_digit() {
                return None;
            }
            any = true;
            let d = (b - b'0') as i64;
            val = val.saturating_mul(10).saturating_add(d);
            i += 1;
        }
        if !any {
            return None;
        }
        Some(if neg { -val } else { val })
    }

    fn convert_value_to(&self, v: Value, ty: &Type) -> Result<Value> {
        match ty {
            // Any integer-like target becomes Value::Int
            Type::Int
            | Type::Int8
            | Type::Int16
            | Type::Int32
            | Type::Int64
            | Type::Int128
            | Type::BigInt
            | Type::UnInt
            | Type::UnInt8
            | Type::UnInt16
            | Type::UnInt32
            | Type::UnInt64
            | Type::UnInt128
            | Type::UnBigInt
            | Type::Usize
            | Type::Isize => match v {
                Value::Int(n) => Ok(Value::Int(n)),
                Value::Float(x) => Ok(Value::Int(x as i64)),
                Value::Double(x) => Ok(Value::Int(x as i64)),
                Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                Value::String(s) => {
                    if let Some(n) = Self::fast_parse_i64(s.as_str()) {
                        Ok(Value::Int(n))
                    } else {
                        s.parse::<i64>()
                            .map(Value::Int)
                            .map_err(|_| Error::runtime("Cannot cast String to Int"))
                    }
                }
                Value::Char(c) => Ok(Value::Int(c as i64)),
                Value::Array(_) | Value::Tuple(_) | Value::Range(_, _) => {
                    Err(Error::runtime("Cannot cast composite value to Int"))
                }
            },
            Type::Float => match v {
                Value::Int(n) => Ok(Value::Float(n as f32)),
                Value::Float(x) => Ok(Value::Float(x)),
                Value::Double(x) => Ok(Value::Float(x as f32)),
                Value::Bool(b) => Ok(Value::Float(if b { 1.0 } else { 0.0 })),
                Value::String(s) => s
                    .as_str()
                    .parse::<f32>()
                    .map(Value::Float)
                    .map_err(|_| Error::runtime("Cannot cast String to Float")),
                Value::Char(c) => Ok(Value::Float((c as u32) as f32)),
                Value::Array(_) | Value::Tuple(_) | Value::Range(_, _) => {
                    Err(Error::runtime("Cannot cast composite value to Float"))
                }
            },
            Type::Double => match v {
                Value::Int(n) => Ok(Value::Double(n as f64)),
                Value::Float(x) => Ok(Value::Double(x as f64)),
                Value::Double(x) => Ok(Value::Double(x)),
                Value::Bool(b) => Ok(Value::Double(if b { 1.0 } else { 0.0 })),
                Value::String(s) => s
                    .as_str()
                    .parse::<f64>()
                    .map(Value::Double)
                    .map_err(|_| Error::runtime("Cannot cast String to Double")),
                Value::Char(c) => Ok(Value::Double((c as u32) as f64)),
                Value::Array(_) | Value::Tuple(_) | Value::Range(_, _) => {
                    Err(Error::runtime("Cannot cast composite value to Double"))
                }
            },
            Type::Bool => match v {
                Value::Int(n) => Ok(Value::Bool(n != 0)),
                Value::Float(x) => Ok(Value::Bool(x != 0.0)),
                Value::Double(x) => Ok(Value::Bool(x != 0.0)),
                Value::Bool(b) => Ok(Value::Bool(b)),
                Value::String(s) => match s.as_str() {
                    "true" => Ok(Value::Bool(true)),
                    "false" => Ok(Value::Bool(false)),
                    _ => Err(Error::runtime("Cannot cast String to Bool")),
                },
                Value::Char(c) => Ok(Value::Bool(c != '\u{0}')),
                Value::Array(_) | Value::Tuple(_) | Value::Range(_, _) => {
                    Err(Error::runtime("Cannot cast composite value to Bool"))
                }
            },
            Type::String => match v {
                Value::String(s) => Ok(Value::String(s)),
                other => Ok(Value::String(std::rc::Rc::new(other.to_string()))),
            },
            Type::Range => Err(Error::runtime("Cannot cast to Iterator")),
            Type::Char => match v {
                Value::Char(c) => Ok(Value::Char(c)),
                Value::Int(n) => match std::char::from_u32(n as u32) {
                    Some(c) => Ok(Value::Char(c)),
                    None => Err(Error::runtime("Invalid code point for Char")),
                },
                Value::Float(x) => match std::char::from_u32(x as u32) {
                    Some(c) => Ok(Value::Char(c)),
                    None => Err(Error::runtime("Invalid code point for Char")),
                },
                Value::Double(x) => match std::char::from_u32(x as u32) {
                    Some(c) => Ok(Value::Char(c)),
                    None => Err(Error::runtime("Invalid code point for Char")),
                },
                Value::Bool(b) => Ok(if b {
                    Value::Char('\u{1}')
                } else {
                    Value::Char('\u{0}')
                }),
                Value::String(s) => {
                    let mut iter = s.chars();
                    if let Some(c) = iter.next() {
                        Ok(Value::Char(c))
                    } else {
                        Err(Error::runtime("Cannot cast empty String to Char"))
                    }
                }
                Value::Array(_) | Value::Tuple(_) | Value::Range(_, _) => {
                    Err(Error::runtime("Cannot cast composite value to Char"))
                }
            },
            Type::Array(_, _) | Type::Tuple(_) => {
                Err(Error::runtime("Cannot cast to composite type"))
            }
        }
    }

    /// Evaluate an if-expression returning Some(value) when any branch matches or else exists,
    /// or None when no branch matched and no else branch is provided.
    fn eval_if_optional(
        &mut self,
        branches: Vec<(Expr, Vec<Stmt>)>,
        else_branch: Option<Vec<Stmt>>,
    ) -> Result<Option<Value>> {
        for (cond, blk) in branches {
            let cv = self.eval_expr(cond)?;
            match cv {
                Value::Bool(true) => {
                    return Ok(Some(self.eval_block(blk)?));
                }
                Value::Bool(false) => {}
                _ => return Err(Error::runtime("If condition must be Bool")),
            }
        }
        if let Some(blk) = else_branch {
            Ok(Some(self.eval_block(blk)?))
        } else {
            Ok(None)
        }
    }

    // Public wrappers for VM access
    pub fn insert_binding_public(&mut self, name: String, binding: Binding) {
        self.loop_cache_insert_if_active(&name, &binding);
        self.map.insert(name, binding);
    }
    pub fn get_binding_cloned_public(&self, name: &str) -> Option<Binding> {
        self.map.get(name).cloned()
    }
    pub fn remove_binding_public(&mut self, name: &str) -> Option<Binding> {
        self.map.remove(name)
    }
    pub fn store_existing_public(&mut self, name: &str, v: Value) -> Result<()> {
        let b = self
            .map
            .get_mut(name)
            .ok_or_else(|| Error::runtime(format!("Unknown variable '{}'", name)))?;
        if !b.mutable {
            return Err(Error::runtime(format!(
                "Cannot assign to immutable variable '{}'",
                name
            )));
        }
        b.value = v;
        Ok(())
    }
    pub fn get_value_public(&self, name: &str) -> Result<Value> {
        let b = self
            .map
            .get(name)
            .ok_or_else(|| Error::runtime(format!("Unknown variable '{}'", name)))?;
        Ok(b.value.clone())
    }
    pub fn convert_value_to_public(&self, v: Value, ty: &Type) -> Result<Value> {
        self.convert_value_to(v, ty)
    }
}

impl Env {
    pub fn exec_stmt_ref(&mut self, stmt: &Stmt) -> Result<(Value, bool)> {
        match stmt {
            Stmt::Let {
                mutable,
                name,
                ty,
                expr,
            } => {
                let val = self.eval_expr_ref(expr)?;
                let final_ty = match ty {
                    Some(t) => t.clone(),
                    None => self.infer_type(&val),
                };
                let binding = Binding {
                    mutable: *mutable,
                    ty: final_ty.clone(),
                    value: val.clone(),
                };
                self.loop_cache_insert_if_active(name, &binding);
                self.map.insert(name.clone(), binding);
                Ok((val, false))
            }
            Stmt::AddAssign { name, expr } => {
                let v = self.apply_compound(name.clone(), expr.clone(), Op::Add)?;
                Ok((v, true))
            }
            Stmt::SubAssign { name, expr } => {
                let v = self.apply_compound(name.clone(), expr.clone(), Op::Sub)?;
                Ok((v, true))
            }
            Stmt::MulAssign { name, expr } => {
                let v = self.apply_compound(name.clone(), expr.clone(), Op::Mul)?;
                Ok((v, true))
            }
            Stmt::DivAssign { name, expr } => {
                let v = self.apply_compound(name.clone(), expr.clone(), Op::Div)?;
                Ok((v, true))
            }
            Stmt::RemAssign { name, expr } => {
                let v = self.apply_compound(name.clone(), expr.clone(), Op::Rem)?;
                Ok((v, true))
            }
            Stmt::ExprStmt(expr) => {
                if let Expr::If {
                    branches,
                    else_branch,
                } = expr
                {
                    match self.eval_if_optional_ref(branches, else_branch.as_ref())? {
                        Some(v) => Ok((v, true)),
                        None => Ok((Value::Int(0), false)),
                    }
                } else {
                    let v = self.eval_expr_ref(expr)?;
                    Ok((v, true))
                }
            }
            &Stmt::Break => {
                if self.loop_depth == 0 {
                    return Err(Error::runtime(
                        "E1001: 'break' used outside of loop. Hint: place 'break' inside for/while/loop.",
                    ));
                }
                self.break_now = true;
                Ok((Value::Int(0), false))
            }
            &Stmt::Continue => {
                if self.loop_depth == 0 {
                    return Err(Error::runtime(
                        "E1002: 'continue' used outside of loop. Hint: place 'continue' inside for/while/loop.",
                    ));
                }
                self.continue_now = true;
                Ok((Value::Int(0), false))
            }
            Stmt::AssignIndex { name, index, expr } => {
                let rhs = self.eval_expr_ref(expr)?;
                let b = self
                    .map
                    .get_mut(name)
                    .ok_or_else(|| Error::runtime(format!("Unknown variable '{}'", name)))?;
                if !b.mutable {
                    return Err(Error::runtime("Cannot assign to immutable object"));
                }
                let res = match &mut b.value {
                    Value::Array(items) => {
                        if *index >= items.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        items[*index] = rhs.clone();
                        (rhs, true)
                    }
                    Value::Tuple(items) => {
                        if *index >= items.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        items[*index] = rhs.clone();
                        (rhs, true)
                    }
                    _ => {
                        return Err(Error::runtime(
                            "Index assignment is only supported for arrays and tuples",
                        ));
                    }
                };
                Ok(res)
            }
            Stmt::For {
                mutable,
                name,
                iter,
                body,
            } => {
                let iterable = self.eval_expr_ref(iter)?;
                let mut last: Option<Value> = None;
                // Precompute optimized ops once for the loop body
                let optimized_ops = self.detect_patterns(body);
                self.loop_depth = self.loop_depth.saturating_add(1);
                match iterable {
                    Value::Array(items) => {
                        'outer_array_ref: for it in items {
                            self.loop_iter_begin();
                            let ty = self.infer_type(&it);
                            let binding = Binding {
                                mutable: *mutable,
                                ty,
                                value: it.clone(),
                            };
                            self.loop_cache_insert_if_active(name, &binding);
                            self.map.insert(name.clone(), binding);
                            let mut last_in_body: Option<Value> = None;
                            for op in &optimized_ops {
                                let (v, show) = self.exec_optimized_op(op)?;
                                if self.echo_inner && show {
                                    println!("{}", v);
                                }
                                last_in_body = Some(v);
                                if self.break_now {
                                    self.break_now = false;
                                    self.loop_iter_end();
                                    break 'outer_array_ref;
                                }
                                if self.continue_now {
                                    self.continue_now = false;
                                    break;
                                }
                            }
                            if let Some(v) = last_in_body {
                                last = Some(v);
                            }
                            self.loop_iter_end();
                        }
                    }
                    Value::Range(start, end) => {
                        let step: i64 = if start <= end { 1 } else { -1 };
                        let mut i = start;
                        'outer_iter_ref: loop {
                            if step > 0 {
                                if i > end {
                                    break;
                                }
                            } else {
                                if i < end {
                                    break;
                                }
                            }
                            self.loop_iter_begin();
                            let it = Value::Int(i);
                            let ty = self.infer_type(&it);
                            let binding = Binding {
                                mutable: *mutable,
                                ty,
                                value: it,
                            };
                            self.loop_cache_insert_if_active(name, &binding);
                            self.map.insert(name.clone(), binding);
                            let mut last_in_body: Option<Value> = None;
                            for op in &optimized_ops {
                                let (v, show) = self.exec_optimized_op(op)?;
                                if self.echo_inner && show {
                                    println!("{}", v);
                                }
                                last_in_body = Some(v);
                                if self.break_now {
                                    self.break_now = false;
                                    self.loop_iter_end();
                                    break 'outer_iter_ref;
                                }
                                if self.continue_now {
                                    self.continue_now = false;
                                    break;
                                }
                            }
                            if let Some(v) = last_in_body {
                                last = Some(v);
                            }
                            self.loop_iter_end();
                            i = i.saturating_add(step);
                        }
                    }
                    other => {
                        self.loop_depth = self.loop_depth.saturating_sub(1);
                        return Err(Error::runtime(format!(
                            "'for' can iterate over Array or Iterator, got {}",
                            other
                        )));
                    }
                }
                self.loop_depth = self.loop_depth.saturating_sub(1);
                Ok((last.unwrap_or(Value::Int(0)), false))
            }
            Stmt::While { cond, body } => {
                let mut last: Option<Value> = None;
                self.loop_depth = self.loop_depth.saturating_add(1);
                'outer_while_ref: loop {
                    let cv = self.eval_expr_ref(cond)?;
                    match cv {
                        Value::Bool(true) => {
                            self.loop_iter_begin();
                            let mut last_in_body: Option<Value> = None;
                            for s in body {
                                let (v, show) = self.exec_stmt_ref(s)?;
                                if self.echo_inner && show {
                                    println!("{}", v);
                                }
                                last_in_body = Some(v);
                                if self.break_now {
                                    self.break_now = false;
                                    self.loop_iter_end();
                                    break 'outer_while_ref;
                                }
                                if self.continue_now {
                                    self.continue_now = false;
                                    break;
                                }
                            }
                            if let Some(v) = last_in_body {
                                last = Some(v);
                            }
                            self.loop_iter_end();
                        }
                        Value::Bool(false) => break,
                        _ => {
                            self.loop_depth = self.loop_depth.saturating_sub(1);
                            return Err(Error::runtime("While condition must be Bool"));
                        }
                    }
                }
                self.loop_depth = self.loop_depth.saturating_sub(1);
                Ok((last.unwrap_or(Value::Int(0)), false))
            }
            Stmt::Loop { body } => {
                self.loop_depth = self.loop_depth.saturating_add(1);
                'outer_loop_ref: loop {
                    self.loop_iter_begin();
                    let mut _last_in_body: Option<Value> = None;
                    for s in body {
                        let (v, show) = self.exec_stmt_ref(s)?;
                        if self.echo_inner && show {
                            println!("{}", v);
                        }
                        _last_in_body = Some(v);
                        if self.break_now {
                            self.break_now = false;
                            self.loop_iter_end();
                            break 'outer_loop_ref;
                        }
                        if self.continue_now {
                            self.continue_now = false;
                            break;
                        }
                    }
                    self.loop_iter_end();
                }
                self.loop_depth = self.loop_depth.saturating_sub(1);
                Ok((Value::Int(0), false))
            }
        }
    }

    fn eval_block_ref(&mut self, stmts: &[Stmt]) -> Result<Value> {
        let mut last: Option<Value> = None;
        for s in stmts {
            let (v, _show) = self.exec_stmt_ref(s)?;
            last = Some(v);
        }
        last.ok_or_else(|| Error::runtime("Empty block has no value"))
    }

    fn eval_if_optional_ref(
        &mut self,
        branches: &Vec<(Expr, Vec<Stmt>)>,
        else_branch: Option<&Vec<Stmt>>,
    ) -> Result<Option<Value>> {
        // Fast paths on first branch
        if let Some((cond0, blk0)) = branches.get(0) {
            match cond0 {
                Expr::Bool(true) => {
                    return Ok(Some(self.eval_block_ref(blk0)?));
                }
                Expr::Bool(false) => { /* skip */ }
                Expr::Binary(l, Op::Eq, r) => {
                    if let (Expr::Ident(a), Expr::Ident(b)) = (&**l, &**r) {
                        if a == b {
                            return Ok(Some(self.eval_block_ref(blk0)?));
                        }
                    }
                    if let (Expr::Int(a), Expr::Int(b)) = (&**l, &**r) {
                        if a == b {
                            return Ok(Some(self.eval_block_ref(blk0)?));
                        }
                    }
                }
                _ => {}
            }
        }
        for (cond, blk) in branches.iter() {
            let cv = self.eval_expr_ref(cond)?;
            match cv {
                Value::Bool(true) => {
                    return Ok(Some(self.eval_block_ref(blk)?));
                }
                Value::Bool(false) => {}
                _ => return Err(Error::runtime("If condition must be Bool")),
            }
        }
        if let Some(blk) = else_branch {
            Ok(Some(self.eval_block_ref(blk)?))
        } else {
            Ok(None)
        }
    }
}

mod opt;
