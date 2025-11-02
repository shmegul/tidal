//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use crate::env::{Binding, Env};
use crate::value::Value;
use tidal_ast::{Program, Type};
use tidal_errors::{Error, Result};
use tidal_vm::{Bytecode, Instr};

pub use tidal_vm::CompileOutcome;

pub fn compile(program: &Program) -> CompileOutcome {
    tidal_vm::compile(program)
}

pub fn run(env: &mut Env, bc: &Bytecode) -> Result<Option<(Value, bool)>> {
    #[inline(always)]
    fn pop2(stack: &mut Vec<Value>) -> Result<(Value, Value)> {
        let b = stack
            .pop()
            .ok_or_else(|| Error::runtime("stack underflow"))?;
        let a = stack
            .pop()
            .ok_or_else(|| Error::runtime("stack underflow"))?;
        Ok((a, b))
    }
    #[inline(always)]
    fn pop2_int(stack: &mut Vec<Value>) -> Result<(i64, i64)> {
        let b = stack
            .pop()
            .ok_or_else(|| Error::runtime("stack underflow"))?;
        let a = stack
            .pop()
            .ok_or_else(|| Error::runtime("stack underflow"))?;
        if let (Value::Int(x), Value::Int(y)) = (a, b) {
            Ok((x, y))
        } else {
            Err(Error::runtime("Type mismatch: expected Int operands"))
        }
    }

    let code = &bc.code;
    let mut ip: usize = 0;
    let mut stack: Vec<Value> = Vec::with_capacity(4096);

    #[inline(always)]
    fn arith_add(stack: &mut Vec<Value>, static_typed: bool) -> Result<()> {
        if static_typed {
            let (x, y) = pop2_int(stack)?;
            stack.push(Value::Int(x.wrapping_add(y)));
            Ok(())
        } else {
            let (a, b) = pop2(stack)?;
            match (a, b) {
                (Value::Int(x), Value::Int(y)) => {
                    stack.push(Value::Int(x.wrapping_add(y)));
                    Ok(())
                }
                _ => Err(Error::runtime(
                    "Unsupported binary operation for given types in VM",
                )),
            }
        }
    }
    #[inline(always)]
    fn arith_sub(stack: &mut Vec<Value>, static_typed: bool) -> Result<()> {
        if static_typed {
            let (x, y) = pop2_int(stack)?;
            stack.push(Value::Int(x.wrapping_sub(y)));
            Ok(())
        } else {
            let (a, b) = pop2(stack)?;
            match (a, b) {
                (Value::Int(x), Value::Int(y)) => {
                    stack.push(Value::Int(x.wrapping_sub(y)));
                    Ok(())
                }
                _ => Err(Error::runtime(
                    "Unsupported binary operation for given types in VM",
                )),
            }
        }
    }
    #[inline(always)]
    fn arith_mul(stack: &mut Vec<Value>, static_typed: bool) -> Result<()> {
        if static_typed {
            let (x, y) = pop2_int(stack)?;
            stack.push(Value::Int(x.saturating_mul(y)));
            Ok(())
        } else {
            let (a, b) = pop2(stack)?;
            match (a, b) {
                (Value::Int(x), Value::Int(y)) => {
                    stack.push(Value::Int(x.saturating_mul(y)));
                    Ok(())
                }
                _ => Err(Error::runtime(
                    "Unsupported binary operation for given types in VM",
                )),
            }
        }
    }
    #[inline(always)]
    fn arith_div(stack: &mut Vec<Value>, static_typed: bool) -> Result<()> {
        if static_typed {
            let (x, y) = pop2_int(stack)?;
            if y == 0 {
                return Err(Error::runtime("Division by zero"));
            }
            stack.push(Value::Int(x / y));
            Ok(())
        } else {
            let (a, b) = pop2(stack)?;
            match (a, b) {
                (Value::Int(x), Value::Int(y)) => {
                    if y == 0 {
                        return Err(Error::runtime("Division by zero"));
                    }
                    stack.push(Value::Int(x / y));
                    Ok(())
                }
                _ => Err(Error::runtime(
                    "Unsupported binary operation for given types in VM",
                )),
            }
        }
    }
    #[inline(always)]
    fn arith_rem(stack: &mut Vec<Value>, static_typed: bool) -> Result<()> {
        if static_typed {
            let (x, y) = pop2_int(stack)?;
            if y == 0 {
                return Err(Error::runtime("Division by zero"));
            }
            stack.push(Value::Int(x % y));
            Ok(())
        } else {
            let (a, b) = pop2(stack)?;
            match (a, b) {
                (Value::Int(x), Value::Int(y)) => {
                    if y == 0 {
                        return Err(Error::runtime("Division by zero"));
                    }
                    stack.push(Value::Int(x % y));
                    Ok(())
                }
                _ => Err(Error::runtime(
                    "Unsupported binary operation for given types in VM",
                )),
            }
        }
    }
    #[inline(always)]
    fn cmp_apply(op: &Instr, stack: &mut Vec<Value>, static_typed: bool) -> Result<()> {
        if static_typed {
            let (x, y) = pop2_int(stack)?;
            let outb = match op {
                Instr::CmpEq => x == y,
                Instr::CmpNe => x != y,
                Instr::CmpLt => x < y,
                Instr::CmpGt => x > y,
                Instr::CmpLe => x <= y,
                Instr::CmpGe => x >= y,
                _ => unreachable!(),
            };
            stack.push(Value::Bool(outb));
            Ok(())
        } else {
            let (a, b) = pop2(stack)?;
            let outb = match (&a, &b) {
                (Value::Int(x), Value::Int(y)) => match op {
                    Instr::CmpEq => *x == *y,
                    Instr::CmpNe => *x != *y,
                    Instr::CmpLt => *x < *y,
                    Instr::CmpGt => *x > *y,
                    Instr::CmpLe => *x <= *y,
                    Instr::CmpGe => *x >= *y,
                    _ => unreachable!(),
                },
                _ => {
                    return Err(Error::runtime(
                        "VM comparisons currently support Int operands only",
                    ));
                }
            };
            stack.push(Value::Bool(outb));
            Ok(())
        }
    }
    let mut last: Option<(Value, bool)> = None;
    // locals storage
    let mut locals: Vec<Value> = vec![Value::Int(0); bc.slot_name_idx.len()];
    // track if we've exported a slot into Env already (avoid repeated inserts)
    let mut slot_exported: Vec<bool> = vec![false; bc.slot_name_idx.len()];
    // per-slot cached type to avoid repeated env.infer_type_public
    let mut slot_type: Vec<Option<Type>> = vec![None; bc.slot_name_idx.len()];
    // track mutability per slot; LetLocal and ForStart set this
    let mut slot_mutable: Vec<bool> = vec![false; bc.slot_name_idx.len()];

    // simple loop stack for for-loops
    #[derive(Clone)]
    struct LoopFrame {
        slot: usize,
        /*mutable: bool,*/ current: i64,
        end: i64,
        step: i64,
        body_start: usize,
        body_end: usize,
    }
    let mut loops: Vec<LoopFrame> = Vec::with_capacity(64);

    // Simple scope stack for Env exports (REPL-only)
    #[derive(Clone)]
    struct ExportedVar {
        name: String,
        slot: usize,
        prev: Option<Binding>,
    }
    let mut scope_stack: Vec<Vec<ExportedVar>> = Vec::new();
    // Global scope frame to catch top-level exports
    scope_stack.push(Vec::new());

    while ip < code.len() {
        match &code[ip] {
            Instr::LoadInt(n) => {
                stack.push(Value::Int(*n));
                ip += 1;
            }
            Instr::LoadBool(b) => {
                stack.push(Value::Bool(*b));
                ip += 1;
            }
            Instr::LoadConstStr(idx) => {
                stack.push(Value::String(bc.consts[*idx].clone()));
                ip += 1;
            }
            Instr::LoadLocal(slot) => {
                stack.push(locals[*slot].clone());
                ip += 1;
            }
            Instr::StoreLocal(slot) => {
                // Enforce mutability on updates
                if !slot_mutable[*slot] {
                    let name_idx = bc.slot_name_idx[*slot];
                    let name = bc.names[name_idx].as_ref();
                    return Err(Error::runtime(format!(
                        "Cannot assign to immutable variable '{}'",
                        name
                    )));
                }
                let v = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                locals[*slot] = v;
                // Do not update `last` here to avoid marking internal stores as printable
                ip += 1;
            }
            Instr::StoreConst(slot, n) => {
                // Enforce mutability on updates
                if !slot_mutable[*slot] {
                    let name_idx = bc.slot_name_idx[*slot];
                    let name = bc.names[name_idx].as_ref();
                    return Err(Error::runtime(format!(
                        "Cannot assign to immutable variable '{}'",
                        name
                    )));
                }
                locals[*slot] = Value::Int(*n);
                // hot path: no Env updates, no last
                ip += 1;
            }
            Instr::ScopeEnter => {
                scope_stack.push(Vec::new());
                ip += 1;
            }
            Instr::ScopeExit => {
                // Pop one scope and clean Env exports if any
                if let Some(frame) = scope_stack.pop() {
                    if env.should_export_vm_locals() {
                        for ev in frame.into_iter().rev() {
                            if let Some(prev) = ev.prev {
                                env.insert_binding_public(ev.name, prev);
                            } else {
                                let _ = env.remove_binding_public(&ev.name);
                            }
                            // Allow re-export of this slot in future scopes
                            slot_exported[ev.slot] = false;
                        }
                    } else {
                        // Even if we didn't export to Env, clear the flags so the slot can be exported in a later scope if needed
                        for ev in frame.into_iter().rev() {
                            slot_exported[ev.slot] = false;
                        }
                    }
                }
                ip += 1;
            }
            Instr::Add => {
                arith_add(&mut stack, bc.has_static_types)?;
                ip += 1;
            }
            Instr::Sub => {
                arith_sub(&mut stack, bc.has_static_types)?;
                ip += 1;
            }
            Instr::Mul => {
                arith_mul(&mut stack, bc.has_static_types)?;
                ip += 1;
            }
            Instr::Div => {
                arith_div(&mut stack, bc.has_static_types)?;
                ip += 1;
            }
            Instr::Rem => {
                arith_rem(&mut stack, bc.has_static_types)?;
                ip += 1;
            }
            Instr::CmpEq
            | Instr::CmpNe
            | Instr::CmpLt
            | Instr::CmpGt
            | Instr::CmpLe
            | Instr::CmpGe => {
                let op = &code[ip];
                cmp_apply(op, &mut stack, bc.has_static_types)?;
                ip += 1;
            }
            Instr::Jump(target) => {
                ip = *target;
            }
            Instr::JumpIfFalse(target) => {
                // Strict semantics: JumpIfFalse expects a Bool on the stack.
                let v = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                let cond = match v {
                    Value::Bool(b) => b,
                    other => {
                        return Err(Error::runtime(format!(
                            "If condition must be Bool at runtime (got {})",
                            other
                        )));
                    }
                };
                if !cond {
                    ip = *target;
                } else {
                    ip += 1;
                }
            }
            Instr::CastTo(ty) => {
                let v = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                let conv = env_convert(env, v, ty)?;
                stack.push(conv);
                ip += 1;
            }
            Instr::Range => {
                let right = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                let left = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => stack.push(Value::Range(a, b)),
                    _ => return Err(Error::runtime("Range '..' bounds must be Int")),
                }
                ip += 1;
            }
            Instr::LetLocal { slot, mutable } => {
                let v = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                locals[*slot] = v.clone();
                // Update mutability for this slot according to declaration
                slot_mutable[*slot] = *mutable;
                // Insert into Env only once per slot to avoid hot-path hashmap churn
                if !slot_exported[*slot] {
                    slot_exported[*slot] = true;
                    if env.should_export_vm_locals() {
                        let name_idx = bc.slot_name_idx[*slot];
                        let name = bc.names[name_idx].as_ref().clone();
                        // Use cached type if available; otherwise infer once and cache
                        let ty = if let Some(t) = &slot_type[*slot] {
                            t.clone()
                        } else {
                            let t = env.infer_type_public(&v);
                            slot_type[*slot] = Some(t.clone());
                            t
                        };
                        let prev = env.get_binding_cloned_public(&name);
                        let binding = Binding {
                            mutable: *mutable,
                            ty,
                            value: v.clone(),
                        };
                        env.insert_binding_public(name.clone(), binding);
                        if let Some(frame) = scope_stack.last_mut() {
                            frame.push(ExportedVar {
                                name,
                                slot: *slot,
                                prev,
                            });
                        }
                    }
                } else {
                    // Slot already exported: do NOT update Env on every iteration; keep updates in locals only to avoid hot-path hashmap lookups.
                }
                // Do not update `last` here — internal declaration
                ip += 1;
            }
            Instr::ForStart {
                slot,
                mutable,
                body_len,
            } => {
                let it = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                let (start, end) = match it {
                    Value::Range(a, b) => (a, b),
                    other => {
                        return Err(Error::runtime(format!(
                            "'for' expects Iterator, got {}",
                            other
                        )));
                    }
                };
                let step = if start <= end { 1 } else { -1 };
                let body_start = ip + 1;
                let body_end = body_start + *body_len;
                if (step > 0 && start > end) || (step < 0 && start < end) {
                    ip = body_end + 1; // skip ForEnd
                } else {
                    let cur = start;
                    locals[*slot] = Value::Int(cur);
                    // Loop variable mutability is defined by the 'mutable' flag
                    slot_mutable[*slot] = *mutable;
                    if env.should_export_vm_locals() {
                        // Insert initial binding into Env for external visibility and track in scope
                        let name_idx = bc.slot_name_idx[*slot];
                        let name = bc.names[name_idx].as_ref().clone();
                        let v = locals[*slot].clone();
                        // Use cached type if available, otherwise infer once and cache
                        let ty = if let Some(t) = &slot_type[*slot] {
                            t.clone()
                        } else {
                            let t = env.infer_type_public(&v);
                            slot_type[*slot] = Some(t.clone());
                            t
                        };
                        let prev = env.get_binding_cloned_public(&name);
                        let binding = Binding {
                            mutable: *mutable,
                            ty,
                            value: v,
                        };
                        env.insert_binding_public(name.clone(), binding);
                        if let Some(frame) = scope_stack.last_mut() {
                            frame.push(ExportedVar {
                                name,
                                slot: *slot,
                                prev,
                            });
                        }
                    }
                    loops.push(LoopFrame {
                        slot: *slot,
                        current: cur,
                        end,
                        step,
                        body_start,
                        body_end,
                    });
                    ip = body_start;
                }
            }
            Instr::ForEnd => {
                let mut frame = loops
                    .pop()
                    .ok_or_else(|| Error::runtime("internal: ForEnd without frame"))?;
                let next = frame.current.saturating_add(frame.step);
                let cont = if frame.step > 0 {
                    next <= frame.end
                } else {
                    next >= frame.end
                };
                if cont {
                    frame.current = next;
                    // Update local directly (no Env hashmap updates on hot path)
                    locals[frame.slot] = Value::Int(next);
                    let restart = frame.body_start;
                    loops.push(frame);
                    ip = restart;
                } else {
                    ip = frame.body_end + 1;
                }
            }
            Instr::Break => {
                // Break out of the innermost loop: pop its frame and jump past ForEnd
                let frame = loops
                    .pop()
                    .ok_or_else(|| Error::runtime("'break' used outside of loop (VM)"))?;
                ip = frame.body_end + 1;
            }
            Instr::Continue => {
                // Continue: jump to ForEnd of the innermost loop to advance iteration
                let frame = loops
                    .last()
                    .ok_or_else(|| Error::runtime("'continue' used outside of loop (VM)"))?
                    .clone();
                ip = frame.body_end; // execute ForEnd next
            }
            Instr::ExprYield => {
                if let Some(v) = stack.last().cloned() {
                    last = Some((v, true));
                }
                ip += 1;
            }
            Instr::Pop => {
                let _ = stack.pop();
                ip += 1;
            }
            Instr::IndexLoad(idx) => {
                let base = stack
                    .pop()
                    .ok_or_else(|| Error::runtime("stack underflow"))?;
                match base {
                    Value::Array(items) => {
                        if *idx >= items.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        stack.push(items[*idx].clone());
                    }
                    Value::Tuple(items) => {
                        if *idx >= items.len() {
                            return Err(Error::runtime("Index out of bounds"));
                        }
                        stack.push(items[*idx].clone());
                    }
                    other => {
                        return Err(Error::runtime(format!(
                            "Indexing not supported on {}",
                            other
                        )));
                    }
                }
                ip += 1;
            }
            Instr::IndexStore(_idx) => {
                return Err(Error::runtime("IndexStore not supported yet in VM"));
            }
            Instr::IncLocal(slot) => {
                // Enforce mutability on updates
                if !slot_mutable[*slot] {
                    let name_idx = bc.slot_name_idx[*slot];
                    let name = bc.names[name_idx].as_ref();
                    return Err(Error::runtime(format!(
                        "Cannot assign to immutable variable '{}'",
                        name
                    )));
                }
                // Fast increment of an Int local by 1, no stack traffic
                match &mut locals[*slot] {
                    Value::Int(x) => {
                        *x = x.wrapping_add(1);
                        // do not update last/Env for hot path
                        ip += 1;
                    }
                    other => {
                        return Err(Error::runtime(format!(
                            "IncLocal expects Int local, got {}",
                            other
                        )));
                    }
                }
            }
        }
    }
    Ok(last)
}

#[cold]
fn env_convert(env: &Env, v: Value, ty: &Type) -> Result<Value> {
    env.convert_value_to_public(v, ty)
}
