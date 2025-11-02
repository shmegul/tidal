//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use std::collections::HashMap;
use tidal_ast::{Expr, Program, Stmt, Type};
use tidal_errors::{Error, Result};

#[derive(Default)]
pub struct TypeCtx {
    pub vars: HashMap<String, Type>,
}

fn is_int_like(t: &Type) -> bool {
    matches!(
        t,
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
            | Type::Isize
    )
}

fn is_numeric_like(t: &Type) -> bool {
    is_int_like(t) || matches!(t, Type::Float | Type::Double)
}

pub fn typecheck_program(program: &Program) -> Result<()> {
    let mut ctx = TypeCtx::default();
    for stmt in &program.stmts {
        typecheck_stmt(stmt, &mut ctx)?;
    }
    Ok(())
}

fn typecheck_stmt(stmt: &Stmt, ctx: &mut TypeCtx) -> Result<()> {
    match stmt {
        Stmt::Let {
            mutable: _,
            name,
            ty,
            expr,
        } => {
            let expr_ty = infer_expr_type(expr, ctx)?;
            if let Some(t) = ty {
                if &expr_ty != t {
                    // Permit Float <-> Double inits without explicit cast for ergonomics
                    let float_double_ok = matches!(
                        (t, &expr_ty),
                        (Type::Float, Type::Double) | (Type::Double, Type::Float)
                    );
                    if !float_double_ok {
                        return Err(Error::parse(format!(
                            "Type mismatch in let '{}': expected {:?}, got {:?}. Use 'as {:?}' to cast.",
                            name, t, expr_ty, t
                        )));
                    }
                }
                ctx.vars.insert(name.clone(), t.clone());
            } else {
                ctx.vars.insert(name.clone(), expr_ty);
            }
            Ok(())
        }
        Stmt::AddAssign { name, expr } => {
            let lhs = ctx
                .vars
                .get(name)
                .ok_or_else(|| Error::parse(format!("Unknown variable '{}'", name)))?
                .clone();
            let rhs = infer_expr_type(expr, ctx)?;
            ensure_binary_ok("+=", &lhs, &rhs)?;
            Ok(())
        }
        Stmt::SubAssign { name, expr } => {
            let lhs = ctx
                .vars
                .get(name)
                .ok_or_else(|| Error::parse(format!("Unknown variable '{}'", name)))?
                .clone();
            let rhs = infer_expr_type(expr, ctx)?;
            ensure_binary_ok("-=", &lhs, &rhs)?;
            Ok(())
        }
        Stmt::MulAssign { name, expr } => {
            let lhs = ctx
                .vars
                .get(name)
                .ok_or_else(|| Error::parse(format!("Unknown variable '{}'", name)))?
                .clone();
            let rhs = infer_expr_type(expr, ctx)?;
            ensure_binary_ok("* =", &lhs, &rhs)?;
            Ok(())
        }
        Stmt::DivAssign { name, expr } => {
            let lhs = ctx
                .vars
                .get(name)
                .ok_or_else(|| Error::parse(format!("Unknown variable '{}'", name)))?
                .clone();
            let rhs = infer_expr_type(expr, ctx)?;
            ensure_binary_ok("/=", &lhs, &rhs)?;
            Ok(())
        }
        Stmt::RemAssign { name, expr } => {
            let lhs = ctx
                .vars
                .get(name)
                .ok_or_else(|| Error::parse(format!("Unknown variable '{}'", name)))?
                .clone();
            let rhs = infer_expr_type(expr, ctx)?;
            ensure_binary_ok("%=", &lhs, &rhs)?;
            Ok(())
        }
        Stmt::AssignIndex { name, index, expr } => {
            let target_ty = ctx
                .vars
                .get(name)
                .ok_or_else(|| Error::parse(format!("Unknown variable '{}'", name)))?
                .clone();
            let rhs_ty = infer_expr_type(expr, ctx)?;
            match target_ty {
                Type::Array(elem, len) => {
                    if *index >= len {
                        return Err(Error::parse("Index out of bounds in assignment"));
                    }
                    if *elem != rhs_ty {
                        return Err(Error::parse(format!(
                            "Type mismatch in index assignment: array element {:?} vs rhs {:?}",
                            *elem, rhs_ty
                        )));
                    }
                    Ok(())
                }
                Type::Tuple(tys) => {
                    if *index >= tys.len() {
                        return Err(Error::parse("Index out of bounds in tuple assignment"));
                    }
                    let et = tys[*index].clone();
                    if et != rhs_ty {
                        return Err(Error::parse(format!(
                            "Type mismatch in tuple assignment: element {:?} vs rhs {:?}",
                            et, rhs_ty
                        )));
                    }
                    Ok(())
                }
                _ => Err(Error::parse(
                    "Index assignment is only valid for arrays and tuples",
                )),
            }
        }
        Stmt::For {
            mutable: _,
            name,
            iter,
            body,
        } => {
            let it_ty = infer_expr_type(iter, ctx)?;
            let loop_var_ty = match it_ty {
                Type::Range => Type::Int,
                Type::Array(elem, _len) => (*elem).clone(),
                other => {
                    return Err(Error::parse(format!(
                        "'for' can iterate over Array or Iterator, got {:?}",
                        other
                    )));
                }
            };
            ctx.vars.insert(name.clone(), loop_var_ty);
            for s in body {
                typecheck_stmt(s, ctx)?;
            }
            Ok(())
        }
        Stmt::While { cond, body } => {
            let ct = infer_expr_type(cond, ctx)?;
            if ct != Type::Bool {
                return Err(Error::parse("While condition must be Bool"));
            }
            for s in body {
                typecheck_stmt(s, ctx)?;
            }
            Ok(())
        }
        Stmt::Loop { body } => {
            for s in body {
                typecheck_stmt(s, ctx)?;
            }
            Ok(())
        }
        Stmt::ExprStmt(expr) => {
            let _ = infer_expr_type(expr, ctx)?;
            Ok(())
        }
        Stmt::Break => Ok(()),
        Stmt::Continue => Ok(()),
    }
}

fn ensure_binary_ok(op: &str, l: &Type, r: &Type) -> Result<()> {
    match op {
        "+=" | "-=" | "*=" | "/=" | "%=" => {
            if !(is_numeric_like(l) && is_numeric_like(r)) {
                return Err(Error::parse(format!(
                    "Operator '{}' is only defined for numeric types, got {:?} and {:?}",
                    op, l, r
                )));
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn infer_expr_type(expr: &Expr, ctx: &TypeCtx) -> Result<Type> {
    match expr {
        Expr::Int(_) => Ok(Type::Int),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::String(_) => Ok(Type::String),
        Expr::Float(_) => Ok(Type::Float),
        Expr::Double(_) => Ok(Type::Double),
        Expr::Char(_) => Ok(Type::Char),
        Expr::Ident(name) => ctx
            .vars
            .get(name)
            .cloned()
            .ok_or_else(|| Error::parse(format!("Unknown variable '{}'", name))),
        Expr::Binary(l, op, r) => {
            use tidal_ast::ops::Op;
            let lt = infer_expr_type(l, ctx)?;
            let rt = infer_expr_type(r, ctx)?;
            match op {
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem => {
                    if lt == rt && is_numeric_like(&lt) {
                        Ok(lt)
                    } else {
                        Err(Error::parse(
                            "Arithmetic operators require numeric operands",
                        ))
                    }
                }
                Op::Eq | Op::Ne | Op::Lt | Op::Le | Op::Gt | Op::Ge => Ok(Type::Bool),
                Op::And | Op::Or => Ok(Type::Bool),
                Op::Shl | Op::Shr | Op::BitAnd | Op::BitOr | Op::BitXor => {
                    if is_int_like(&lt) && is_int_like(&rt) {
                        Ok(lt)
                    } else {
                        Err(Error::parse("Bitwise operators require integer operands"))
                    }
                }
                Op::Pow => {
                    if is_int_like(&lt) && is_int_like(&rt) {
                        Ok(lt)
                    } else {
                        Err(Error::parse("Power operator requires integer operands"))
                    }
                }
            }
        }
        Expr::BitNot(e) => {
            let t = infer_expr_type(e, ctx)?;
            if is_int_like(&t) {
                Ok(t)
            } else {
                Err(Error::parse("Bitwise not '~' is only supported for Int"))
            }
        }
        Expr::Not(e) => {
            let t = infer_expr_type(e, ctx)?;
            if matches!(t, Type::Bool) {
                Ok(Type::Bool)
            } else {
                Err(Error::parse("Logical 'not' expects Bool operand"))
            }
        }
        Expr::Block(stmts) => {
            let mut inner = TypeCtx {
                vars: ctx.vars.clone(),
            };
            let mut last: Option<Type> = None;
            for s in stmts {
                typecheck_stmt(s, &mut inner)?;
                last = Some(Type::Int);
            } // block value typing not used by VM now
            Ok(last.unwrap_or(Type::Int))
        }
        Expr::If {
            branches,
            else_branch,
        } => {
            // Ensure each condition is Bool and branch bodies are type-checked. Result type is ignored by VM.
            for (cond, blk) in branches {
                let ct = infer_expr_type(cond, ctx)?;
                if ct != Type::Bool {
                    return Err(Error::parse("If condition must be Bool"));
                }
                let mut inner = TypeCtx {
                    vars: ctx.vars.clone(),
                };
                for s in blk {
                    typecheck_stmt(s, &mut inner)?;
                }
            }
            if let Some(blk) = else_branch {
                let mut inner = TypeCtx {
                    vars: ctx.vars.clone(),
                };
                for s in blk {
                    typecheck_stmt(s, &mut inner)?;
                }
            }
            Ok(Type::Int)
        }
        Expr::Cast(e, ty) => {
            let _ = infer_expr_type(e, ctx)?; // ensure source type exists
            Ok(ty.clone())
        }
        Expr::Array(items) => {
            let n = items.len();
            if n == 0 {
                return Ok(Type::Array(Box::new(Type::Int), 0));
            }
            let et = infer_expr_type(&items[0], ctx)?;
            for it in items.iter().skip(1) {
                let t = infer_expr_type(it, ctx)?;
                if t != et {
                    return Err(Error::parse("Array elements must have the same type"));
                }
            }
            Ok(Type::Array(Box::new(et), n))
        }
        Expr::Tuple(items) => {
            let mut tys = Vec::with_capacity(items.len());
            for it in items {
                tys.push(infer_expr_type(it, ctx)?);
            }
            Ok(Type::Tuple(tys))
        }
        Expr::Index(base, idx) => {
            let bt = infer_expr_type(base, ctx)?;
            match bt {
                Type::Array(elem, len) => {
                    if *idx >= len {
                        return Err(Error::parse("Index out of bounds"));
                    }
                    Ok(*elem)
                }
                Type::Tuple(tys) => {
                    if *idx >= tys.len() {
                        return Err(Error::parse("Index out of bounds"));
                    }
                    Ok(tys[*idx].clone())
                }
                _ => Err(Error::parse(
                    "Indexing is supported only for arrays and tuples",
                )),
            }
        }
        Expr::Range(_, _) => Ok(Type::Range),
    }
}
