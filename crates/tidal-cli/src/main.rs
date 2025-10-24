//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use std::env;
use std::fs;
use std::path::Path;
use std::process;

use tidal_parser::parse_program;
use tidal_runtime::Env;

fn print_usage(program: &str) {
    eprintln!("Usage: {} <file.td> [-sbc]", program);
}

fn main() {
    let mut args = env::args();
    let program = args.next().unwrap_or_else(|| "tidal".to_string());

    let mut show_bc = false;
    let mut path: Option<String> = None;
    for arg in args {
        if arg == "-sbc" {
            show_bc = true;
        } else if arg.starts_with('-') {
            eprintln!("Unknown flag: {}", arg);
            print_usage(&program);
            process::exit(1);
        } else if path.is_none() {
            path = Some(arg);
        } else {
            eprintln!("Too many arguments");
            print_usage(&program);
            process::exit(1);
        }
    }

    let path = match path {
        Some(p) => p,
        None => {
            print_usage(&program);
            process::exit(1);
        }
    };

    let src = match fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", path, e);
            process::exit(1);
        }
    };

    let program_ast = match parse_program(&src) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}: {}", path, e);
            process::exit(1);
        }
    };

    if show_bc {
        use tidal_runtime::vm::{compile as vm_compile, CompileOutcome};
        match vm_compile(&program_ast) {
            CompileOutcome::Bytecode(bc) => {
                let dump_path = {
                    let p = Path::new(&path);
                    let mut s = p.with_extension("");
                    if s == *p { s = p.to_path_buf(); }
                    s.set_extension("tdbc");
                    s
                };
                let dump_str = bc.to_string();
                if let Err(e) = fs::write(&dump_path, dump_str.as_bytes()) {
                    eprintln!("Failed to write bytecode dump {}: {}", dump_path.display(), e);
                } else {
                    println!("{}", bc);
                }
            }
            CompileOutcome::Unsupported => {
                eprintln!("Bytecode dump not available: VM does not support all features in this file.");
            }
        }
    }

    let mut env = Env::new();
    match env.exec_program(program_ast) {
        Ok(Some((val, show))) => {
            if show {
                println!("{}", val);
            }
            process::exit(0);
        }
        Ok(None) => {
            process::exit(0);
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}
