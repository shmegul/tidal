//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use std::fs;
use std::path::Path;

use tidal_parser::parse_program;
use tidal_runtime::Env;

use crate::cli::Command;

pub fn execute(cmd: Command) -> i32 {
    match cmd {
        Command::Help => 0,
        Command::Version => 0,
        Command::RunFile {
            path,
            dump_bc,
            dump_ast,
        } => run_file(&path, dump_bc, dump_ast),
        Command::Fatal(_) => 1, // Should be handled in main; return non-zero as a fallback
    }
}

fn run_file(path: &str, dump_bc: bool, dump_ast: bool) -> i32 {
    let src = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", path, e);
            return 1;
        }
    };

    let program_ast = match parse_program(&src) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}: {}", path, e);
            return 1;
        }
    };

    if dump_ast && !dump_bc {
        println!("{:#?}", program_ast);
        return 0;
    }

    if dump_bc {
        use tidal_runtime::vm::{CompileOutcome, compile as vm_compile};
        return match vm_compile(&program_ast) {
            CompileOutcome::Bytecode(bc) => {
                let dump_path = {
                    let p = Path::new(path);
                    let mut s = p.with_extension("");
                    if s == p {
                        s = p.to_path_buf();
                    }
                    s.set_extension("tdbc");
                    s
                };
                // Match exactly what println!("{}", bc) would output by adding a trailing newline
                let dump_str = format!("{}\n", bc);
                if let Err(e) = fs::write(&dump_path, dump_str.as_bytes()) {
                    eprintln!(
                        "Failed to write bytecode dump {}: {}",
                        dump_path.display(),
                        e
                    );
                    return 1;
                }
                0
            }
            CompileOutcome::Unsupported => {
                eprintln!(
                    "Bytecode dump not available: VM does not support all features in this file."
                );
                1
            }
        };
    }

    let mut env = Env::new();
    match env.exec_program(program_ast) {
        Ok(Some((val, show))) => {
            if show {
                println!("{}", val);
            }
            0
        }
        Ok(None) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    }
}
