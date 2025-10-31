//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

use std::env;

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    Help,
    Version,
    RunFile { path: String, dump_bc: bool, dump_ast: bool },
    Fatal(String),
}

const VERSION_FILE: &str = include_str!("../../../VERSION");
pub const VERSION: &str = VERSION_FILE;

pub fn help_text(program: &str) -> String {
    format!(
        "Tidal Programming Language \n\n\
         Usage:\n\
           {0} --help                     Show this help message\n\
           {0} --version                  Show Tidal language version\n\
           {0} <file.td>                  Execute a Tidal source file\n\
           {0} <file.td> --dump-ast       Parse and print the AST to stdout\n\
           {0} <file.td> --dump-bytecode  Compile to VM bytecode and write adjacent .tdbc file\n\n\
         Flags:\n\
           --help                         Show this help message\n\
           --version                      Show Tidal language version\n",
        program
    )
}

pub fn parse_env() -> (String, Command) {
    let mut args = env::args();
    let program = args.next().unwrap_or_else(|| "tidal".to_string());
    let first = match args.next() {
        None => return (program, Command::Help),
        Some(s) => s,
    };
    match first.as_str() {
        "--help" | "-h" => (program, Command::Help),
        "--version" | "-V" => (program, Command::Version),
        other => {
            let path = other.to_string();
            if !path.ends_with(".td") {
                return (program, Command::Fatal(format!("\x1b[31mfatal\x1b[0m: unknown command '{}'", other)));
            }
            let mut dump_bc = false;
            let mut dump_ast = false;
            for a in args {
                match a.as_str() {
                    "--dump-bytecode" => dump_bc = true,
                    "--dump-ast" => dump_ast = true,
                    unknown => return (program, Command::Fatal(format!("\x1b[31mfatal\x1b[0m: unknown command '{}'", unknown))),
                }
            }
            (program, Command::RunFile { path, dump_bc, dump_ast })
        }
    }
}
