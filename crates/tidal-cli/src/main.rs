//! Copyright (c) 2025 Tidal Dev Group.
//!
//! This file is part of the Tidal Programming Language project.
//! Licensed under the MIT License.
//!
//! See the LICENSE file in the root directory of this project for license details.

mod cli;
mod run;

fn main() {
    let (program, cmd) = cli::parse_env();
    match cmd {
        cli::Command::Help => {
            println!("{}", cli::help_text(&program));
            std::process::exit(0);
        }
        cli::Command::Version => {
            println!("{}", cli::VERSION);
            std::process::exit(0);
        }
        cli::Command::Fatal(msg) => {
            eprintln!("{}", msg);
            std::process::exit(1);
        }
        other => {
            let code = run::execute(other);
            std::process::exit(code);
        }
    }
}
