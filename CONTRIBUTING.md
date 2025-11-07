# Contributing to Tidal

Thanks for your interest in contributing! This document explains how to set up your environment, make changes, and submit a high‑quality pull request.

If you have any questions at any point, please open a GitHub Discussion/Issue or draft PR early to get feedback.


## Code of Conduct

This project adheres to our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold it.


## Project overview

Tidal is a programming language. This repository is a Rust workspace that implements the language via multiple crates:

- `crates/tidal-cli` — the reference CLI/driver (binary) for running Tidal code
- `crates/tidal-ast`, `crates/tidal-lexer`, `crates/tidal-parser`, `crates/tidal-runtime`, `crates/tidal-vm`, `crates/tidal-jit`, `crates/tidal-errors` — core libraries that implement the language

The workspace root `Cargo.toml` manages these crates together. Most commands (build, test, lint) run from the workspace root and affect all crates.


## Prerequisites

- Rust toolchain via rustup (latest stable)
  - Install: https://rustup.rs
- Recommended components:
  - `rustfmt` (formatter)
  - `clippy` (linter)
- Optional but helpful:
  - `just` (command runner) if you like, though not required
  - Nix (dev shell) — see below
  - Docker (containerized build) — see below

Ensure the toolchain is up to date:

```sh
rustup update
rustup component add rustfmt clippy
```


## Quick start

1. Fork and clone the repository.
2. Create a feature branch:
   ```sh
   git checkout -b feat/my-change
   ```
3. Build the workspace:
   ```sh
   cargo build --workspace
   ```
4. Run the linter and formatter checks:
   ```sh
   cargo fmt --all
   cargo clippy --workspace --all-targets --all-features -- -D warnings
   ```
5. Run tests:
   ```sh
   cargo test --workspace
   ```
6. Run the CLI locally (example):
   ```sh
   cargo run -p tidal-cli -- --help
   ```


## Development workflow

- Small, focused PRs are easier to review and merge.
- Add tests for new behavior and bug fixes.
- Keep public APIs documented. Prefer `#[deny(missing_docs)]` in new crates/modules when practical.
- Keep the code idiomatic: favor `clippy` suggestions when they improve readability or safety.
- Prefer zero‑cost abstractions and avoid premature optimization; benchmark when performance matters.


## Formatting, linting, and tests

- Format everything before committing:
  ```sh
  cargo fmt --all
  ```
- Lint with clippy and treat warnings as errors locally:
  ```sh
  cargo clippy --workspace --all-targets --all-features -- -D warnings
  ```
- Run the full test suite:
  ```sh
  cargo test --workspace
  ```
- Some crates may have examples. You can run them like:
  ```sh
  cargo run -p tidal-cli --example <name>
  ```


## Adding or changing crates

- Workspace membership is defined in the root `Cargo.toml` under `[workspace].members`.
- To create a new crate inside the workspace:
  ```sh
  cargo new crates/my-new-crate --lib
  # or for a binary crate
  cargo new crates/my-tool --bin
  ```
- Then add `"crates/my-new-crate"` to `[workspace].members` in the root `Cargo.toml`.
- Prefer reusing shared types from existing crates when it keeps layers clean.
- Keep crate boundaries coherent (lexer vs parser vs runtime, etc.).


## Dependency management

- Prefer minimal, well‑maintained dependencies.
- Use exact features; avoid `default-features = true` if you don’t need them.
- Keep MSRV aligned with `stable` unless otherwise noted.
- If a dependency increases compile times significantly, consider gating behind a feature.


## Commit messages and branching

We use Conventional Commits for clarity and automated tooling compatibility:

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

Common types: `feat`, `fix`, `docs`, `refactor`, `perf`, `test`, `build`, `ci`, `chore`.

Examples:

- `feat(cli): add run subcommand`
- `fix(parser): handle unterminated string literal`
- `docs: document VM bytecode format`

Branching tips:
- Use short, descriptive names like `feat/jit-const-folding` or `fix/lexer-offset`.
- Rebase on `main` to keep history tidy before merging.


## Running with Nix (optional)

This repo includes `flake.nix` and `shell.nix`. If you use Nix:

```sh
# With flakes enabled
nix develop
# or classic
nix-shell
```

That should provide a reproducible dev shell with the necessary toolchain(s).


## Docker (optional)

A `Dockerfile` is provided. You can build and run the project in a container as needed:

```sh
docker build -t tidal-dev .
# Run tests inside
docker run --rm -it -v "$PWD":/work -w /work tidal-dev cargo test --workspace
```


## Documentation

- Update `README.md` when user‑visible behavior changes.
- Inline documentation (`///` doc comments) should explain non‑obvious behavior and public APIs.
- Consider adding module‑level docs for complex areas like the parser, type checker, VM, and JIT.

To build and view docs locally:

```sh
cargo doc --workspace --no-deps --open
```


## Testing guidance

- Prefer unit tests colocated with the code they cover.
- For cross‑crate integration, add tests in the most appropriate crate or consider a dedicated integration test crate.
- Tests should be deterministic. Avoid reliance on wall‑clock timing or randomness without seeding.

Run tests with additional output:

```sh
cargo test --workspace -- --nocapture
```


## Releases

- We follow semantic versioning.
- The repository includes a `VERSION` file; when preparing a release, update it using `MAJOR.MINOR.PATCH`.
- Ensure CHANGELOG or release notes summarize notable changes, migration steps, and deprecations.


## Security

Please see our Security Policy in [SECURITY.md](SECURITY.md) for how to report vulnerabilities and how we handle them.

In short, do not open public issues for security problems — use the private channels described there so we can coordinate a fix and responsible disclosure.


## License

By contributing to this repository, you agree that your contributions will be licensed under the terms of the repository’s [LICENSE](LICENSE).


## Getting help

If you’re unsure about anything:
- Open an issue with the tag `question` or `help wanted`.
- Start a discussion or a draft PR early for architectural feedback.

Thanks again for helping improve Tidal!