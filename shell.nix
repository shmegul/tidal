{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "tidal-dev";

  buildInputs = with pkgs; [
    # Rust
    rustc
    cargo
    rustfmt
    clippy
    rust-analyzer

    # C/C++ toolchain
    gcc
    gdb
    cmake
    pkg-config
    gnumake

    hyperfine

    openssl.dev
    zlib.dev

    git
    curl
    wget
    file
  ];

  shellHook = ''
    echo "Rust:  $(rustc --version)"
    echo "Cargo: $(cargo --version)"
    echo "GCC:   $(gcc --version | head -1)"
  '';

  RUST_BACKTRACE = "1";
  RUST_LOG = "info";
}
