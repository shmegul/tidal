
# USAGE INSTRUCTIONS:
# 1. Ensure Nix and Flakes are enabled on your system.
# 2. Navigate to the project root directory.
# 3. Activate the development environment using the command:
#    nix develop

{
  description = "A reproducible development environment for the Tidal Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShell = pkgs.mkShell {
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
          ];

          shellHook = ''
            echo "Rust:  $(rustc --version)"
            echo "Cargo: $(cargo --version)"
            echo "CGG:   $(gcc --version | head -1)"
          '';

          RUST_BACKTRACE = "1";
          RUST_LOG = "info";
        };
      }
    );
}