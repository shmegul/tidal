FROM debian:bookworm-slim

LABEL maintainer="tidal-dev"
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gcc \
    g++ \
    gdb \
    cmake \
    pkg-config \
    make \
    git \
    curl \
    wget \
    file \
    hyperfine\
    openssl \
    libssl-dev \
    zlib1g-dev \
    ca-certificates \
    fish \
    && curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain stable \
    && . "$HOME/.cargo/env" \
    && rustup component add rustfmt clippy rust-analyzer \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV PATH="/root/.cargo/bin:${PATH}"

ENV RUST_BACKTRACE=1
ENV RUST_LOG=info

RUN echo "Rust: $(rustc --version)" && \
    echo "Cargo: $(cargo --version)" && \
    echo "GCC: $(gcc --version | head -1)"

WORKDIR /workspace

CMD ["fish"]
