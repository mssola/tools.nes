#!/usr/bin/env sh

set -ex

export RUSTFLAGS="-Dwarnings"

cargo build --all --all-targets --all-features
cargo clippy --all-targets --all-features
cargo test --all-targets --all-features -- --nocapture

./scripts/test-e2e.sh
./scripts/fuzz.sh
