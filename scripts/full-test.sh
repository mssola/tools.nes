#!/usr/bin/env sh

set -ex

cargo build --all --all-targets --all-features
cargo clippy --all-targets --all-features
cargo test --all-targets --all-features -- --nocapture

pushd lib/xixanta
cargo +nightly fuzz run fuzz_target_parser -- -max_total_time=180
cargo +nightly fuzz run fuzz_target_assembler -- -max_total_time=180
popd

pushd lib/header
cargo +nightly fuzz run fuzz_target_header -- -max_total_time=180
popd
