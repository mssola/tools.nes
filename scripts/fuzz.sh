#!/usr/bin/bash

set -ex

pushd lib/xixanta
cargo +nightly fuzz run fuzz-target-parser -- -max_total_time=180
cargo +nightly fuzz run fuzz-target-assembler -- -max_total_time=180
popd

pushd lib/header
cargo +nightly fuzz run fuzz-target-header -- -max_total_time=180
popd
