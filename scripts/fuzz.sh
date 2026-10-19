#!/usr/bin/bash

set -ex

pushd lib/xixanta
cargo +nightly fuzz run fuzz-target-parser -- -max_total_time=180
cargo +nightly fuzz run fuzz-target-assembler -- -max_total_time=180
popd

pushd lib/header
cargo +nightly fuzz run fuzz-target-header -- -max_total_time=180
popd

# For vnf, we want to give it some ROM files as a corpus so it does a better job
# at fuzzing, otherwise vnf would just return a super early normal error and
# quit.
mkdir -p lib/vnf/fuzz/corpus/fuzz-target-vnf
cp tests/out/*.nes lib/vnf/fuzz/corpus/fuzz-target-vnf/
pushd lib/vnf
cargo +nightly fuzz run fuzz-target-vnf -- -max_total_time=180
popd
