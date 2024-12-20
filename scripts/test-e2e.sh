#!/usr/bin/env sh

set -ex

rm -rf tests/out/
mkdir -p tests/out/

cargo build

./target/debug/nasm -c nrom -Werror -o tests/out/sprite.nes tests/src/sprite.s
diff tests/out/sprite.nes tests/bin/sprite.nes
