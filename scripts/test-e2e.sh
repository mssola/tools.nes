#!/usr/bin/env sh

set -ex

pushd tests
rm -rf out/
mkdir -p out/

rm -rf code.nes
tar xzf code.nes.tar.gz
popd

cargo build

./target/debug/nasm -c nrom -Werror -o tests/out/sprite.nes tests/code.nes/basics/sprite.s
diff tests/out/sprite.nes tests/code.nes/out/basics/sprite.nes

./target/debug/nasm -c unrom -Werror -o tests/out/chr-ram.nes tests/code.nes/basics/chr-ram.s
diff tests/out/chr-ram.nes tests/code.nes/out/basics/chr-ram.nes

./target/debug/nasm -c nrom -Werror -o tests/out/flicker.nes tests/code.nes/basics/flicker.s
diff tests/out/flicker.nes tests/code.nes/out/basics/flicker.nes

./target/debug/nasm -c nrom -Werror -o tests/out/space.nes tests/code.nes/space/src/space.s
diff tests/out/space.nes tests/code.nes/out/space/space.nes
