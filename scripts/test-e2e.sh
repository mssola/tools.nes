#!/usr/bin/env sh

set -ex

pushd tests
rm -rf out/
mkdir -p out/

rm -rf code.nes
tar xzf code.nes.tar.gz

rm -rf aoc2023.nes
tar xzf aoc2023.nes.tar.gz
popd

cargo build

# Custom

./target/debug/nasm -c empty -Werror -o tests/out/defines-undefined.nes tests/defines.s
diff tests/out/defines-undefined.nes tests/expected/defines-undefined.nes

./target/debug/nasm -D LALA -c empty -Werror -o tests/out/defines-one.nes tests/defines.s
diff tests/out/defines-one.nes tests/expected/defines-one.nes

./target/debug/nasm -D LALA=2 -c empty -Werror -o tests/out/defines-two.nes tests/defines.s
diff tests/out/defines-two.nes tests/expected/defines-two.nes

# code.nes

./target/debug/nasm -c nrom -Werror -o tests/out/sprite.nes tests/code.nes/basics/sprite.s
diff tests/out/sprite.nes tests/code.nes/out/basics/sprite.nes

./target/debug/nasm -c unrom -Werror -o tests/out/chr-ram.nes tests/code.nes/basics/chr-ram.s
diff tests/out/chr-ram.nes tests/code.nes/out/basics/chr-ram.nes

./target/debug/nasm -c nrom -Werror -o tests/out/flicker.nes tests/code.nes/basics/flicker.s
diff tests/out/flicker.nes tests/code.nes/out/basics/flicker.nes

./target/debug/nasm -c nrom -Werror -o tests/out/space.nes tests/code.nes/space/src/space.s
diff tests/out/space.nes tests/code.nes/out/space/space.nes

# aoc2023.nes

./target/debug/nasm -c tests/aoc2023.nes/config/nes.cfg -o tests/out/1.nes tests/aoc2023.nes/src/1.s
./target/debug/nasm -c tests/aoc2023.nes/config/nes.cfg -o tests/out/2.nes tests/aoc2023.nes/src/2.s
./target/debug/nasm -c tests/aoc2023.nes/config/nes.cfg -o tests/out/3.nes tests/aoc2023.nes/src/3.s
./target/debug/nasm -c tests/aoc2023.nes/config/nes.cfg -o tests/out/4.nes tests/aoc2023.nes/src/4.s
