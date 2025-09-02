#!/usr/bin/env sh

# Instead of setting -e or something like that, we will accumulate errors and
# exit with the accumulated result at the end.
exit_code=0

AS65=${AS65:-"./target/debug/xa65"}

##
# Ensure git submodules are there and that the environment is set up correctly.

pushd tests
rm -rf out/
mkdir -p out/

if [ ! -d code.nes ]; then
    echo "(error) You need 'code.nes' for end-to-end tests."
    exit 1
fi

if [ ! -d aoc2023.nes ]; then
    echo "(error) You need 'aoc2023.nes' for end-to-end tests."
    exit 1
fi

popd

##
# Build.

echo "(info): build development binaries"
cargo build

##
# Custom tests.

echo "test: custom => defines-undefines.nes"
./target/debug/nasm -c empty -Werror -o tests/out/defines-undefined.nes tests/defines.s
diff tests/out/defines-undefined.nes tests/expected/defines-undefined.nes
exit_code=$((exit_code + $?))

echo "test: custom => defines-one.nes"
./target/debug/nasm -D LALA -c empty -Werror -o tests/out/defines-one.nes tests/defines.s
diff tests/out/defines-one.nes tests/expected/defines-one.nes
exit_code=$((exit_code + $?))

echo "test: custom => defines-two.nes"
./target/debug/nasm -D LALA=2 -c empty -Werror -o tests/out/defines-two.nes tests/defines.s
diff tests/out/defines-two.nes tests/expected/defines-two.nes
exit_code=$((exit_code + $?))

##
# code.nes

echo "test: code.nes => basics/sprite.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/basics/sprite.s
exit_code=$((exit_code + $?))

echo "test: code.nes => basics/input.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/basics/input.s
exit_code=$((exit_code + $?))

echo "test: code.nes => basics/persist.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/mmc1.cfg -o /dev/null tests/code.nes/basics/persist.s
exit_code=$((exit_code + $?))

echo "test: code.nes => basics/flicker.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/basics/flicker.s
exit_code=$((exit_code + $?))

echo "test: code.nes => basics/unrom.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/unrom.cfg -o /dev/null tests/code.nes/basics/unrom.s
exit_code=$((exit_code + $?))

echo "test: code.nes => basics/chr-ram.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/unrom.cfg -o /dev/null tests/code.nes/basics/chr-ram.s
exit_code=$((exit_code + $?))

echo "test: code.nes => fx/blink.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/mmc3.cfg -o /dev/null tests/code.nes/fx/blink.s
exit_code=$((exit_code + $?))

echo "test: code.nes => scroll/toggle.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/scroll/toggle.s
exit_code=$((exit_code + $?))

echo "test: code.nes => scroll/level.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/scroll/level.s
exit_code=$((exit_code + $?))

echo "test: code.nes => scroll/sprite0.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/scroll/sprite0.s
exit_code=$((exit_code + $?))

echo "test: code.nes => scroll/mmc3.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/mmc3.cfg -o /dev/null tests/code.nes/scroll/mmc3.s
exit_code=$((exit_code + $?))

echo "test: code.nes => rand/rand.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/rand/rand.s
exit_code=$((exit_code + $?))

echo "test: code.nes => scroll/roulette.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/mmc3.cfg -o /dev/null tests/code.nes/scroll/roulette.s
exit_code=$((exit_code + $?))

echo "test: code.nes => space/space.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/code.nes/config/nrom.cfg -o /dev/null tests/code.nes/space/src/space.s
exit_code=$((exit_code + $?))

##
# aoc2023.nes

echo "test: aoc2023.nes => 1.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/cfg/nrom65.cfg -o /dev/null tests/aoc2023.nes/src/1.s
exit_code=$((exit_code + $?))

echo "test: aoc2023.nes => 2.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/cfg/nrom65.cfg -o /dev/null tests/aoc2023.nes/src/2.s
exit_code=$((exit_code + $?))

echo "test: aoc2023.nes => 3.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/cfg/nrom65.cfg -o /dev/null tests/aoc2023.nes/src/3.s
exit_code=$((exit_code + $?))

echo "test: aoc2023.nes => 4.nes"
$AS65 --no-errors --strict -b ./target/debug/nasm -C tests/cfg/nrom65.cfg -o /dev/null tests/aoc2023.nes/src/4.s
exit_code=$((exit_code + $?))

##
# Done!

exit $exit_code
