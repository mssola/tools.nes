Utilities for NES/Famicom development. Note that this repository is under heavy
development and it's **not** currently usable.

## `nasm`

`nasm` is an assembler specifically tailored for the NES/Famicom. Build it and
run it with cargo, or you can install it somewhere in your `$PATH` and simply:

```
$ nasm awesome.s
```

This will produce an `out.nes` file placed under the same working directory. You
can change the name of the file with the `-o/--output` flag. Hence, you can call
it like so:

```
$ nasm -o awesome.nes awesome.s
```

Last but not least, you can actually tell `nasm` to redirect the output to
stdout instead with the `--stdout` flag. This is useful when debugging the
binary format with another CLI tool. For example:

```
$ nasm --stdout awesome.s | hexdump -C
```

By default it will assume the configuration for an `NROM` mapper, but this can
be changed with the `-c/--configuration` flag, which accepts the following
values:

- [empty](./lib/xixanta/src/mappings/empty.toml): just `HEADER` and `CODE`.
  Useful for one-liners (e.g. "how is this instruction encoded in binary?").
- [nrom](./lib/xixanta/src/mappings/nrom.toml): the default configuration.
  It has the following segments: `HEADER`, `CODE`, `VECTORS`, and `CHARS`.
- [nrom65](./lib/xixanta/src/mappings/nrom65.toml): same as `nrom` but it
  also has `STARTUP` for compatibility with the default linker configuration
  from [cc65](https://github.com/cc65/cc65).
- [unrom or uxrom](./lib/xixanta/src/mappings/unrom.toml): a configuration for
  UxROM chips, with seven swappable banks bound at 0x8000 and 16KB of size
  (`PRG0`..`PRG6`), and a fixed bank on 0xC000 and 16KB of size as well
  (`FIXED`).

Alternatively, you can also pass a path to a configuration of your own. Check
out the [ones already bundled](./lib/xixanta/src/mappings) on this application
for reference.

## `readrom`

The `readrom` reads a given ROM file and shows all the information that can be
gathered from it. For now this only applies to information on the header, but in
the future we might want to add disassembling user-specified segments, for
example.

## License

This repository holds two licenses, as you can also note on the `Cargo.toml`
file. As it's written there:

- The source code on the `crates/` directory is licensed under the GNU GPLv3 (or
  any later version).
- The source code on the `lib/` directory is licensed under the GNU LGPLv3 (or
  any later version).

In practice, for the libraries under `lib/` this means that if you plan to
compile your binary statically, you still need to abide by the LGPLv3+ license.
This means at least providing the object files necessary to allow someone to
recompile your program using a modified version of these libraries. See the
LGPLv3 license for more details.
