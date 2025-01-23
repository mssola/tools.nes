Tools I have created to help with NES/Famicom development.

**Note**: this is under heavy development and it's not currently usable.

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

Moreover, you can actually tell `nasm` to redirect the output to stdout instead
with the `--stdout` flag. This is useful when debugging the binary format with
another CLI tool. For example:

```
$ nasm --stdout awesome.s | hexdump -C
```

By default it will assume the configuration for an `NROM` mapper, but this can
be changed with the `-c/--configuration` flag, which accepts the following
values: `empty`, `nrom`, `nrom65`, `unrom`, `uxrom` and `mmc1`. These values
correspond to the configurations [already bundled](./lib/xixanta/src/mappings)
on this application, which follow a simplified syntax from the
[ld65](https://www.cc65.org/doc/ld65-5.html) one. Alternatively, you can also
pass the path to a configuration file to this flag. This file can either follow
the same [ld65](https://www.cc65.org/doc/ld65-5.html) syntax, or the simplified
one.

## `xa65`

Since `nasm` is still under heavy development, it's a good idea to compare the
results that it produces with a mature and stable assembler like
[cc65](https://github.com/cc65/cc65). The purpose of `xa65` is to provide a
bridge, and so it simply executes both `nasm` and `cc65` with the given
arguments. If the results from both assemblers are not the same, then it will
display a warning and produce the binary as taken from `cc65`.

## `readrom`

The `readrom` program reads a given ROM file and shows all the information that
can be gathered from it. For now this only applies to information on the header,
but in the future we might want to add disassembling user-specified segments,
for example.

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
