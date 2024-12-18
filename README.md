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

- `empty`: just `HEADER` and `CODE`. Useful for one-liners (e.g. "how is this
  instruction encoded in binary?").
- `nrom`: the default configuration. It has the following segments: `HEADER`,
  `CODE`, `VECTORS`, and `CHARS`.
- `nrom65`: same as `nrom` but it also has `STARTUP` for compatibility with the
  default linker configuration from [cc65](https://github.com/cc65/cc65).

## `readrom`

The `readrom` reads a given ROM file and shows all the information that can be
gathered from it. For now this only applies to information on the header, but in
the future we might want to add disassembling user-specified segments, for
example.
