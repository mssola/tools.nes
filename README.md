Utilities for NES/Famicom development. Note that this repository is under heavy
development and it's **not** currently usable.

## `nasm`

`nasm` is an assembler specifically tailored for the NES/Famicom. You can simply
run it like so:

```
$ cargo run --bin nasm <path-to-assembly-file>
```

By default it will assume the configuration for an `NROM` file, but this can be
changed with the `-c/--configuration` flag, which accepts the following values:

- `empty`: just `HEADER` and `CODE`. Useful for one-liners (e.g. "how is this
  instruction encoded in binary?").
- `nrom`: the default configuration. It has the following segments: `HEADER`,
  `CODE`, `VECTORS`, and `CHARS`.
- `nrom65`: same as `nrom` but it also has `STARTUP` for compatibility with the
  default linker configuration from [cc65](https://github.com/cc65/cc65).
