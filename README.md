Tools I have created to help with NES/Famicom development.

**Note**: this is under heavy development and it's not currently usable.

## `nasm`

`nasm` is an assembler specifically tailored for the NES/Famicom. Read more
about it in [./crates/nasm/README.md](./crates/nasm/README.md).

## `xa65`

Since `nasm` is still under heavy development, it's a good idea to compare the
results that it produces with a mature and stable assembler like
[cc65](https://github.com/cc65/cc65). The purpose of `xa65` is to provide a
bridge, and so it simply executes both `nasm` and `cc65` with the given
arguments. If the results from both assemblers are not the same, then it will
display a warning and produce the binary as taken from `cc65`. Moreover, if you
want this warning to be an error instead, then use the `--no-errors`.

Last but not least, you can also pass flags to `xa65` like `--strict`, which
will invoke `nasm` with more pedantic features like its address sanitizer.

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
