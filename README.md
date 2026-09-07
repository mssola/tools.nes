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

`readrom` is an objdump-like utility that reads a given ROM file and shows
information about it. Read more about it in
[./crates/readrom/README.md](./crates/readrom/README.md).

## `runrom`

`runrom` is an NES/Famicom emulator that doesn't attempt to run a ROM
graphically. Instead, it just runs code and exposes the data on memory,
registers, etc. for a given run. This is all done via the [vnf](./lib/vnf)
library, which allows a developer to programatically run a ROM file from a given
address, poke memory addresses, submit joypad inputs, etc. For some uses,
running `runrom` will be fine to get a glimpse of the execution of a piece of
code, but in some other cases (e.g. unit tests for a specific function on your
NES game), using `vnf` will be a better fit. In any case, you can read more
about all of this on [runrom's documentation](./crates/runrom/README.md), and on
[vnf's documentation](./lib/vnf/README.md).

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
