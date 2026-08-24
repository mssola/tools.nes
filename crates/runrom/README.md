This is an NES/Famicom emulator that doesn't attempt to run a ROM
graphically. Instead, it just runs code and exposes the data on memory,
registers, etc. for a given run. Thus, `runrom` is a tool to run an NES/Famicom
programatically, so developers can use it to test their ROM files under certain
conditions.

This is all supported via the [vnf](../../lib/vnf) library, which is the one
that implements the Virtual Machine and all its interfaces. The main goal is to
be able to run ROM files programmatically, either by a given amount of steps, or
via some desired conditions. In any case, `runrom` is just a wrapper on top of
this library with a set of options that toggle certain features from it. This is
a nice thing to have if you don't need to write very specific conditions with a
tailored program.

Also note that `runrom` runs with no graphics nor sound. That is, it performs a
headless run. Thus, it can be run on your non-graphical testing infrastructure,
so you can run continuous integration on critical paths from your games.

Thus, the goal is that for any given game you would:

1. Have a script that calls `runrom` in different ways to test specific
   functions on certain conditions.
2. Have more specialized programs linking [vnf](../../lib/vnf) whenever the
   conditions need to be more intricate (e.g. "after calling this function with
   this memory layout, these memory addresses are changed this way, and
   registers are left like this").
3. Have a process on CI/CD that calls all of this to validate the code.

This allows for a more robust development environment.

## Basic usage

You can run a ROM file by simply:

```
$ runrom <your-game-path>/game.nes
```

This will display all of the instructions being run.

### From where should the VM start?

By default `runrom` will start from the advertised reset vector. You can change
that with the `-s/--start` option, which accepts a 16-bit address from where to
start execution. The address can be given in hexadecimal format, but you can
also provide a full address name with the `-n/--nasm` option. With this option
you provide the location to the hidden `.nasm/` directory for the project (see
more details on [nasm's README file](../nasm/README.md)), which is then used to
translate the given identifier with the actual address. So:

```
$ runrom --start my-function --nasm <path-to-nasm-directory> game.nes
```

### When should the VM end?

A lot of times you want to test a specific function. For that, you can toggle
the `-f/--function` option, which tells `runrom` that the address is just a
function and, whenever a top-level `rts`/`rti` instruction is found, then
execution should be halted.

### Other features

Moreover, you may also find interesting the `-d/--dump-memory` option, which
will display a summary of memory addresses which have been updated along
execution, and some statistics about them.
