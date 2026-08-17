This is yet another NES/Famicom emulator. Only this time around it's
specifically tailored to NES/Famicom developers, not players.

First of all, the `vnf` library used for this binary exposes the virtual machine
with a proper interface. This way, you can run ROM files programmatically. Then,
`runrom` is just a wrapper on top of this library with a set of options that
toggle certain features from it. This is a nice thing to have if you don't need
to write very specific conditions with a tailored program. Second of all,
`runrom` runs with no graphics nor sound. That is, it runs headless. Thus, it
can be run on your testing infrastructure, so you can run continuous integration
on critical paths from your games.

## Basic usage

You can run a ROM file by simply:

```
$ runrom <your-game-path>/game.nes
```

This will display all of the instructions being run. By default it will run from
the reset vector. You can change that with the `-s/--start` option, which
accepts a 16-bit address from where to start execution.

That being said, most of the times you want to test a specific function. For
that, you can toggle the `-f/--function` option, which tells `runrom` that the
address is just a function and, whenever a top-level `rts`/`rti` instruction is
found, then execution can be halted.

Moreover, you may also find interesting the `-d/--dump-memory` option, which
will display a summary of memory addresses which have been updated along
execution, and some statistics about them.
