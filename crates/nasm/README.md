## Usage

The most basic way to use this assembler is by running:

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

The syntax for this assembler is virtually the same as the one for
[ca65](https://cc65.github.io/doc/ca65.html), even if some functions might be
missing. One difference that you will find in contrast with `ca65` is that
`nasm` is a bit more pedantic. Let's consider the following example:

```asm
.scope Scope
  .macro MACRO
    lda #0
  .endmacro
.endscope
```

Here `ca65` will place `MACRO` on the global scope. `nasm` will do the same but
it will also print a warning telling the programmer about this, as this might be
unexpected at first. Hence, as a general rule `nasm` will be more noisy than
`ca65`, in the hope that the programmer is more aware about the end result.

That being said, `nasm` is at the same time more flexible than `ca65`. For
exemple, the order of declaration for variables, proc's, etc. is not
important. Hence, the following code is valid in `nasm`, but not in `ca65`:

```asm
.proc foo
  ;; 'ca65' will complain that 'Variable' is not known.
  lda #Variable
  rts
.endproc

Variable = $00
```

## Mappers

By default `nasm` will assume the configuration for an `NROM` mapper, but this
can be changed with the `-c/--configuration` flag, with which you can pass the
path for the linker configuration file you'd like to use. This file can either
follow the same [ld65 syntax](https://www.cc65.org/doc/ld65-5.html), or a
simplified one (check out some samples for the "simplified" syntax
[here](../../lib/xixanta/src/mappings)).

All of that being said, and out of convenience, this flag also accepts this set
of **values**: `empty`, `nrom`, `nrom65`, `unrom`, `uxrom` and `mmc1`. These
values correspond to the configurations [already
bundled](../../lib/xixanta/src/mappings) on this application.

## Defining global values from the command line

You can define global values with the `-D` flag which follows a `NAME=VALUE`
syntax. Note that the value is expected to be in decimal format and it has to
fit in a byte. Hence, you could have a code like follows:

```asm
.ifdef PAL
  lda #1
.else
  lda #0
.endif
```

If you compile the code with `-D PAL=1`, then the first branch will be taken
instead of the second one.

## Address sanitizer

This assembler comes with a set of tools that builds up an "address
sanitizer". Some of its functionality is already included, while some other is
opt-in via a special command line option.

### Reserved memory

This assembler can detect the memory regions being used and make decisions out
of it. This comes with a few gotchas that the programmer has to be aware in
order for the assembler to be useful. Because of this, the tooling around this
detection is behind the `-a/--asan` flag.

The address sanitizer will blindly follow the naming conventions from
[style.nes](https://github.com/mssola/style.nes), and assume at first that each
variable takes 1 byte exactly. Hence, in order to reserve one byte, you can
simply:

```asm
zp_variable = $20
```

Then the address sanitizer will assume that you are reserving a byte at address
`$20` which will be used throughout the code. If you want to reserve more than a
byte, then:

```asm
zp_buffer = $20   ; asan:reserve $0F
```

Then the address sanitizer will assume that `zp_buffer = $20..$2F
(included)`. Any access to this reserved range that is not strictly via
`zp_buffer` will be considered a conflict. For example:

```asm
zp_buffer = $20   ; asan:reserve $0F
zp_bad    = $22   ; NOTE: The address sanitizer will mark it as a *conflict*.
```

If you want to ignore this (e.g. you are using a variable that shadows other
ones), then you can explicitely tell the address sanitizer to ignore a given
assignment or instruction:

```asm
zp_buffer = $20   ; asan:reserve $0F
zp_good   = $22   ; asan:ignore
lda $200          ; asan:ignore
```

That is, in order for the address sanitizer to be successful, it will also warn
programmers whenever an access to memory is being done without using
variables. This allows for the address sanitizer to be more thorough, even if
there is never the guarantee that memory accesses will be safe. For example:

```asm
zp_variable = $20

ldx #20
lda zp_variable, x
```

This will access memory far beyond to `zp_variable` which was only reserving a
single byte. This is beyond the scope of this tool and other tools should be
used instead (e.g. an emulator with breakpoints on accesses to unexpected memory
regions, or `vnf` from this project).

On another note, the address sanitizer is also able to do some basic bound
checks. For example:

```asm
zp_variable = $20     ; asan:reserve $02

lda zp_variable       ; good
lda zp_variable + 1   ; good: arithmetics within the reserved limits.
lda zp_variable + 2   ; bad: arithmetics that would point to out of bounds.
lda zp_variable - 1   ; bad: same as before but wrapping around.
```

With all of this, `nasm` will be able to tell how much memory you've been using
so far, and if you add the `--write-info` option, you will also know how this
memory is laid out by reading the `.nasm/memory.txt` file. Couple that with the
`--stats` option, and for a given project you will be able to know:

1. How much do you have left in memory and in your ROM segments.
2. You have no conflicting variables or variables that shadow others in
   unexpected ways.
3. Where exactly you are reserving RAM and ROM space.
4. Basic arithmetics don't make you fall out of bounds.

### Working RAM

In NES/Famicom programs you have to advertise on the header whether Working RAM
is being used or not (see [byte 6 on the iNES
format](https://www.nesdev.org/wiki/INES)). The programmer is expected to set
this flag on if PRG RAM is available. If there are memory accesses to PRG RAM
but the programmer did not advertise that on the header flag, then `nasm` will
error out as it cannot assume that these accesses are valid. This check will
happen regardless of the `-a/--asan` flag.
