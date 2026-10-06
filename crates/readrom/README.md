`readrom` is an objdump-like utility that reads a given NES/Famicom ROM file and
shows information about it. You can run it by simply passing a ROM file to
it. For example, for the [Jetpac NTSC ROM
file](https://git.mssola.com/releases/jetpac.nes/v1.0/):

```
$ readrom jetpac.NTSC.nes
# => output
Header:
  Kind:             NES 2.0
  PRG ROM size:     32768 bytes (32KB)
  CHR ROM size:     8192 bytes (8KB)
  Mapper:           NROM
  Mirroring:        Horizontal
  CPU/PPU timing:   NTSC
Vectors:
  NMI:              0xa2f8
  Reset:            0xa3c2
  IRQ:              0xa3c1
```

But this utility can do some more complex things. For example, you can tell it
to disassemble a subroutine via the `-d/--disassemble` flag. If the above output
is telling us that NMI code starts at 0xa2f8, we can try:

```
$ readrom -d '$a2f8' jetpac.NTSC.nes
# => output
$A2F8:	24 20		bit $20
```

Note that hexadecimal values can also be formatted like this `0xa2fb` or simply
`a2fb`, whatever feels more convenient to use. In any case, they should be
written as a 16-bit address. This flag also accepts ranges. Hence, we can also
try the following:

```
$ readrom -d '$a2f8-$a300' jetpac.NTSC.nes
# => output
$A2F8:	24 20		bit $20
$A2FA:	30 01		bmi $01
$A2FC:	40		rti
$A2FD:	48		pha
$A2FE:	8A		txa
$A2FF:	48		pha
```

All of that being said, poking for addresses can be tedious. That's why you can
also pass names that you already know. For that, you will need to also pass the
`-n/--nasm-directory`, pointing to the path to your `.nasm/` directory. With
that, if you have assembled the ROM file with `nasm` with the `--write-info`
flag, you will have something like this:

```
# On the jetpac.nes repository
$ nasm --write-info -o jetpac.NTSC.nes src/jetpac.s
$ readrom -d nmi -n .nasm/ jetpac.NTSC.nes
# => output
$A2F8:	24 20		bit $20
$A2FA:	30 01		bmi @save_registers
$A2FC:	40		rti

  @save_registers:
$A2FD:	48		pha
$A2FE:	8A		txa
$A2FF:	48		pha
$A300:	98		tya
$A301:	48		pha
$A302:	A9 00		lda #$00
$A304:	8D 03 20	sta $2003
$A307:	A9 02		lda #$02
$A309:	8D 14 40	sta $4014
$A30C:	24 28		bit $28
$A30E:	10 03		bpl @check_pause
$A310:	20 EE 8C	jsr nmi_update_scores

  @check_pause:
$A313:	24 38		bit $38
$A315:	50 03		bvc @increase_rand
$A317:	20 62 A2	jsr nmi_hud_toggle_pause

# and much more...
```

Moreover, if you have built your ROM file with the `--asan` flag from `nasm`,
then you will have the `memory.txt` file inside of `.nasm/`. This tool is able
to pick up this file and decorate some of the values from the previous
output. Hence:

```
# On the jetpac.nes repository
$ nasm --write-info --asan -o jetpac.NTSC.nes src/jetpac.s
$ readrom -d nmi -n .nasm/ jetpac.NTSC.nes
# => output
$A2F8:	24 20		bit Globals::zp_flags
$A2FA:	30 01		bmi @save_registers
$A2FC:	40		rti

  @save_registers:
$A2FD:	48		pha
$A2FE:	8A		txa
$A2FF:	48		pha
$A300:	98		tya
$A301:	48		pha
$A302:	A9 00		lda #$00
$A304:	8D 03 20	sta OAM::m_address
$A307:	A9 02		lda #$02
$A309:	8D 14 40	sta OAM::m_dma
$A30C:	24 28		bit Globals::zp_extra_flags
$A30E:	10 03		bpl @check_pause
$A310:	20 EE 8C	jsr nmi_update_scores

  @check_pause:
$A313:	24 38		bit Driver::zp_flags
$A315:	50 03		bvc @increase_rand
$A317:	20 62 A2	jsr nmi_hud_toggle_pause

# and much more...
```

Now you can see the same output as before, but the first instruction reads as
`bit Globals::zp_flags` instead of `bit $20`, because `readrom` now knows that
the memory address `$20` is associated with this variable.

Moreover, and as you can tell, by default `readrom` will print things in a
human-readable format. But you can also tell it to just write all the bytes with
the `--raw` flag so you can further manipulate the related bytes with another
tool. For example:

```
# On the jetpac.nes repository
$ nasm --write-info -o jetpac.NTSC.nes src/jetpac.s
$ readrom -d nmi -n .nasm/ --raw jetpac.NTSC.nes | hexdump -C
# => output
00000000  24 20 30 01 40 48 8a 48  98 48 a9 00 8d 03 20 a9  |$ 0.@H.H.H.... .|
00000010  02 8d 14 40 24 28 10 03  20 ee 8c 24 38 50 03 20  |...@$(.. ..$8P. |
00000020  62 a2 e6 0a a9 08 25 20  d0 6e 24 2e 10 0b a9 00  |b.....% .n$.....|
00000030  50 02 a9 70 85 04 20 bc  a2 a5 50 29 08 f0 34 2c  |P..p.. ...P)..4,|
00000040  02 20 a9 28 8d 06 20 a9  4b 8d 06 20 a5 53 18 69  |. .(.. .K.. .S.i|
00000050  10 8d 07 20 24 27 10 15  2c 02 20 a9 28 8d 06 20  |... $'..,. .(.. |
00000060  a9 56 8d 06 20 a5 54 18  69 10 8d 07 20 a5 50 29  |.V.. .T.i... .P)|
00000070  f7 85 50 a5 20 aa 29 20  f0 13 a5 cb d0 06 20 78  |..P. .) ...... x|
00000080  89 4c 7f a3 20 59 91 a5  20 29 df 85 20 8a 29 01  |.L.. Y.. ).. .).|
00000090  d0 06 a5 30 f0 02 c6 30  24 20 50 13 a9 bf 25 20  |...0...0$ P...% |
000000a0  85 20 2c 02 20 a5 81 8d  01 20 a5 80 8d 00 20 2c  |. ,. .... .... ,|
000000b0  02 20 a9 00 8d 05 20 8d  05 20 20 1d 8b a9 7f 25  |. .... ..  ....%|
000000c0  20 85 20 68 a8 68 aa 68  40                       | . h.h.h@|
```

## Support for Memory Mapper Chips

All of the above is fine but it only applies safely to NROM. That is, whenever
the cartridge fits neatly on the basic 40KB size. But if the cartridge goes
beyond that thanks to a Memory Mapper Chip, then it will happen that the code
references addresses which might be from different banks of memory.

For now, only [MMC3](https://www.nesdev.org/wiki/MMC3) is supported, and in
order for things to work you have to provide the `--mmc` flag, which accepts a
string with the following format: `kind=mmc3; prg_rom_mode=<0|1>; r6=<int>;
r7=<int>`. That is, you need to specify:

- How banks are going to be mapped via the `prg_rom_mode` bit.
- The values for the registers `r6` and `r7`.

With all of this, you can take
[fx/blink.s](https://git.mssola.com/nes/code.nes/tree/fx/blink.s) and build it
like so:

```
$ nasm -c config/mmc3.cfg --asan --write-info -o blink.nes fx/blink.s
```

And then, if you'd like to disassemble the `nmi` handler, you have to:

- Provide the path to the `.nasm/` directory as before.
- Set `prg_rom_mode` to 0, as that's the setting for this game.
- Set `r6` to 0 and `r7` to 1, as that's what the game is expecting.

Then:

```
$ readrom -d nmi -n .nasm/ --mmc "kind=mmc3; prg_rom_mode=0; r6=0; r7=1" blink.nes
# => output
$E138:	24 20		bit $20
$E13A:	10 2A		bpl @next
$E13C:	48		pha
$E13D:	8A		txa
$E13E:	48		pha
$E13F:	98		tya
$E140:	48		pha
$E141:	E6 00		inc Vars::zp_counter
$E143:	20 8B C0	jsr Diskun::nmi_update
$E146:	A9 00		lda #$00
$E148:	8D 03 20	sta $2003
$E14B:	A9 02		lda #$02
$E14D:	8D 14 40	sta $4014
$E150:	2C 02 20	bit $2002
$E153:	A9 00		lda #$00
$E155:	8D 05 20	sta $2005
$E158:	8D 05 20	sta $2005
$E15B:	A9 7F		lda #$7F
$E15D:	25 20		and $20
$E15F:	85 20		sta $20
$E161:	68		pla
$E162:	A8		tay
$E163:	68		pla
$E164:	AA		tax
$E165:	68		pla

  @next:
$E166:	40		rti

  irq:
```
