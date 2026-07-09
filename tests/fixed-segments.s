;;; asan:fixed-segments BANK0, FIXED

.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $08                   ; 128KB  of PRG-ROM (8 x 16KB)
    .byte $00                   ; No CHR-ROM. Games using this chip used RAM instead.

    ;; On the 6th byte we have to set the lower nibble of the mapper (#%0010 for
    ;; UNROM).
    .byte $20

    ;; Forcing iNES 2.0 format, which will help us for the next bytes.
    .byte $08

    ;; And now iNES 2.0-specific thingies.
    .byte $00       ; No submapper
    .byte $00       ; PRG ROM not 4 MiB or larger
    .byte $00       ; No PRG RAM
    .byte $07       ; 8192 (64 * 2^7) bytes CHR RAM, no battery
    .byte $00       ; NTSC; use $01 for PAL
    .byte $00       ; No special PPU

.segment "VECTORS"
    .addr nmi, reset, irq

.segment "BANK0"

hello_bank0:
    lda #2
    sta $10
    rts

.segment "BANK1"

hello_bank1:
    lda #3
    sta $11
    rts

.segment "BANK2"
.byte $00

.segment "BANK3"
.byte $00

.segment "BANK4"
.byte $00

.segment "BANK5"
.byte $00

.segment "BANK6"
.byte $00

.segment "FIXED"

banktable:
  .byte $00, $01, $02, $03, $04, $05, $06

zp_current_bank = $00

bankswitch:
    sty zp_current_bank
    tya
    sta banktable, y
    rts

.proc reset
    sei
    cld
    ldx #$40
    stx $4017

    ldx #$FF
    txs

    inx
    stx $2000
    stx $2001
    stx $4010

    bit $2002
@vblankwait1:
    bit $2002
    bpl @vblankwait1

    ldx #0
    lda #0
@ram_reset_loop:
    sta $000, x
    sta $100, x
    sta $200, x
    sta $300, x
    sta $400, x
    sta $500, x
    sta $600, x
    sta $700, x
    inx
    bne @ram_reset_loop

@vblankwait2:
    bit $2002
    bpl @vblankwait2

    ;;;
    ;; NOTE: configuration/reset is done, the code below is our actual program :D

    ldy #0
    jsr bankswitch
    jsr hello_bank0

    ldy #1
    jsr bankswitch
    jsr hello_bank1

    lda $10
    clc
    adc $11
    sta $12

@loop:
    jmp @loop
.endproc

.proc nmi
    rti
.endproc

.proc irq
    rti
.endproc
