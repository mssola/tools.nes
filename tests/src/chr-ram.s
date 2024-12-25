;;; From https://github.com/mssola/code.nes which will be open sourced soon
;;; (pinky promess!).

.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $08                   ; 128KB  of PRG-ROM (8 x 16KB)
    .byte $00                   ; No CHR-ROM.

    .byte $20, $08              ; Mapper 2, horizontal mirroring, NES 2.0

    .byte $00
    .byte $00
    .byte $00
    .byte $07                   ; 8192 (64 * 2^7) bytes CHR RAM, no battery

.segment "VECTORS"
    .addr nmi, reset, irq

.segment "BANK0"

chr: .incbin "../assets/basic.chr"

;;;
;; We use the fixed bank for the core functionality.

.segment "FIXED"

;;;
;; Bank switching. Not actually used here :D

banktable:
  .byte $00, $01, $02, $03, $04, $05, $06

m_current_bank = $00

bankswitch:
    sty m_current_bank
bankswitch_nosave:
    tya
    sta banktable, y
    rts

;;;
;; From here on the code is basically the same as `sprite.s`, but with a special
;; twist that will be commented in.

reset:
    sei
    cld

    ldx #$40
    stx $4017

    ldx #$ff
    txs
    inx
    stx $2000
    stx $2001
    stx $4010

@vblankwait1:
    bit $2002
    bpl @vblankwait1

    ldx #0
    lda #0
@ram_reset_loop:
    sta $000, x
    sta $100, x
    sta $300, x
    sta $400, x
    sta $500, x
    sta $600, x
    sta $700, x
    inx
    bne @ram_reset_loop

    lda #$ef
@sprite_reset_loop:
    sta $200, x
    inx
    bne @sprite_reset_loop

    ;; NOTE: after sprites have been reset we can transfer data from PRG-ROM
    ;; into RAM so the rest of the code can assume that the data is there. This
    ;; is the main difference with the `sprite.s` example.
    jsr transfer_to_chr_ram

    lda #$00
    sta $2003
    lda #$02
    sta $4014

@vblankwait2:
    bit $2002
    bpl @vblankwait2

    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    lda #$0F
    ldx #$20
@palettes_reset_loop:
    sta $2007
    dex
    bne @palettes_reset_loop

    jmp main

;;;
;; Transfer the CHR data from PRG-ROM into RAM.
.proc transfer_to_chr_ram
    ;;;
    ;; The code is pretty much taken from the NESDev wiki (see:
    ;; https://www.nesdev.org/wiki/CHR_ROM_vs._CHR_RAM).

    ;; First we set a 16-bit pointer that points `chr`, which contains the
    ;; actual data. This pointer will be stored at $00-$01, which is probably
    ;; not ideal, but this is just an example.
    lda #<chr
    sta $00
    lda #>chr
    sta $01

    ;; Load the destination address into the PPU. We have to initialize both
    ;; pattern tables, one starting at $0000, and the other at $1000. Hence we
    ;; start by simply pointing at PPU address $0000, and the loop will simply
    ;; fill both tables from here.
    ldy #0
    sty $2006
    sty $2006

    ;; - x contains the number of 256-byte pages to copy.
    ;; - y will index within the page ($00-$FF).
    ldx #32
loop:
    ;; First part of the loop: copy each byte of the current page.
    lda ($00), y
    sta $2007
    iny
    bne loop

    ;; Go to the next page and repeat the first part of the loop.
    inc $01
    dex
    bne loop

    rts
.endproc

;;;
;; And from here on it's pretty much copied from `sprite.s`.

.proc main
    jsr init_palettes
    jsr init_nametable
    jsr init_sprites

    cli
    lda #%10110000
    sta $2000
    lda #%00011110
    sta $2001

@main_game_loop:
    lda #%10000000
    ora $20
    sta $20
@wait_for_render:
    bit $20
    bmi @wait_for_render

    jmp @main_game_loop
.endproc

.proc init_palettes
    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    ldx #0
@load_palettes_loop:
    lda palettes, x
    sta $2007
    inx
    cpx #$20
    bne @load_palettes_loop
    rts
palettes:
    .byte $0F, $12, $22, $32
    .byte $0F, $00, $28, $30
    .byte $0F, $28, $16, $2D
    .byte $0F, $28, $16, $2D

    .byte $0F, $00, $05, $30
    .byte $0F, $00, $00, $00
    .byte $0F, $00, $00, $00
    .byte $0F, $00, $00, $00
.endproc

.macro WRITE_PPU_DATA address, value
    bit $2002
    lda #.HIBYTE(address)
    sta $2006
    lda #.LOBYTE(address)
    sta $2006
    lda #value
    sta $2007
.endmacro

.proc init_nametable
    bit $2002

    WRITE_PPU_DATA $20C8, $02
    WRITE_PPU_DATA $20B9, $04
    WRITE_PPU_DATA $21CE, $04
    WRITE_PPU_DATA $21BA, $04
    WRITE_PPU_DATA $22B8, $04
    WRITE_PPU_DATA $22E7, $04
    WRITE_PPU_DATA $227A, $02

    WRITE_PPU_DATA $23CE, %00000001

    rts
.endproc

.proc init_sprites
    NUM_SPRITES = 2

    ldx #$00
@load_sprites_loop:
    lda initial_sprite_data, x
    sta $0200, x
    inx
    cpx #(4 * NUM_SPRITES)
    bne @load_sprites_loop
    rts
initial_sprite_data:
    .byte $B0, $00, %00000000, $7A
    .byte $B0, $00, %01000000, $82
.endproc

nmi:
    bit $20
    bpl @next

    pha
    txa
    pha
    tya
    pha

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    bit $2002
    lda #$00
    sta $2005
    sta $2005

    lda #%01111111
    and $20
    sta $20

    pla
    tay
    pla
    tax
    pla
@next:
    rti

irq:
    rti
