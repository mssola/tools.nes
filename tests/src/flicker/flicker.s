;;;
;; This is a simple example of sprite cycling, so sprites don't disappear on a
;; scanline overflow. This example is extremely simple, and on a real game you'd
;; take into account lots of things that would make this process less tedious
;; and less cycle consuming.
;; Taken from https://github.com/mssola/code.nes.

.segment "HEADER"
    .byte 'N', 'E', 'S', $1A

    .byte $02, $01

    .byte $00
    .byte $00

.segment "VECTORS"
    .addr nmi, reset, irq

.segment "CHARS"
.incbin "../../assets/diskun.chr"

.segment "CODE"

.include "../shared/diskun.s"

.proc main
    jsr Diskun::init_palettes
    jsr init_sprites

    cli
    lda #%10010000
    sta $2000                   ; PPUCTRL
    lda #%00011110
    sta $2001                   ; PPUMASK

@main_game_loop:
    ;; NOTE: the logic is pretty simple: read the pad, move the player
    ;; accordingly, and apply the flickering effect.
    jsr joypad_read
    jsr Diskun::update

    ;; NOTE: comment this `jsr` out if you want to see what happens if no
    ;; flickering effect is applied (spoiler alert: the last character will
    ;; suddenly disappear :P).
    ;;
    ;; NOTE: this function is called mindlessly. In a real game you'd try to
    ;; detect scanline overflows, or you will try to be more careful with sprite
    ;; priorities and apply the flickering more carefully. Here it's applied for
    ;; (almost) every sprite on each frame and we roll with it.
    jsr apply_flicker

    lda #%10000000
    ora $20
    sta $20
@wait_for_render:
    bit $20
    bmi @wait_for_render

    jmp @main_game_loop
.endproc


;;;
;; NOTE: this is the actual meat of the example :D
.proc apply_flicker
    ;; We will store a 16-bit pointer to the first sprite that can be flickered.
    ;; In this case, we will point to the first sprite from the first NPC. That
    ;; is, we suppose that, for whatever reason, we don't want to apply the
    ;; flickering effect for the player.
    lda #$10
    sta $40
    lda #$02
    sta $41

    ;;;
    ;; The algorithm is really straight-forward here: we will simply cycle the
    ;; sprites starting from the one being pointed at $40-$41 until the last
    ;; sprite. The main idea is that the OAM will show sprites on a scanline in
    ;; order. Hence, those sprites that are left beyond the eigth on a scanline
    ;; will suddenly disappear. Flickering is a technique by which sprites are
    ;; rotated from OAM priority, and hence they won't disappear always, but
    ;; just for a small amount of time: just enough to see them, creating a
    ;; flickering effect.
    ;;
    ;; For this, we will reserve four bytes (1 sprite) for auxiliary temporary
    ;; values ($42-$45) which will then be used in order to carry values along
    ;; the cycle.

    ;; We initialize the auxiliary bytes with the data from the last sprite to
    ;; be iterated since these are the values to be stored for the first sprite
    ;; now (that is, the last element is now the first one).
    lda $24C
    sta $42
    lda $24D
    sta $43
    lda $24E
    sta $44
    lda $24F
    sta $45

@loop:
    ;; For each of the four bytes from a sprite, pick the value stored on its
    ;; auxiliary byte and save the old value into the auxiliary byte afterwards.
    ;; This way the current byte holds the value from the previous sprite, but
    ;; its value will be carry on into the next sprite.
    .repeat 4, I
        ldy #I
        lda $42, y
        pha
        lda ($40), y
        sta $42, y
        pla
        sta ($40), y
    .endrepeat

    ;; And move the sprite pointer 4 bytes (1 sprite). Note that we only need to
    ;; move the low byte since we know beforehand that everything will be inside
    ;; of the $2xx range.
    lda $40
    clc
    adc #4
    sta $40

    ;; We know that the last byte from the last sprite is held at $24F. Thus, if
    ;; the sprite pointer is already passed this point, we can break the loop.
    ;; Otherwise just carry on.
    cmp #$50
    bne @loop

    rts
.endproc

;;;
;; NOTE: and from here on stuff that is not relevant for sprite flickering.

nmi:
    bit $20
    bpl @next

    pha
    txa
    pha
    tya
    pha

    jsr Diskun::nmi_update

    lda #$00
    sta $2003                   ; OAMADDR
    lda #$02
    sta $4014                   ; OAMDMA

    bit $2002                   ; PPUSTATUS
    lda #$00
    sta $2005                   ; PPUSCROLL
    sta $2005                   ; PPUSCROLL

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

reset:
    sei
    cld

    ldx #$40
    stx $4017                   ; APU Frame Counter

    ldx #$ff
    txs

    inx
    stx $2000                   ; PPUCTRL
    stx $2001                   ; PPUMASK
    stx $4010                   ; APU DMC

@vblankwait1:
    bit $2002                   ; PPUSTATUS
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
    bne @ram_reset_loop         ; if x overflows back to #00, then we are done.

    lda #$ef
@sprite_reset_loop:
    sta $200, x
    inx
    bne @sprite_reset_loop

    lda #$00
    sta $2003                   ; OAMADDR
    lda #$02
    sta $4014                   ; OAMDMA

@vblankwait2:
    bit $2002                   ; PPUSTATUS
    bpl @vblankwait2

    lda #$3F
    sta $2006                   ; PPUADDR
    lda #$00
    sta $2006                   ; PPUADDR

    lda #$0F
    ldx #$20
@palettes_reset_loop:
    sta $2007                   ; PPUDATA
    dex
    bne @palettes_reset_loop

    jmp main

irq:
    rti

.proc init_sprites
    NUM_SPRITES = 20

    lda #$40
    sta Diskun::m_screen_y
    lda #$46
    sta Diskun::m_screen_x

    ldx #$00
@load_sprites_loop:
    lda initial_sprite_data, x
    sta $0200, x
    inx
    cpx #(4 * NUM_SPRITES)
    bne @load_sprites_loop
    rts
initial_sprite_data:
    ;; $200-$20F
    .byte $40, $01, %00000000, $46
    .byte $40, $01, %01000000, $4E
    .byte $48, $11, %00000000, $46
    .byte $48, $11, %01000000, $4E

    ;; $210-$21F
    .byte $60, $01, %00000000, $68
    .byte $60, $01, %01000000, $70
    .byte $68, $11, %00000000, $68
    .byte $68, $11, %01000000, $70

    ;; $220-$22F
    .byte $60, $01, %00000000, $7A
    .byte $60, $01, %01000000, $82
    .byte $68, $11, %00000000, $7A
    .byte $68, $11, %01000000, $82

    ;; $230-$23F
    .byte $60, $01, %00000000, $8C
    .byte $60, $01, %01000000, $94
    .byte $68, $11, %00000000, $8C
    .byte $68, $11, %01000000, $94

    ;; $240-$24F
    .byte $60, $01, %00000000, $9E
    .byte $60, $01, %01000000, $A6
    .byte $68, $11, %00000000, $9E
    .byte $68, $11, %01000000, $A6
.endproc
