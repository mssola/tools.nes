.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CHARS"
.byte 0

.segment "VECTORS"
    .addr reset, reset, reset

.segment "CODE"

reset:
    lda #1
    sta $00

    sec
    adc $00
    ldx #$FF
    inx
    inx
    sta $00, x

    clc
    adc $00
    sta $02

    txa
    sec
    sbc $00
    sta $03

    sec
    rol $03
    sta $04

    sec
    ror $02
    sta $05

    asl
    sta $06

    ldy #1
    tya
    and $00, x
    sta $07

    ora $00
    sta $08

    inc $00
    eor $00
    sta $09

    lda #$FF
    clc
    adc #1
    sta $0A

    lda #$00
    sec
    sbc #1
    sta $0B

    rts
