.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CHARS"
.byte 0

.segment "CODE"
;; asan:stack full

.proc reset
    brk
    .byte $42
    rts
.endproc

.proc irq
    pha
    txa
    pha
    tya
    pha
    php

    lda #1
    ldy #2
    ldx #$FF
    inx
    inx

    plp
    pla
    tay
    pla
    tax
    pla

    rti
.endproc

.segment "VECTORS"
    .addr reset, reset, irq
