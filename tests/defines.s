.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

.ifdef LALA
    .if LALA == 1
        lda #0
    .else
        lda #1
    .endif
.else
    lda #2
.endif
