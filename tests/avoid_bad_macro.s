.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

.ifndef __NASM__
    .macro __fallthrough__ arg
        lda #0
    .endmacro
.endif

lda #1
