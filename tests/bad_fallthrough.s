.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"

.proc foo
    lda #0
    .fallthrough bar
.endproc

.fallthrough bar
.fallthrough other
 lda #0

.proc bar
    lda #0
    .fallthrough other
.endproc

.proc other
    rts
.endproc

.fallthrough other
