.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"

.proc foo
    lda #0
    __fallthrough__ bar
.endproc

__fallthrough__ bar
__fallthrough__ other
 lda #0

.proc bar
    lda #0
    __fallthrough__ other
.endproc

.proc other
    rts
.endproc

__fallthrough__ other
