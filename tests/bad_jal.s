.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

.include "jal.s"

.proc foo
    JAL bar
.endproc

.proc bar
    rts
.endproc
