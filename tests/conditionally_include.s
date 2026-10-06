.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"

;;; asan:stack full

.ifdef DEBUG
    .include "code_to_include.s"
    jsr foo
.else
    nop
.endif
