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
;; asan:stack full

.proc reset
    brk
    .byte $42
    rts
.endproc
