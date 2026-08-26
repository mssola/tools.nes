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
    jsr foo
    rts
.endproc

.proc foo
    ldx #0
@loop:
    inx
    bne @loop

@end_loop:
    rts
.endproc

.segment "VECTORS"
    .addr reset, reset, reset
