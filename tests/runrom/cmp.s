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

zp_mem = $00

.proc reset
    lda #0
    sta zp_mem

    cmp zp_mem

    inc zp_mem
    cmp zp_mem

    lda #2
    cmp zp_mem

    lda #1
    cmp zp_mem

    lda #$FF
    cmp zp_mem

    rts
.endproc
