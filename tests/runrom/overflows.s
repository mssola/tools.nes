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

zp_buffer = $00                 ; asan:reserve $10

.proc reset
    ;; Fine.
    ldx #0
    lda zp_buffer, x

    ;; NOTE: overflow!
    dex
    lda zp_buffer, x

    ;; Just so there are no "unused" warnings :)
    jsr overflow_on_arithmetics
    jsr stack_underflow
    jsr bad_rom_data_indexing

    rts
.endproc

.proc overflow_on_arithmetics
    ;; NOTE: overflow!
    ;; NOTE: another overflow scenario could be 'lda zp_buffer + 16, x', but
    ;; 'nasm' can statically point this out and it will throw an error.
    ldx #1
    lda zp_buffer + 15, x

    rts
.endproc

;; asan:stack $FD-$FF
.proc stack_underflow
    lda #0

    ;; NOTE: stack underflow!
    pha
    pla

    rts
.endproc

.proc bad_rom_data_indexing
    ;; Fine.
    ldx #0
    lda data, x

    clc
    adc #4
    tax

    ;; NOTE: overflow!
    lda data, x

    rts
data:
    .byte $00, $01, $02, $03

    lda zp_buffer
.endproc
