.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

.macro JAL ADDR
    jmp ADDR
.endmacro

.macro DEC_MOVEMENT_X ADDR
    lda ADDR
    rts
.endmacro

.proc bounce
    rts
.endproc

.proc chase
    JAL bounce
@wait:
    DEC_MOVEMENT_X #1
    DEC_MOVEMENT_X #2
    JAL bounce
    DEC_MOVEMENT_X #3
    JAL chase
    bne @wait
    rts
.endproc

.macro NEXT_EXPLOSION_INDEX_X
    inx
    inx
    inx
.endmacro

__fallthrough__ Explosions::init

.scope Explosions
    .proc init
    @loop:
        NEXT_EXPLOSION_INDEX_X
        bne @loop

        rts
    .endproc
.endscope

;;; asan:stack full
