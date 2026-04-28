.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"

__fallthrough__ foo

.proc foo
    lda #0
    __fallthrough__ bar
.endproc

__fallthrough__ bar

.proc bar
    lda #0
    __fallthrough__ other
.endproc

.proc other
    rts
    __fallthrough__ Scope::foo
.endproc

.scope Scope
    .proc foo
        __fallthrough__ Scope::bar
    .endproc

    .proc bar
        rts
    .endproc
.endscope

;;; asan:stack full
