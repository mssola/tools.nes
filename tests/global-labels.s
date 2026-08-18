.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"
;; asan:stack full

.scope Scope
    .proc foo
        lda #1                  ; $8000
        jmp Scope::bar          ; $8002
      #@known_address:
        rts                     ; $8005
    .endproc

    .proc bar
        jmp @label              ; $8006
        nop                     ; $8009
    @label:
        jmp @known_address      ; $800A
    .endproc
.endscope

jsr Scope::foo                  ; $800D
