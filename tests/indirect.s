.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"

zp_something = $20              ; asan:reserve $02

lda #<lala
sta zp_something
lda #>lala
sta zp_something + 1
jmp (zp_something)

lala:
    rts
