.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

CONST = $02

zp_var         = $00    ; asan:reserve CONST
zp_another     = $01
zp_yet_another = $02
zp_and_yet     = $03

lda zp_var
lda zp_another
lda zp_yet_another
lda zp_and_yet
