.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02, $01
    .byte $00
    .byte $00

.segment "CODE"

;;; asan:stack full

.macro BCD_ADD ADDR, OTHER
    adc ADDR
    bcc :+
    nop
:
.endmacro

.proc add_to_player_y
    BCD_ADD additions, 2

    rts

additions:
    .byte $FF
.endproc

jsr add_to_player_y
