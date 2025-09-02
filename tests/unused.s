.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $00
    .byte $00

.segment "CODE"

zp_unused  = $00
zp_used    = $01
zp_another = $02                ; asan:ignore
zp_more    = $03
zp_yet     = $04

lda zp_used
lda (zp_more), y
lda #.lobyte(zp_yet)
