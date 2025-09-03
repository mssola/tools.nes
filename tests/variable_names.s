.segment "HEADER"
    .byte 'N', 'E', 'S', $1A
    .byte $02
    .byte $01
    .byte $12                   ; let's fake working RAM
    .byte $00

.segment "CODE"

zp_good  = $00
zp_bad   = $0300
m_good   = $0200
m_bad    = $02
wr_bad   = $01
wr_good  = $6001
wr_out   = $0400

;; Just so we don't get "unused" warnings
lda zp_bad
lda zp_good
lda m_bad
lda m_good
lda wr_bad
lda wr_good
lda wr_out
