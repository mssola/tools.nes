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

;; From: https://git.mssola.com/nes/jetpac.nes/.
.scope Globals
    ;; | Bit | Short name       | Meaning                  |
    ;; |-----+------------------+--------------------------|
    ;; |   7 | enabled          | Multiplayer enabled.     |
    ;; | 6-3 | -                | Unused.                  |
    ;; |   2 | player's 2 state | 0: over; 1: alive        |
    ;; |   1 | player's 1 state | 0: over; 1: alive        |
    ;; |   0 | active           | 0: player 1; 1: player 2 |
    zp_multiplayer = $27

    ;; Extra bitmap that was needed beyond the ones that we already have. Yeah,
    ;; I know, bad planning from my side, but now it's a bit complex to untangle
    ;; variables like 'Globals::zp_flags'.
    ;;
    ;; | Bit | Short name       | Meaning                          |
    ;; |-----+------------------+----------------------------------|
    ;; |   7 | score            | The score has been updated.      |
    ;; |   6 | high             | The high score has been updated. |
    ;; | 5-0 | -                | Unused.                          |
    zp_extra_flags = $28
.endscope

;; From: https://git.mssola.com/nes/jetpac.nes/.
.scope Score
    ;; Scores for both players are stored in a single buffer. Even indeces
    ;; contain digits for the first player, and odd indeces contain digits for
    ;; the second player. Digits are stored in little-endian format.
    ;;
    ;; Interweaving digits this way might seem weird, but it actually makes
    ;; indexing things super easy: the 'active' bit from
    ;; 'Globals::zp_multiplayer' can be used to index the first item, and then
    ;; it's a matter of advancing the 'x' register twice in order to get the
    ;; next digit.
    PLAYERS_BUFF_SIZE = $0C
    m_players = $300     ; asan:reserve PLAYERS_BUFF_SIZE

    ;; The high score for this session.
    m_hi = $30C           ; asan:reserve $06
.endscope

;; This is basically Score::save_hi_score() from
;; https://git.mssola.com/nes/jetpac.nes/.
.proc reset
    ;;;
    ;; Check player 1.

    ldx #(Score::PLAYERS_BUFF_SIZE - 2)
    ldy #5

@player1_loop:
    lda Score::m_hi, y
    cmp Score::m_players, x
    bcc @save_player1
    dex
    dex
    dey
    cpy #$FF
    bne @player1_loop

    ;; No dice! If we are in multiplayer mode, then check player 2,
    ;; otherwise just quit.
    bit Globals::zp_multiplayer
    bpl @end

    ;;;
    ;; Check player 2.

    ldx #(Score::PLAYERS_BUFF_SIZE - 1)
    ldy #5

@player2_loop:
    lda Score::m_hi, y
    cmp Score::m_players, x
    bcc @save_player2
    dex
    dex
    dey
    cpy #$FF
    bne @player2_loop

    ;; Not player 2 either. Just quit.
    rts

    ;;;
    ;; One of the players actually achieved a high score. Let's save it.

@save_player1:
    ldx #0
    beq @save
@save_player2:
    ldx #1

@save:
    ldy #0
@save_loop:
    lda Score::m_players, x
    sta Score::m_hi, y

    inx
    inx
    iny
    cpy #6
    bne @save_loop

    ;; And set the 'high' bit so this change is reflected on screen.
    lda Globals::zp_extra_flags
    ora #$40
    sta Globals::zp_extra_flags

@end:
    rts
.endproc
