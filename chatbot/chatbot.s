; AlexMon Monitor by Adumont
;
; Originally from @KedalionDaimon Nino Ivanov
; SRC: https://github.com/KedalionDaimon/MOS-KIM-1-chatbot/
;
; SPDX-License-Identifier: GPL-3.0-only

.pc02 ; 65C02 mode
.debuginfo      +       ; Generate debug info

.segment  "CODE"

DB_SZ = $10

_init:

; initialize knowledge base with seed data
   LDX #DB_SZ
   LDY #$00
@seed_next:
   LDA seed,y
   BEQ @zero_next
   STA DB_IN-1,x
   STA DB_OU-1,x
   INY
   DEX
   BRA @seed_next
; empty the rest of knowledge base with 00
@zero_next:
   STZ DB_IN-1,X
   STZ DB_OU-1,X
   DEX
   BNE @zero_next
; end initialization

   BRK            ; wait for user to give his first input (in INPUT) and continue execution
next:
   STZ z:SCORE
   STZ z:MATCH
   STZ z:REPLY
   JSR response   ; compute response
   BRK            ; wait for next user input
   JSR shift_kb
   ; add last interaction to KB
   LDX #DB_SZ
   LDA z:REPLY
   STA DB_IN-1,x
   LDA z:INPUT
   STA DB_OU-1,x
   BRA next

shift_kb:
   LDX #1
@next:
   LDA DB_IN,X
   STA DB_IN-1,x
   LDA DB_OU,X
   STA DB_OU-1,x
   INX
   CPX #DB_SZ
   BNE @next
   RTS

response:
   LDX #DB_SZ
   NOP
; Find a match in our kb:
@try_next:
   LDA DB_OU-1,X     ; skip possible responses that would be 00
   BEQ @next
   LDA DB_IN-1,X     ; possible input match
   JSR scoreA        ; return score for A in Y

   PHA
   TYA
   STA DB_SC-1,X     ; save score
   PLA

   CPY z:SCORE
   BEQ @next
   BMI @next
   ; here score found > previous SCORE
   STY z:SCORE
   LDA DB_IN-1,X
   STA z:MATCH
   LDA DB_OU-1,X
   STA z:REPLY
@next:
   DEX
   BNE @try_next
   RTS

scoreA:
   ; Input:  data we want to score in A
   ; Output: returns score in A
   PHX
   EOR z:INPUT   ; we XOR the data with the input
   ROR
   LDX #8	; bit counter
   LDY #8	; score
@next:
   ROL
   BPL @bit7is0
   DEY
@bit7is0:
   DEX
   BNE @next
   PLX
   RTS

; initial data for knowledge base
seed:
   .BYTE $FF, $F0, $0F, $CC, $33, $AA, $55, $99, $66, $C3, $3C
   .BYTE $00

; -----------------------------------------------------------------------
; Non-maskable interrupt (NMI) service routine

_nmi_int:  RTI                    ; Return from all NMI interrupts

; -----------------------------------------------------------------------
; Maskable interrupt (IRQ) service routine

_irq_int:  RTI


; -----------------------------------------------------------------------
; reserve space for global variables

.ZEROPAGE         ; NOTICE remember to use z:NAME to force zeropage addressing.
INPUT:   .res  1  ; input of the user
SCORE:   .res  1  ; the match score
MATCH:   .res  1  ; interpreted input
REPLY:   .res  1  ; corresponding output

.BSS
; knowledge base:
DB_IN: .res DB_SZ    ; Database of input (challenges)
DB_OU: .res DB_SZ    ; Database of output (responses)
DB_SC: .res DB_SZ    ; latest scores

.segment  "VECTORS"

.addr      _nmi_int    ; NMI vector
.addr      _init       ; Reset vector
.addr      _irq_int    ; IRQ/BRK vector
