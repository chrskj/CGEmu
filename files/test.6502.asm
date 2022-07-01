    * = $8000
    LDX #$0A
    STX $0000
    LDX #$03
    STX $0001
    LDY $0000
    LDA #$00
    CLC
L8010   ADC $0001
    DEY
    BNE L8010
    STA $0002
    NOP
    NOP
    NOP
    .END

;auto-generated symbols and labels
L8010   $8010