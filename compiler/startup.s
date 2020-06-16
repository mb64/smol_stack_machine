; Assembly startup glue, to put at address 0
    imm inf_loop
    imm main
    jmp
inf_loop:
    imm inf_loop
    jmp
