PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
li $a0, ~1
jal tig_exit_TigMain
j L3791
L3791:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
li $a0, ~2
jal tig_exit_TigMain
j L3792
L3792:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li $v0, 0
j L3793
L3793:
jr $ra
END tig_MAIN_TIGER_PROG
