PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
li $a0, ~1
jal tig_exit_TigMain
j L3787
L3787:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
li $a0, ~2
jal tig_exit_TigMain
j L3788
L3788:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li $a0, 1
li $a1, 0
li $a3, 22
li $a2, 7
li $a1, 2
mul $a0, $a1, $a0
div $a0, $a3, $a2
mul $a0, $a0, $a0
mul $a2, $a0, $a0
li $a1, 2
div $a0, $a2, $a1
div $v0, $a0, $a0
j L3789
L3789:
jr $ra
END tig_MAIN_TIGER_PROG
