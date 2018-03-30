PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li $v0, 3
li t230, 0
move $a0, t230
jal tig_exit_TigMain
tig_dereference_NIL:
li t231, ~1
move $a0, t231
jal tig_exit_TigMain
tig_outOfBounds:
li t232, ~2
move $a0, t232
jal tig_exit_TigMain
j L57
L57:
jr $ra
END tig_MAIN_TIGER_PROG
