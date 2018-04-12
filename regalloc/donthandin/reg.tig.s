PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
move t272, t108
move t273, t110
li t285, ~1
move $a0, t285
jal tig_exit_TigMain
move t108, t272
move t110, t273
j L19
L19:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
move t274, t108
move t275, t110
li t319, ~2
move $a0, t319
jal tig_exit_TigMain
move t108, t274
move t110, t275
j L20
L20:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
move $a1, $ra
move $a0, $s0
li $v0, 0
move $ra, $a1
move $s0, $a0
j L21
L21:
jr $ra
END tig_MAIN_TIGER_PROG
