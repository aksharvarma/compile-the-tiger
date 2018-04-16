PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
move $a3, $ra
li $a0, ~1
# move $a0, $a0
jal tig_exit_TigMain
move $ra, $a3
j L2991
L2991:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
move $a3, $ra
li $a0, ~2
# move $a0, $a0
jal tig_exit_TigMain
move $ra, $a3
j L2992
L2992:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
move $a2, $ra
li $a3, 1
# move $a3, $a3
li $a1, 0
li $v0, 22
li $a0, 7
li $a1, 2
mul $a1, $a1, $a3
# move $a1, $a1
div $a1, $v0, $a0
# move $a1, $a1
mul $a1, $a1, $a1
# move $a1, $a1
mul $a0, $a1, $a3
li $a3, 2
div $a3, $a0, $a3
# move $a3, $a3
div $v0, $a3, $a1
# move $v0, $v0
move $ra, $a2
j L2993
L2993:
jr $ra
END tig_MAIN_TIGER_PROG
