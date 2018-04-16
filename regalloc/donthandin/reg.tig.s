PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
move $a3, $ra
li $a0, ~1
# move $a0, $a0
jal tig_exit_TigMain
move $ra, $a3
j L2987
L2987:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
move $a3, $ra
li $a0, ~2
# move $a0, $a0
jal tig_exit_TigMain
move $ra, $a3
j L2988
L2988:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
# move $ra, $ra
li $v0, 0
# move $ra, $ra
j L2989
L2989:
jr $ra
END tig_MAIN_TIGER_PROG
