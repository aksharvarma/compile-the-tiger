PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
add $t0, $sp, tig_dereference_NIL_framesize($zero)
sw $ra -4($t0)
li $a0, -1
jal tig_exit_TigMain
add $t0, $sp, tig_dereference_NIL_framesize($zero)
lw $ra -4($t0)
j L36
L36:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
add $t0, $sp, tig_outOfBounds_framesize($zero)
sw $ra -4($t0)
li $a0, -2
jal tig_exit_TigMain
add $t0, $sp, tig_outOfBounds_framesize($zero)
lw $ra -4($t0)
j L37
L37:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li $v0, 0
j L38
L38:
jr $ra
END tig_MAIN_TIGER_PROG
