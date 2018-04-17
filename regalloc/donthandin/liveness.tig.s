PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
la $t0 tig_dereference_NIL_framesize
lw $t0 0($t0)
add $t0, $sp, $t0
sw $ra -4($t0)
li $a0, -1
jal tig_exit_TigMain
la $t0 tig_dereference_NIL_framesize
lw $t0 0($t0)
add $t0, $sp, $t0
lw $ra -4($t0)
j L47
L47:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
la $t0 tig_outOfBounds_framesize
lw $t0 0($t0)
add $t0, $sp, $t0
sw $ra -4($t0)
li $a0, -2
jal tig_exit_TigMain
la $t0 tig_outOfBounds_framesize
lw $t0 0($t0)
add $t0, $sp, $t0
lw $ra -4($t0)
j L48
L48:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li $t0, 1
li $t1, 0
li $t3, 22
li $t2, 7
li $t1, 2
mul $t0, $t1, $t0
div $t0, $t3, $t2
mul $t0, $t0, $t0
mul $t2, $t0, $t0
li $t1, 2
div $t0, $t2, $t1
div $v0, $t0, $t0
j L49
L49:
jr $ra
END tig_MAIN_TIGER_PROG
