PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
move $ra, $ra
move $s0, $s0
move $s1, $s1
move $s2, $s2
move $s3, $s3
move $s4, $s4
move $s5, $s5
move $s6, $s6
move $a3, $s7
move $a2, $r30
move $a1, $v1
li $a0, ~1
move $a0, $a0
jal tig_exit_TigMain
move $ra, $ra
move $s0, $s0
move $s1, $s1
move $s2, $s2
move $s3, $s3
move $s4, $s4
move $s5, $s5
move $s6, $s6
move $s7, $a3
move $r30, $a2
move $v1, $a1
j L159
L159:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
move $ra, $ra
move $s0, $s0
move $s1, $s1
move $s2, $s2
move $s3, $s3
move $s4, $s4
move $s5, $s5
move $s6, $s6
move $a3, $s7
move $a2, $r30
move $a1, $v1
li $a0, ~2
move $a0, $a0
jal tig_exit_TigMain
move $ra, $ra
move $s0, $s0
move $s1, $s1
move $s2, $s2
move $s3, $s3
move $s4, $s4
move $s5, $s5
move $s6, $s6
move $s7, $a3
move $r30, $a2
move $v1, $a1
j L160
L160:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
move $ra, $ra
move $s0, $s0
move $s1, $s1
move $s2, $s2
move $s3, $s3
move $s4, $s4
move $s5, $s5
move $a3, $s6
move $a2, $s7
move $a1, $r30
move $a0, $v1
li $v0, 0
move $ra, $ra
move $s0, $s0
move $s1, $s1
move $s2, $s2
move $s3, $s3
move $s4, $s4
move $s5, $s5
move $s6, $a3
move $s7, $a2
move $r30, $a1
move $v1, $a0
j L161
L161:
jr $ra
END tig_MAIN_TIGER_PROG
