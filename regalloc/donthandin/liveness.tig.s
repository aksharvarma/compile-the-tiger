PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
move $ra, $ra
move $a1, $s0
li $a0, ~1
move $a0, $a0
jal tig_exit_TigMain
move $ra, $ra
move $s0, $a1
j L27
L27:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
move $ra, $ra
move $a1, $s0
li $a0, ~2
move $a0, $a0
jal tig_exit_TigMain
move $ra, $ra
move $s0, $a1
j L28
L28:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
mo--| move $a1, $ra
FP--| la $a0 tig_MAIN_TIGER_PROG_framesize
FP--| lw $a0 0($a0)
FP--| add $a0, $sp, $a0
DEF-| sw $a1 ~4($a0)
mo--| move $t0, $s0
op--| li $a0, 1
mo--| move $s0, $a0
op--| li $a0, 0
op--| li $ra, 22
op--| li $a1, 7
op--| li $a0, 2
op--| mul $a0, $a0, $s0
mo--| move $a0, $a0
op--| div $a1, $ra, $a1
mo--| move $a1, $a1
op--| mul $a0, $a0, $a1
mo--| move $a0, $a0
op--| mul $ra, $a0, $s0
op--| li $a1, 2
op--| div $a1, $ra, $a1
mo--| move $a1, $a1
op--| div $a0, $a1, $a0
mo--| move $v0, $a0
FP--| la $a0 tig_MAIN_TIGER_PROG_framesize
FP--| lw $a0 0($a0)
FP--| add $a0, $sp, $a0
USE-| lw $a0 ~4($a0)
mo--| move $ra, $a0
mo--| move $s0, $t0
op--| j L29
L29:
op--| jr $ra
END tig_MAIN_TIGER_PROG
