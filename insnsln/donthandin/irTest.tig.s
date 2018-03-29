PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li t402, 3
addi t401, t402, 4
move $v0, t401
li t403, 0
move $a0, t403
jal tig_exit_TigMain
tig_dereference_NIL:
li t404, ~1
move $a0, t404
jal tig_exit_TigMain
tig_outOfBounds:
li t405, ~2
move $a0, t405
jal tig_exit_TigMain
j L150
L150:
END tig_MAIN_TIGER_PROG
