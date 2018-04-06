PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
li t144, ~1
move $a0, t144
jal tig_exit_TigMain
j L11
L11:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
li t145, ~2
move $a0, t145
jal tig_exit_TigMain
j L12
L12:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li t133, 1
move t134, t133
li t135, 0
li t136, 22
li t137, 7
li t147, 2
mul t146, t147, t134
move t138, t146
div t148, t136, t137
move t139, t148
mul t149, t138, t139
move t140, t149
mul t151, t140, t134
li t152, 2
div t150, t151, t152
move t141, t150
div t153, t141, t140
move $v0, t153
j L13
L13:
jr $ra
END tig_MAIN_TIGER_PROG
