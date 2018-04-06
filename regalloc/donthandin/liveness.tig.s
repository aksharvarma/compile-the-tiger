PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
li t165, ~1
move $a0, t165
jal tig_exit_TigMain
j L15
L15:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
li t166, ~2
move $a0, t166
jal tig_exit_TigMain
j L16
L16:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li t154, 1
move t155, t154
li t156, 0
li t157, 22
li t158, 7
li t168, 2
mul t167, t168, t155
move t159, t167
div t169, t157, t158
move t160, t169
mul t170, t159, t160
move t161, t170
mul t172, t161, t155
li t173, 2
div t171, t172, t173
move t162, t171
div t174, t162, t161
move $v0, t174
j L17
L17:
jr $ra
END tig_MAIN_TIGER_PROG
