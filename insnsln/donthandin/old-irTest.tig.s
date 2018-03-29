L193:
li t570, 3
addi t569, t570, 4
move $v0, t569
li t571, 0
move $a0, t571
jal tig_exit_TigMain
tig_dereference_NIL:
li t572, ~1
move $a0, t572
jal tig_exit_TigMain
tig_outOfBounds:
li t573, ~2
move $a0, t573
jal tig_exit_TigMain
j L192
L192:
