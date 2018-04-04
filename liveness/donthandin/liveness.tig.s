PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li t582, 1
move t583, t582
li t584, 0
li t585, 22
li t586, 7
li t592, 2
mul t591, t592, t583
move t587, t591
div t593, t585, t586
move t588, t593
mul t594, t587, t588
move t589, t594
mul t596, t589, t583
li t597, 2
div t595, t596, t597
move t590, t595
div t598, t590, t589
move $v0, t598
li t599, 0
move $a0, t599
jal tig_exit_TigMain
tig_dereference_NIL:
li t600, ~1
move $a0, t600
jal tig_exit_TigMain
tig_outOfBounds:
li t601, ~2
move $a0, t601
jal tig_exit_TigMain
j L136
L136:
procEntryExit2_FOR_DEBUG_ONLY
jr $ra
END tig_MAIN_TIGER_PROG
