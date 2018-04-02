PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li t301, 1
li t302, 0
addi t305, t301, 2
move t303, t305
li t307, 4
sub t306, t303, t307
move t304, t306
move $v0, t304
li t308, 0
move $a0, t308
jal tig_exit_TigMain
tig_dereference_NIL:
li t309, ~1
move $a0, t309
jal tig_exit_TigMain
tig_outOfBounds:
li t310, ~2
move $a0, t310
jal tig_exit_TigMain
j L95
L95:
procEntryExit2_FOR_DEBUG_ONLY
jr $ra
END tig_MAIN_TIGER_PROG
