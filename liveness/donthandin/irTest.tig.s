PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li $v0, 3
li t349, 0
move $a0, t349
jal tig_exit_TigMain
tig_dereference_NIL:
li t350, ~1
move $a0, t350
jal tig_exit_TigMain
tig_outOfBounds:
li t351, ~2
move $a0, t351
jal tig_exit_TigMain
j L112
L112:
procEntryExit2_FOR_DEBUG_ONLY
jr $ra
END tig_MAIN_TIGER_PROG
