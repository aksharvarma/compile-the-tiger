tig_tig_MAIN_TIGER_PROG_entryExit1_moveArgs_step4:
tig_tig_MAIN_TIGER_PROG_entryExit1_storeCalleeSaves_step5:
li 'd0, 3
addi 'd0, 's0, 4
lw 'd0 0('s0)
tig_tig_MAIN_TIGER_PROG_entryExit1_restoreCalleeSaves_step8:
tig_DONE:
j L51
tig_dereference_NIL:
jal tig_print
j tig_DONE
tig_outOfBounds:
jal tig_print
j tig_DONE
L51:
L50: .asciiz "Index out of bounds exception"
L49: .asciiz "Attempted to deref a nil record"
