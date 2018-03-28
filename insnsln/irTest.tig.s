L33:
li 'd0, 3
addi 'd0, 's0, 4
lw 'd0 0('s0)
tig_DONE:
j L32
tig_dereference_NIL:
jal tig_print
j tig_DONE
tig_outOfBounds:
jal tig_print
j tig_DONE
L32:
L31: .asciiz "Index out of bounds exception"
L30: .asciiz "Attempted to deref a nil record"
