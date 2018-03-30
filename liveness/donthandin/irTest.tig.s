PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
la t2499, tig_MAIN_TIGER_PROG_framesize
lw t2498, 0(t2499)
add t2497, $sp, t2498
move $a0, t2497
li t2500, 3
move $a1, t2500
jal L533
move $v0, $v0
li t2507, 0
move $a0, t2507
jal tig_exit_TigMain
tig_dereference_NIL:
li t2514, ~1
move $a0, t2514
jal tig_exit_TigMain
tig_outOfBounds:
li t2521, ~2
move $a0, t2521
jal tig_exit_TigMain
j L535
L535:
jr $ra
END tig_MAIN_TIGER_PROG
PROCEDURE L533
L533:
la t2530, L533_framesize
lw t2529, 0(t2530)
add t2528, $sp, t2529
move $a0, t2528
jal L534
move $v0, $v0
j L536
L536:
jr $ra
END L533
PROCEDURE L534
L534:
la t2534, L534_framesize
lw t2533, 0(t2534)
add t2532, $sp, t2533
lw t2531, 0(t2532)
lw $v0, 4(t2531)
j L537
L537:
jr $ra
END L534
