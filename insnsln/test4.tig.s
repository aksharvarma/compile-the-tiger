PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
la t2453, tig_MAIN_TIGER_PROG_framesize
lw t2452, 0(t2453)
add t2451, $sp, t2452
move $a0, t2451
li t2454, 10
move $a1, t2454
jal L523
move $v0, $v0
li t2461, 0
move $a0, t2461
jal tig_exit_TigMain
tig_dereference_NIL:
li t2468, ~1
move $a0, t2468
jal tig_exit_TigMain
tig_outOfBounds:
li t2475, ~2
move $a0, t2475
jal tig_exit_TigMain
j L527
L527:
jr $ra
END tig_MAIN_TIGER_PROG
PROCEDURE L523
L523:
beqz t2443, L525
L526:
move t2477, t2443
la t2487, L523_framesize
lw t2486, 0(t2487)
add t2485, $sp, t2486
lw t2484, 0(t2485)
move $a0, t2484
li t2489, 1
sub t2488, t2443, t2489
move $a1, t2488
jal L523
move t2476, $v0
mul t2490, t2477, t2476
move t2444, t2490
L524:
move $v0, t2444
j L528
L525:
li t2444, 1
j L524
L528:
jr $ra
END L523
