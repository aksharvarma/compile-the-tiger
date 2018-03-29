PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
addi $sp, $sp, -8
la t1372, tig_MAIN_TIGER_PROG_framesize
lw t1373, 0(t1372)
addi t1374, t1373, 8
sw t1374, 0(t1372)
la t1378, tig_MAIN_TIGER_PROG_framesize
lw t1377, 0(t1378)
add t1376, $sp, t1377
lw t1375, 0(t1376)
move $a0, t1375
li t1379, 10
move $a1, t1379
jal L423
la t1372, tig_MAIN_TIGER_PROG_framesize
lw t1373, 0(t1372)
addi t1374, t1373, -8
sw t1374, 0(t1372)
addi $sp, $sp, 8
move $v0, $v0
li t1380, 0
move $a0, t1380
jal tig_exit_TigMain
tig_dereference_NIL:
li t1381, ~1
move $a0, t1381
jal tig_exit_TigMain
tig_outOfBounds:
li t1382, ~2
move $a0, t1382
jal tig_exit_TigMain
j L427
L427:
END tig_MAIN_TIGER_PROG
PROCEDURE L423
L423:
beqz t1370, L425
L426:
move t1384, t1370
addi $sp, $sp, -8
la t1385, tig__framesize
lw t1386, 0(t1385)
addi t1387, t1386, 8
sw t1387, 0(t1385)
la t1392, tig__framesize
lw t1391, 0(t1392)
add t1390, $sp, t1391
lw t1389, 0(t1390)
lw t1388, 0(t1389)
move $a0, t1388
li t1394, 1
sub t1393, t1370, t1394
move $a1, t1393
jal L423
la t1385, tig__framesize
lw t1386, 0(t1385)
addi t1387, t1386, -8
sw t1387, 0(t1385)
addi $sp, $sp, 8
move t1383, $v0
mul t1395, t1384, t1383
move t1371, t1395
L424:
move $v0, t1371
j L428
L425:
li t1371, 1
j L424
L428:
END L423
