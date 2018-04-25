.text
tig_dereference_NIL:
L89:
move t351, $ra
move t352, $s0
move t353, $s1
move t354, $s2
move t355, $s3
move t356, $s4
move t357, $s5
move t358, $s6
move t359, $s7
move t360, $r30
move t361, $r3
move t349, $a0
li t373, -1
move $a0, t373
jal tig_exit_TigMain
move $ra, t351
move $s0, t352
move $s1, t353
move $s2, t354
move $s3, t355
move $s4, t356
move $s5, t357
move $s6, t358
move $s7, t359
move $r30, t360
move $r3, t361
j L88
L88:
jr $ra
tig_outOfBounds:
L91:
move t362, $ra
move t363, $s0
move t364, $s1
move t365, $s2
move t366, $s3
move t367, $s4
move t368, $s5
move t369, $s6
move t370, $s7
move t371, $r30
move t372, $r3
move t350, $a0
li t374, -2
move $a0, t374
jal tig_exit_TigMain
move $ra, t362
move $s0, t363
move $s1, t364
move $s2, t365
move $s3, t366
move $s4, t367
move $s5, t368
move $s6, t369
move $s7, t370
move $r30, t371
move $r3, t372
j L90
L90:
jr $ra
L84:
L93:
move t327, $ra
move t328, $s0
move t329, $s1
move t330, $s2
move t331, $s3
move t332, $s4
move t333, $s5
move t334, $s6
move t335, $s7
move t336, $r30
move t337, $r3
move t325, $a1
addi t377, $sp, L84_framesize
sw $a0, 0(t377)
beqz t325, L86
L87:
move t376, t325
addi t379, $sp, L84_framesize
lw t378, 0(t379)
move $a0, t378
addi t380, t325, -1
move $a1, t380
jal L84
move t375, $v0
mul t381, t376, t375
move t326, t381
L85:
move $v0, t326
move $ra, t327
move $s0, t328
move $s1, t329
move $s2, t330
move $s3, t331
move $s4, t332
move $s5, t333
move $s6, t334
move $s7, t335
move $r30, t336
move $r3, t337
j L92
L86:
li t326, 1
j L85
L92:
jr $ra
tig_main:
L95:
move t338, $ra
move t339, $s0
move t340, $s1
move t341, $s2
move t342, $s3
move t343, $s4
move t344, $s5
move t345, $s6
move t346, $s7
move t347, $r30
move t348, $r3
addi t382, $sp, tig_main_framesize
sw $a0, 0(t382)
addi t383, $sp, tig_main_framesize
move $a0, t383
li t384, 10
move $a1, t384
jal L84
move $v0, $v0
move $ra, t338
move $s0, t339
move $s1, t340
move $s2, t341
move $s3, t342
move $s4, t343
move $s5, t344
move $s6, t345
move $s7, t346
move $r30, t347
move $r3, t348
j L94
L94:
jr $ra
