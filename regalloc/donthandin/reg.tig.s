PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
move t973, $ra
move t974, $s0
move t975, $s1
move t976, $s2
move t977, $s3
move t978, $s4
move t979, $s5
move t980, $s6
move t981, $s7
move t982, $r30
move t983, $v1
li $a0, ~1
move $a0, $a0
jal tig_exit_TigMain
move $ra, t973
move $s0, t974
move $s1, t975
move $s2, t976
move $s3, t977
move $s4, t978
move $s5, t979
move $s6, t980
move $s7, t981
move $r30, t982
move $v1, t983
j L97
L97:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
move t984, $ra
move t985, $s0
move t986, $s1
move t987, $s2
move t988, $s3
move t989, $s4
move t990, $s5
move t991, $s6
move t992, $s7
move t993, $r30
move t994, $v1
li $a0, ~2
move $a0, $a0
jal tig_exit_TigMain
move $ra, t984
move $s0, t985
move $s1, t986
move $s2, t987
move $s3, t988
move $s4, t989
move $s5, t990
move $s6, t991
move $s7, t992
move $r30, t993
move $v1, t994
j L98
L98:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
li $a0, 0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
move $a0, $a0
j L99
L99:
jr $ra
END tig_MAIN_TIGER_PROG
