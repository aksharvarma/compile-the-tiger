PROCEDURE tig_dereference_NIL
tig_dereference_NIL:
move t1270, $ra
move t1271, $s0
move t1272, $s1
move t1273, $s2
move t1274, $s3
move t1275, $s4
move t1276, $s5
move t1277, $s6
move t1278, $s7
move t1279, $r30
move t1280, $v1
li t1292, ~1
move $a0, t1292
jal tig_exit_TigMain
move $ra, t1270
move $s0, t1271
move $s1, t1272
move $s2, t1273
move $s3, t1274
move $s4, t1275
move $s5, t1276
move $s6, t1277
move $s7, t1278
move $r30, t1279
move $v1, t1280
j L277
L277:
jr $ra
END tig_dereference_NIL
PROCEDURE tig_outOfBounds
tig_outOfBounds:
move t1281, $ra
move t1282, $s0
move t1283, $s1
move t1284, $s2
move t1285, $s3
move t1286, $s4
move t1287, $s5
move t1288, $s6
move t1289, $s7
move t1290, $r30
move t1291, $v1
li t1293, ~2
move $a0, t1293
jal tig_exit_TigMain
move $ra, t1281
move $s0, t1282
move $s1, t1283
move $s2, t1284
move $s3, t1285
move $s4, t1286
move $s5, t1287
move $s6, t1288
move $s7, t1289
move $r30, t1290
move $v1, t1291
j L278
L278:
jr $ra
END tig_outOfBounds
PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
move t1257, $ra
move t1258, $s0
move t1259, $s1
move t1260, $s2
move t1261, $s3
move t1262, $s4
move t1263, $s5
move t1264, $s6
move t1265, $s7
move t1266, $r30
move t1267, $v1
li t1248, 1
move t1249, t1248
li t1250, 0
li t1251, 22
li t1252, 7
li t1295, 2
mul t1294, t1295, t1249
move t1253, t1294
div t1296, t1251, t1252
move t1254, t1296
mul t1297, t1253, t1254
move t1255, t1297
mul t1299, t1255, t1249
li t1300, 2
div t1298, t1299, t1300
move t1256, t1298
div t1301, t1256, t1255
move $v0, t1301
move $ra, t1257
move $s0, t1258
move $s1, t1259
move $s2, t1260
move $s3, t1261
move $s4, t1262
move $s5, t1263
move $s6, t1264
move $s7, t1265
move $r30, t1266
move $v1, t1267
j L279
L279:
jr $ra
END tig_MAIN_TIGER_PROG
