PROCEDURE tig_MAIN_TIGER_PROG
tig_MAIN_TIGER_PROG:
li t1924, 5
la t1927, tig_MAIN_TIGER_PROG_framesize
lw t1926, 0(t1927)
add t1925, $sp, t1926
sw t1924, ~4(t1925)
addi $sp, $sp, -4
la t1928, tig_MAIN_TIGER_PROG_framesize
lw t1930, 0(t1928)
addi t1932, t1930, 4
sw t1932, 0(t1928)
la t1936, tig_MAIN_TIGER_PROG_framesize
lw t1935, 0(t1936)
add t1934, $sp, t1935
move $a0, t1934
jal L443
la t1929, tig_MAIN_TIGER_PROG_framesize
lw t1931, 0(t1929)
addi t1933, t1931, -4
sw t1933, 0(t1929)
addi $sp, $sp, 4
move $v0, $v0
addi $sp, $sp, -4
la t1937, tig_MAIN_TIGER_PROG_framesize
lw t1939, 0(t1937)
addi t1941, t1939, 4
sw t1941, 0(t1937)
li t1943, 0
move $a0, t1943
jal tig_exit_TigMain
la t1938, tig_MAIN_TIGER_PROG_framesize
lw t1940, 0(t1938)
addi t1942, t1940, -4
sw t1942, 0(t1938)
addi $sp, $sp, 4
tig_dereference_NIL:
addi $sp, $sp, -4
la t1944, tig_MAIN_TIGER_PROG_framesize
lw t1946, 0(t1944)
addi t1948, t1946, 4
sw t1948, 0(t1944)
li t1950, ~1
move $a0, t1950
jal tig_exit_TigMain
la t1945, tig_MAIN_TIGER_PROG_framesize
lw t1947, 0(t1945)
addi t1949, t1947, -4
sw t1949, 0(t1945)
addi $sp, $sp, 4
tig_outOfBounds:
addi $sp, $sp, -4
la t1951, tig_MAIN_TIGER_PROG_framesize
lw t1953, 0(t1951)
addi t1955, t1953, 4
sw t1955, 0(t1951)
li t1957, ~2
move $a0, t1957
jal tig_exit_TigMain
la t1952, tig_MAIN_TIGER_PROG_framesize
lw t1954, 0(t1952)
addi t1956, t1954, -4
sw t1956, 0(t1952)
addi $sp, $sp, 4
j L444
L444:
jr $ra
END tig_MAIN_TIGER_PROG
PROCEDURE L443
L443:
la t1961, L443_framesize
lw t1960, 0(t1961)
add t1959, $sp, t1960
lw t1958, 0(t1959)
lw $v0, ~4(t1958)
j L445
L445:
jr $ra
END L443
