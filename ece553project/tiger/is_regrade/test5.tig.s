L11
addi t132, fp, -4
move t131, t132
addi t133, r0,8
move a0, t133
jal malloc
move t130, rv
addi t134, r0,0
lw t135, 0(t130)
sw t134 t135
addi t136, r0,0
lw t137, 4(t130)
sw t136 t137
lw t138, 0(t131)
sw t130 t138
lw t139, -4(fp)
move rv, t139
jump L10
L10
