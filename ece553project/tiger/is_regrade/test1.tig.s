L11
addi t134, fp, -4
move t133, t134
addi t135, r0,10
move t130, t135
addi t136, t130, 1
move a0, t136
addi t137, r0,0
move a1, t137
jal initArray
move t132, rv
addi t138, t132, 4
move t131, t138
lw t139, -4(t131)
sw t130 t139
lw t140, 0(t133)
sw t131 t140
lw t141, -4(fp)
move rv, t141
jump L10
L10
