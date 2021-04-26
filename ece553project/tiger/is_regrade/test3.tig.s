SomebodyNobodyemit tig_main
L17
addi t160, fp, -4 (t160 = fp -4)
sw t160, t159 (store new frame pointer at mem[t159])
addi t162, r0,8 (t162=8)
sw t162, t161 (store t162 at mem[t161])
jal malloc (malloc space on heap for record?)
sw t101, t158 (store pointer to malloced space at mem[t158])
la t163, L14 (add address of Nobody into t163)
lw t164, 0(t158) (load address of malloced space into t164)
sw t163 t164 (store address to Nobody in malloced space)
addi t165, r0,1000 (t165 = 1000)
lw t166, 4(t158) (load address of 2nd malloced space into 166)
sw t165 t166 (store 1000 at second malloced space)
lw t167, 0(t159) ????
sw t158 t167 ??????
la t168, L15 (load address of Somebody into t168)
lw t171, -4(fp) (load address of malloced space from frame pointer)
addi t173, r0,4 (t173 = 4)
addi t174, r0,0 (t174 = 0)
mult t172, t173, t174 (t172 = 4)
add t170, t171, 's1 (t170 = ???)
lw t169, 0(t170) 
sw t168 t169 (load address of Somebody into mem[t169])
lw t175, -4(fp) (load pointer to record at start of new frame)
sw t175, t101 (store pointer into a register)
jump L16 (end)
L16
