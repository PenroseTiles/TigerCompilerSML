emit tig_main
L13
addi t148, fp, -4 (move frame pointer down to fit array pointer)
sw t148, t147 (store new fp at mem[t147])
addi t149, r0,10 (t149 = 10)
sw t149, t144 (store 10 at mem[t144])
addi t151, t144, 1 (t151 = t144+1???)
sw t151, t150 (store t151 at mem[t150])
addi t153, r0,0 (t153 =0)
sw t153, t152 (store t153 at mem[t152])
jal initArray (call initArray)
sw t101, t146 
addi t154, t146, 4
sw t154, t145
lw t155, -4(t145)
sw t144 t155
lw t156, 0(t147)
sw t145 t156
lw t157, -4(fp)
sw t157, t101
jump L12
L12
