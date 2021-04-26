emit tig_main
L46
addi t227, r0,0 (t227 =0)
lw t228, -4(fp) (load address of aa to t228)
sw t227 t228 (store 0 at a])
addi t229, r0,0 (t229=0)
sw t229, t226 (store 0 at mem[t226])
addi t230, r0,100 (t230 =100)
ble t226 t230 (if i <100 then branch)
L42
addi t231, r0,0 (if i>=100, do nothing)
sw t231, t101 
jump L45
L44
addi t232, t226, 1 (t232 = i+1)
sw t232, t226 (store new i at 226)
L43
lw t234, -4(fp) (load a into t234)
addi t233, t234, 1 (a+1)
lw t235, -4(fp) (load a into 235)
sw t233 t235 (store a+1 in a )
addi t236, r0,100 (t236 =100)
blt t226 t236 (blt i<100)
L47
jump L42
L45
