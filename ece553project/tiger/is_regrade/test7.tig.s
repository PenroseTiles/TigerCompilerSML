str2 stremit do_nothing1
L34
lw t208, 0(fp) (load b into t208)
sw t208, t207 (store a in mem[t207])
lw t211, -4(fp) (load a into t211)
addi t210, t211, 1 (t210 = a+1)
sw t210, t209 (store a +1 into mem[t209])
jal L29 (call do_nothing2)
addi t212, r0,0 (t212= 0)
sw t212, t101 (Store 0 into return register)
jump L33
L33
emit do_nothing2
L36
lw t214, 0(fp) (load d into t214)
sw t214, t213 (store d in mem[t213])
lw t216, -4(fp)
sw t216, t215
la t218, L30 (load address of str into t218)
sw t218, t217 (store address of str into t217)
jal L28 (call do_nothing1)
la t219, L31 (load address of " " into t129)
sw t219, t101 (store " " in return register)
jump L35
L35
emit tig_main
L38
sw fp, t220 (store frame pointer at mem[t220])
addi t222, r0,0 (t222 = 0)
sw t222, t221 (Store 0 at mem[t221])
la t224, L32 (address address of "str2" in t224)
sw t224, t223 (store address of "str2" into mem[t223])
jal L28 (call do _nothing1)
sw t101, t101 (return register)
jump L37
L37
