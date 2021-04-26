.text
.align 4
g:
emit g
L16
lw t134, -4(fp)
move rv, t134
jump L15
L15
.text
.align 4
tig_main:
emit tig_main
L18
move a0, fp
addi t135, r0,2
move a1, t135
jal g
move rv, rv
jump L17
L17
