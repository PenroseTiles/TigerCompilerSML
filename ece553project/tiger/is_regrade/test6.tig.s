.data
L13:
.word 4
.ascii "str2"
.data
L12:
.word 3
.ascii "str"
.text
.align 4
do_nothing1:
emit do_nothing1
L17
lw t136, 0(fp)
move a0, t136
lw t138, -4(fp)
addi t137, t138, 1
move a1, t137
jal L11
move rv, rv
jump L16
L16
.text
.align 4
do_nothing2:
emit do_nothing2
L19
lw t139, 0(fp)
move a0, t139
lw t140, -4(fp)
move a1, t140
la t141, L12
move a2, t141
jal L10
move rv, rv
jump L18
L18
.text
.align 4
tig_main:
emit tig_main
L21
move a0, fp
addi t142, r0,0
move a1, t142
la t143, L13
move a2, t143
jal L10
move rv, rv
jump L20
L20
.text
.align 4
do_nothing1:
emit do_nothing1
L23
lw t144, 0(fp)
move a0, t144
lw t146, -4(fp)
addi t145, t146, 1
move a1, t145
jal L15
move rv, rv
jump L22
L22
.text
.align 4
do_nothing2:
emit do_nothing2
L25
lw t147, 0(fp)
move a0, t147
lw t148, -4(fp)
move a1, t148
la t149, L12
move a2, t149
jal L14
move rv, rv
jump L24
L24
