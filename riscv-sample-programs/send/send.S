#include "../common.h"

_start:
    init_all_regs
    la a0, _start
    csrr a0, mhartid
.BRANCH:
    bne a0,zero,.THREAD_1
.THREAD_0:
    li t0, 0x0000000000000001
    li t1, 0x0123456789ABCDEF
    li t2, 0x0000000000000001
    .insn r 0b0110011, 0b100, 0b11, zero, t0, t1, t2 //send rd:意味なし、rs1:送信先のスレッドIDの値を格納したレジスタ、rs2:送信する値を格納したレジスタ、rs3:channel
    j .END
.THREAD_1:
    li t2, 0x0000000000000001
    .insn r 0b0110011, 0b100, 0b0000110, t0, zero, t2 //receive rd:送信先のレジスタ、rs1:送信元のスレッドIDの値を格納したレジスタ、rs2:channel
    add t0,t0,zero
.END:
    j .
