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
    .insn r 0b0110011, 0b100, 0b0000011, t0, t0, t1 //send rd:送信先のレジスタ番号、rs1:送信先のスレッドIDの値を格納したレジスタ、rs2:送信する値を格納したレジスタ
    j .END
.THREAD_1:
    .insn r 0b0110011, 0b100, 0b0000111, t0, zero, a0 //receive rd:送信先のレジスタ、rs1:送信元のスレッドIDの値を格納したレジスタ、rs2:意味なし
    add t0,t0,zero
.END:
    j .
