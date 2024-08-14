#include "../common.h"

.extern __BINARY_END_

.section .text.init
.global _start
_start:
    init_all_regs
    la sp, __BINARY_END_
    la a0, _start
    csrw mtvec, a0
    csrr a0, mhartid
.L_LOOP:
    addi sp,sp,1024
    beq a0,zero,.L_PROGRAM
    addi a0,a0,-1
    j .L_LOOP
.L_PROGRAM:
    rdcycle t0
    rdinstret t1
    la a1, loop_count
    lw a0, 0(a1)
    call main

    rdcycle t2
    rdinstret t3
    sub t0,t2,t0
    sub t1,t3,t1

    mv x3,a0

    j .

.section .data
.align 8
loop_count:  .byte 100