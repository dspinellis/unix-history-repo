| /* Copyright (c) 1982, Regents, University of California */
        .text
        .globl  _callg_
_callg_:
        link    a6,#0
        movl    a6@(12),a0
        movl    sp,a1
        movl    a0@+,d0
        asll    #2,d0
        subl    d0,a1
        tstb    a1@
	movl	a1,sp
.L13:
        subql   #4,d0
        blt     .L14
        movl    a0@+,a1@+
        bra     .L13
.L14:
        movl    a6@(8),a0
        jsr     a0@
        unlk    a6
        rts
