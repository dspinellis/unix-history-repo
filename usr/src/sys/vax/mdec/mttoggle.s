/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)mttoggle.s	6.2 (Berkeley) %G%"
#endif not lint

/*
 * Prototype toggle in bootstrap code for mt type tapes.
 * If on anything but a 780 with a tape at drive 0 of mba 1
 * this will have to be repaired by patching mba and mt.
 */
	movl	mba,r10
	mull3	mt,$0x80,r11
	addl3	r11,r10,r11
	addl2	$0x400,r11
	movzbl	$1,4(r10)
	movzbl	$4,8(r11)		/* drive zero, one record */
	clrl	12(r10)			/* set address to zero */
	movl	$0x80000000,0x800(r10)	/* validate map register */
	cvtwl	$-512,16(r10)		/* set byte count */
	movzwl	$512,20(r11)		/* set byte count */
0:
	movl	0x44(r11),r0
	bbc	$15,r0,0b
	movzbl	$071,(r11)		/* read forward, go */
	halt
	.align	2
mba:	.long	0x20012000
mt:	.long	0
