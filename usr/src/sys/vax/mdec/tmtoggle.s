/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)tmtoggle.s	6.2 (Berkeley) %G%"
#endif not lint

/*
 * Prototype toggle in bootstrap code for tm type tapes.
 * If on anything but a 780 with the drive on uba0
 * this will have to be repaired by patching uba and umem.
 */
begin:
	movl	uba,r1
	movl	$0x80200000,0x800(r1)
	clrl	0x804(r1)
	movl	umem,r2
	bisl2	$0172520,r2
	mnegw	$512,4(r2)
	clrw	6(r2)
	movw	$03,2(r2)
	halt
	.align	2
uba:	.long	0x20006000
umem:	.long	0x2013e000
