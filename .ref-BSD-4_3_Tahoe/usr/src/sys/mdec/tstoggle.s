/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)tstoggle.s	7.1 (Berkeley) 6/5/86"
#endif not lint

/*
 * Prototype toggle in bootstrap code for ts type tapes.
 * If on anything but a 780 with the drive on uba0
 * this will have to be repaired by patching uba and umem.
 */
	.set	UBA0,0x20006000
	.set	UMEM0,0x2013e000
	.set	UBA_MAP,0x800
	.set	TSADDR,0772520-0760000

start:
	movl	uba,r10
	movl	mrv,UBA_MAP(r10)
	addl3	mrv,$1,UBA_MAP+4(r10)
	addl3	umem,$TSADDR,r11
	clrw	2(r11)
1:	tstb	2(r11)
	bgeq	1b
	movw	$0x200+setchr,(r11)
1:	tstb	2(r11)
	bgeq	1b
	movw	$0x200+read,(r11)
	halt
	.align	2
uba:	.long	UBA0
umem:	.long	UMEM0
mrv:	.long	0x80000000
setchr:	.word	0xc004,0x200+char,0,0x8	# set characteristics command
char:	.word	0x200+status,0,0xe,0	# characteristics
read:	.word	0xc001,0,0,0x200	# read command
status:
