/*
 * Copyright (c) 1980,1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)raboot.c	7.2 (Berkeley) %G%" */
#include <sys/disklabel.h>

	.set	MAJOR,9			/* major("/dev/ra0a") */

/*
 * 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 * Except for MAJOR definition above, should work
 * with any disk using 750 boot rom.
 */
	.set	RELOC,0x50000
	.set	BOOTLAST,15		/* last boot sector */
	.set	RABPSECT,512		/* bytes per sector */

init:
	.word	0  			/* entry mask for dec monitor */
	nop;nop;nop;nop;nop;nop;nop;nop /* some no-ops for 750 boot to skip */
	nop;nop;
start:
	movl	$MAJOR,r10		/* major("/dev/xx0a") */
	extzv	$18,$1,r1,r4		/* get UBA number from R1 */
	xorb2	$0x01,r4		/* complement bit */
	insv	r4,$24,$8,r10		/* set UBA number */
	insv	r3,$16,$8,r10		/* drive number */
	extzv	$12,$4,r5,r4		/* get partition from r5 */
	bicw2	$0xf000,r5		/* remove from r5 */
	insv	r4,$8,$8,r10		/* set partition */
	movl	r5,r11			/* boot flags */
	movl	r1,r9			/* UNIBUS I/O page address */
	movl	r2,r8			/* boot device CSR */
	movl	r3,r7			/* unit number */
	brw	start0

/*
 * Leave space for pack label.
 */
pad:
	.space	LABELOFFSET - (pad - init)
packlabel:
	.space	d_end_

start0:
	movl	$RELOC,sp
	moval	init,r4
	movc3	$end,(r4),(sp)
	movl	r9,r1			/* UNIBUS I/O page address */
	movl	r8,r2			/* boot device CSR */
	movl	r7,r3			/* unit number */
	jmp	*$RELOC+start2
/* now running relocated */
/* bring in the boot program */
start2:					/* running relocated */
	pushr	$0xffff			/* BEGIN FIREWALL */
	movl	$1,r4			/* first boot sector */
	clrl	r5			/* transfer address */
	clrl	-(sp)			/* transfer address */
1:
	movl	r4,r8			/* requested sector # */
	jsb	(r6)			/* call ROM-based driver */
	blbs	r0,2f
	halt				/* read error */
2:
	addl2	$RABPSECT,r5		/* bump address */
	movl	r5,(sp)
	aobleq	$BOOTLAST,r4,1b

	.set	PROGSIZE,(BOOTLAST*RABPSECT)
done:
	tstl	(sp)+			/* pop address */
	popr	$0xffff			/* END FIREWALL */
	movl	$PROGSIZE,r4
clrcor:
	clrq	(r4)
	acbl	$RELOC,$8,r4,clrcor
/* start loaded program */
	calls	$0,*$0
	brw	start2
end:
