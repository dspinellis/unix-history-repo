/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)raboot.c	6.3 (Berkeley) %G%" */

/*
 * UDA50 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 */
	.set	RELOC,0x50000
	.set	BOOTLAST,15		/* last boot sector */
	.set	RABPSECT,512		/* bytes per sector */

init:
	.word	0  			/* entry mask for dec monitor */
	nop;nop;nop;nop;nop;nop;nop;nop /* some no-ops for 750 boot to skip */
	nop;nop;
start:
	movl	r1,r7			/* UNIBUS I/O page address */
	movl	r2,r8			/* boot device CSR */
	movl	r3,r9			/* unit number */
	movl	r5,r11			/* boot flags */
	movl	$RELOC,sp
	moval	init,r10
	movc3	$end,(r10),(sp)
	movl	r7,r1			/* UNIBUS I/O page address */
	movl	r8,r2			/* boot device CSR */
	movl	r9,r3			/* unit number */
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

	.set	PROGSIZE,((BOOTLAST-1)*RABPSECT)
done:
	tstl	(sp)+			/* pop address */
	popr	$0xffff			/* END FIREWALL */
	movl	$PROGSIZE,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* start loaded program */
	movl	$9,r10			/* major("/dev/ra0a") */
	calls	$0,*$0
	brw	start2
end:
