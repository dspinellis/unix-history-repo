/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)upboot.c	7.3 (Berkeley) 2/21/87" */
#include <sys/disklabel.h>

	.set	MAJOR,2			/* major("/dev/up0a") */

/*
 * UP 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 * Always reads from drive 0.
 */
	.set	BOOTSIZE,15 		/* size of boot in sectors */
	.set	RELOC,0x50000
	.set	UPBPSECT,512		/* bytes per sector */
/* UBA registers */
	.set	UBA_CNFGR,0		/* UBA configuration register */
	.set	UBA_CR,4		/* UBA control register offset */
	.set	UBA_MAP,0x800		/* UBA offset to map reg's */
/* UP registers and bits */
	.set	UP_cs1,0		/* control and status */
	.set	UP_wc,2			/* word count */
	.set	UP_ba,4			/* bus address */
	.set	UP_da,6			/* disk address */
	.set	UP_cs2,010		/* cs2 register */
	.set	UP_of,032		/* offset register */
	.set	UP_dc,034		/* desired cylinder */
	.set	UP_hr,036		/* holding register */
	.set	UP_GO,1			/* go bit */
	.set	UP_PACK,022		/* pack acknowledge */
	.set	UP_DCLR,010		/* drive clear */
	.set	UP_PRESET,020		/* read-in-preset */
	.set	UP_RCOM,070		/* read command */
	.set	UPCS2_CLR,040
	.set	UP_pRDY,7		/* position of ready bit */
	.set	UP_pERR,15		/* position of error bit */
	.set	UP_FMT22,010000

init:
/* r9   UBA address */
/* r8	UP addr */
	.word	0  			/* entry mask for dec monitor */
	nop;nop;nop;nop;nop;nop;nop;nop /* some no-ops for 750 boot to skip */
	nop;nop;
	movl	$MAJOR,r10		/* major("/dev/xx0a") */
	extzv	$18,$1,r1,r9		/* get UBA number from R1 */
	xorb2	$0x01,r9		/* complement bit */
	insv	r9,$24,$8,r10		/* set UBA number */
	insv	r3,$16,$8,r10		/* drive number */
	extzv	$12,$4,r5,r4		/* get partition from r5 */
	bicw2	$0xf000,r5		/* remove from r5 */
	insv	r4,$8,$8,r10		/* set partition */
	movl	r5,r11			/* boot flags */

	movl	r2,r8			/* boot device CSR */
	brw	start0

/*
 * Leave space for pack label.
 */
pad:
	.space	LABELOFFSET - (pad - init)
packlabel:
	.space	d_end_

start0:
	movl	physUBA[r9],r9		/* UNIBUS adaptor address */
	movl	r3,r7			/* unit number */
	movl	$RELOC,sp
	moval	init,r4
	movc3	$end,(r4),(sp)
/* init up, set vv in drive; if any errors, give up */
/* probably unneeded: rom should have already done this */
	bisw3	r7,$UPCS2_CLR,UP_cs2(r8)
	movw	$UP_DCLR+UP_GO,UP_cs1(r8)
	movw	$UP_PRESET+UP_GO,UP_cs1(r8)
	movw	$UP_FMT22,UP_of(r8)
1:
	movw	UP_cs1(r8),r0
	bbc	$UP_pRDY,r0,1b
/* relocate to high core */
start:
	movl	$RELOC,sp
	moval	init,r6
	movc3	$end,(r6),(sp)
	jmp	*$RELOC+start2
/* now running relocated */
	.set	PROGSIZE,(BOOTSIZE*UPBPSECT)
start2:
	movw	$0,UP_dc(r8)
	movw	$1,UP_da(r8)
	movw	$-PROGSIZE/2,UP_wc(r8)
	clrl	r0
1:
	bisl3	$0x80000000,r0,UBA_MAP(r9)
	addl2	$4,r9
	aobleq	$BOOTSIZE,r0,1b
	clrw	UP_ba(r8)
	movw	$UP_RCOM+UP_GO,UP_cs1(r8)
uprdy:
	movw	UP_cs1(r8),r0
	bbc	$UP_pRDY,r0,uprdy
clear:
	movl	$PROGSIZE,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* run loaded program */
	calls	$0,*$0
	brw	start2
physUBA:
	.long	0xf30000		/* uba0 */
	.long	0xf32000		/* uba1 */
end:
