/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* @(#)hkboot.c	7.2 (Berkeley) 8/28/86 */
#include <sys/disklabel.h>

	.set	MAJOR,3		/* major("/dev/hk0a") */

/*
 * RK07 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 */
	.set	RELOC,0x50000
	.set	BOOTSIZE,15		/* size of boot in sectors */
/* UBA registers */
	.set	UBA_CNFGR,0		/* UBA configuration register */
	.set	UBA_CR,4		/* UBA control register offset */
	.set	UBA_MAP,0x800		/* UBA offset to map reg's */
/* RK611 registers and bits */
	.set	HK_cs1,0		/* control and status */
	.set	HK_wc,2			/* word count */
	.set	HK_ba,4			/* bus address */
	.set	HK_da,6			/* disk address */
	.set	HK_cs2,8		/* control and status */
	.set	HK_dc,020		/* desired cylinder */
	.set	HKBPSECT,512		/* bytes per sector */
	.set	HK_GO,1			/* go bit */
	.set	HK_PACK,2		/* pack acknowledge */
	.set	HK_RCOM,020		/* read command */
	.set	HK_SEL7,02000		/* select RK07 disk */
	.set	HK_pRDY,7		/* position of ready bit */
	.set	HK_pERR,15		/* position of error bit */

init:
/* r9	UBA address */
/* r8	HK addr */
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
	movl	r2,r8			/* boot device CSR */
	movl	r3,r7			/* unit number */

/* select unit, init rk611, set vv in drive; if any errors, give up */
	movw	r7,HK_cs2(r8)
	movw	$HK_SEL7+HK_GO,HK_cs1(r8)
1:
	movw	HK_cs1(r8),r0
	bbc	$HK_pRDY,r0,1b
	bbs	$HK_pERR,r0,9f
	movw	$HK_SEL7+HK_PACK+HK_GO,HK_cs1(r8)
1:
	movw	HK_cs1(r8),r0
	bbc	$HK_pRDY,r0,1b
	bbc	$HK_pERR,r0,start
9:
	halt
/* relocate to high core */
start:
	movl	r5,r11			/* boot flags */
	movl	$RELOC,sp
	moval	init,r6
	movc3	$end,(r6),(sp)
	jmp	*$RELOC+start2
/* now running relocated */
/* bring in the boot program */
	.set	PROGSIZE,(BOOTSIZE*HKBPSECT)
start2:
	movw	$0,HK_dc(r8)
	movw	$1,HK_da(r8)
	movw	$-PROGSIZE/2,HK_wc(r8)
	clrl	r0
1:
/*	bisl3	$0x80000000,r0,UBA_MAP(r9) */
/*	addl2	$4,r9 */
/*	aobleq	$BOOTSIZE,r0,1b */
	clrw	HK_ba(r8)
	movw	$HK_SEL7+HK_RCOM+HK_GO,HK_cs1(r8)
hkrdy:
	movw	HK_cs1(r8),r0
	bbc	$HK_pRDY,r0,hkrdy
	bbs	$HK_pERR,r0,hkerr
	brw	done
hkerr:
	halt			/* ungraceful */
done:
	movl	$PROGSIZE,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* start loaded program */
	calls	$0,*$0
	brw	start2
physUBA:
	.long	0xf30000		/* uba0 */
	.long	0xf32000		/* uba1 */
end:
