/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)upboot.c	6.3 (Berkeley) %G%" */

/*
 * UP 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 * Always reads from drive 0.
 */
	.set	BOOTSIZE,15 		/* size of boot in sectors */
	.set	RELOC,0x50000
	.set	UPBPSECT,512		/* bytes per sector */
	.set	SID,62			/* system ID register */
/* UBA registers */
	.set	UBA_CNFGR,0		/* UBA configuration register */
	.set	UBA_CR,4		/* UBA control register offset */
	.set	UBA_MAP,0x800		/* UBA offset to map reg's */
	.set	UBAinit,1		/* UBA init bit in UBA control reg */
	.set	pUBIC,16		/* Unibus init complete */
/* UP registers and bits */
	.set	UP,0176700-0160000	/* address of UP controller */
	.set	UP_cs1,UP+0		/* control and status */
	.set	UP_wc,UP+2		/* word count */
	.set	UP_ba,UP+4		/* bus address */
	.set	UP_da,UP+6		/* disk address */
	.set	UP_cs2,UP+010		/* cs2 register */
	.set	UP_of,UP+032		/* offset register */
	.set	UP_dc,UP+034		/* desired cylinder */
	.set	UP_hr,UP+036		/* holding register */
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
	.word	0  			/* entry mask for dec monitor */
	nop;nop;nop;nop;nop;nop;nop;nop /* some no-ops for 750 boot to skip */
	nop;nop;
/* get cpu type and find the first uba */
	mfpr	$SID,r0
	extzv	$24,$8,r0,r0		/* get cpu type */
	ashl	$2,r0,r1
	movab	physUBA,r2		/* get physUBA[cpu] */
	addl2	r1,r2
	movl	(r2),r9
	movab	physUMEM,r2		/* get physUMEM[cpu] */
	addl2	r1,r2
	movl	(r2),r10
/* if 780, init uba */
	cmpl	r0,$1
	bneq	2f
	movl	$UBAinit,UBA_CR(r9)
1:
	bbc	$pUBIC,UBA_CNFGR(r9),1b
2:
	movl	$5000000,r0
1:
	sobgtr	r0,1b
/* init up, set vv in drive 0; if any errors, give up */
	movw	$UPCS2_CLR,UP_cs2(r10)
	movw	$UP_DCLR+UP_GO,UP_cs1(r10)
	movw	$UP_PRESET+UP_GO,UP_cs1(r10)
	movw	$UP_FMT22,UP_of(r10)
1:
	movw	UP_cs1(r10),r0
	bbc	$UP_pRDY,r0,1b
/* relocate to high core */
start:
	movl	r5,r11			/* boot flags */
	movl	$RELOC,sp
	moval	init,r6
	movc3	$end,(r6),(sp)
	jmp	*$RELOC+start2
/* now running relocated */
	.set	PROGSIZE,(BOOTSIZE*UPBPSECT)
start2:
	movw	$0,UP_dc(r10)
	movw	$1,UP_da(r10)
	movw	$-PROGSIZE/2,UP_wc(r10)
	clrl	r0
1:
	bisl3	$0x80000000,r0,UBA_MAP(r9)
	addl2	$4,r9
	aobleq	$BOOTSIZE,r0,1b
	clrw	UP_ba(r10)
	movw	$UP_RCOM+UP_GO,UP_cs1(r10)
uprdy:
	movw	UP_cs1(r10),r0
	bbc	$UP_pRDY,r0,uprdy
clear:
	movl	$PROGSIZE,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* run loaded program */
	movl	$2,r10			/* major("/dev/up0a") */
	calls	$0,*$0
	brw	start2
physUBA:
	.long	0
	.long	0x20006000	/* 11/780 */
	.long	0xf30000	/* 11/750 */
	.long	0xf26000	/* 11/730 */
physUMEM:
	.long	0
	.long	0x2013e000	/* 11/780 */
	.long	0xffe000	/* 11/750 */
	.long	0xffe000	/* 11/730 */
end:
