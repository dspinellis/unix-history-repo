/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)hkboot.c	6.2 (Berkeley) %G%"
#endif not lint


/*
 * RK07 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 * Always reads from drive 0.
 */
	.set	RELOC,0x50000
	.set	BOOTSIZE,15		/* size of boot in sectors */
	.set	SID,62			/* system ID register */
/* UBA registers */
	.set	UBA_CNFGR,0		/* UBA configuration register */
	.set	UBA_CR,4		/* UBA control register offset */
	.set	UBA_MAP,0x800		/* UBA offset to map reg's */
	.set	UBAinit,1		/* UBA init bit in UBA control reg */
	.set	pUBIC,16		/* Unibus init complete */
/* RK611 registers and bits */
	.set	HK,0177440-0160000	/* address of RK611 */
	.set	HK_cs1,HK+0		/* control and status */
	.set	HK_wc,HK+2		/* word count */
	.set	HK_ba,HK+4		/* bus address */
	.set	HK_da,HK+6		/* disk address */
	.set	HK_dc,HK+020		/* desired cylinder */
	.set	HKBPSECT,512		/* bytes per sector */
	.set	HK_GO,1			/* go bit */
	.set	HK_PACK,2		/* pack acknowledge */
	.set	HK_RCOM,020		/* read command */
	.set	HK_SEL7,02000		/* select RK07 disk */
	.set	HK_pRDY,7		/* position of ready bit */
	.set	HK_pERR,15		/* position of error bit */

init:
/* r9	UBA address */
/* r10	umem addr */
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
/* init rk611, set vv in drive 0; if any errors, give up */
	movw	$HK_SEL7+HK_GO,HK_cs1(r10)
1:
	movw	HK_cs1(r10),r0
	bbc	$HK_pRDY,r0,1b
	bbs	$HK_pERR,r0,9f
	movw	$HK_SEL7+HK_PACK+HK_GO,HK_cs1(r10)
1:
	movw	HK_cs1(r10),r0
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
	movw	$0,HK_dc(r10)
	movw	$1,HK_da(r10)
	movw	$-PROGSIZE/2,HK_wc(r10)
	clrl	r0
1:
	bisl3	$0x80000000,r0,UBA_MAP(r9)
	addl2	$4,r9
	aobleq	$BOOTSIZE,r0,1b
	clrw	HK_ba(r10)
	movw	$HK_SEL7+HK_RCOM+HK_GO,HK_cs1(r10)
hkrdy:
	movw	HK_cs1(r10),r0
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
	movl	$3,r10			/* major("/dev/hk0a") */
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
