/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)rlboot.c	7.3 (Berkeley) 2/21/87" */
#include <sys/disklabel.h>

	.set	MAJOR,14		/* major("/dev/rl0a") */

/*
 * RL02 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 */
	.set	BOOTSIZE,15		/* 15 ``sectors'' */
        .set    RELOC,0x50000
/* UBA registers */
        .set    UBA_CNFGR,0		/* UBA configuration register */
        .set    UBA_CR,4		/* UBA control register offset */
        .set    UBA_MAP,0x800		/* UBA offset to map reg's */
/* RL11 registers and bits */
        .set    HL,0174400-0160000	/* address of RL11 */
        .set    HLBPSECT,512		/* sector size in bytes (kludge) */
        .set    HL_cs,HL+0		/* control and status */
        .set    HL_ba,HL+2		/* bus address */
        .set    HL_da,HL+4		/* disk address */
        .set    HL_wc,HL+6		/* word count */
        .set    HL_RDY,0200		/* READY  */
        .set    HL_RCOM,014		/* read command */
        .set    HL_SEEK,06		/* seek */
        .set    HL_RESET,013		/* reset drive */
        .set    HL_GSTAT,04		/* get status command */
        .set    HL_pRDY,7		/* position of ready bit */
        .set    HL_pERR,15		/* position of error bit */

init:
/* r9   UBA address */
/* r8	RL addr */
        .word   0			/* entry mask for dec monitor */
        nop;nop;nop;nop;nop;nop;nop;nop	/* some no-ops for 750 boot to skip */
	nop;nop;
	cvtbl	$MAJOR,r10		/* major("/dev/xx0a") */
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
	ashl	$8,r3,r7		/* unit number, shifted for HL_cs */

/* init rl11, and drive, don't check for any errors now */
        movw    $HL_RESET,HL_da(r8)
        bisw3	r7,$HL_GSTAT,HL_cs(r8)
/* relocate to high core */
start:
        movl    $RELOC,sp
        moval   init,r6
        movc3   $end,(r6),(sp)
        jmp     *$RELOC+start2
/* now running relocated */
/* read in the boot program */
	.set	PROGSIZE,(BOOTSIZE*HLBPSECT)
start2:
	movw	$1,HL_da(r8)			/* seek to cylinder 0 */
	bisw3	r7,$HL_SEEK,HL_cs(r8)
1:
        movw    HL_cs(r8),r0
        bbc     $HL_pRDY,r0,1b
        bbs     $HL_pERR,r0,hlerr
	/* Rl has 256 byte sectors */
	movw	$2,HL_da(r8)			/* read program */
	movw	$-PROGSIZE/2,HL_wc(r8)
	clrl	r0
1:
	bisl3	$0x80000000,r0,UBA_MAP(r9)
	addl2	$4,r9
	aobleq	$BOOTSIZE,r0,1b
	clrw	HL_ba(r8)
	bisw3	r7,$HL_RCOM,HL_cs(r8)
1:
        movw    HL_cs(r8),r0
        bbc     $HL_pRDY,r0,1b
        bbc     $HL_pERR,r0,done
hlerr:
        halt				/* ungraceful */
done:
        movl    $PROGSIZE,r3
clrcor:
        clrq    (r3)
        acbl    $RELOC,$8,r3,clrcor
/* run loaded program */
        calls   $0,*$0
        brw     start2
physUBA:
	.long	0xf30000		/* uba0 */
	.long	0xf32000		/* uba1 */
end:
