/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
_sccsid:.asciz	"@(#)rlboot.c	6.2 (Berkeley) %G%"
#endif not lint

/*
 * RL02 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 * Always reads from drive 0.
 * 	UNTESTED
 */
	.set	BOOTSIZE,15		/* 15 ``sectors'' */
        .set    RELOC,0x50000
        .set    SID,62			/* system ID register */
/* UBA registers */
        .set    UBA_CNFGR,0		/* UBA configuration register */
        .set    UBA_CR,4		/* UBA control register offset */
        .set    UBA_MAP,0x800		/* UBA offset to map reg's */
        .set    UBAinit,1		/* UBA init bit in UBA control reg */
        .set    pUBIC,16		/* Unibus init complete */
/* RL11 registers and bits */
        .set    HL,0174400-0160000	/* address of RL11 */
        .set    HLBPSECT,512		/* sector size in bytes (kludge) */
        .set    HL_cs,HL+0		/* control and status */
        .set    HL_ba,HL+2		/* bus address */
        .set    HL_da,HL+4		/* disk address */
        .set    HL_wc,HL+6		/* word count */
        .set    HL_GO,0			/* go bit */
        .set    HL_RCOM,014		/* read command */
        .set    HL_SEEK,06		/* seek */
        .set    HL_RESET,013		/* reset drive */
        .set    HL_GSTAT,04		/* get status command */
        .set    HL_pRDY,7		/* position of ready bit */
        .set    HL_pERR,15		/* position of error bit */

init:
/* r9   UBA address */
/* r10  umem addr */
        .word   0			/* entry mask for dec monitor */
        nop;nop;nop;nop;nop;nop;nop;nop	/* some no-ops for 750 boot to skip */
	nop;nop;
/* get cpu type and find the first uba */
        mfpr    $SID,r0
        extzv   $24,$8,r0,r0		/* get cpu type */
        ashl    $2,r0,r1
        movab   physUBA,r2		/* get physUBA[cpu] */
        addl2   r1,r2
        movl    (r2),r9
        movab   physUMEM,r2		/* get physUMEM[cpu] */
        addl2   r1,r2
        movl    (r2),r10
/* if 780, init uba */
        cmpl    r0,$1
        bneq    2f
        movl    $UBAinit,UBA_CR(r9)
1:
        bbc     $pUBIC,UBA_CNFGR(r9),1b
2:
/* init rl11, and drive 0, don't check for any errors now */
        movw    $HL_RESET,HL_da(r10)
        movw    $HL_GSTAT+HL_GO,HL_cs(r10)
/* relocate to high core */
start:
        movl    r5,r11			/* save boot flags */
        movl    $RELOC,sp
        moval   init,r6
        movc3   $end,(r6),(sp)
        jmp     *$RELOC+start2
/* now running relocated */
/* read in the boot program */
	.set	PROGSIZE,(BOOTSIZE*HLBPSECT)
start2:
	movw	$1,HL_da(r10)			/* seek to cylinder 0 */
	movw    $HL_SEEK+HL_GO,HL_cs(r10)
1:
        movw    HL_cs(r10),r0
        bbc     $HL_pRDY,r0,1b
        bbs     $HL_pERR,r0,hlerr
	/* Rl has 256 byte sectors */
	movw	$2,HL_da(r10)			/* read program */
	movw	$-PROGSIZE/2,HL_wc(r10)
	clrl	r0
1:
	bisl3	$0x80000000,r0,UBA_MAP(r9)
	addl2	$4,r9
	aobleq	$BOOTSIZE,r0,1b
	clrw	HL_ba(r10)
	movw	$HL_RCOM+HL_GO,HL_cs(r10)
1:
        movw    HL_cs(r10),r0
        bbc     $HL_pRDY,r0,1b
        bbs     $HL_pERR,r0,hlerr
	brw	done
hlerr:
        halt				/* ungraceful */
done:
        movl    $PROGSIZE,r3
clrcor:
        clrq    (r3)
        acbl    $RELOC,$8,r3,clrcor
/* run loaded program */
        movl    $14,r10			/* major("/dev/hl0a") */
        calls   $0,*$0
        brw     start2
physUBA:
        .long   0
        .long   0x20006000		/* 11/780 */
        .long   0xf30000		/* 11/750 */
        .long   0xf26000		/* 11/730 */
physUMEM:
        .long   0
        .long   0x2013e000		/* 11/780 */
        .long   0xffe000		/* 11/750 */
        .long   0xffe000		/* 11/730 */
end:
