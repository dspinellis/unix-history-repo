/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)hpboot.c	6.3 (Berkeley) %G%" */


/*
 * RP??/RM?? 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 * Always reads from drive 0.
 */
	.set	BOOTSIZE,15		/* size of boot in sectors */
	.set	RELOC,0x70000
/* MBA registers */
	.set	M_cr,4			/* MBA control reg */
	.set	M_var,12		/* MBA virt addr reg */
	.set	M_bc,16			/* MBA byte count reg */
	.set	M_map,0x800		/* start of MBA map reg's */
	.set	MBAinit,1		/* MBA init bit in MBA control reg */
/* Drive information */
	.set	RP,0x400		/* start of drive registers */
	.set	RP_cr,RP+0		/* control status register */
	.set	RP_sr,RP+4		/* drive status reg */
	.set	RP_stk,RP+0x14		/* desired track/sector reg */
	.set	RP_dt,RP+0x18		/* drive type reg */
	.set	RP_off,RP+0x24		/* RP offset reg */
	.set	RP_cyl,RP+0x28		/* desired cyl reg */
	.set	RPBPSECT,512		/* bytes per sector */
/* RP?? function codes, status bits  */
	.set	RP_GO,1			/* go */
	.set	RP_RED,070		/* read */
	.set	RP_DC,010		/* drive clear */
	.set	RP_RIP,020		/* read in preset */
	.set	RP_FMT,0x1000		/* format 22 */
	.set	RP_MOL,0x1000		/* medium on line */
	.set	RP_DRY,0200		/* drive ready */
	.set	RP_ERR,040000		/* composite error */
	.set	RP_pDRY,7		/* bit position of RP_DRY */
	.set	RP_pERR,14		/* bit position of RP_ERR */

init:
	.word	0			/* entry mask for DEC monitor */
	nop;nop;nop;nop;nop;nop;nop;nop	/* some no-ops for 750 boot to skip */
	nop;nop;
start:
	movl	r5,r11
	movl	$RELOC,sp
	moval	init,r6
	movc3	$end,(r6),(sp)
	jmp	*$RELOC+start1
/* running relocated */
start1:
/* get cpu type */
	.set	SID,0x3e
	mfpr	$SID,r0
	extzv	$24,$8,r0,r0
	ashl	$2,r0,r1
/* get mba location and init it */
	moval	physMBA,r2
	addl3	r1,r2,r3
	movl	(r3),r9
	movl	$MBAinit,M_cr(r9)
/* read-in-preset the drive and set format */
	movl	$RP_RIP+RP_GO,RP_cr(r9)
	movl	$RP_FMT,RP_off(r9) 

	.set	PROGSIZE,(BOOTSIZE*RPBPSECT)
start2:
	movl	$0,RP_cyl(r9)
	movl	$1,RP_stk(r9)
	movl	$-PROGSIZE,M_bc(r9)
/* set up MASSBUS map for DMA */
	clrl	r0
1:
	bisl3	$0x80000000,r0,M_map(r9)[r0]
	aobleq	$BOOTSIZE,r0,1b
	clrl	M_var(r9)
	movl	$RP_RED+RP_GO,RP_cr(r9)
rprdy:
	movl	RP_sr(r9),r0
	bbc	$RP_pDRY,r0,rprdy
	bbs	$RP_pERR,r0,rperr
	clrl	r3
/* Eagle's are too fast for the controller. Slow the thing down. */
buzz:	acbl	$2000,$1,r3,buzz
	bicpsw	$2
	jbr	clear
rperr:
	halt
/* clear core and execute program */
clear:
	movl	$PROGSIZE,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* run loaded program */
	clrl	r10			/* major("/dev/hp0a") */
	calls	$0,*$0
	brw	start2

	.align	2
physMBA:
	.long	0
	.long	0x20010000
	.long	0xf28000

end:
