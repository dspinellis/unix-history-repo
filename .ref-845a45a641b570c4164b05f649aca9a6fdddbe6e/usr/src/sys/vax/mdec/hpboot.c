/*
 * Copyright (c) 1980,1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)hpboot.c	7.2 (Berkeley) %G%" */
#include <sys/disklabel.h>


/*
 * RP??/RM?? 1st level boot program: loads next 7.5Kbytes from
 * boot sectors of file system and sets it up to run.
 * Reads from the controller and drive passed in from the boot
 * rom.
 *   R1:  address of the boot device's adapter
 *   R2:  controller number of the boot device
 *   R3:  unit number of the boot device
 *   R5:  software boot control flags
 *   R6:  address of driver subroutine from ROM
 *   SP:  base address of usable memory + 0x200
 */
	.set	BOOTSIZE,15		/* size of boot in sectors */
	.set	RELOC,0x70000
/* MBA registers */
	.set	M_cr,4			/* MBA control reg */
	.set	M_sr,8			/* MBA status reg */
	.set	M_var,12		/* MBA virt addr reg */
	.set	M_bc,16			/* MBA byte count reg */
	.set	M_map,0x800		/* start of MBA map reg's */
	.set	MBAinit,1		/* MBA init bit in MBA control reg */
	.set	MBABUSY,0x80000000	/* MBA SR: data transfer busy */
	.set	pMBABUSY,31		/* bit position of  MBABUSY */
/* Drive information */
	.set	RP,0x400		/* start of drive registers */
	.set	RPDR,0x80		/* offset per drive unit */
	.set	RP_cr,0			/* control status register */
	.set	RP_sr,4			/* drive status reg */
	.set	RP_stk,0x14		/* desired track/sector reg */
	.set	RP_dt,0x18		/* drive type reg */
	.set	RP_off,0x24		/* RP offset reg */
	.set	RP_cyl,0x28		/* desired cyl reg */
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
	clrl	r10			/* major("/dev/hp0a") */
	extzv	$13,$2,r1,r4		/* get MBA number from R1 */
	insv	r4,$24,$8,r10		/* set MBA number */
	insv	r3,$16,$8,r10		/* drive number */
	extzv	$12,$4,r5,r4		/* get partition from r5 */
	bicw2	$0xf000,r5		/* remove from r5 */
	insv	r4,$8,$4,r10		/* set partition */
	movl	r5,r11
	movl	r1,r9			/* save adaptor address */
	movl	r3,r8			/* and unit number */
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
	moval	init,r6
	movc3	$end,(r6),(sp)
	jmp	*$RELOC+start1
/* running relocated */
start1:
	movl	$MBAinit,M_cr(r9)
/* read-in-preset the drive and set format */
	mull2	$RPDR,r8
	movab	RP(r9)[r8],r8
	movl	$RP_RIP+RP_GO,RP_cr(r8)
	movl	$RP_FMT,RP_off(r8) 

	.set	PROGSIZE,(BOOTSIZE*RPBPSECT)
start2:
	movl	$0,RP_cyl(r8)
	movl	$1,RP_stk(r8)
	movl	$-PROGSIZE,M_bc(r9)
/* set up MASSBUS map for DMA */
	clrl	r0
1:
	bisl3	$0x80000000,r0,M_map(r9)[r0]
	aobleq	$BOOTSIZE,r0,1b
	clrl	M_var(r9)
	movl	$RP_RED+RP_GO,RP_cr(r8)
rprdy:
	movl	RP_sr(r8),r0
	bbc	$RP_pDRY,r0,rprdy
	bbs	$RP_pERR,r0,rperr
rprdy2:
	bbs	$pMBABUSY,M_sr(r9),rprdy2

/* Eagles are too fast for the controller. Slow the thing down. */
/* (May not be needed with wait for mba above.) */
	clrl	r3
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
	calls	$0,*$0
	brw	start2

end:
