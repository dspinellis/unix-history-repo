/*	raboot.c	4.1	83/02/16	*/

/*
 * From 4.1 boot sector code by Scott Comer (Rice University).
 */
	.set	BOOTBASE, 0xfe00	/* relocated home of boot block */
	/*
	 * These three hold the register contents needed by the
	 * ROM driver subroutine to access the boot device.
	 */
	.set	driver_r1, DATA		
	.set	driver_r2, driver_r1+4	
	.set	driver_r3, driver_r2+4	
	.set	driver, driver_r3+4		/* addr of driver routine */

	.set	NOT_FIRST_64K, 		0x1001
	.set	UNSUPPORTED_DEVICE, 	0x1002
	.set	RETURN_FROM_BOOT, 	0x1003
	.set	COULD_NOT_FIND_BOOT, 	0x1004
	.set	FILE_TOO_LARGE, 	0x1005
	.set	FILE_READ_ERROR, 	0x1006

init:
	.long	0			/* boot block parameters */
	.long	0			/* (all unused, hence 0) */
	.long	0

/*
 * The registers are set by the console subsystem as follows.
 * Those marked with stars are saved by the driver subroutine.
 * Those marked with a "d" are used by the driver subroutine,
 * and must contain the indicated values before calling the driver.
 * 
 *    r0  = type of boot device (see 750 hardware reference, console)
 * ds r1  = address of the unibus i/o page
 * ds r2  = boot device CSR address
 * ds r3  = boot device unit number
 *  s r4  = 
 * ds r5  = software boot flags (driver: offset to buffer for read)
 *  s r6  = driver subroutine address
 *    r7  = 
 * d  r8  = LBN of block to read from disk
 *    r9  = 
 *  s r10 = 
 *  s r11 = 
 *  s ap  = 
 *    fp  = 
 *  s sp  = 
 * 
 * Memory is mapped as follows:
 * 
 * 0000 to 01ff	Boot block program
 * 0200 to f9ff	Available
 * fa00 to fdff	Drivers and control routines
 * fe00 to ffff	Available
 */
start:
	clrl	r7
	movl	r0, r10			/* save the device type */
	moval	init, r11		/* base address of good memory */
	movl	r5, ap			/* save the boot flags */
	tstl	r11			/* see if it is zero */
	beql	1f
	movzwl	$NOT_FIRST_64K, r7
	halt				/* not in first 64k of memory */
1:	moval	STACK(r11), sp		/* put the stack somewhere good */
	/* save the register contents needed by the boot driver */
	movl	r1, driver_r1(r11)
	movl	r2, driver_r2(r11)
	movl	r3, driver_r3(r11)
	movl	r6, driver(r11)
	/* relocate the boot program */
	movc3	$end, (r11), BOOTBASE(r11)
	jmp	BOOTBASE+start2(r11)
start2:					/* running relocated */
	calls	$0, BOOTBASE+read_file(r11)
	movl	r11, r9				/* save the base pointer */
	/* boot strap device codes from microcode routines */
	.set	AnyMassBus, 0
	.set	RK07, 1
	.set	RL02, 2
	.set	UDA50, 17
	.set	TU58, 64

	cmpb	r10, $AnyMassBus
	bneq	1f
	movzbl	$0, r10				/* massbus disk */
	brb	2f
1:
	cmpb	r10, $RK07
	bneq	1f
	movzbl	$3, r10				/* rk07 disk */
	brb	2f
1:
	cmpb	r10, $UDA50
	bneq	1f
	movzbl	$9, r10				/* uda50 */
	brb	2f
1:
	movzwl	$UNSUPPORTED_DEVICE, r7
	halt					/* unsupported device */
2:
	movl	ap, r11				/* software boot flags */
	addl3	di_size(r9), r9, r2		/* address to start clear */
	moval	BOOTBASE(r9), r1		/* address to stop clearing */
1:

	cmpl	r2, r1
	bgeq	2f
	clrb	(r2)+
	brb	1b
2:
	calls	$0, (r9)
	movzwl	$RETURN_FROM_BOOT, r7
	halt					/* end of program */
read_block:
	.word	0xffc			/* r11-r2 */
	clrq	-(sp)			/* make room for the buf addr */
	movl	driver_r1(r11), r1
	movl	driver_r2(r11), r2
	movl	driver_r3(r11), r3
	mull3	$2, 4(ap), r4		/* mult by 2 to get lbn */
	movl	r4, r8
	movl	8(ap), r5
	addl3	r5, r11, (sp)		/* for massbus babies */
	jsb	*driver(r11)		/* read the first block */
	blbs	r0, 1f
	movzwl	$FILE_READ_ERROR, r7
	halt				/* error reading file */
1:
	addl3	$1, r4, r8
	addl2	$512, r5
	addl3	r5, r11, (sp)		/* for massbus babies */
	jsb	*driver(r11)		/* read the second block */
	blbs	r0, 1f
	movzwl	$FILE_READ_ERROR, r7
	halt				/* error reading file */
1:
	ret
end:
