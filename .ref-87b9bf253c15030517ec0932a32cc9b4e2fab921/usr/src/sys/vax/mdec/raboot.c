/*	raboot.c	4.2	83/02/20	*/

/*
 * UDA50 1st level boot program: loads next 7.5Kbytes from
 * boot sector of file system and sets it up to run.
 *	UNTESTED
 */
	.set	RELOC,0x50000
	.set	BOOTSIZE,15		/* size of boot in sectors */
	.set	RABPSECT,512		/* bytes per sector */

init:
	.word	0  			/* entry mask for dec monitor */
	nop;nop;nop;nop;nop;nop;nop;nop /* some no-ops for 750 boot to skip */
	nop;nop;
start:
	movl	r5,r11			/* boot flags */
	movl	$RELOC,sp
	moval	init,r9
	movc3	$end,(r9),(sp)
	jmp	*$RELOC+start2
/* now running relocated */
/* bring in the boot program */
start2:					/* running relocated */
	clrl	r9			/* transfer counter */
	clrl	r5			/* transfer address */
	movl	$1,r8			/* requested sector # */
1:
	pushr	$0xffff			/* BEGIN FIREWALL */
	calls	$0,(r6)			/* call ROM-based driver */
	blbs	r0,2f
	halt				/* read error */
2:
	popr	$0xffff			/* END FIREWALL */
	incl	r8			/* bump sector */
	addl2	$RABPSECT,r5		/* bump memory location */
	aobleq	BOOTSIZE,r9,1b

	.set	PROGSIZE,(BOOTSIZE*RABPSECT)
done:
	movl	$PROGSIZE,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* start loaded program */
	movl	$9,r10			/* major("/dev/ra0a") */
	calls	$0,*$0
	brw	start2
end:
