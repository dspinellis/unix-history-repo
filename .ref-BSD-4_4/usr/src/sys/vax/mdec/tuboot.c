/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)tuboot.c	7.2 (Berkeley) 8/28/86" */

/*
 * VAX tu58 console cassette boot block
 *
 * Helge Skrivervik CSRG/UCB 18jun83
 *
 * Reads a program from a rt-11 directory on tape
 * and executes it.  Programs must be stripped of
 * the header and is loaded ``bits as is''.
 * You can return to this loader via ``ret'' as
 * you are called ``calls $0,ent''.
 * Error checking and recovery is almost nonexistant
 * due to the severe space constraints.
 *
 * NOTE: Any changes to this program are likely to
 *	 bring the size over 512 bytes ....
 *
 * Based on tp format bootstrap originally written by Thomas Ferrin.
 *
 */
	.set	CTABLE,0x400	/* where to load the rad50 cnv table */
	.set	RELOC,0x70000
/* rt-11 directory definitions */
	.set	DIRBLK,6	/* rt-11 directory starts at block 6 */
	.set	FILSIZ,8	/* rt-11 direc entry offset for file size */
	.set	ENTSIZ,14	/* size of 1 rt-11 dir entry, bytes */
	.set	BLKSIZ,512	/* tape block size, bytes */
	.set	NUMDIR,2	/* no. of dir blocks on tape */
	.set	FNSIZ,8		/* size of rad50 filename + 2 */
	.set	NAME,2		/* direc entry offset for filename */
	.set	STATUS,1	/* direc entry offset for entry status */
/* rt-11 directory entry status */
	.set	RT_ESEG,8	/* end of directory segment */
	.set	RT_NULL,2	/* empty entry */
	.set	RT_FILE,4	/* valid file entry */
/* processor registers and bits */
	.set	RXCS,32
	.set	RXDB,33
	.set	TXCS,34
	.set	TXDB,35
	.set	RXCS_DONE,0x80
	.set	TXCS_RDY,0x80
	.set	TXCS_pr,7	/* bit position of TXCS ready bit */
	.set	RXCS_pd,7	/* bit position of RXCS done bit */
/* console storage registers and bits */
	.set	CSRS,0x1c
	.set	CSRD,0x1d
	.set	CSTS,0x1e
	.set	CSTD,0x1f
/* TU commands and bits */
	.set	TU_BREAK,1
	.set	TU_INIT,4
	.set	TU_CONTINUE,16
	.set	TU_READY,7	/* bit position of CSRS ready bit */
	.set	TU_PACKETLEN,8	/* length of readcom block */
/* local stack variables */
	.set	ext,-4			/* file ext. */
	.set	name,-20		/* 12 bytes for full name */
	.set	rt_name,-20-FNSIZ	/* rad50 file name */
/* reboot flags for boot */
	.set	RB_ASK,3		/* ask name and come up single user */

/* 
 * Initialization.
 */
init:
	.word	0 		/* entry mask for dec monitor */
	nop;nop;nop;nop;nop	/* some no-ops for 750 boot rom to skip */
	nop;nop;nop;nop;nop 
	movl	$RELOC,fp	/* core loc to which to move this program */
	addl3	$rt_name,fp,sp	/* set stack pointer; leave room for locals */
	clrl	r0
1:
	movc3	$end,(r0),(fp)	/* move boot up to relocated position */
	jmp	start+RELOC

start:
	mtpr	$TU_BREAK,$CSTS		/* set break condition */
	clrl	r2			/* nulls */
	bsbw	xmit2			/* wait 2 character times */
	mfpr	$CSRD,r2		/* clear receive buffer */
	movzwl	$TU_INIT|(TU_INIT<<8),r2	/* load 2 INIT opcodes */
	bsbw	xmit2			/* xmit 'em */
1:
	mfpr	$CSRD,r7		/* get recv data */
	cmpb	r7,$TU_CONTINUE		/* is it a continue flag? */
	bneq	1b			/* nope, look more */

	movab	name(fp),r4		/* start of filename storage */
	clrq	(r4)			/* init name field */
	clrq	name+8(fp)
	clrq	rt_name(fp)		/* init rad50 filename */
	movzbl	$'=,r0			/* prompt character */
	bsbw	putc			/* output char to main console */

/* 
 * Read in a file name from console. 
 */
	movl	r4,r1			/* loc at which to store file name */
nxtc:
	bsbw	getc			/* get input char's in file name */
	cmpb	r0,$012			/* terminator ? */
	beql	nullc
	movb	r0,(r1)+
	brb	nxtc
nullc:
	cmpl	r4,r1
	beql	start			/* restart if empty string */
	clrb	(r1)			/* add null byte at end */

/*
 * User-specified filename has been stored at name(fp),
 * read the entire directory contents into low core.
 */
dirred:
	movl	$DIRBLK,r10		/* directory starts at block DIRBLK */
	movl	$(NUMDIR*BLKSIZ),r6	/* no. bytes in total dir */
	clrl	r11			/* start address */
	bsbw	taper			/* read no. bytes indicated */
/*
 * Read in the character conversion table which reside in block 1
 * (the second block) on the cassette. Place it after the directory
 * on low core (from 0x400).
 */
	movl	$1,r10			/* block number */
	movl	$BLKSIZ,r6		/* read one block */
	bsbw	taper

/*
 * Convert the ascii filename to rad50.
 * R4 still points to name(fp)
 */
	movl	$6,r3			/* max length of filename */
1:
	cmpb	$'.,(r4)+		/* look for '.' */
	beql	1f
	sobgtr	r3,1b
	incl	r4			/* point past '.' if ext is present */
1:
	clrb	-1(r4)			/* end name with null */
	movl	$3,r3			/* max length of extension */
	movab	ext(fp),r5		/* place extension here */
1:
	movb	(r4)+,(r5)+
	beql	1f			/* the string is null terminated */
	sobgtr	r3,1b
1:
	movab	name(fp),r4
	movab	rt_name(fp),r5		/* ptr to rad50 name */
	bsbw	rad50			/* convert filename */
	movab	ext(fp),r4
	movab	rt_name+4(fp),r5
	bsbw	rad50			/* convert extension */

/*
 * Search entire directory for user-specified file name.
 */

	movab	rt_name(fp),r4		/* search for this file */
	movl	$10,r5			/* point to first file entry */
	movzwl	-2(r5),r10		/* r10 = block # where files begin */
2:
	cmpc3	$6,NAME(r5),(r4)	/* see if dir entry matches filename */
	beql	fndfil			/* found match */
1:
	addw2	FILSIZ(r5),r10		/* add file length to block pointer */
	addl2	$ENTSIZ,r5		/* move to next entry */
/*	cpmb	STATUS(r5),$RT_NULL	/* check if deleted file */
/*	beql	1b /* not really necessary since deleted entries will fail */
		   /* to compare anyway */
	cmpb	STATUS(r5),$RT_ESEG	/* check if end of segment */
	bneq	2b
	brw	start			/* entry not in directory; start over */

/* 
 * Found desired directory entry 
 */
fndfil:
					/* start block no., 2 bytes in r10 */
	movzwl	FILSIZ(r5),r6		/* file size (blocks) */
	mull2	$BLKSIZ,r6		/* file size (bytes) */
	cmpl	r6,$RELOC-512		/* check if file fits below stack */
	blss	filok
	brw	start			/* file too large */

/* 
 * Read in desired file from tape.
 */
filok:
	movl	r6,r5			/* start of bss space */
	clrl	r11			/* start address */
	bsbb	taper

/* 
 * Clear core.
 */
	subl3	r5,$RELOC-4,r0		/* no. bytes to clear */
1:
	clrb	(r5)+
	sobgtr	r0,1b

/* 
 * Jump to start of file & execute.
 */
	addl3	$20,fp,ap		/* ?? */
	clrl	r5
	movl	$RB_ASK,r11
	calls	$0,(r5)
bad:
	brw	start

/* 
 * Read (r6) bytes from block (r10) 
 * into loc (r11).
 */
taper:
	clrl	r8			/* initialize checksum */
	movab	readcom,r0		/* read command packet addr */
	movzbl	$TU_PACKETLEN/2,r1	/* size of readcom block */
1:
	movzwl	(r0)+,r2		/* get 2 chars from block */
	bsbb	xmit			/* xmit and update ckecksum */
	sobgtr	r1,1b			/* loop if more */

/* 
 * Now do variable part of packet. 
 */
	movl	r6,r2			/* byte count */
	bsbb	xmit
	movl	r10,r2			/* starting block number */
	bsbb	xmit
	movzwl	r8,r2			/* accumulated ckecksum */
	bsbb	xmit

/* 
 * Collect read packet from device. 
 */
1:
	bsbb	recv2			/* get 2 packet characters */
	decb	r2			/* data packet? */
	bneq	1f			/* branch on end of data */
	movzbl	r1,r8			/* get byte count of packet */

/* 
 * Read data into memory.
 */
2:
	bsbb	recv1			/* get a char */
	movb	r1,(r11)+		/* stuff into memory */
	sobgtr	r8,2b			/* loop if more */
	bsbb	recv2			/* skip checksum */
	brb	1b			/* read next packet */

/* 
 * End of data xfer; check for errors.
 */
1:
	bsbb	recv2			/* get success code */
	tstl	r1			/* error in read? */
	blss	9f			/* branch if status error */
	movl	$5,r0
1:
	bsbb	recv2			/* discard 10 bytes */
	sobgtr	r0,1b
	rsb

/* Fatal error */
9:
	movab	ermsg,r1
1:
	movb	(r1)+,r0
	beql	bad
	bsbb	putc
	brb	1b

/* 
 * Update checksum in r8 and xmit 2 characters.
 */
xmit:
	addw2	r2,r8			/* update checksum */
	adwc	$0,r8			/* add  in carry */

/* send the 2 characters contained in r2 */
xmit2:
	bsbb	1f			/* xmit one of 'em */
	ashl	$-8,r2,r2		/* get next char */
					/* fall into... */
1:
	mfpr	$CSTS,r7		/* get xmit status */
	bbc	$TU_READY,r7,1b		/* loop until ready */
	mtpr	r2,$CSTD		/* send char */
	rsb

/* 
 * Receive 2 characters, return in r2 and r1.
 */
recv2:
	bsbb	recv1			/* recv one of 'em */
					/* fall into... */

/* 
 * Receive 1 character.
 */
recv1:
	movzbl	r1,r2			/* save previous byte */
1:
	mfpr	$CSRS,r7		/* get recv status */
	bbc	$TU_READY,r7,1b		/* loop until ready */
	mfpr	$CSRD,r1		/* get char */
	blss	9b			/* branch on recv error */
	rsb

getc:
	mfpr	$RXCS,r0
	bbc	$RXCS_pd,r0,getc	/* receiver ready ? */
	mfpr	$RXDB,r0
	extzv	$0,$7,r0,r0
	cmpb	r0,$015
	bneq	putc			/* echo and return */
	bsbb	putc			/* carriage return */
/*	movb	$0,r0	*/
/*	bsbb	putc	*/		/* delay */
	movb	$012,r0			/* send line feed and return */
putc:
	mfpr	$TXCS,r2
	bbc	$TXCS_pr,r2,putc	/* transmitter ready ? */
	mtpr	r0,$TXDB
	rsb

/*
 * Convert the filename given from the console
 * to radix 50 (rt-11) format.
 */
rad50:
	clrw	r1
	bsbb	getb50			/* get next ascii byte, exit if null */
	mull3	$03100,r0,r1
	bsbb	getb50
	mull3	$050,r0,r2
	addl2	r2,r1
	bsbb	getb50
	addl2	r0,r1			/* last byte, just add it in */
	movw	r1,(r5)+		/* save result */
	brb	rad50

getb50:
	movzbl	(r4)+,r0		/* get next ascii byte */
	beql	1f			/* if zero: end of string */
	movzbl	CTABLE(r0),r0		/* and get the r50 byte from the table*/
	rsb
1:
	tstl	(sp)+			/* we're through, get back to where */
					/* rad50 was called */
	movw	r1,(r5)			/* but first save the result */
	rsb

	.align	2
readcom:
	.byte	2			/* command packet flag */
	.byte	10			/* number of bytes in message */
	.byte	2			/* tu read opcode */
	.byte	0			/* modifier */
	.byte	0			/* unit number */
	.byte	0			/* switches */
	.word	0			/* sequence number */
					/* byte count and block number follow */

ermsg:
	.asciz	"tuerr\r\n"
end:

/*
 * Ascii to rad 50 conversion table,
 * stored on the second block on the cassette
 *
 * NOTE: Always make sure this table ends up
 * starting at byte 512!!!!
 */
	.align	2
	.data	2
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d00
	.long	0x1d1d1d1b
	.long	0x1d1d1d1d
	.long	0x1d1c1d1d
	.long	0x21201f1e
	.long	0x25242322
	.long	0x1d1d2726
	.long	0x1d1d1d1d
	.long	0x302011d
	.long	0x7060504
	.long	0xb0a0908
	.long	0xf0e0d0c
	.long	0x13121110
	.long	0x17161514
	.long	0x1d1a1918
	.long	0x1d1d1d1d
	.long	0x302011d
	.long	0x7060504
	.long	0xb0a0908
	.long	0xf0e0d0c
	.long	0x13121110
	.long	0x17161514
	.long	0x1d1a1918
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d1d
	.long	0x1d1d1d00
	.long	0x1d1d1d1b
	.long	0x1d1d1d1d
	.long	0x1d1c1d1d
	.long	0x21201f1e
	.long	0x25242322
	.long	0x1d1d2726
	.long	0x1d1d1d1d
	.long	0x302011d
	.long	0x7060504
	.long	0xb0a0908
	.long	0xf0e0d0c
	.long	0x13121110
	.long	0x17161514
	.long	0x1d1a1918
	.long	0x1d1d1d1d
	.long	0x302011d
	.long	0x7060504
	.long	0xb0a0908
	.long	0xf0e0d0c
	.long	0x13121110
	.long	0x17161514
	.long	0x1d1a1918
	.long	0x1d1d1d
	.data
