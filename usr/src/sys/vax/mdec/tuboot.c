/*	tuboot.s	4.1	83/02/16	*/

/*
 * VAX tu58 console cassette boot block
 *
 * Thomas Ferrin  27oct82
 *
 * Reads a program from a tp directory on tape
 * and executes it.  Program must be stripped of
 * the header and is loaded ``bits as is''.
 * You can return to this loader via ``ret'' as
 * you are called ``calls $0,ent''.
 * 
 * Helge Skrivervik CSRG/UCB 18jun83
 * 	Changed to use rt-11 format directory & files
 *	instead of tp format
 */
	.set	RELOC,0x70000
/* a.out defines */
	.set	HDRSIZ,040	/* size of file header for VAX */
	.set	MAGIC,0410	/* file type id in header */
	.set	TSIZ,4		/* text size */
	.set	DSIZ,8		/* data size */
	.set	BSIZ,12		/* bss size */
	.set	TENT,024	/* task header entry loc */
/* rt-11 directory definitions */
	.set	DIRBLK,6	/* rt-11 directory starts at block 6 */
	.set	FILSIZ,8	/* rt-11 direc entry offset for file size */
	.set	ENTSIZ,14	/* size of 1 rt-11 dir entry, bytes */
	.set	BLKSIZ,512	/* tape block size, bytes */
	.set	NUMDIR,2	/* no. of dir blocks on tape */
	.set	RT_FNSIZ,8	/* size of rad50 filename + 2 */
	.set	NAME,2		/* direc entry offset for filename */
	.set	RT_STAT,1	/* direc entry offset for entry status */
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
	.set	ext,-4				/* file ext. */
	.set	name,-20			/* 12 bytes for full name */
	.set	rt_name,-20-RT_FNSIZ		/* rad50 file name */

/* 
 * Initialization.
 */
init:
	.word	0 		/* entry mask for dec monitor */
	nop;nop;nop;nop;nop	/* some no-ops for 750 boot rom to skip */
	nop;nop;nop;nop;nop 
	movl	$RELOC,fp	/* core loc to which to move this program */
	addl3	$name,fp,sp	/* set stack pointer; leave room for locals */
	clrl	r0
1:
	movc3	$end,(r0),(fp)	/* move boot up to relocated position */
	jmp	start+RELOC

start:
/* init tu58 */
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

	clrq	rt_name(fp)		/* init rad50 filename */
	movab	name(fp),r4		/* start of filename storage */
	movzbl	$'=,r0			/* prompt character */
	bsbw	putc			/* output char to main console */

/* read in a file name */
	movl	r4,r1			/* loc at which to store file name */
nxtc:
	bsbw	getc			/* get input char's in file name */
	cmpb	r0,$012			/* terminator ? */
	beql	nullc
	movb	r0,(r1)+
	brb	nxtc
nullc:
	subl3	r4,r1,r9		/* size of path name */
	beql	start			/* restart if empty string */
	clrb	(r1)			/* add null byte at end */
	incl	r9			/* and fix length */

/*
 * user-specified filename has been stored at name(fp) 
 * read in entire directory contents into low core 
 */
dirred:
	movl	$DIRBLK,r10		/* directory starts at block DIRBLK */
	movl	$(NUMDIR*BLKSIZ),r6	/* no. bytes in total dir */
	clrl	r11			/* start address */
	bsbw	taper			/* read no. bytes indicated */
/*
 * Read in the character conversion table which reside in block 1
 * (the second block) on the cassette.
 */
	movl	$1,r10			/* start block */
	movl	$BLKSIZ,r6		/* read one block */
	movl	0x400,r11		/* place it after the directory */
	bsbw	taper

/*
 * Convert the ascii filename to rad50.
 */
	movab	name(fp),r4		/* ptr to ascii name */
	movl	$6,r3			/* max length of filename */
1:
	cmpb	$'.,(r4)+		/* look for '.' */
	sobgtr	r3,1b
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
	movl	$10,r5			/* dir buff loc = 0, point to first */
					/* file entry */
	movzwl	-2(r5),r3		/* r3 = block # where files begin */
2:
	cmpc3	$6,NAME(r5),(r4)	/* see if dir entry matches filename */
	beql	fndfil			/* found match */
1:
	addw2	FILSIZ(r5),r3		/* add file length to block pointer */
	addl2	$ENTSIZ,r5		/* move to next entry */
#	cpmb	RT_STAT(r5),$RT_NULL	/* check if deleted file */
#	beql	1b
	cmpb	RT_STAT(r5),$RT_ESEG	/* check if end of segment */
	bneq	2b
	brw	start			/* entry not in directory; start over */

/* 
 * Found desired directory entry 
 */
fndfil:
	movl	r3,r10			/* start block no., 2 bytes */
	movzwl	FILSIZ(r5),r6		/* file size (blocks) */
	mull2	$BLKSIZ,r6		/* file size (bytes) */
#	cmpl	r6,$RELOC-512		/* check if file fits below stack */
#	blss	filok
#	brw	start			/* file too large */

/* 
 * Read in desired file from tape.
 */
filok:
	movl	r6,r5			/* start of bss space */
	clrl	r11			/* start address */
	bsbb	taper
#	bsbb	rew

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
	addl3	$20,fp,ap
	clrl	r5
	calls	$0,(r5)
bad:
	brw	start

/* rewind tape */
#ifdef notdef
rew:
	movb	$5,readcom+2		/* position opcode */
	clrl	r10			/* block 0 */
	clrl	r6			/* 0 bytes */
	bsbb	taper
	movb	$2,readcom+2		/* read opcode */
	rsb
#endif

/* read (r6) bytes from (r10) into loc (r11) */
taper:
	clrl	r8			/* initialize checksum */
	movab	readcom,r0		/* read command packet addr */
	movzbl	$TU_PACKETLEN/2,r1	/* size of readcom block */
1:
	movzwl	(r0)+,r2		/* get 2 chars from block */
	bsbb	xmit			/* xmit and update ckecksum */
	sobgtr	r1,1b			/* loop if more */

	/* now do variable part of packet */
	movl	r6,r2			/* byte count */
	bsbb	xmit
	movl	r10,r2			/* starting block number */
	bsbb	xmit
	movzwl	r8,r2			/* accumulated ckecksum */
	bsbb	xmit

	/* collect read packet from device */
	movl	r11,r0			/* starting addr */
1:
	bsbb	recv2			/* get 2 packet characters */
	decb	r2			/* data packet? */
	bneq	1f			/* branch on end of data */
	movzbl	r1,r8			/* get byte count of packet */

	/* read data into memory */
2:
	bsbb	recv1			/* get a char */
	movb	r1,(r0)+		/* stuff into memory */
	sobgtr	r8,2b			/* loop if more */
	bsbb	recv2			/* skip checksum */
	brb	1b			/* read next packet */

	/* end of data xfer; check for errors */
1:
	subl2	r6,r0			/* all bytes xfered? */
	bneq	9f			/* nope, error */
	bsbb	recv2			/* get success code */
	tstl	r1			/* error in read? */
	blss	9f			/* branch if status error */
	movl	$5,r0
1:
	bsbb	recv2			/* discard 10 bytes */
	sobgtr	r0,1b
	rsb

	/* fatal error */
9:
	movab	ermsg,r1
1:
	movb	(r1)+,r0
	beql	bad
	bsbb	putc
	brb	1b

	/* update checksum in r8 and xmit 2 characters */
xmit:
	addw2	r2,r8			/* update checksum */
	bcc	xmit2			/* branch if no overflow */
	incw	r8			/* add  in carry */

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

	/* receive 2 characters, return in r2 and r1 */
recv2:
	bsbb	recv1			/* recv one of 'em */
					/* fall into... */

	/* receive 1 character */
recv1:
	movzbl	r1,r2			/* save previous byte */
1:
	mfpr	$CSRS,r7		/* get recv status */
	bbc	$TU_READY,r7,1b		/* loop until ready */
	mfpr	$CSRD,r1		/* get char */
#	blss	9b			/* branch on recv error */
	rsb

getc:
	mfpr	$RXCS,r0
	bbc	$RXCS_pd,r0,getc	/* receiver ready ? */
	mfpr	$RXDB,r0
	movzbl	r0,r0
	cmpb	r0,$015
	bneq	putc			/* echo and return */
	bsbb	putc			/* carriage return */
#	movb	$0,r0
#	bsbb	putc			/* delay */
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
	movl	$0x400,r6		/* address of conversion table */
1:
	bsbb	getb50			/* get next ascii byte, exit if null */
	mull3	$03100,r0,r1
	bsbb	getb50
	mull3	$050,r0,r2
	addl2	r2,r1
	bsbb	getb50
	addl2	r0,r1			/* last byte, just add it in */
	movw	r1,(r5)+		/* save result */
	brb	1b

getb50:
	movzbl	(r4)+,r0		/* get next ascii byte */
	beql	1f			/* if zero: end of string */
	addl2	r6,r0			/* calculate conversion table address */
	movzbl	(r0),r0			/* and get the r50 byte from the table*/
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
	.asciz	"tu58 err\r\n"
end:
