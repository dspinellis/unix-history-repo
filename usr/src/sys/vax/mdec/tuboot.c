/*	tuboot.c	4.1	83/02/16	*/

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
 */
	.set	RELOC,0x70000
/* a.out defines */
	.set	HDRSIZ,040	/* size of file header for VAX */
	.set	MAGIC,0410	/* file type id in header */
	.set	TSIZ,4		/* text size */
	.set	DSIZ,8		/* data size */
	.set	BSIZ,12		/* bss size */
	.set	TENT,024	/* task header entry loc */
/* tp directory definitions */
/*	.set	DIRBLK,8	/* tp directory starts at block 8 */
	.set	DIRBLK,1	/* tp directory starts at block 1 */
	.set	FILSIZ,38	/* tp direc offset for file size */
	.set	BNUM,44		/* tp dir offset for start block no. */
	.set	ENTSIZ,64	/* size of 1 TP dir entry, bytes */
	.set	PTHSIZ,32	/* size of TP path name, bytes */
	.set	BLKSIZ,512	/* tape block size, bytes */
	.set	NUMDIR,24	/* no. of dir blocks on tape */
	.set	ENTBLK,8	/* no. of dir entries per tape block */
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
	.set	tapa,-4		/* desired tape addr */
	.set	name,-8-PTHSIZ	/* operator-typed file name */
/* ===== */

/* initialization */
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
	clrl	r2		/* nulls */
	bsbw	xmit2		/* wait 2 character times */
	mfpr	$CSRD,r2	/* clear receive buffer */
	movzwl	$TU_INIT|(TU_INIT<<8),r2	/* load 2 INIT opcodes */
	bsbw	xmit2		/* xmit 'em */
1:
	mfpr	$CSRD,r7	/* get recv data */
	cmpb	r7,$TU_CONTINUE		/* is it a continue flag? */
	bneq	1b		/* nope, look more */

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
	beql	start			/* dumb operator */
	clrb	(r1)+
	incl	r9

/* user-specified tp filename has been stored at name(fp) */
/* read in entire tp directory contents into low core */
dirred:
	movl	$DIRBLK,tapa(fp)	/* tp directory starts at block DIRBLK */
	movl	$(NUMDIR*BLKSIZ),r6	/* no. bytes in total dir */
	bsbw	taper			/* read no. bytes indicated */

/* search entire directory for user-specified file name */
	clrl	r5			/* dir buff loc = 0 */
nxtdir:
	cmpc3	r9,(r5),(r4)		/* see if dir entry matches filename */
	beql	fndfil			/* found match */
	acbl	$NUMDIR*BLKSIZ-1,$ENTSIZ,r5,nxtdir
					/* see if done with tp dir */
	brw	start			/* entry not in directory; start over */

/* found desired tp dir entry */
fndfil:
	movzwl	BNUM(r5),tapa(fp)	/* start block no., 2 bytes */
	addl2	$DIRBLK-1,tapa(fp)	/* skip boot block(s) */
	movzwl	FILSIZ(r5),r6		/* low 2 bytes file size */
	insv	FILSIZ-1(r5),$16,$8,r6	/* file size, high byte */
	cmpl	r6,$RELOC-512		/* check if file fits below stack */
	blss	filok 			/* file o.k. */
	brw	start			/* file too large */

/* time to read in desired file from tape */
filok:
	movl	r6,r5			/* start of bss space */
	bsbb	taper
	bsbb	rew

/* clear core */
	subl3	r5,$RELOC-4,r0		/* no. bytes to clear */
1:
	clrb	(r5)+
	sobgtr	r0,1b

/* time to jump to start of file & execute */
	addl3	$20,fp,ap
	clrl	r5
	calls	$0,(r5)
bad:
	brw	start

/* rewind tape */
rew:
	movb	$5,readcom+2	/* position opcode */
	clrl	tapa(fp)	/* block 0 */
	clrl	r6		/* 0 bytes */
	bsbb	taper
	movb	$2,readcom+2	/* read opcode */
	rsb

/* read (r6) bytes from tapa(fp) into loc 0 */
taper:
	clrl	r8		/* initialize checksum */
	movab	readcom,r0	/* read command packet addr */
	movzbl	$TU_PACKETLEN/2,r1	/* size of readcom block */
1:
	movzwl	(r0)+,r2	/* get 2 chars from block */
	bsbb	xmit		/* xmit and update ckecksum */
	sobgtr	r1,1b		/* loop if more */

	/* now do variable part of packet */
	movl	r6,r2		/* byte count */
	bsbb	xmit
	movzwl	tapa(fp),r2	/* starting block number */
	bsbb	xmit
	movzwl	r8,r2		/* accumulated ckecksum */
	bsbb	xmit

	/* collect read packet from device */
	clrl	r0		/* starting addr */
1:
	bsbb	recv2		/* get 2 packet characters */
	decb	r2		/* data packet? */
	bneq	1f		/* branch on end of data */
	movzbl	r1,r8		/* get byte count of packet */

	/* read data into memory */
2:
	bsbb	recv1		/* get a char */
	movb	r1,(r0)+	/* stuff into memory */
	sobgtr	r8,2b		/* loop if more */
	bsbb	recv2		/* skip checksum */
	brb	1b		/* read next packet */

	/* end of data xfer; check for errors */
1:
	subl2	r6,r0		/* all bytes xfered? */
	bneq	9f		/* nope, error */
	bsbb	recv2		/* get success code */
	tstl	r1		/* error in read? */
	blss	9f		/* branch if status error */
	movl	$5,r0
1:
	bsbb	recv2		/* discard 10 bytes */
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
	addw2	r2,r8		/* update checksum */
	bcc	xmit2		/* branch if no overflow */
	incw	r8		/* add  in carry */

	/* send the 2 characters contained in r2 */
xmit2:
	bsbb	1f		/* xmit one of 'em */
	ashl	$-8,r2,r2	/* get next char */
				/* fall into... */
1:
	mfpr	$CSTS,r7	/* get xmit status */
	bbc	$TU_READY,r7,1b	/* loop until ready */
	mtpr	r2,$CSTD	/* send char */
	rsb

	/* receive 2 characters, return in r2 and r1 */
recv2:
	bsbb	recv1		/* recv one of 'em */
				/* fall into... */

	/* receive 1 character */
recv1:
	movzbl	r1,r2		/* save previous byte */
1:
	mfpr	$CSRS,r7	/* get recv status */
	bbc	$TU_READY,r7,1b	/* loop until ready */
	mfpr	$CSRD,r1	/* get char */
	blss	9b		/* branch on recv error */
	rsb

getc:
	mfpr	$RXCS,r0
	bbc	$RXCS_pd,r0,getc	/* receiver ready ? */
	mfpr	$RXDB,r0
	extzv	$0,$7,r0,r0
	cmpb	r0,$015
	bneq	putc			/* echo and return */
	bsbb	putc			/* carriage return */
	movb	$0,r0
	bsbb	putc			/* delay */
	movb	$012,r0			/* send line feed and return */
putc:
	mfpr	$TXCS,r2
	bbc	$TXCS_pr,r2,putc	/* transmitter ready ? */
	extzv	$0,$7,r0,r0
	mtpr	r0,$TXDB
	rsb

	.align	2
readcom:
	.byte	2	/* command packet flag */
	.byte	10	/* number of bytes in message */
	.byte	2	/* tu read opcode */
	.byte	0	/* modifier */
	.byte	0	/* unit number */
	.byte	0	/* switches */
	.word	0	/* sequence number */
			/* byte count and block number follow */

ermsg:
	.asciz	"tu58 err\r\n"
end:
