/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)tsboot.c	7.1 (Berkeley) 6/5/86" */

/*
 * VAX tape boot block for distribution tapes
 * works on unibus ts11
 *
 * reads a program from a tp directory on a tape and executes it
 * program must be stripped of the header and is loaded ``bits as is''
 * you can return to this loader via ``ret'' as you are called ``calls $0,ent''
 */
	.set	RELOC,0x70000
/* a.out defines */
	.set	HDRSIZ,040	/* size of file header for VAX */
	.set	MAGIC,0410	/* file type id in header */
	.set	TSIZ,4		/* text size */
	.set	DSIZ,8		/* data size */
	.set	BSIZ,12		/* bss size */
	.set	TENT,024	/* task header entry loc */
/*  tp directory definitions */
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
/* UBA registers */
	.set	UBA_DPR1,68
	.set	UBA_MAP,2048
	.set	BNE,0x80000000
	.set	MRV,0x80000000
	.set	MR_BDP1,0x200000
/* TS UBA registers */
	.set	TSDB,0
	.set	TSSR,2
/* TS commands and bits */
	.set	TSA,0x200	/* page 1, ts command buffer relocation */
	.set	TS_ACK,0100000	/* ack packet */
	.set	TS_CVC,040000	/* clear volume check */
	.set	TS_SETCHR,4	/* set characteristics */
	.set	TS_READ,1	/* read */
	.set	TS_RETRY,01000	/* retry, or with read */
	.set	TS_REWIND,02010
/* local stack variables */
	.set	tapa,-4		/* desired tape addr */
	.set	mtapa,-8	/* current tape addr */
	.set	name,-8-PTHSIZ	/* operator-typed file name */
/* register usage */
	.set	rUBA,r10
	.set	rTS,r11
/* ===== */

/* initialization */
init:
	movl	$RELOC,fp	/* core loc to which to move this program */
	addl3	$name,fp,sp	/* set stack pointer; leave room for locals */
	clrl	r0
1:
	movc3	$end,(r0),(fp)	/* move boot up to relocated position */
	jmp	start+RELOC
start:
	ashl	$-9,$RELOC,r0
	bisl3	$MRV,r0,UBA_MAP+4(%rUBA)
	clrw	TSSR(%rTS)
	bsbw	tsquiet
	movw	$TSA+setchr,TSDB(%rTS)
	bsbw	tsquiet
	bsbw	rew			/* rewind input tape */
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
/* user-specified TP filename has been stored at name(fp) */
/* read in entire tp directory contents into low core */
dirred:
	movl	$8,tapa(fp)		/* tp directory starts at block 8 */
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
	addl2	$7,tapa(fp)		/* skip 7 boot blocks */
	movzwl	FILSIZ(r5),r6		/* low 2 bytes file size */
	insv	FILSIZ-1(r5),$16,$8,r6  /* file size, high byte */
	cmpl	r6,$RELOC-512		/* check if file fits below stack */
	blss	filok 			/* file o.k. */
	brw	start			/* file too large */
/* time to read in desired file from tape */
filok:
	movl	r6,r7			/* save r6 */
	bsbb	taper
	bsbw	rew
/* clear core */
	subl3	r7,$RELOC-4,r0		/* no. bytes to clear */
1:
	clrb	(r7)+
	sobgtr	r0,1b
/* time to jump to start of file & execute */
	addl3	$20,fp,ap
	clrl	r5
	calls	$0,(r5)
	brw	start
/* taper: movcTAPE (r6),tapa(fp),0 */
rew2:
	bsbb	rew			/* beginning of tape */
taper0:
	bsbb	rrec			/* advance 1 block; never want blk 0 */
taper:
	clrl	r0			/* page no. */
	cmpl	mtapa(fp),tapa(fp)	/* current position .vs. desired */
	bgtr	rew2
	blss	taper0
1:
	bsbb	rrec
	acbl	$1,$-BLKSIZ,r6,1b
	rsb
/* rew: rewind the tape */
rew:
	clrl	mtapa(fp)		/* current position */
	movw	$TSA+rewind,TSDB(%rTS)
	bsbb	tsquiet
	rsb
/* rrec: read 1 block from mag tape into page (r0) */
rrec:
	/* pushl r0; movzbl $'r,r0; bsbw putc; movl (sp)+,r0; */
	bisl3	$MRV,r0,UBA_MAP(%rUBA)
	movw	$TS_ACK|TS_CVC|TS_READ,tsread
1:
	movw	$TSA+tsread,TSDB(%rTS)
	bsbb	tsquiet
/*	bisl2	$BNE,UBA_DPR1(%rUBA) */
	tstw	TSSR(%rTS)
	bgeq	2f
	bisw2	$TS_RETRY,tsread
	brb	1b
2:
	incl	r0			/* next page no. */
	incl	mtapa(fp)		/* mag tape block position */
	rsb
tsquiet:
	tstb	TSSR(%rTS)
	bgeq	tsquiet
	rsb
getc:
	mfpr	$RXCS,r0
	bbc	$RXCS_pd,r0,getc	/* receiver ready ? */
	mfpr	$RXDB,r0
	extzv	$0,$7,r0,r0
	cmpb	r0,$015
	bneq	putc
	bsbb	putc
	movb	$0,r0
	bsbb	putc
	movb	$012,r0
putc:
	mfpr	$TXCS,r2
	bbc	$TXCS_pr,r2,putc	/* transmitter ready ? */
	extzv	$0,$7,r0,r0
	mtpr	r0,$TXDB
	rsb
	.align	2
setchr:
	.word	TS_ACK|TS_CVC|TS_SETCHR
	.long	TSA+char
	.word	0xe
char:
	.long	TSA+tsstat
	.word	0xe
	.word	0
tsread:
	.word	TS_ACK|TS_CVC|TS_READ
	.long	0
	.word	BLKSIZ
rewind:
	.word	TS_ACK|TS_CVC|TS_REWIND
	.long	0
	.word	0
tsstat:
	.space	7*2
end:
