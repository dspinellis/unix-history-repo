/*
 * VAX tape boot block for distribution tapes
 * works on massbus tu78
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
/* tp directory definitions */
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
/* MBA registers */
	.set	MBA_CSR,0	/* configuration and status register */
	.set	MBA_CR,4	/* MBA control reg */
	.set	MBA_SR,8	/* MBA status reg */
	.set	MBA_VAR,12	/* MBA virt addr reg */
	.set	MBA_BCR,16	/* MBA byte count reg */
	.set	MBA_MAP,0x800	/* start of MBA map reg's */
	.set	MRV,0x80000000
	.set	MBA_pdtc,13	/* position of data transfer complete bit */
/* TU78 mba registers */
	.set	MTCS,0		/* MT control reg */
	.set	MTER,4		/* error reg */
	.set	MTCA,8		/* command address */
	.set	MTAS,16		/* attention summary */
	.set	MTBC,20		/* byte count */
	.set	MTNER,44	/* non-data transfer error register */
	.set	MTNCS,48	/* non-data transfer command register */
	.set	MTID,68		/* MT internal data */
/* MT commands */
	.set	GO,1		/* GO bit */
	.set	MT_REW,6	/* rewind, on-line */
	.set	MT_SREV,022	/* space reverse */
	.set	MT_RCOM,070	/* read forward */
/* MT bits */
	.set	CCLR,040000	/* controller clear */
	.set	MT_pr,15	/* bit position of MT TMRDY bit */
/* local stack variables */
	.set	tapa,-4		/* desired tape addr */
	.set	mtapa,-8	/* current tape addr */
	.set	name,-8-PTHSIZ	/* operator-typed file name */
/* register usage */
	.set	rMBA,r10
	.set	rMT,r11

/* initialization */
init:
	mull2	$0x80,%rMT
	addl2	$0x400,%rMT
	addl2	%rMBA,%rMT
	movl	$RELOC,fp	/* core loc to which to move this program */
	addl3	$name,fp,sp	/* set stack pointer, leaving room for locals */
	clrl	r0
1:
	movc3	$end,(r0),(fp)	/* move boot up to relocated position */
	jmp	start+RELOC
start:
	movl	$1,MBA_CR(%rMBA)	/* MBA init */
	movl	$CCLR,MTID(%rMT)	/* controller clear */
	movl	$250,r0
0:	sobgtr	r0,0b
0:	movl	MTID(%rMT),r0		/* wait for ready */
	bbc	$MT_pr,r0,0b
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
	bsbb	rrec			/* advance 1 block; never want blk0 */
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
	clrl	MTNER(%rMT)		/* clear error */
	movl	$MT_REW+GO,MTNCS(%rMT)	/* rewind */
1:
	movl	MTNER(%rMT),r2		/* done? */
	movl	MTAS(%rMT),MTAS(%rMT)
	bicl2	$~077,r2
	cmpl	$1,r2
	bneq	1b
	rsb
/* rrec: read 1 block from mag tape into page (r0) */
rrec:
	/* pushl r0; movzbl $'r,r0; bsbw putc; movl (sp)+,r0; */
rrec2:
	movl	$-BLKSIZ,MBA_BCR(%rMBA)
	movl	$BLKSIZ,MTBC(%rMT)
	bisl3	$MRV,r0,MBA_MAP(%rMBA) 
	clrl	MBA_VAR(%rMBA)
	clrl	MTER(%rMT)
	movl	MBA_SR(%rMBA),MBA_SR(%rMBA)
	movzbl	$4,MTCA(%rMT)		/* drive zero, one record */
	movl	$MT_RCOM+GO,MTCS(%rMT)	/* read forward */
1:
	movl	MTER(%rMT),r2
	bicl2	$~077,r2
	beql	1b
	movl	$1000,r1
0:	sobgtr	r1,0b
	cmpl	$1,r2			/* any read errors ? */
	beql	2f
	 pushl r0; movzbl $'b,r0; bsbw putc; movl (sp)+,r0; 
	brb	rrec2
2:
	incl	r0			/* next page no. */
	incl	mtapa(fp)		/* mag tape block position */
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
end:
