/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

/* "@(#)htboot.c	7.1 (Berkeley) %G%" */


/*
 * VAX tape boot block for distribution tapes
 * works on massbus tu10/te16/tu45/tu77
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
/* TE16/TU45/TU77 mba registers */
	.set	HTCS1,0		/* HT control 1 reg */
	.set	HTDS,4		/* status reg */
	.set	HTER,8		/* error reg */
	.set	HTAS,16		/* attention summary */
	.set	HTFC,20		/* frame count */
	.set	HTTC,36		/* HT tape control */
/* HT commands */
	.set	GO,1		/* GO bit */
	.set	HT_REW,6	/* rewind, on-line */
	.set	HT_DCLR,010	/* drive clear */
	.set	HT_SREV,032	/* space reverse */
	.set	HT_RCOM,070	/* read forward */
/* HT bits */
	.set	ERR,040000 	/* composite error bit in status reg */
	.set	TCHAR,012300 	/* unit 0, odd parity, PDP11, 1600 BPI NRZ, */
				/* abort on error - for tape controller */
	.set	DRDY,0200	/* HT/drive ready in status reg */
	.set	HT_pd,7 	/* bit position of HT DRDY bit */
	.set	HT_pe,14	/* bit position of HT ERROR bit */
/* local stack variables */
	.set	tapa,-4		/* desired tape addr */
	.set	mtapa,-8	/* current tape addr */
	.set	name,-8-PTHSIZ	/* operator-typed file name */
/* register usage */
	.set	rMBA,r10
	.set	rHT,r11

/* initialization */
init:
	mull2	$0x80,%rHT
	addl2	$0x400,%rHT
	addl2	%rMBA,%rHT
	movl	$RELOC,fp	/* core loc to which to move this program */
	addl3	$name,fp,sp	/* set stack pointer, leaving room for locals */
	clrl	r0
1:
	movc3	$end,(r0),(fp)	/* move boot up to relocated position */
	jmp	start+RELOC
start:
	movl	$1,MBA_CR(%rMBA)	/* MBA init */
	movl	$TCHAR,HTTC(%rHT)	/* drive no., etc. */
	movl	$HT_DCLR+GO,HTCS1(%rHT)	/* drive clear */
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
	movl	$HT_REW+GO,HTCS1(%rHT)  /* rewind */
	rsb
/* rrec: read 1 block from mag tape into page (r0) */
rrec:
	/* pushl r0; movzbl $'r,r0; bsbw putc; movl (sp)+,r0; */
	movl	HTDS(%rHT),r2
	bbc	$HT_pd,r2,rrec		/* HT & drive ready ? */
	movl	$-BLKSIZ,MBA_BCR(%rMBA)
	bisl3	$MRV,r0,MBA_MAP(%rMBA) 
	clrl	MBA_VAR(%rMBA)
	movl	$HT_RCOM+GO,HTCS1(%rHT)	/* read forward */
1:
	movl	HTDS(%rHT),r2
	bbc	$HT_pd,r2,1b
	movl	HTER(%rHT),r2
	bbc	$HT_pe,r2,2f		/* any read errors ? */
	clrl	HTDS(%rHT)		/* clear status - try to recover */
	mnegl	$1,HTFC(%rHT)		/* frame count for backspace */
	movl	$HT_SREV+GO,HTCS1(%rHT)	/* space reverse */
	brb	rrec
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
