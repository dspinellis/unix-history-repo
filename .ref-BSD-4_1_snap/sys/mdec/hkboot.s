/*
 * RK07 disk boot program to load "/boot" from
 * a UNIX filesystem (starting at block 1 on pack on
 * drive 0) into low core and to execute that file.
 *
 * This program can only read regular small 1k byte (3bsd+) files
 * from the root of a UNIX filesystem.
 */
	.set	BLKSIZ,1024 		/* file system block size */
	.set	RELOC,0x50000
	.set	INOSIZ,64 		/* no. bytes/inode entry */
	.set	INOBLK,BLKSIZ/INOSIZ	/* no. inodes/disc block */
	.set	INOMSK,0xfffffff0	/* changes with inode size */
	.set	NAMSIZ,14  		/* bytes in directory name */
	.set	ENTADR,024  		/* offset to entry addr in a.out */
	.set	DIRSIZ,16		/* size of directory entry, bytes */
	.set	ROOTINO,2 		/* root dir inode no. */
	.set	NBOO,1
	.set	NSUP,1
	.set	SID,62			/* system ID register */
/* UBA registers */
	.set	UBA_CNFGR,0		/* UBA configuration register */
	.set	UBA_CR,4		/* UBA control register offset */
	.set	UBA_MAP,0x800		/* UBA offset to map reg's */
	.set	UBAinit,1		/* UBA init bit in UBA control reg */
	.set	pUBIC,16		/* Unibus init complete */
/* RK611 registers and bits */
	.set	HK,0177440-0160000	/* address of RK611 */
	.set	HK_cs1,HK+0		/* control and status */
	.set	HK_wc,HK+2		/* word count */
	.set	HK_ba,HK+4		/* bus address */
	.set	HK_da,HK+6		/* disk address */
	.set	HK_dc,HK+020		/* desired cylinder */
	.set	HKSECT,22
	.set	HKTRAC,3
	.set	HKST,HKSECT*HKTRAC
	.set	HK_GO,1			/* go bit */
	.set	HK_PACK,2		/* pack acknowledge */
	.set	HK_RCOM,020		/* read command */
	.set	HK_SEL7,02000		/* select RK07 disk */
	.set	HK_pRDY,7		/* position of ready bit */
	.set	HK_pERR,15		/* position of error bit */

init:
/* r9	UBA address */
/* r10	umem addr */
	.word	0  			/* entry mask for dec monitor */
	nop;nop;nop;nop;nop;nop;nop;nop /* some no-ops for 750 boot to skip */
	nop;nop;
/* get cpu type and find the first uba */
	mfpr	$SID,r0
	extzv	$24,$8,r0,r0		/* get cpu type */
	ashl	$2,r0,r1
	movab	physUBA,r2		/* get physUBA[cpu] */
	addl2	r1,r2
	movl	(r2),r9
	movab	physUMEM,r2		/* get physUMEM[cpu] */
	addl2	r1,r2
	movl	(r2),r10
/* if 780, init uba */
	cmpl	r0,$1
	bneq	2f
	movl	$UBAinit,UBA_CR(r9)
1:
	bbc	$pUBIC,UBA_CNFGR(r9),1b
2:
/* init rk611, set vv in drive 0; if any errors, give up */
	movw	$HK_SEL7+HK_GO,HK_cs1(r10)
1:
	movw	HK_cs1(r10),r0
	bbc	$HK_pRDY,r0,1b
	bbs	$HK_pERR,r0,9f
	movw	$HK_SEL7+HK_PACK+HK_GO,HK_cs1(r10)
1:
	movw	HK_cs1(r10),r0
	bbc	$HK_pRDY,r0,1b
	bbc	$HK_pERR,r0,start
9:
	halt
/* relocate to high core */
start:
	movl	r5,r11			/* boot flags */
	movl	$RELOC,sp
	moval	init,r6
	movc3	$end,(r6),(sp)
	jmp	*$RELOC+start2
/* now running relocated */
/* search for ``boot'' in root inode */
start2:
	movl	$names+RELOC,r6
	movzbl	$ROOTINO,r0
nxti:
	clrw	*$bno
	bsbw	iget
	tstb	(r6)
	beql	getfil
get1b:
	bsbw	rmblk
	beql	start2
	movl	$buf,r7
nxtent:
	tstw	(r7)
	beql	dirchk
	cmpc3	$NAMSIZ,(r6),2(r7)
	bneq	dirchk
	movzwl	(r7),r0
	addl2	$NAMSIZ,r6
	brb	nxti
dirchk:
	acbl	$buf+BLKSIZ-1,$DIRSIZ,r7,nxtent 
	brb	get1b
/* found inode for desired file... read it in */
getfil:
	clrl	bufptr
getlop:
	bsbb	rmblk
	beql	clear
	addl2	$BLKSIZ,bufptr
	brb	getlop
clear:
	movl	*$size,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* run loaded program */
	movl	$3,r10			/* major("/dev/hk0a") */
	calls	$0,*$0
	brw	start2
/* iget: get inode block whose # is in r0 */
iget:
	addl3	$(INOBLK*(NBOO+NSUP))-1,r0,r8
	divl3	$INOBLK,r8,r4
	bsbw	rblk
	bicl2	$INOMSK,r8
	mull2	$INOSIZ,r8
	addl2	$buf,r8
	movc3	$time-inode,(r8),*$inode
	rsb
/* rmblk: read in bno into addr */
rmblk:
	movzwl	*$bno,r0
	addw2	$3,*$bno
	addl2	$addr,r0
/* this boot assumes only small files (<=20 blocks) */
	extzv	$0,$24,(r0),r4
	bneq	rblk
	rsb
/* rblk: read disk block whose number is in r4 */
rblk:
	mull2	$BLKSIZ/512,r4
	clrl	r5
	ediv	$HKST,r4,r0,r1
	movw	r0,HK_dc(r10)
	clrl	r2
	ediv	$HKSECT,r1,r1,r0
	insv	r1,$8,$3,r0
	movw	r0,HK_da(r10)
	movw	$-BLKSIZ/2,HK_wc(r10)
	ashl	$-9,bufptr,r0
	bisl3	$0x80000000,r0,UBA_MAP(r9)
	incl	r0
	bisl3	$0x80000000,r0,UBA_MAP+4(r9)
	clrw	HK_ba(r10)
	movw	$HK_SEL7+HK_RCOM+HK_GO,HK_cs1(r10)
hkrdy:
	movw	HK_cs1(r10),r0
	bbc	$HK_pRDY,r0,hkrdy
	bbs	$HK_pERR,r0,hkerr
	bicpsw	$2
	rsb
hkerr:
	halt			/* ungraceful */
bufptr:	.long  buf
names:	.byte	'b,'o,'o,'t,0,0,0,0,0,0,0,0,0,0
	.byte  0
physUBA:
	.long	0
	.long	0x20006000	/* 11/780 */
	.long	0xf30000	/* 11/750 */
	.long	0xf26000	/* 11/7ZZ */
physUMEM:
	.long	0
	.long	0x2013e000	/* 11/780 */
	.long	0xffe000	/* 11/750 */
	.long	0xffe000	/* 11/7ZZ */
end:
	.set	buf,RELOC-1536
	.set	inode,RELOC-512
	.set	mode,inode
	.set	nlink,mode+2
	.set	uid,nlink+2
	.set	gid,uid+2
	.set	size,gid+2
	.set	addr,size+4
	.set	time,addr+40
	.set	bno,time+12
