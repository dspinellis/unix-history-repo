/*
 * RP??/RM?? disk boot program to load "/boot" from
 * a UNIX filesystem (starting at block 1 on pack on
 * drive 0) into low core and to execute that file.
 * This program can only read regular small version 7 files
 * from the root of a UNIX filesystem.
 */
	.set	BLKSIZ,1024		/* file system block size */
	.set	RELOC,0x70000
	.set	HDRSIZ,040
	.set	INOSIZ,64		/* no. bytes/inode entry */
	.set	INOBLK,BLKSIZ/INOSIZ	/* no. inodes/disc block */
	.set	INOMSK,0xfffffff0	/* changes with inode size */
	.set	NAMSIZ,14		/* bytes in directory name */
	.set	ENTADR,024		/* offset to entry addr in a.out */
	.set	DIRSIZ,16		/* size of directory entry, bytes */
	.set	ROOTINO,2		/* root dir inode no. */
	.set	NBOO,1
	.set	NSUP,1
/* MBA registers */
	.set	M_cr,4			/* MBA control reg */
	.set	M_var,12		/* MBA virt addr reg */
	.set	M_bc,16			/* MBA byte count reg */
	.set	M_map,0x800		/* start of MBA map reg's */
	.set	MBAinit,1		/* MBA init bit in MBA control reg */
/* Drive information */
	.set	RP6TRK,19
	.set	RP6SEC,22
	.set	RM3SEC,32
	.set	RM3TRK,5
	.set	RM5SEC,32
	.set	RM5TRK,19
	.set	RM80SEC,31
	.set	RM80TRK,14
	.set	RP7TRK,32
	.set	RP7SEC,50
	.set	RP6typ,022
	.set	RM3typ,024
	.set	RM5typ,027
	.set	RM80typ,026
	.set	RP7typ,042
	.set	RP,0x400		/* start of drive registers */
	.set	RP_cr,RP+0		/* control status register */
	.set	RP_sr,RP+4		/* drive status reg */
	.set	RP_stk,RP+0x14		/* desired track/sector reg */
	.set	RP_dt,RP+0x18		/* drive type reg */
	.set	RP_off,RP+0x24		/* RP offset reg */
	.set	RP_cyl,RP+0x28		/* desired cyl reg */
/*  RP06 function codes, status bits  */
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
	movl	r5,r11
	movl	$RELOC,sp
	moval	init,r6
	movc3	$end,(r6),(sp)
	jmp	*$RELOC+start1
/* running relocated */
start1:
/* get cpu type */
	.set	SID,0x3e
	mfpr	$SID,r0
	extzv	$24,$8,r0,r0
	ashl	$2,r0,r1
/* get mba location and init it */
	moval	physMBA,r2
	addl3	r1,r2,r3
	movl	(r3),r9
	movl	$MBAinit,M_cr(r9)
/* read-in-preset the drive and set format */
	movl	$RP_RIP+RP_GO,RP_cr(r9)
	movl	$RP_FMT,RP_off(r9) 
/* get drive type */
	movl	RP_dt(r9),r0
	cmpb	$RP6typ,r0; bneq 1f; movzwl $(RP6SEC<<8)|RP6TRK,r1; 1:
	cmpb	$RM3typ,r0; bneq 1f; movzwl $(RM3SEC<<8)|RM3TRK,r1; 1:
	cmpb	$RM5typ,r0; bneq 1f; movzwl $(RM5SEC<<8)|RM5TRK,r1; 1:
	cmpb	$RM80typ,r0; bneq 1f; movzwl $(RM80SEC<<8)|RM80TRK,r1; 1:
	cmpb	$RP7typ,r0; bneq 1f; movzwl $(RP7SEC<<8)|RP7TRK,r1; 1:
	movzbl	r1,*$rptrk
	ashl	$-8,r1,r1
	movzbl	r1,*$rpsec
	mull3	*$rpsec,*$rptrk,*$rpst
start2:
/* search for ``boot'' program in root inode */
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
/* found inode, get desired file */
getfil:
	clrl	bufptr
getlop:
	bsbb	rmblk
	beql	clear
	addl2	$BLKSIZ,bufptr
	brb	getlop
/* clear core and execute program */
clear:
	movl	*$size,r3
clrcor:
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
/* run loaded program */
	clrl	r10			/* major("/dev/hp0a") */
	calls	$0,*$0
	brw	start2
/* iget: get inode whose number is in r0 */
iget:
	addl3	$(INOBLK*(NBOO+NSUP))-1,r0,r8
	divl3	$INOBLK,r8,r4
	bsbw	rblk
	bicl2	$INOMSK,r8
	mull2	$INOSIZ,r8
	addl2	$buf,r8
	movc3	$time-inode,(r8),*$inode
	rsb
/* rmblk: read block bno into addr */
rmblk:
	movzwl	*$bno,r0
	addw2	$3,*$bno
	addl2	$addr,r0
/*  this boot assumes only small files (<=20 blocks) */
	extzv	$0,$24,(r0),r4
	bneq	rblk
	rsb
/* rblk: read block in r4 */
rblk:
	mull2	$BLKSIZ/512,r4
	clrl	r5
	ediv	*$rpst,r4,RP_cyl(r9),r1
	clrl	r2
	ediv	*$rpsec,r1,r1,r0
	insv	r1,$8,$5,r0
	movl	r0,RP_stk(r9)
	movl	$-BLKSIZ,M_bc(r9)
	ashl	$-9,bufptr,r0
	bisl3	$0x80000000,r0,M_map(r9)
	incl	r0
	bisl3	$0x80000000,r0,M_map+4(r9)
	clrl	M_var(r9)
	movl	$RP_RED+RP_GO,RP_cr(r9)
rprdy:
	movl	RP_sr(r9),r0
	bbc	$RP_pDRY,r0,rprdy
	bbs	$RP_pERR,r0,rperr
	bicpsw	$2
	rsb
rperr:
	halt

bufptr:  .long  buf
names:
	.byte	'b,'o,'o,'t,0,0,0,0,0,0,0,0,0,0
	.byte  0

	.align	2
physMBA:
	.long	0
	.long	0x20010000
	.long	0xf28000

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
	.set	rptrk,bno+4
	.set	rpsec,rptrk+4
	.set	rpst,rpsec+4
