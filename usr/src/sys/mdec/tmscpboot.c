/*
 *	@(#)tmscpboot.c	7.2 (Berkeley) 1/22/88
 *
 * TK50 tape boot block for distribution tapes
 * works on Q-bus tk50 drive on uVaxen
 *
 * Rick Lindsley
 * richl@tektronix.tek.com
 *
 * reads a program from a tp directory on a tape and executes it
 * program must be stripped of the header and is loaded ``bits as is''
 * you can return to this loader via ``ret'' as you are called ``calls $0,ent''
 */
	.set	RELOC,0x70000
/* tp directory definitions */
	.set	FILSIZ,38	# tp direc offset for file size
	.set	BNUM,44		# tp dir offset for start block no.
	.set	ENTSIZ,64	# size of 1 TP dir entry, bytes
	.set	PTHSIZ,32	# size of TP path name, bytes
	.set	BLKSIZ,512	# tape block size, bytes
	.set	NUMDIR,24	# no. of dir blocks on tape
	.set	ENTBLK,8	# no. of dir entries per tape block
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
	.set	MAPSTART,0x20088000	# for a uVax, anyway
	.set	UBAMEM,0x1ffc2000	# again, for a uVax
	.set	MRV,0x80000000		# map register valid bit
/* TMSCP UBA registers */
	.set	TMSCP_CSR, 0774500	# CSR of tk50
	.set	TMSCPip,0		# initialization and polling
	.set	TMSCPsa,2		# status and address
/* handy values for tmscp communication area */
	.set	TMSCP_OWN,0x80000000
	.set	TMSCP_ERR,0x8000
	.set	TMSCP_STEP4,0x4000
	.set	TMSCP_STEP3,0x2000
	.set	TMSCP_STEP2,0x1000
	.set	TMSCP_STEP1,0x800
	.set	TMSCP_IE,0x80
	.set	TMSCP_GO,1
/* handy offsets into tmscp communication area (from tmscpca) */
	.set	cmdint,4
	.set	rspint,6
	.set	rspdsc,8
	.set	cmddsc,12
/* handy offsets into mscp packets (from %rCMD or %rRSP) */
	.set	msglen,0
	.set	vcid,3
	.set	unit,8
	.set	op,12
	.set	status,14
	.set	modifier,14
	.set	bytecnt,16
	.set	cntflgs,18
	.set	buffer,20
	.set	tmkcnt,20
	.set	lbn,32
	.set	dscptr,40
/* TMSCP commands and modifiers */
	.set	M_OP_STCON,4
	.set	M_OP_ONLIN,9
	.set	M_OP_READ,33
	.set	M_OP_REPOS,37
	.set	M_MD_REWND,2
	.set	M_MD_IMMED,0x80
	.set	M_MD_CLSEX,0x200
	.set	M_ST_MASK,0x1f
	.set	M_ST_TAPEM,14
/* miscellaneous */
	.set	IUR, 0x37
	.set	SID, 0x3e
	.set	VAX_630,8
/* local stack variables */
	.set	tmscpca,-240-PTHSIZ-26	# struct tmscpca (see tmscpreg.h)
	.set	rsp,-240-PTHSIZ-10	# tmscp response area
	.set	cmd,-120-PTHSIZ-10	# tmscp command area
	.set	name,-PTHSIZ-10		# operator-typed file name
	.set	dirread,-10		# is the tape directory incore already?
	.set	mtapa,-8		# cur tape addr (last blk we read)
	.set	tapa,-4			# desired tape addr (inclusive)
/* register usage */
	.set	rCMD,r7
	.set	rRSP,r8
	.set	rUBADDR,r9
	.set	rMAPREGS,r10
	.set	rCSR,r11
/* ===== */

/* initialization */
init:
	#
	# if on a uVax, we were loaded by VMB from tape. We also have
	# only one unibus, at 0x1fffc2000 (see above). Elstwise, this
	# boot program will almost certainly need help.
	#
	mfpr	$SID,r0
	cmpzv	$24,$8,r0,$VAX_630
	beql	1f
	halt
	#
	# We must have been loaded by VMB, and thus we are at a non-zero
	# location.  sp will contain the base address of the area at which
	# we were loaded. So we add sp to $end to get the true end-of-program
	# address.
	#
1:	movl	sp,r6		# r6 - beginning of program
	movl	$RELOC,fp	# core loc to which to move this program
	addl3	$-512,fp,sp	# set stack pointer; leave room for locals
	addl3	$-512,fp,r0	# zero our destination mem .. we start here
	addl3	$end,fp,r1	# and end here
clr:	clrl	(r0)+
	cmpl	r0,r1
	jlss	clr

	movc3	$end,(r6),(fp)	# copy to relocated position
	addl3	$reginit,$RELOC,r0
	jmp	(r0)		# and go there
reginit:
	/* initialize our registers. Should need to do this only once */
	addl3	$UBAMEM, $TMSCP_CSR, %rCSR	# set up CSR register
	movl	$MAPSTART, %rMAPREGS	# locate map registers

	moval	tmscpca(fp), %rUBADDR	# set unibus address for comm area
	extzv	$0,$9,%rUBADDR,%rUBADDR	# format: (MR# << 9) | (&comm & 0x1ff)
	ashl	$-9,$RELOC-512,r0	# setting up map register for our stack
	bisl3	$MRV,r0,(%rMAPREGS)	# mark our stack valid (MR #0)

	moval	cmd(fp),%rCMD		# location of cmd mscp packet
	moval	rsp(fp),%rRSP		# location of rsp mscp packet
	bsbw	inittmscp		# init the unit
	bsbw	onlin			# set tape online
	bsbw	rew			# rewind tape

start:
#ifdef DEBUG
	movzbl	$11,r0			# newline
	bsbw	putc
	movzbl	$13,r0			# return
	bsbw	putc
#endif
	movzbl	$'=,r0			# prompt
	bsbw	putc
	bsbw	getname

	# desired TP filename is in name(fp).  Now read in entire tp directory
	# contents into low core, starting at loc 0. Because tk50's are slow,
	# and because we are going to go over 512 bytes anyway, and because
	# it requires so little effort, we'll keep track of whether the data
	# at location 0 is the tape directory.

	tstw	dirread(fp)	# if directory needs to be read in, do so
	bneq	1f
	bsbw	readdir
1:
	#
	# all of directory is now in locore, @ 0.
	# search for filename; return to start if it isn't there.
	#
	clrl	r0			# start at location 0
nxtdir:	moval	name(fp),r2
	movl	r0,r1
1:	cmpb	(r1),(r2)
	bneq	2f
	tstb	(r1)
	beql	found
	incl	r1
	incl	r2
	brb	1b
2:	acbl	$NUMDIR*BLKSIZ-1,$ENTSIZ,r0,nxtdir
	brw	start			# entry not in directory; start over

	# entry IS here; read it in from tape

found:	movzwl	BNUM(r0),tapa(fp)	# start block no., 2 bytes
	addl2	$2-1,tapa(fp)		# skip over this program (2 blocks)
					# minus 1 because we will read THROUGH
					# this block; so we want to stop just
					# before it
	movzwl	FILSIZ(r0),r4		# low 2 bytes file size
	insv	FILSIZ-1(r0),$16,$8,r4  # file size, high byte
	cmpl	r4,$RELOC-512		# check if file fits below stack
	bgeq	start 			# file too large

	# Now advance to proper place on tape. tapa has our
	# desired address

	clrw	dirread(fp)	# we are about to obliterate our incore copy
				# of the directory
2:	clrl	r3	# rrec expects r3 to point to a buffer. 0 will do ...
	bsbw	rrec
	cmpl	mtapa(fp),tapa(fp)
	blss	2b

	# tape now positioned correctly. Read in program. Number of bytes
	# to read is in r4. We must round up to an even BLKSIZ boundary.
	# Clear the area we are putting it at; unix expects zeroes in its
	# data and bss section.

	addl2	$BLKSIZ-1,r4		# round up
	bicl2	$BLKSIZ-1,r4		# mask out
	movl	r4,r5			# use r5; need to save r4 for later
1:	clrl	(r5)
	sobgtr	r5,1b

	# now read in file.

	clrl	r3			# read into page 0 (incremented by rrec)
	ashl	$-9,r4,r5		# r5 now holds # blks to read
	addl2	r5,tapa(fp)		# compute desired tape blk #
1:	bsbw	rrec
	cmpl	mtapa(fp),tapa(fp)	# got it yet?
	blss	1b

	# begin execution. Call as a function.
	clrl	r5
	calls	$0,(r5)

	# now, since the called function has reset the tape drive for
	# us (!) we must reinit it again ourselves.

	ashl	$-9,$RELOC-512,r0	# set up map register for our stack
	bisl3	$MRV,r0,(%rMAPREGS)	# mark our stack valid (MR #0)
	bsbw	inittmscp		# re-init drive
	bsbw	onlin			# re-online it
	brw	start

	# getname will set name(fp) and leave len(name(fp)) in r6
getname:moval	name(fp),r1		# mov to register for ease of access
nxtc:	bsbw	getc
	cmpb	r0,$012			# end of line?
	beql	nullc
	movb	r0,(r1)+
	brb	nxtc
nullc:	moval	name(fp),r0
	subl3	r0,r1,r6		# length of path name
	jeql	start			# just hit return; nothing useful here
	clrb	(r1)+			# add null at end
	incl	r6			# add null to length
	rsb

getc:	mfpr	$RXCS,r0
	bbc	$RXCS_pd,r0,getc	/* receiver ready ? */
	mfpr	$RXDB,r0
	extzv	$0,$7,r0,r0
	cmpb	r0,$015
	bneq	putc
	bsbw	putc
	movb	$0,r0
	bsbw	putc
	movb	$012,r0

putc:	mfpr	$TXCS,r2
	bbc	$TXCS_pr,r2,putc	/* transmitter ready ? */
	extzv	$0,$7,r0,r0
	mtpr	r0,$TXDB
	rsb

inittmscp:
	movw	$0,TMSCPip(%rCSR)		# start step 1
1:	bitw	$TMSCP_STEP1,TMSCPsa(%rCSR)
	beql	1b
#ifdef DEBUG
	movzbl	$'1,r0
	bsbw	putc
#endif
init2:	movw	$TMSCP_ERR,TMSCPsa(%rCSR)	# start step 2
2:	bitw	$TMSCP_STEP2,TMSCPsa(%rCSR)
	beql	2b
#ifdef DEBUG
	movzbl	$'2,r0
	bsbw	putc
#endif
init3:	addl3	$8,%rUBADDR,r0			# start step 3
	cvtlw	r0,TMSCPsa(%rCSR)
3:	bitw	$TMSCP_STEP3,TMSCPsa(%rCSR)
	beql	3b
#ifdef DEBUG
	movzbl	$'3,r0
	bsbw	putc
#endif
init4:	addl3	$8,%rUBADDR,r0			# start step 4
	ashl	$-16,r0,r0
	cvtlw	r0,TMSCPsa(%rCSR)
4:	bitw	$TMSCP_STEP4,TMSCPsa(%rCSR)
	beql	4b
#ifdef DEBUG
	movzbl	$'4,r0
	bsbw	putc
#endif
setchar:
	movw	$TMSCP_GO,TMSCPsa(%rCSR)
	moval	140(%rUBADDR),tmscpca+cmddsc(fp)
	moval	tmscpca+cmddsc(fp),dscptr(%rCMD)
	movb	$1,vcid(%rCMD)
	moval	20(%rUBADDR),tmscpca+rspdsc(fp)
	moval	tmscpca+rspdsc(fp),dscptr(%rRSP)
	clrw	cntflgs(%rCMD)

	movb	$M_OP_STCON,op(%rCMD)
	clrw	modifier(%rCMD)
	clrl	buffer(%rCMD)
	clrl	bytecnt(%rCMD)
	bsbw	tmscpcmd
#ifdef DEBUG
	movzbl	$'S,r0
	bsbw	putc
#endif
	rsb

tmscpcmd:
	movw	$116,msglen(%rCMD)		# 116 -- size of an mscp packet
	bisl2	$TMSCP_OWN,tmscpca+cmddsc(fp)
	movw	$116,msglen(%rRSP)
	bisl2	$TMSCP_OWN,tmscpca+rspdsc(fp)
	movw	TMSCPip(%rCSR),r0		# start polling
wait:	cvtwl	TMSCPsa(%rCSR),r0
	bitl	$TMSCP_ERR,r0
	beql	1f
	movw	modifier(%rRSP),r1	# so we can read status easily
	halt				# some error or other
1:	tstl	tmscpca+4(fp)
	beql	2f
	clrw	tmscpca+4(fp)
2:	bitl	$TMSCP_OWN,tmscpca+rspdsc(fp)
	bneq	wait

	# cmd done

	clrw	tmscpca+rspint(fp)
	extzv	$0,$5,status(%rRSP),r0
	tstl	r0
	beql	ok			# no errors
	cmpl	$M_ST_TAPEM, r0
	beql	ok			# not an error, just a tape mark
	halt				# some unknown error
ok:	rsb

rew:	movb	$M_OP_REPOS,op(%rCMD)
	movw	$M_MD_REWND|M_MD_IMMED,modifier(%rCMD)
	clrl	buffer(%rCMD)
	clrl	bytecnt(%rCMD)
	bsbw	tmscpcmd
#ifdef DEBUG
	movzbl	$'r,r0			# to indicate r)ewind
	bsbw	putc
#endif
	movl	$-1,mtapa(fp)		# no blocks read yet
	rsb

onlin:	movb	$M_OP_ONLIN,op(%rCMD)
	clrw	modifier(%rCMD)
	clrl	buffer(%rCMD)
	clrl	bytecnt(%rCMD)
	bsbw	tmscpcmd
#ifdef DEBUG
	movzbl	$'O,r0			# to indicate O)nline
	bsbw	putc
#endif
	rsb

	# Read the tp directory. Number of blocks to read is in tapa(fp),
	# and will be read into memory starting at location 0.
readdir:bsbw	rew			# beginning of tape
	addl3	$2,$NUMDIR,tapa(fp)	# blocks to read (skip this 1k program)
	clrl	r3			# using mem starting at 0 as free space
	bsbw	rrec; bsbw rrec		# read and discard first two blocks --
					# those are this program
	bsbw	rrec			# read and discard first tp block
	clrl	r3			# reset starting place
	incw	dirread(fp)		# show that directory is incore
1:	bsbw	rrec
	cmpl	mtapa(fp),tapa(fp)	# done yet?
	blss	1b
	rsb

	# read 1 block from mag tape into page indicated by r3, which will
	# automatically be incremented here. mtapa is also advanced.

rrec:	bisl3	$MRV,r3,4(%rMAPREGS)	# using map register #1
	movl	$BLKSIZ,bytecnt(%rCMD)	# how much to read
	ashl	$9,$1,buffer(%rCMD)	# indicating mr #1. We just happen to
					# be on a page boundary, so filling in
					# the low 9 bits is not necessary.
	movb	$M_OP_READ,op(%rCMD)
	clrw	modifier(%rCMD)
	bsbw	tmscpcmd
#ifdef DEBUG
	movzbl	$'R,r0			# to indicate R)ead a record
	bsbw	putc
#endif
	incl	mtapa(fp)
	incl	r3
	rsb
end:
