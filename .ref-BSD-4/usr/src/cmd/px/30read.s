#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)30read.s 4.2 10/29/80";
#
# READ OPERATIONS
#
_GET:
	incl	r10
	calls	$0,_iosync	#insure that something is in the window
	jbs	$fEOF,FUNIT(r7),eeof
	bisw2	$SYNC,FUNIT(r7)	#throw it away
	jmp	(r8)
eeof:
	movw	$EPASTEOF,_perrno
	jbr	error
_FNIL:
	incl	r10
	movl	(sp),r0
	jbs	$fWRITE,FUNIT(r0),l3002	#ignore sync of output files
	movl	r7,r6
	calls	$0,_unit	#do not discard arguement to unit on return
	calls	$0,_iosync
	jbs	$fEOF,FUNIT(r7),eeof
	movl	r6,r7
	movl	PFNAME(r7),_file
l3002:
	jmp	(r8)
_READ4:
	incl	r10
	calls	$0,_unsync	#prepare input stream
	pushl	$0		#space for answer
	pushl	sp		#ptr to answer space
	pushal	rd4		#ptr to input format
	pushl	FBUF(r7)	#stream
	calls	$3,_fscanf
	cmpl	$1,r0
	bneq	eiread
	bisw2	$SYNC,FUNIT(r7)
	jmp	(r8)
eiread:
	movw	$EBADINUM,_perrno
	jbr	error
_READ8:
	incl	r10
	calls	$0,_unsync	#prepare input stream
	clrd	-(sp)		#space for answer
	pushl	sp		#ptr to answer space
	pushal	rd8		#ptr to input format
	pushl	FBUF(r7)	#stream
	calls	$3,_fscanf
	cmpl	$1,r0
	bneq	efread
	bisw2	$SYNC,FUNIT(r7)
	jmp	(r8)
efread:
	movw	$EBADFNUM,_perrno
	jbr	error
_READLN:
	incl	r10
	calls	$0,_iosync
	jbs	$fEOLN,FUNIT(r7),l3005  #check for already at end of line
	movab	-1024(sp),sp	#temp space
	pushl	FBUF(r7)	#file
	pushl	$1024		#buffer
	pushal	8(sp)		#ptr to buffer
	calls	$3,_fgets
	movab	1024(sp),sp	#pop temp space
l3005:
	jbs	$fEOF,FUNIT(r7),eeof
	bisw2	$SYNC,FUNIT(r7)
	jmp	(r8)
_READC:
	incl	r10
	calls	$0,_iosync
	cvtbw	(r7),-(sp)
	jbs	$fEOF,FUNIT(r7),eeof
	bisw2	$SYNC,FUNIT(r7)
	jmp	(r8)
_READE:
	incl	r10
	calls	$0,_unsync	#push back char if present
	subl2	$bufsze,sp	#allocate space for name
	pushl	sp		#ptr to buffer
	pushal	rden		#format string
	pushl	FBUF(r7)	#FILE ptr
	calls	$3,_fscanf	#read name
	cmpl	r0,$1		#check for valid input
	bneq	entfd
	locc	$0,$bufsze,(sp)	#find size of input
	subl3	r0,$bufsze+1,r6	#r6 has length of input
	addl3	(r10),ap,r5	#r5 points to candidate data
	cvtwl	(r5)+,r4	#r4 has number of candidates
	movaw	2(r5)[r4],r1	#r1 has addr of candidate name list
l3006:
	subw3	(r5)+,(r5),r0	#r0 has candidate length
	cmpw	r0,r6		#check for correct length
	bneq	l3007
	cmpc3	r0,(r1),(sp)	#check for actual match
	beql	l3008
l3007:
	addl2	r0,r1		#update ptr to next candidate
	sobgtr	r4,l3006
entfd:
	addl2	$bufsze,sp	#deallocate buffer
	addl2	$4,r10
	movw	$ENUMNTFD,_perrno
	jbr	error
l3008:
	addl2	$bufsze,sp	#deallocate buffer
	addl3	(r10)+,ap,r0	#r0 has number of cases
	subw3	r4,(r0),-(sp)   #push internal value
	jmp	(r8)

	.set	bufsze,84
rd4:	.asciz	"%ld"
rd8:	.asciz	"%lf"
rden:	.asciz	"%*[ \t\n]%80[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789]",
