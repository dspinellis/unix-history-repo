#
# 30read.s
#
# READ OPERATIONS
#
_GET:
	incl	r10
	calls	$0,_iosync	#insure that something is in the window
	bisw2	$SYNC,FUNIT(r7)	#throw it away
	jmp	(r8)
_FNIL:
	incl	r10
	movl	(sp),r0
	bbs	$fWRITE,FUNIT(r0),l3002	#ignore sync of output files
	movl	r7,r2
	movl	_file,r3
	calls	$0,_unit	#do not discard arguement to unit on return
	calls	$0,_iosync
	movl	r2,r7
	movl	r3,_file
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
	bbs	$fEOLN,FUNIT(r7),l3005  #check for already at end of line
	pushal	rdln
	pushl	FBUF(r7)
	calls	$2,_fscanf
l3005:
	bisw2	$SYNC,FUNIT(r7)
	jmp	(r8)
_READC:
	incl	r10
	calls	$0,_iosync
	cvtbw	(r7),-(sp)
	bisw2	$SYNC,FUNIT(r7)
	jmp	(r8)

rd4:	.byte	'%,'l,'d, 0
rd8:	.byte	'%,'l,'f, 0
rdln:	.byte	'%,'*,'[,'^,linefeed,'],'%,'*,'c, 0
