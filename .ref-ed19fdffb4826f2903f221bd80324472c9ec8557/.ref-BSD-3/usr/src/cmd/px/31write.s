#
# 31write.s
#
# WRITE OPERATIONS
#
_PUT:
	incl	r10
	bbc	$fWRITE,FUNIT(r7),ewriteit
	pushl	FBUF(r7)	#stream
	pushl	$1		#number of items
	pushl	FSIZE(r7)	#item size
	pushl	r7		#ptr to data
	calls	$4,_fwrite
cleanup:
	movl	FBUF(r7),r5	#ptr to FILE
	bbs	$ioERR,FLAG(r5),ewrite
	cmpl	r7,$stdout	#check for output to stdout
	bneq	l3101
	tstw	_bufopt		#check for buffering on stdout
	bneq	l3101
	pushl	r5		#if unbuffered then flush
	calls	$1,_fflush
l3101:
	jmp	(r8)
ewriteit:
	movw	$EWRITEIT,_perrno
	jbr	error
ewrite:
	movw	$EWRITE,_perrno
	jbr	error

_WRITEF:
	cvtbl	(r10)+,r6	#r6 has length of format string
	cvtwl	(r10)+,r5	#r5 has number of longword arguements
fentry:
	bbc	$fWRITE,FUNIT(r7),ewriteit
	pushal	(sp)[r5]	#addr of format string
	pushl	FBUF(r7)	#stream
	addl2	$2,r5		#r5 has total number of arguements
	calls	r5,_fprintf	#output formatted data
	addl2	r6,sp		#pop format string
	jbr	cleanup

_WRITLN:
	aobleq	LLIMIT(r7),LCOUNT(r7),l3105
	movw	$ELLIMIT,_perrno
	jbr	error
l3105:
	movw	$linefeed,-(sp)	#push a linefeed
	clrl	r6
	cmpl	r7,$stdout	#check for flushing
	bneq	l3102
	cmpw	$1,_bufopt	#check for eoln flushing
	bneq	l3102
	incl	r6		#set flush request
	brb	l3102
_PAGE:
	movw	$formfeed,-(sp)	#push a formfeed
_WRITEC:
	clrl	r6
l3102:
	incl	r10
	jbc	$fWRITE,FUNIT(r7),ewriteit
	cvtwl	(sp)+,r2	#hold data
	pushl	FBUF(r7)	#stream
	pushl	r2		#push data
	calls	$2,_fputc
	jlbc	r6,cleanup	#if no flush request, normal exit
	movl	FBUF(r7),r5
	jbs	$ioERR,FLAG(r5),ewrite	#check for I/O error
	pushl	r5		#flush
	calls	$1,_fflush
	jmp	(r8)

_WRITES:
	cvtbl	(r10)+,r5	#r5 has length of format string
	cvtwl	(r10)+,r6	#r6 has length of data
sentry:
	jbc	$fWRITE,FUNIT(r7),ewriteit
	addl2	sp,r6		#r6 pts to format string
	pushl	sp		#ptr to data
	pushl	r6		#ptr to format string
	addl2	r5,r6		#r6 points to cleared top of stack
	pushl	FBUF(r7)	#stream
	calls	$3,_fprintf	#output string
	movl	r6,sp		#pop data and format string
	jbr	cleanup

_WRITEB:
	cvtbl	(r10)+,r6	#r6 has length of format string
bentry:
	jbc	$fWRITE,FUNIT(r7),ewriteit
	movw	(sp)+,r0	#push addr of appropriate string
	beql	l3103
	pushal	s_true
	brb	l3104
l3103:
	pushal	s_false
l3104:
	pushal	4(sp)		#addr of format string
	pushl	FBUF(r7)	#stream
	calls	$3,_fprintf	#print boolean
	addl2	r6,sp		#pop format string
	jbr	cleanup

s_true:	.byte	't,'r,'u,'e,linefeed,0
s_false:.byte	'f,'a,'l,'s,'e,linefeed,0
