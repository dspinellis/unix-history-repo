#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)31write.s 4.1 10/10/80";
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
	movl	FBUF(r7),r5	#ptr to FILE
	bbs	$ioERR,FLAG(r5),ewrite
	jmp	(r8)

_WRITEF:
	cvtbl	(r10)+,r5	#r5 has number of longword arguements
	bbc	$fWRITE,FUNIT(r7),ewriteit
	calls	r5,_fprintf	#output formatted data
	movl	FBUF(r7),r5	#ptr to FILE
	bbs	$ioERR,FLAG(r5),ewrite
	jmp	(r8)

_WRITEC:
	incl	r10
	bbc	$fWRITE,FUNIT(r7),ewriteit
	pushl	FBUF(r7)	#stream
	pushl	4(sp)		#push data
	calls	$3,_fputc
	movl	FBUF(r7),r5
	bbs	$ioERR,FLAG(r5),ewrite	#check for I/O error
	jmp	(r8)
ewriteit:
	movw	$EWRITEIT,_perrno
	jbr	error
ewrite:
	movw	$EWRITE,_perrno
	jbr	error

_WRITES:
	incl	r10
	bbc	$fWRITE,FUNIT(r7),ewriteit
	calls	$4,_fwrite	#output string
	movl	FBUF(r7),r5	#ptr to FILE
	bbs	$ioERR,FLAG(r5),ewrite
	jmp	(r8)

_WRITLN:
	incl	r10
	aobleq	LLIMIT(r7),LCOUNT(r7),l3101
	movw	$ELLIMIT,_perrno
	jbr	error
l3101:
	bbc	$fWRITE,FUNIT(r7),ewriteit
	pushl	FBUF(r7)	#stream
	pushl	$linefeed	#push a linefeed
	calls	$2,_fputc
	movl	FBUF(r7),r5
	bbs	$ioERR,FLAG(r5),ewrite	#check for I/O error
	jmp	(r8)

_PAGE:
	incl	r10
	bbc	$fWRITE,FUNIT(r7),ewriteit
	pushl	FBUF(r7)	#stream
	pushl	$formfeed	#push a formfeed
	calls	$2,_fputc
	movl	FBUF(r7),r5
	jbs	$ioERR,FLAG(r5),ewrite	#check for I/O error
	jmp	(r8)

_NAM:
	incl	r10
	addl3	(r10)+,ap,r6	#r6 points to scalar name list
	movl	(sp)+,r3	#r3 has data value
	cmpw	r3,(r6)+	#check for value out of range
	bgequ	enamrng
	movzwl	(r6)[r3],r4	#r4 has string index
	pushab	(r6)[r4]	#push string ptr
	jmp	(r8)
enamrng:
	movw	$ENAMRNG,_perrno
	jbr	error
_MAX:
	cvtbl	(r10)+,r0	#r0 has width value
	bneq	l3105
	movzwl	(r10)+,r0
l3105:
	movzwl	(r10)+,r1	#r1 has minimum width value
	movl	(sp),r2		#r2 has requested width
	blss	efmt		#check for negative values
	subl2	r0,r2		#shave down value
	cmpl	r1,r2		#check for below minimum width
	bleq	l3106
	movl	r1,(sp)		#force to be at least minimum width
	jmp	(r8)
l3106:
	movl	r2,(sp)		#set to reduced value
	jmp	(r8)
efmt:
	movw	$EFMTSIZE,_perrno
	jbr	error
_MIN:
	cvtbl	(r10)+,r0	#r0 has width value
	bneq	l3107
	movzwl	(r10)+,r0
l3107:
	cmpl	(sp),r0		#check for greater than max allowed
	blss	l3108
	movl	r0,(sp)		#use smaller value
l3108:
	jmp	(r8)
