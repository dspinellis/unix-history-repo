#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)21rang.s 4.1 10/10/80";
#
# range checking
#
_RANG2:
	cvtbl	(r10)+,r1
	bneq	l2101
	cvtwl	(r10)+,r1
l2101:
	cmpw	(sp),r1
	blss	erange
	cmpw	(sp),(r10)+
	bgtr	erange
	jmp	(r8)
_RANG24:
	incl	r10
	cvtwl	(sp),r0
	cmpl	r0,(r10)+
	blss	erange
	cmpl	r0,(r10)+
	bgtr	erange
	jmp	(r8)
_RANG42:
	cvtbl	(r10)+,r0
	bneq	l2102
	cvtwl	(r10)+,r0
l2102:
	cvtwl	(r10)+,r1
	cmpl	(sp),r0
	blss	erange
	cmpl	(sp),r1
	bgtr	erange
	jmp	(r8)
_RANG4:
	incl	r10
	cmpl	(sp),(r10)+
	blss	erange
	cmpl	(sp),(r10)+
	bgtr	erange
	jmp	(r8)
erange:
	movw	$ERANGE,_perrno
	jbr	error
_RSNG2:
	cvtbl	(r10)+,r1
	bneq	l2103
	cvtwl	(r10)+,r1
l2103:
	movw	(sp),r0
	blss	erange
	cmpw	r0,r1
	bgtr	erange
	jmp	(r8)
_RSNG24:
	incl	r10
	cvtwl	(sp),r0
	blss	erange
	cmpl	r0,(r10)+
	bgtr	erange
	jmp	(r8)
_RSNG42:
	cvtbl	(r10)+,r1
	bneq	l2104
	cvtwl	(r10)+,r1
l2104:
	movl	(sp),r0
	blss	erange
	cmpl	r0,r1
	bgtr	erange
	jmp	(r8)
_RSNG4:
	incl	r10
	movl	(sp),r0
	blss	erange
	cmpl	r0,(r10)+
	bgtr	erange
	jmp	(r8)
