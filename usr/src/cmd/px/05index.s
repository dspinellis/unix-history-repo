#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)05index.s 4.1 10/10/80";
#
# OFF, INDEX and NIL
#
_OFF:
	cvtbl	(r10)+,r0
	bneq	l0501
	movzwl	(r10)+,r0
l0501:
	addl2	r0,(sp)
	jmp	(r8)
_INX2:
	cvtbl	(r10)+,r0
	bneq	l0502
	cvtwl	(r10)+,r0	#r0 has size
l0502:
	clrl	r3		#r3 has base subscript
	subw3	(r10)+,(sp)+,r3
	blss	esubscr		#check lower bound
	cmpw	r3,(r10)+	#check upper bound
	bgtru	esubscr
	mull2	r0,r3		#calc byte offset
	addl2	r3,(sp)		#calculate actual address
	jmp	(r8)
_INX4:
	cvtbl	(r10)+,r0
	bneq	l0503
	cvtwl	(r10)+,r0	#r0 has size
l0503:
	cvtwl	(r10)+,r1	#r1 has lower bound
	movzwl	(r10)+,r2	#r2 has upper-lower bound
	subl3	r1,(sp)+,r3	#r3 has base subscript
	cmpl	r3,r2		#check for out of bounds
	bgtru	esubscr
	mull2	r0,r3		#calc byte offset
	addl2	r3,(sp)		#calculate actual address
	jmp	(r8)
esubscr:
	movl	$ESUBSCR,_perrno
	jbr	error
_NIL:
	incl	r10
	tstl	(sp)
	jeql	l0504
	jmp	(r8)
l0504:
	movw	$ENILPTR,_perrno
	jbr	error
_INX4P2:
	cvtbl	(r10)+,r0	#r0 has shift amount
	cvtwl	(r10)+,r2	#r2 has lower bound
	subl3	r2,(sp)+,r1	#r1 has base subscript
	ashl	r0,r1,r1
	addl2	r1,(sp)
	jmp	(r8)
_INX2P2:
	cvtbl	(r10)+,r0	#r0 has shift amount
	cvtwl	(r10)+,r1	#r1 has base array value
	cvtwl	(sp)+,r2	#r2 has subscript value
	subl2	r1,r2		#r2 has element offset
	ashl	r0,r2,r2	#r2 has byte offset
	addl2	r2,(sp)
	jmp	(r8)
